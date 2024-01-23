//Source: https://iquilezles.org/articles/distfunctions2d/


//Lines
//=====

//Segment Rounded (Segment)
void SignedDistance2DSegmentRounded_float(float2 p, float2 a, float2 b, float width, out float distance)
{
    float2 pa = p - a, ba = b - a;
	float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
	distance = length(pa - ba * h) - (width * 0.5);
}

//Segment Flat (Oriented Box)
void SignedDistance2DSegmentFlat_float(float2 p, float2 a, float2 b, float width, out float distance)
{
	float l = length(b - a);
	//float2 d = (b - a) / l;
	float2 d = (float2(b.x, a.y) - float2(a.x, b.y)) / l;
	float2 q = (p - (a + b) * 0.5);
	q = mul(float2x2(d.x, -d.y, d.y, d.x), q);
	q = abs(q) - float2(l, width) * 0.5;
	distance = length(max(q, 0.0)) + min(max(q.x, q.y), 0.0);
}

//Circular Shapes
//===============

//Circle
void SignedDistance2DCircle_float(float2 p, float radius, out float distance)
{
    distance = length(p) - radius;
}

//Circle Sector (Pie)
void SignedDistance2DCircleSector_float(float2 p, float radius, float angle, out float distance)
{
    angle = clamp(angle, 0.0, PI);

    float2 c = float2(sin(angle), cos(angle)); //c=sin/cos of angle
    p.x = abs(p.x);
    float l = length(p) - radius;
    float m = length(p - c * clamp(dot(p, c), 0.0,radius));
    distance = max(l, m * sign(c.y * p.x - c.x * p.y));
}

//Circle Segment (Cut Disk)
void SignedDistance2DCircleSegment_float(float2 p, float radius, float height, out float distance)
{
    height = clamp(height, -radius, radius);

    float w = sqrt(radius * radius - height * height); // constant for any given shape
    p.x = abs(p.x);
    float s = max((height - radius) * p.x * p.x + w * w * (height + radius - 2.0 * p.y), height * p.x - w * p.y);
    distance = (s < 0.0) ? length(p) - radius :
           (p.x < w) ? height - p.y :
                       length(p - float2(w, height));
}

//Arc Flat (Ring)
void SignedDistance2DArcFlat_float(float2 p, float radius, float width, float angle, out float distance)
{
    angle = -clamp(angle, 0.0, PI);
    float2 n = float2(cos(angle), sin(angle));

    p.x = abs(p.x);
    p = mul(float2x2(n.x, n.y, -n.y, n.x), p);
    distance = max(abs(length(p) - radius) - width * 0.5,
                   length(float2(p.x, max(0.0, abs(radius - p.y) - width * 0.5))) * sign(p.x));
}

//Arc Rounded (Arc)
void SignedDistance2DArcRounded_float(float2 p, float radius, float width, float angle, out float distance)
{
	width *= 0.5;
    angle = clamp(angle, 0.0, PI);

    float2 sc = float2(sin(angle), cos(angle)); //sc is the sin/cos of the arc's aperture
    p.x = abs(p.x);
    distance = ((sc.y * p.x > sc.x * p.y) ? length(p - sc * radius) : 
                                  abs(length(p) - radius)) - width;
}

//Ellipse
void SignedDistance2DEllipse_float(float2 p, float2 radius, out float distance)
{
	if (abs(radius.x - radius.y) < 0.01)
	{
		SignedDistance2DCircle_float(p, radius.x, distance);
		return;
	}
	if (radius.x <= 0.01)
	{
		SignedDistance2DSegmentRounded_float(p, float2(0.0, -radius.y), float2(0.0, radius.y), 0.0, distance);
		return;
	}
	if (radius.y <= 0.01)
	{
		SignedDistance2DSegmentRounded_float(p, float2(-radius.x, 0.0f), float2(radius.x, 0.0), 0.0, distance);
		return;
	}

	p = abs(p);
	if (p.x > p.y)
	{
		p = p.yx;
		radius = radius.yx;
	}

	float l = radius.y * radius.y - radius.x * radius.x;
	float m = radius.x * p.x / l; float m2 = m * m;
	float n = radius.y * p.y / l; float n2 = n * n;
	float c = (m2 + n2 - 1.0) / 3.0; float c3 = c * c * c;
	float q = c3 + m2 * n2 * 2.0;
	float d = c3 + m2 * n2;
	float g = m + m * n2;
	float co;
	if (d < 0.0)
	{
		float h = acos(q / c3) / 3.0;
		float s = cos(h);
		float t = sin(h) * sqrt(3.0);
		float rx = sqrt(-c * (s + t + 2.0) + m2);
		float ry = sqrt(-c * (s - t + 2.0) + m2);
		co = (ry + sign(l) * rx + abs(g) / (rx * ry) - m) / 2.0;
	}
	else
	{
		float h = 2.0 * m * n * sqrt(d);
		float s = sign(q + h) * pow(abs(q + h), 1.0 / 3.0);
		float u = sign(q - h) * pow(abs(q - h), 1.0 / 3.0);
		float rx = -s - u - c * 4.0 + 2.0 * m2;
		float ry = (s - u) * sqrt(3.0);
		float rm = sqrt(rx * rx + ry * ry);
		co = (ry / sqrt(rm - rx) + 2.0 * g / rm - m) / 2.0;
	}
	float2 r = radius * float2(co, sqrt(1.0 - co * co));
	distance = length(r - p) * sign(p.y - r.y);
}

//Capsule (not available on iquilezles.org)


//Capsule Uneven (Uneven Capsule)
void SignedDistance2DCapsuleUneven_float(float2 p, float2 radius, float height, out float distance)
{
	if (radius.x > height + radius.y)
	{
		SignedDistance2DCircle_float(p, radius.x, distance);
		return;
	}
	if (radius.y > height + radius.x)
	{
		SignedDistance2DCircle_float(p - float2(0.0, height), radius.y, distance);
		return;
	}

	p.x = abs(p.x);
	float b = (radius.x - radius.y) / height;
	float a = sqrt(1.0 - b * b);
	float k = dot(p, float2(-b, a));
	if (k < 0.0)
	{
		distance = length(p) - radius.x;
	}
	else if (k > a * height)
	{
		distance = length(p - float2(0.0, height)) - radius.y;
	}
	else
	{
		distance = dot(p, float2(a, b)) - radius.x;
	}
}


//Triangles
//=========

//Triangle Equilateral (Equilateral Triangle)
void SignedDistance2DTriangleEquilateral_float(float2 p, float len, out float distance)
{
	len *= 0.5;

	const float k = sqrt(3.0);
	p.x = abs(p.x) - len;
	p.y = p.y + len / k;
	if (p.x + k * p.y > 0.0) p = float2(p.x - k * p.y, -k * p.x - p.y) / 2.0;
	p.x -= clamp(p.x, -2.0 * len, 0.0);
	distance = -length(p) * sign(p.y);
}

//Triangle Isosceles (Isosceles Triangle)
void SignedDistance2DTriangleIsosceles_float(float2 p, float2 size, out float distance)
{
	size.x *= 0.5;

	p.x = abs(p.x);
	p.y = -p.y + size.y;
	float2 a = p - size * clamp(dot(p, size) / dot(size, size), 0.0, 1.0);
	float2 b = p - size * float2(clamp(p.x / size.x, 0.0, 1.0), 1.0);
	float s = -sign(size.y);
	float2 d = min(float2(dot(a, a), s * (p.x * size.y - p.y * size.x)),
					float2(dot(b, b), s * (p.y - size.y)));
	distance = -sqrt(d.x) * sign(d.y);
}

//Triangle
void SignedDistance2DTriangle_float(float2 p, float2 a, float2 b, float2 c, out float distance)
{
	float2 e0 = b - a, e1 = c - b, e2 = a - c;
	float2 v0 = p - a, v1 = p - b, v2 = p - c;
	float2 pq0 = v0 - e0 * clamp(dot(v0, e0) / dot(e0, e0), 0.0, 1.0);
	float2 pq1 = v1 - e1 * clamp(dot(v1, e1) / dot(e1, e1), 0.0, 1.0);
	float2 pq2 = v2 - e2 * clamp(dot(v2, e2) / dot(e2, e2), 0.0, 1.0);
	float s = sign(e0.x * e2.y - e0.y * e2.x);
	float2 d = min(min(float2(dot(pq0, pq0), s * (v0.x * e0.y - v0.y * e0.x)),
						float2(dot(pq1, pq1), s * (v1.x * e1.y - v1.y * e1.x))),
						float2(dot(pq2, pq2), s * (v2.x * e2.y - v2.y * e2.x)));
	distance = -sqrt(d.x) * sign(d.y);
}


//Quadrangles
//===========

//Rectangle
void SignedDistance2DRectangle_float(float2 p, float2 size, out float distance)
{
	size *= 0.5;

    float2 d = abs(p) - size;
    distance = length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);
}

//Rectangle Rounded (Rounded Box)
void SignedDistance2DRectangleRounded_float(float2 p, float2 size, float4 radius, out float distance)
{
	size *= 0.5;
	radius = clamp(radius, 0.0, min(size.x, size.y));

	radius.xy = (p.x > 0.0) ? radius.xy : radius.zw;
	radius.x = (p.y > 0.0) ? radius.x : radius.y;
	float2 q = abs(p) - size + radius.x;
	distance = min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - radius.x;
}

//Rhombus
void SignedDistance2DRhombus_float(float2 p, float2 size, out float distance)
{
	size *= 0.5;

	p = abs(p);
	float2 h1 = size - 2.0 * p;
	float h = clamp((h1.x * size.x - h1.y * size.y) / dot(size, size), -1.0, 1.0);
	float d = length(p - 0.5 * size * float2(1.0 - h, 1.0 + h));

	distance = d * sign(p.x * size.y + p.y * size.x - size.x * size.y);
}

//Trapezoid Isosceles (Isosceles Trapezoid)
void SignedDistance2DTrapezoid_float(float2 p, float base, float top, float height, out float distance)
{
	base *= 0.5;
	top *= 0.5;
	height *= 0.5;

	float2 k1 = float2(top, height);
	float2 k2 = float2(top - base, 2.0 * height);
	p.x = abs(p.x);
	float2 ca = float2(p.x - min(p.x, (p.y < 0.0) ? base : top), abs(p.y) - height);
	float2 cb = p - k1 + k2 * clamp(dot(k1 - p, k2) / dot(k2, k2), 0.0, 1.0);
	float s = (cb.x < 0.0 && ca.y < 0.0) ? -1.0 : 1.0;
	distance = s * sqrt(min(dot(ca, ca), dot(cb, cb)));
}

//Parallelogram
void SignedDistance2DParallelogram_float(float2 p, float2 size, float skew, out float distance)
{
	size *= 0.5;

	float2 e = float2(skew, size.y);
	p = (p.y < 0.0) ? -p : p;
	float2 w = p - e; w.x -= clamp(w.x, -size.x, size.x);
	float2 d = float2(dot(w, w), -w.y);
	float s = p.x * e.y - p.y * e.x;
	p = (s < 0.0) ? -p : p;
	float2 v = p - float2(size.x, 0); v -= e * clamp(dot(v, e) / dot(e, e), -1.0, 1.0);
	d = min(d, float2(dot(v, v), size.x * size.y - abs(s)));
	distance = sqrt(d.x) * sign(-d.y);
}


//Polygons
//========

//Pentagon (Regular Pentagon)
void SignedDistance2DPentagon_float(float2 p, float radius, out float distance)
{
	radius *= 0.5;

	float3 k = float3(0.809016994, 0.587785252, 0.726542528);
	p.x = abs(p.x);
	p -= 2.0 * min(dot(float2(-k.x, k.y), p), 0.0) * float2(-k.x, k.y);
	p -= 2.0 * min(dot(float2(k.x, k.y), p), 0.0) * float2(k.x, k.y);
	p -= float2(clamp(p.x, -radius * k.z, radius * k.z), radius);
	distance = length(p) * sign(p.y);
}

//Hexagon (Regular Hexagon)
void SignedDistance2DHexagon_float(float2 p, float radius, out float distance)
{
	radius *= 0.5;

	float3 k = float3(-0.866025404, 0.5, 0.577350269);
	p = abs(p);
	p -= 2.0 * min(dot(k.xy, p), 0.0) * k.xy;
	p -= float2(clamp(p.x, -k.z * radius, k.z * radius), radius);
	distance = length(p) * sign(p.y);
}

//Octogon (Regular Octogon)
void SignedDistance2DOctogon_float(float2 p, float radius, out float distance)
{
	radius *= 0.5;

	float3 k = float3(-0.9238795325, 0.3826834323, 0.4142135623);
	p = abs(p);
	p -= 2.0 * min(dot(float2(k.x, k.y), p), 0.0) * float2(k.x, k.y);
	p -= 2.0 * min(dot(float2(-k.x, k.y), p), 0.0) * float2(-k.x, k.y);
	p -= float2(clamp(p.x, -k.z * radius, k.z * radius), radius);
	distance = length(p) * sign(p.y);
}

//Stars
//=====

//Pentagram (Star 5)
void SignedDistance2DPentagram_float(float2 p, float radius, float m, out float distance)
{
	radius *= 0.5;
	m = clamp(m, 0.0, 1.0);

	const float2 k1 = float2(0.809016994375, -0.587785252292);
	const float2 k2 = float2(-k1.x, k1.y);
	p.x = abs(p.x);
	p -= 2.0 * max(dot(k1, p), 0.0) * k1;
	p -= 2.0 * max(dot(k2, p), 0.0) * k2;
	p.x = abs(p.x);
	p.y -= radius;
	float2 ba = m * float2(-k1.y, k1.x) - float2(0, 1);
	float h = clamp(dot(p, ba) / dot(ba, ba), 0.0, radius);
	distance = length(p - ba * h) * sign(p.y * ba.x - p.x * ba.y);
}

//Hexagram (Hexagram)
void SignedDistance2DHexagram_float(float2 p, float radius, out float distance)
{
	radius *= 0.25;

	const float4 k = float4(-0.5, 0.8660254038, 0.5773502692, 1.7320508076);
	p = abs(p);
	p -= 2.0 * min(dot(k.xy, p), 0.0) * k.xy;
	p -= 2.0 * min(dot(k.yx, p), 0.0) * k.yx;
	p -= float2(clamp(p.x, radius * k.z, radius * k.w), radius);
	distance = length(p) * sign(p.y);
}

//Star (Regular Star)
void SignedDistance2DStar_float(float2 p, float radius, float n, float m, out float distance)
{
	radius *= 0.5;
	n = max(floor(n), 2.0);
	m = clamp(m, 0.0, 1.0);
	m = 2.0 + (m * m * (n - 2.0));

	// next 4 lines can be precomputed for a given shape
	float an = 3.141593 / n;
	float en = 3.141593 / m; //m = [2,n]
	float2 acs = float2(cos(an), sin(an));
	float2 ecs = float2(cos(en), sin(en)); //ecs = float2(0, 1) for regular polygon

	float bn1 = atan2(p.x, p.y);
	float bn2 = 2.0 * an;
	float bn = (bn1 - bn2 * floor(bn1 / bn2)) - an;
	p = length(p) * float2(cos(bn), abs(sin(bn)));
	p -= radius * acs;
	p += ecs * clamp(-dot(p, ecs), 0.0, radius * acs.y / ecs.y);
	distance = length(p) * sign(p.x);
}


//Crosses
//======

//Cross Flat (Cross)
void SignedDistance2DCrossFlat_float(float2 p, float size, float width, out float distance)
{
	size *= 0.5;
	width *= 0.5;

	p = abs(p); p = (p.y > p.x) ? p.yx : p.xy;
	float2 b = float2(size, width);
	float2 q = p - b;
	float k = max(q.y, q.x);
	float2 w = (k > 0.0) ? q : float2(b.y - p.x, -k);
	distance = sign(k) * length(max(w, 0.0));
}

//Cross Rounded (Rounded X)
void SignedDistance2DCrossRounded_float(float2 p, float size, float radius, out float distance)
{
	size -= radius;
	//size *= 0.5;

	p = abs(p);
	distance = length(p - min(p.x + p.y, size) * 0.5) - radius;
}

//Cross Circle (Circle Cross)
void SignedDistance2DCrossCircle_float(float2 p, float2 size, out float distance)
{
	size *= 0.5;
	if (size.y > size.x)
	{
		p = p.yx;
		size = size.yx;
	}
	if (size.y < 0.00001)
	{
		SignedDistance2DSegmentRounded_float(p, float2(-size.x, 0.0), float2(size.x, 0.0), 0.0, distance);
		return;
	}

	float k = size.x * 0.5 * ((size.y/size.x) + (size.x / size.y));
	p = abs(p);
	float2 a = p - float2(0, size.y);
	float2 b = p - float2(size.x, 0);
	distance = (p.x < size.x && p.y < p.x * ((k - size.y) / size.x) + size.y) ?
		k - length(p - float2(size.x, k)) :
		sqrt(min(dot(a, a),	dot(b, b)));
}

//Cross Bobbly (Bobbly Cross)
void SignedDistance2DCrossBlobby_float(float2 p, float size, float he, out float distance)
{
	size *= 0.5;
	if (size < 0.000001)
	{
		distance = length(p);
		return;
	}

	p = abs(p) / size;
	p = float2(abs(p.x - p.y), 1.0 - p.x - p.y) / sqrt(2.0);

	float p1 = (he - p.y - 0.25 / he) / (6.0 * he);
	float q1 = p.x / (he * he * 16.0);
	float h = q1 * q1 - p1 * p1 * p1;

	float x;
	if (h > 0.0) { float r = sqrt(h); x = pow(q1 + r, 1.0 / 3.0) - pow(abs(q1 - r), 1.0 / 3.0) * sign(r - q1); }
	else { float r = sqrt(p1); x = 2.0 * r * cos(acos(q1 / (p1 * r)) / 3.0); }
	x = min(x, sqrt(2.0) / 2.0);

	float2 z = float2(x, he * (1.0 - 2.0 * x * x)) - p;
	distance = length(z) * sign(z.y) * size;
}


//Misc
//====

//Vesica
//Vesica Oriented (Oriented Vesica)
//Horseshoe
//Moon
//Egg Simple (Simple Egg)
//Heart

//Polygon
//Parabola
//Parabola Segment
//Bezier Quadratic (Quadratic Bezier)
//Tunnel
//Stairs
//Quadratic Circle
//Hyperbola
//Cool S
//Circle Wave