void SDFDistanceToShape_float(float distance, out float shape)
{
    shape = saturate(-sign(distance));
}


void SDFDistancePreview_float(float distance,  float3 colorPositive, float3 colorNegative, out float3 color)
{
	color = (distance > 0.0) ? colorPositive : colorNegative;
	color *= 1.0 - exp(-4.0 * abs(distance));
	color *= 0.8 + 0.2 * cos(80.0 * distance);
	color = lerp(color, 1.0, 1.0 - smoothstep(0.0, 0.01, abs(distance)));
}