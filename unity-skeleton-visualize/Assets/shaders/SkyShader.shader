Shader "Custom/Sky"
{
	Properties
	{
	
    	_UpDir("Up Direction", Vector) = (0.0, 1.0, 0.0)
		[Space]
		_x_var("x var", Range(0,100)) = 0
		_mod_var("mod var", Range(0,100)) = 0
		_baseColor("main color", Color) = (1.0, 1.0, 1.0, 0)
		_gridColor("grid color", Color) = (1.0, 0.0, 0.0, 0.0)
	}
	SubShader
	{
		Tags
		{
			"PreviewType" = "Plane"
		}
		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#include "UnityCG.cginc"

			struct VertIn
			{
				float4 vertex : POSITION;
			};
	
			float3 _UpDir;

            float _x_var;
            float _mod_var;
			float4 _baseColor;
			float4 _gridColor;
			float4 _screenPosition;
		
			struct VertOut
			{
				float4 position : SV_POSITION;
				float4 worldPosition : TEXCOORD0;
			};

			VertOut vert(VertIn v)
			{
				VertOut o;

				o.position = UnityObjectToClipPos(v.vertex);
				o.worldPosition = mul(unity_ObjectToWorld, v.vertex);
				_screenPosition = ComputeScreenPos(o.position);

				return o;
			}

			float4 frag(VertOut i) : SV_Target
			{
			
			float3 fragDir = normalize(i.worldPosition - _WorldSpaceCameraPos);
            float3 upDir = normalize(_UpDir);
            
            float elevation = dot(fragDir, upDir); // -1..1

            float3 camDir = normalize(-UNITY_MATRIX_V[2].xyz);
            float3 camUpDir = normalize(-UNITY_MATRIX_V[1].xyz);
            // Prevent vertical cam vector when looking straight up or down.
            // This would lead to a zero-length vector after being horizontally projected,
            // resulting in NAN when normalized.
            float3 camForwardishDir = camDir + camUpDir * elevation;

            float3 camDirH = normalize(cross(cross(upDir, camForwardishDir), upDir));
            float3 lightDir = normalize(_WorldSpaceLightPos0);
            float3 lightDirH = normalize(cross(cross(upDir, lightDir), upDir));

            float3 camLightHMatch = dot(camDirH, lightDirH); // -1..1
            camLightHMatch = (1 + camLightHMatch) / 2; // 0..1

			
			float4 returnColor = float4(i.worldPosition.xyzw * _baseColor.rgba); 
				
				
			return i.worldPosition.x /  i.worldPosition.w; 
			}
			ENDCG
		}
	}
}