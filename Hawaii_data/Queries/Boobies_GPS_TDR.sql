SELECT B.DeployID, B.FieldID, B.Species,
	D.GPSRecovered, D.TDRRecovered
FROM BirdMetadata B JOIN
	DeploymentMetadata D ON 
		B.DeployID = D.DeployID JOIN
	
	
WHERE Species IN ("BRBO", "RFBO") AND
	GPSRecovered IN (1, 3) AND
	TDRRecovered IN (1, 3)