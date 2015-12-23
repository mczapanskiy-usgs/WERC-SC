SELECT D.DeployID, D.DiveID, D.Begin 'DiveBegin', D.Duration, D.MaxDepth 'DiveDepth',
	RT.UTC, RT.Latitude, RT.Longitude,
	T.TripID, 
	BM.Species, BM.Sex
FROM Dive D
	JOIN RediscretizedTrack RT
		ON D.DeployID = RT.DeployID
			AND D.Begin >= RT.UTC
			AND D.Begin < RT.UTC + 180 -- rediscretization interval
	JOIN Trip T
		ON D.DeployID = T.DeployID
			AND RT.TripID = T.TripID
	JOIN DeploymentMetadata DM
		ON DM.DeployID = D.DeployID
	JOIN BirdMetadata BM
		ON BM.DeployID = D.DeployID
WHERE DM.TDRRecovered = 1
	AND DM.GPSRecovered = 1
	AND T.BeginComplete = 1
	AND T.EndComplete = 1
	