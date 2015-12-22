SELECT D.DeployID, D.DiveID, D.Begin 'DiveBegin', D.Duration, D.MaxDepth 'DiveDepth',
	T.TripID, T.Begin 'TripBegin', T.End 'TripEnd', 
	BM.Species, BM.Sex
FROM Dive D
	JOIN Trip T
		ON D.DeployID = T.DeployID
			AND D.Begin >= T.Begin
			AND D.Begin < T.End
	JOIN DeploymentMetadata DM
		ON DM.DeployID = D.DeployID
	JOIN BirdMetadata BM
		ON BM.DeployID = D.DeployID
WHERE DM.TDRRecovered = 1
	AND DM.GPSRecovered = 1
	AND T.BeginComplete = 1
	AND T.EndComplete = 1
	