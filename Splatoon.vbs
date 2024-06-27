Option Explicit
LightsOut()
Attract
Dr1 = 0
Dr3 = 0
Dr4 = 0
Dr5 = 0
Dr6 = 0
Dr7 = 0
Dr8 = 0
Dr9 = 0
Dr10 = 0
Dr2 = 0
DrTimer.enabled = 1
R6.State = 0
R7.State = 0
R8.State = 0
R5.State = 0
R1.State = 0
R2.State = 0
R3.State = 0
R4.State = 0
Dim BIG
BIG = 0

Dim QRr
Qrr = True

Dim Multi 
Multi = 0

Dim MultiBIP
MultiBIP = False

Dim Tacount
Tacount = 0

Dim TKO
TKO = 0

Dim BKO 
BKO = 0

Dim RKO 
RKO = 0


Dim ll1r
ll1r = 7

Dim ll2r
ll2r = 7

Dim ll3r
ll3r = 7

Dim ll4r
ll4r = 7

Dim ll5r
ll5r = 7

Dim pass
pass = 0

Dim LUP
LUP = 0

Dim lane1l
lane1l = ll1.State

Dim lane2l
lane1l = ll2.State

Dim lane3l
lane1l = ll3.State

Dim lane4l
lane1l = ll4.State

Dim lane5l
lane1l = ll5.State

Dim lane1lb
lane1lb = False

Dim lane2lb
lane1lb = False

Dim lane3lb
lane1lb = False

Dim lane4lb
lane1lb = False

Dim lane5lb
lane1lb = False


Dim levelup
levelup = 0

Dim BIP
BIP = 0

Dim a1
a1 = 0

Dim a2
a2 = -1

Dim a3 
a3 = -2

Dim ss
ss = 0

Dim no1p
no1p = -29

Dim qsj1p
qsj1p = -32

Dim qr1p
qr1p = -32

Dim rn1p
rn1p = -32

Dim spp1p
spp1p = -32

Dim sps1p
sps1p = -32


Dim sbp1p
sbp1p = -32

Dim sbs1p
sbs1p = -32

Dim sbr1p
sbr1p = -32

Dim mns1p
mns1p = -32

Dim rec1p
rec1p = -32

Dim rez1p
rez1p = -32

Dim int1p
int1p = -32

Dim ssp1p
ssp1p = -32

Dim run1p
run1p = -32







Dim no2p
no2p = -29

Dim qsj2p
qsj2p = -32

Dim qr2p
qr2p = -32

Dim rn2p
rn2p = -32

Dim spp2p
spp2p = -32

Dim sps2p
sps2p = -32


Dim sbp2p
sbp2p = -32

Dim sbs2p
sbs2p = -32

Dim sbr2p
sbr2p = -32

Dim mns2p
mns2p = -32

Dim rec2p
rec2p = -32

Dim rez2p
rez2p = -32

Dim int2p
int2p = -32

Dim ssp2p
ssp2p = -32

Dim run2p
run2p = -32


Dim Bonus
Bonus = 0



Dim no3p
no3p = -29

Dim qsj3p
qsj3p = -32

Dim qr3p
qr3p = -32

Dim rn3p
rn3p = -32

Dim spp3p
spp3p = -32

Dim sps3p
sps3p = -32


Dim sbp3p
sbp3p = -32

Dim sbs3p
sbs3p = -32

Dim sbr3p
sbr3p = -32

Dim mns3p
mns3p = -32

Dim rec3p
rec3p = -32

Dim rez3p
rez3p = -32

Dim int3p
int3p = -32

Dim ssp3p
ssp3p = -32

Dim run3p
run3p = -32

Dim IsBonus
IsBonus = False

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

'Const cGameName = Splatoon 3

If Splatoon3.ShowDT = false then
    Scoretext.Visible = false
End If


Dim EnableBallControl
EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys

Const BallSize = 25  'Ball radius

Dim Score
Score = 0

Sub AddScore(points)
If two.State = 0 And three.State = 0 Then
 'we also need to Dim Score in the beginning of script. all Variables should go in the beginning of script.
Score = Score + points ' This adds your score + the points in the (Brackets) when something is hit & it contains AddScore(#)
ScoreText.Text = FormatNumber(Score, 0, -1, 0, -1) 'this Displays the points added in scoretext.Text box on the backdrop
Uhigh
Else
If two.State = 2 Then
 'we also need to Dim Score in the beginning of script. all Variables should go in the beginning of script.
Score = Score + points * 2 ' This adds your score + the points in the (Brackets) when something is hit & it contains AddScore(#)
ScoreText.Text = FormatNumber(Score, 0, -1, 0, -1) 'this Displays the points added in scoretext.Text box on the backdrop
Uhigh
Else
If three.State = 2 Then
 'we also need to Dim Score in the beginning of script. all Variables should go in the beginning of script.
Score = Score + points * 3 ' This adds your score + the points in the (Brackets) when something is hit & it contains AddScore(#)
ScoreText.Text = FormatNumber(Score, 0, -1, 0, -1) 'this Displays the points added in scoretext.Text box on the backdropUhigh
Uhigh
Else
End If
End If 
End If
End Sub

Sub AddBonus(p)
Bonus = Bonus + p 
Gauge
End Sub

Sub Gauge()
If Bonus >= 50000 Then 
g000.State = 1
isBonus = True
End If
If Bonus >= 100000 Then 
g001.State = 1
End If
If Bonus >= 150000 Then 
g002.State = 1
End If
If Bonus >= 200000 Then 
g003.State = 1
End If
If Bonus >= 250000 Then 
g004.State = 1
End If
If Bonus >= 300000 Then 
g005.State = 1
End If
If Bonus >= 350000 Then 
g006.State = 1
End If
If Bonus >= 400000 Then 
g007.State = 1
End If
If Bonus >= 450000 Then 
g008.State = 1
End If
If Bonus >= 500000 Then 
g009.State = 1
End If
If Bonus >= 550000 Then 
g010.State = 1
End If
If Bonus >= 600000 Then 
g011.State = 1
End If
If Bonus >= 650000 Then 
g012.State = 1
End If
If Bonus >= 700000 Then 
g013.State = 1
End If
If Bonus >= 750000 Then 
g014.State = 1
End If
If Bonus >= 800000 Then 
g015.State = 1
End If
If Bonus >= 850000 Then 
g016.State = 1
End If
If Bonus >= 900000 Then 
g017.State = 1
End If
If Bonus >= 950000 Then 
g018.State = 1
End If
If Bonus >= 1000000 Then 
g019.State = 1
End If
If Bonus >= 1050000 Then 
g020.State = 1
End If
If Bonus >= 1100000 Then 
g021.State = 1
End If
If Bonus >= 1150000 And g022.State = 0 Then 
g022.State = 1
Playsound "special"
multb.State = 2
End If
End Sub

Sub SBonus
If g022.State = 1 And Multi = 0 Then
g022.Timerenabled = 1
Else
If g021.State = 1 And Multi = 0 Then
g021.Timerenabled = 1
Else
If g020.State = 1 And Multi = 0 Then
g020.Timerenabled = 1
Else
If g019.State = 1 And Multi = 0 Then
g019.Timerenabled = 1
Else
If g018.State = 1 And Multi = 0 Then
g018.Timerenabled = 1
Else
If g017.State = 1 And Multi = 0 Then
g017.Timerenabled = 1
Else
If g016.State = 1 And Multi = 0 Then
g016.Timerenabled = 1
Else
If g015.State = 1 And Multi = 0 Then
g015.Timerenabled = 1
Else
If g014.State = 1 And Multi = 0 Then
g014.Timerenabled = 1
Else
If g013.State = 1 And Multi = 0 Then
g013.Timerenabled = 1
Else
If g012.State = 1 And Multi = 0 Then
g012.Timerenabled = 1
Else
If g011.State = 1 And Multi = 0 Then
g011.Timerenabled = 1
Else
If g010.State = 1 And Multi = 0 Then
g010.Timerenabled = 1
Else
If g009.State = 1 And Multi = 0 Then
g009.Timerenabled = 1
Else
If g008.State = 1 And Multi = 0 Then
g008.Timerenabled = 1
Else
If g007.State = 1 And Multi = 0 Then
g007.Timerenabled = 1
Else
If g006.State = 1 And Multi = 0 Then
g006.Timerenabled = 1
Else
If g005.State = 1 And Multi = 0 Then
g005.Timerenabled = 1
Else
If g004.State = 1 And Multi = 0 Then
g004.Timerenabled = 1
Else
If g003.State = 1 And Multi = 0 Then
g003.Timerenabled = 1
Else
If g002.State = 1 And Multi = 0 Then
g002.Timerenabled = 1
Else
If g001.State = 1 And Multi = 0 Then
g001.Timerenabled = 1
Else
If g000.State = 1 And Multi = 0 Then
g000.Timerenabled = 1
Else


End If

End If

End If

End If

End If

End If

End If

End If

End If
End If
End If
End If
End If
End If
End If
End If
End If
End If
End If
End If
End If
End If
End If
End Sub
Sub g022_Timer
AddScore(50000)
PlaySound "dring"
g022.State = 0
g021.Timerenabled = 1
g022.Timerenabled = 0
Uhigh
End Sub

Sub g021_Timer
AddScore(50000)
PlaySound "dring"
g021.State = 0
g020.Timerenabled = 1
g021.Timerenabled = 0
Uhigh
End Sub

Sub g020_Timer
AddScore(50000)
PlaySound "dring"
g020.State = 0
g019.Timerenabled = 1
g020.Timerenabled = 0
Uhigh
End Sub

Sub g019_Timer
AddScore(50000)
PlaySound "dring"
g019.State = 0
g018.Timerenabled = 1
g019.Timerenabled = 0
Uhigh
End Sub

Sub g018_Timer
AddScore(50000)
PlaySound "dring"
g018.State = 0
g017.Timerenabled = 1
g018.Timerenabled = 0
Uhigh
End Sub

Sub g017_Timer
AddScore(50000)
PlaySound "dring"
g017.State = 0
g016.Timerenabled = 1
g017.Timerenabled = 0
Uhigh
End Sub

Sub g016_Timer
AddScore(50000)
PlaySound "dring"
g016.State = 0
g015.Timerenabled = 1
g016.Timerenabled = 0
Uhigh
End Sub

Sub g015_Timer
AddScore(50000)
PlaySound "dring"
g015.State = 0
g014.Timerenabled = 1
g015.Timerenabled = 0
Uhigh
End Sub

Sub g014_Timer
AddScore(50000)
PlaySound "dring"
g014.State = 0
g013.Timerenabled = 1
g014.Timerenabled = 0
Uhigh
End Sub

Sub g013_Timer
AddScore(50000)
PlaySound "dring"
g013.State = 0
g012.Timerenabled = 1
g013.Timerenabled = 0
Uhigh
End Sub

Sub g012_Timer
AddScore(50000)
PlaySound "dring"
g012.State = 0
g011.Timerenabled = 1
g012.Timerenabled = 0
Uhigh
End Sub

Sub g011_Timer
AddScore(50000)
PlaySound "dring"
g011.State = 0
g010.Timerenabled = 1
g011.Timerenabled = 0
Uhigh
End Sub

Sub g010_Timer
AddScore(50000)
PlaySound "dring"
g010.State = 0
g009.Timerenabled = 1
g010.Timerenabled = 0
Uhigh
End Sub

Sub g009_Timer
AddScore(50000)
PlaySound "dring"
g009.State = 0
g008.Timerenabled = 1
g009.Timerenabled = 0
Uhigh
End Sub

Sub g008_Timer
AddScore(50000)
PlaySound "dring"
g008.State = 0
g007.Timerenabled = 1
g008.Timerenabled = 0
Uhigh
End Sub

Sub g007_Timer
AddScore(50000)
PlaySound "dring"
g007.State = 0
g006.Timerenabled = 1
g007.Timerenabled = 0
Uhigh
End Sub

Sub g006_Timer
AddScore(50000)
PlaySound "dring"
g006.State = 0
g005.Timerenabled = 1
g006.Timerenabled = 0
Uhigh
End Sub

Sub g005_Timer
AddScore(50000)
PlaySound "dring"
g005.State = 0
g004.Timerenabled = 1
g005.Timerenabled = 0
Uhigh
End Sub

Sub g004_Timer
AddScore(50000)
PlaySound "dring"
g004.State = 0
g003.Timerenabled = 1
g004.Timerenabled = 0
Uhigh
End Sub

Sub g003_Timer
AddScore(50000)
PlaySound "dring"
g003.State = 0
g002.Timerenabled = 1
g003.Timerenabled = 0
Uhigh
End Sub

Sub g002_Timer
AddScore(50000)
PlaySound "dring"
g002.State = 0
g001.Timerenabled = 1
g002.Timerenabled = 0
Uhigh
End Sub

Sub g001_Timer
AddScore(50000)
PlaySound "dring"
g001.State = 0
g000.Timerenabled = 1
g001.Timerenabled = 0
Uhigh
End Sub

Sub g000_Timer
PlaySound "dring"
AddScore(50000)
g000.State = 0
IsBonus = False
g000.Timerenabled = 0
AddBonus(0)
Uhigh
If BIP = 0 And tl1.State = 0 Then
Attract
End If
End Sub

Sub Uhigh
nscore = Score
If nscore > hscore Then
hscore = nscore
high.Text = FormatNumber(hscore, 0, -1, 0, -1)
If pass = 0 Then 
Playsound "knocker"
pass = 1
End If
End If

End Sub



Dim credits
credits = 0

Sub Addcredits(creds) 'we also need to Dim Score in the beginning of script. all Variables should go in the beginning of script.
credits = credits + creds ' This adds your score + the points in the (Brackets) when something is hit & it contains AddScore(#)
coin.Text = FormatNumber(credits, 0, -1, 0, -1) 'this Displays the points added in scoretext.Text box on the backdrop
End Sub

Dim hpop
hpop = 33

Dim cko
cko = 100

Sub hitshield(shots)
hpop = hpop - shots
hits.text = FormatNumber(hpop, 0, -1, 0, -1)
End Sub

Sub hitbasket(clams)
cko = cko - clams
If Cko < 1 Then
cko = 100
Addscore(153210)
Playsound "DRNK"
animation.UpdateInterval = 8
	animation.Play SeqRadarRightOn,20,1
BKO = 1
xob
End If
chits.text = FormatNumber(cko, 0, -1, 0, -1)
End Sub



Dim hscore
hscore  = 3333333

Dim nscore

high.Text = FormatNumber(hscore, 0, -1, 0, -1)

Dim tilts
tilts = 0

Dim TTL
TTL = 1

Sub Splatoon3_KeyDown(ByVal keycode)

	If keycode = AddCreditKey Then
	Addcredits(.5)
	PlaySound "fx_coin",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	PlaySound "whosh"
	End If

	If keycode = StartGameKey and credits > .6  And BIG = 0 And IsBonus = False Then
	PlaySound "crack"
	Addcredits(-1)
	NewGame()
	End If



	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySound "plungerpull"
		PlaySound "chargec"
	End If

	If keycode = LeftFlipperKey And TTL = 0 And BIP > 0 And Multi = 0 Then
		LeftFlipper.RotateToEnd
		PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
		LF1.RotateToEnd
llleft

	End If
	If keycode = RightFlipperKey And TTL = 0 And BIP > 0 And Multi = 0 Then
		RightFlipper.RotateToEnd
		PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
		RF1.RotateToEnd

llright

End If





	If keycode = LeftTiltKey Then
		Nudge 90, 2
		If BIP > 0 Then
		tilts = tilts + 1
		Timer1.Enabled = 1
		End If
		Tilt()
	End If
	
	If keycode = RightTiltKey Then
		Nudge 270, 2
		If BIP > 0 Then
		tilts = tilts + 1
		Timer1.Enabled = 1
		End If
		Tilt()
	End If

	If keycode = CenterTiltKey Then
		Nudge 0, 2
	If BIP > 0 Then
		tilts = tilts + 1
		Timer1.Enabled = 1
		End If
		Tilt()
	End If

    ' Manual Ball Control
	If keycode = 46 Then	 				' C Key
		If EnableBallControl = 1 Then
			EnableBallControl = 0
		Else
			EnableBallControl = 1
		End If
	End If
    If EnableBallControl = 1 Then
		If keycode = 48 Then 				' B Key
			If BCboost = 1 Then
				BCboost = BCboostmulti
			Else
				BCboost = 1
			End If
		End If
		If keycode = 203 Then BCleft = 1	' Left Arrow
		If keycode = 200 Then BCup = 1		' Up Arrow
		If keycode = 208 Then BCdown = 1	' Down Arrow
		If keycode = 205 Then BCright = 1	' Right Arrow
	End If
End Sub

Sub Timer1_Timer
If tilts > 0 Then
tilts = tilts - 1
Timer1.Enabled = 0
End If

End Sub
Sub TILT
If tilts = 3 Then
TTL = 1
PlaySound "error"
ScoreText.Text = "TILT!"
LightsOut()
jpo1.Enabled = False
jpo2.Enabled = False
R6.State = 0
R7.State = 0
R8.State = 0
R5.State = 0
R1.State = 0
R2.State = 0
R3.State = 0
R4.State = 0
DF1.RotateToStart
LeftFlipper.RotateToStart
RightFlipper.RotateToStart
RF1.RotateToStart
LF1.RotateToStart
g000.State = 0
IsBonus = False
g001.State = 0
g002.State = 0
g003.State = 0
g004.State = 0
g005.State = 0
g006.State = 0
g007.State = 0
g008.State = 0
g009.State = 0
g010.State = 0
g011.State = 0
g012.State = 0
g013.State = 0
g014.State = 0
g015.State = 0
g016.State = 0
g017.State = 0
g018.State = 0
g019.State = 0
g020.State = 0
g021.State = 0
g022.State = 0
Bonus = 0
AddBonus(0)
StopMusic()

End If
End Sub

Sub ball()
ba.Text = FormatNumber(BIG, 0, -1, 0, -1)
End Sub


Sub bali()
bap.Text = FormatNumber(BIP, 0, -1, 0, -1)
End Sub

Sub Splatoon3_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySound "plunger"
		PlaySound "directc"
If BIP > 0 And ss = 1 And TTL = 0 Then
		animation.UpdateInterval = 8
    animation.Play SeqUpOn,75,0
End If
	End If

	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToStart
		PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
		LF1.RotateToStart
	End If

	If keycode = RightFlipperKey Then
		RightFlipper.RotateToStart
		PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
		RF1.RotateToStart

	End If

    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If
End Sub








Sub Drain_Hit()
	PlaySound "drain",0,1,AudioPan(Drain),0.25,0,0,1,AudioFade(Drain)
	PlaySound "DETH",0,1,AudioPan(Drain),0.25,0,0,1,AudioFade(Drain)
	Drain.DestroyBall
	BIP = BIP - 1
	ball
bali
If BIP = 1 Then
Multi = 0
MultiBIP = False
multb.State = 0
SBonus
Bonus = 0
End If

If multb.State = 1 And BIP = 0 Then 
jpo1.Enabled = True
jpo2.Enabled = True
jpo1.CreateBall
jpo2.CreateBall
BallRelease.CreateBall
		BallRelease.Kick 90, 7
		PlaySound SoundFX("ballrelease",DOFContactors), 0,1,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
BIP = BIP + 3
bali
ball
Else

If ShootAgain.State = 1 And BIP = 0  Then 
BIG = BIG - 1
ball
End If
If ShootAgain.State = 2 And BIP = 0  Then 
BIG = BIG - 1
ball
End If
End If

If BIP = 0 And BIG = 3 And Multi <> 1 Then
multb.State = 0
SBonus
Bonus = 0
	LightsOut()
StopMusic()
Snail

DF1.RotateToStart
JIL.State = 0
R6.State = 0
R7.State = 0
R8.State = 0
R5.State = 0
R1.State = 0
R2.State = 0
R3.State = 0
R4.State = 0
	BIG =0
ball
Dr1 = 0
Dr3 = 0
Dr4 = 0
Dr5 = 0
Dr6 = 0
Dr7 = 0
Dr8 = 0
Dr9 = 0
Dr10 = 0
Dr2 = 0
DrTimer.enabled = 1
BKO = 0
RKO = 0
TKO = 0
nscore = Score
Addscore(0)
AbilityDown1
AbilityDown2
AbilityDown3
no1.Transy = (no1.Transy + 3)
noab2.Transy = (noab2.Transy + 3)
noab3.Transy = (noab3.Transy + 3)
no1p = -29
no2p = -29
no3p = -29

If IsBonus = 0 Then 
Attract
End If
Else


	If BIP = 0 And BIG < 3 and Multb.State <> 1 then
SBonus
Bonus = 0
multb.State = 0
	LightsOn()
	StopMusic()
	Music()
	a1 = 0
	a2 = -1
	a3 = -2
AbilityDown1
AbilityDown2
AbilityDown3
no1.Transy = (no1.Transy + 3)
noab2.Transy = (noab2.Transy + 3)
noab3.Transy = (noab3.Transy + 3)
no1p = -29
no2p = -29
no3p = -29
Snail
		Addscore(0)
		BallRelease.CreateBall
		BallRelease.Kick 90, 7
		PlaySound SoundFX("ballrelease",DOFContactors), 0,1,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
		BIP = BIP + 1
	BIG = BIG + 1
bali
ball()
Dr1 = 0
Dr3 = 0
Dr4 = 0
Dr5 = 0
Dr6 = 0
Dr7 = 0
Dr8 = 0
Dr9 = 0
Dr10 = 0
Dr2 = 0
DrTimer.enabled = 1
If Multi = 0 Then 
ShootAgain.State = 2
End If
If R1.State  = 1 And R2.State  = 1 And R3.State  = 1 And R4.State  = 1 And R5.State  = 1 And R6.State  = 1 And R7.State  = 1 And R8.State  = 1 Then
XB.State = 2
End If

If TTL = 1 Then
TTL = 0
tilts = 0
	StopMusic()
Music()
End If
End If
End If
ball
End Sub



Sub BallSaver_Timer()
ShootAgain.State = 0
BallSaver.Enabled = 0
End Sub


Sub NewGame()
Score = 0
AddScore(0)
pass = 0
If BIP = 0 And Tl1.State = 0 And BIG = 0 Then
Sp1.State = 0
Sp2.State = 0
Sp3.State = 0
Sp1.Timerenabled = 0
Sp2.Timerenabled = 0
Sp3.Timerenabled = 0
at1.Enabled = False
at2.Enabled = False
at3.Enabled = False
g000.State = 0
g001.State = 0
g002.State = 0
g003.State = 0
g004.State = 0
g005.State = 0
g006.State = 0
g007.State = 0
g008.State = 0
g009.State = 0
g010.State = 0
g011.State = 0
g012.State = 0
g013.State = 0
g014.State = 0
g015.State = 0
g016.State = 0
g017.State = 0
g018.State = 0
g019.State = 0
g020.State = 0
g021.State = 0
g022.State = 0
jpol1.State = 0
jpol2.state = 0
R1.State = 0
R2.State = 0
R3.State = 0
R4.State = 0
R5.State = 0
R6.State = 0
R7.State = 0
R8.State = 0
ap3.State = 0
ap2.State = 0
ap1.State = 0
ll1.State = 0
ll2.State = 0
ll3.State = 0
ll4.State = 0
ll5.State = 0
JIL.State = 0
multb.State = 0
XB.State = 0
ShootAgain.State = 2
two.State = 0
three.State = 0
LightsOn()
ShootAgain.State = 2
'Plunger.CreateBall
		BallRelease.CreateBall
		BallRelease.Kick 90, 7
		PlaySound SoundFX("ballrelease",DOFContactors), 0,1,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
		tilts = 0
		TTL = 0
		BIP = BIP + 1
	BIG = BIG + 1
ball()
bali
StopMusic()
Music()
End If
End Sub

Sub LightsOut()
For each xx in GI:xx.State = 0: Next
tl1.State = 0
tl2.State = 0
tl3.State = 0
tl4.State = 0
tl5.State = 0
tl6.State = 0
tl7.State = 0
tl9.State = 0
tl10.State = 0
tl11.State = 0
tl12.State = 0
tl13.State = 0
tl14.State = 0
BL1.State = 0
BL2.State = 0
BL3.State = 0
BB1.State = 0
BB1.State = 0
CHK1.State = 0
CHK2.State = 0
CHK3.State = 0
TW1.State = 0
TW2.State = 0
SP1.State = 0
SP2.State = 0
SP3.State = 0
Sp1.Timerenabled = 0
SP2.Timerenabled = 0
SP3.Timerenabled = 0
ll1.State = 0
ll2.State = 0
ll3.State = 0
ll4.State = 0
ll5.State = 0
XB.State = 0
ShootAgain.State = 0
ap1.State = 0
ap2.State = 0
ap3.state = 0
two.State = 0
three.State = 0
jpol1.State = 0
jpol2.State = 0
JIL.State = 0
Claml.State = 0
Claml2.State = 0
Claml3.State = 0
multb.State = 0
AbilityTimer.Enabled = 0
Timer1.Enabled = 0
End Sub

Sub LightsOn()
For each xx in GI:xx.State = 1: Next
tl1.State = 1
tl2.State = 1
tl3.State = 1
tl4.State = 1
tl5.State = 1
tl6.State = 1
tl7.State = 1
tl9.State = 1
tl10.State = 1
tl11.State = 1
tl12.State = 1
tl13.State = 1
tl14.State = 1
BL1.State = 1
BL2.State = 1
BL3.State = 1
BB1.State = 1
BB1.State = 1
CHK1.State = 2
CHK2.State = 1
CHK3.State = 1
TW1.State = 1
TW2.State = 1
XB.State = 0
ShootAgain.State = 0
ap1.State = 1
ap2.State = 1
ap3.state = 1
two.State = 0
three.State = 0
End Sub


Sub llleft
    Dim TempState
    TempState = ll1.State
    ll1.State = ll2.State
    ll2.State = ll3.State
    ll3.State = ll4.State
    ll4.State = ll5.State
    ll5.State = TempState
End Sub

Sub llright
    Dim TempState
    TempState = ll5.State
    ll5.State = ll4.State
    ll4.State = ll3.State
    ll3.State = ll2.State
    ll2.State = ll1.State
    ll1.State = TempState
End Sub







'BUMPERS

Sub B1_Hit()
	If TTL = 0 Then
	PlaySound SoundFX("fx_bumper3",DOFContactors), 0,1,AudioPan(B1),0,0,0,1,AudioFade(B1)
	BL11.State = 1
	PlaySound "hit",0,1,AudioPan(B1),0.25,0,0,1,AudioFade(B1)
	AddScore(2001)
	AddBonus(10000)
	hitshield(1)
	Explode()
	Me.TimerEnabled = 1
End If
End Sub

Sub B1_Timer
	BL11.State = 0
	Me.Timerenabled = 0
End Sub

Sub B2_Hit()
If TTL = 0 Then
	PlaySound "hit",0,1,AudioPan(B2),0.25,0,0,1,AudioFade(B2)
	PlaySound SoundFX("fx_bumper3",DOFContactors), 0,1,AudioPan(B2),0,0,0,1,AudioFade(B2)
	BL22.State = 1
	AddScore(2001)
AddBonus(10000)
	hitshield(1)
	Explode()
	Me.TimerEnabled = 1
End If
End Sub

Sub B2_Timer
	BL22.State = 0
	Me.Timerenabled = 0
End Sub

Sub B3_Hit()
If TTL = 0 Then
	PlaySound "hit",0,1,AudioPan(B3),0.25,0,0,1,AudioFade(B3)
	PlaySound SoundFX("fx_bumper3",DOFContactors), 0,1,AudioPan(B3),0,0,0,1,AudioFade(B3)
	BL33.State = 1
	AddScore(2001)
AddBonus(10000)
	hitshield(1)
	Explode()
	Me.TimerEnabled = 1
End If
End Sub

Sub B3_Timer
	BL33.State = 0
	Me.Timerenabled = 0
End Sub

Sub Explode()
If hpop = 0 Then
AddScore(53333)
AddBonus(25000)
hpop = 33
PlaySound "POP"
animation.UpdateInterval = 8
    animation.Play SeqCircleOutOn,50,1
RKO = 1
xob
hitshield(0)
End If
End Sub

Sub Attract
Randomize
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "attract",-1
		Case 2 : PlaySound "pulse",-1
		Case 3 : PlaySound "rain",-1
End Select
g000.State = 1
g003.State = 1
g006.State = 1
g009.State = 1
g012.State = 1
g015.State = 1
g018.State = 1
g021.State = 1
two.State = 2
three.State = 2
tw1.State = 1
tw2.State = 1
chk1.State = 2
chk2.State = 1
chk3.State = 1
ap3.State = 0
ap2.State = 0
ap1.State = 1
Sp1.State = 1
Sp1.timerenabled = 1
at1.Enabled = True
at2.Enabled = true
at3.Enabled = true
End Sub

Sub at1_Timer
Dim tstate 
tstate = g022.State 
g022.State = g021.State 
g021.State = g020.State 
g020.State = g019.State 
g019.State = g018.State 
g018.State = g017.State 
g017.State = g016.State 
g016.State = g015.State 
g015.State = g014.State 
g014.State = g013.State 
g013.State = g012.State 
g012.State = g011.State 
g011.State = g010.State 
g010.State = g009.State 
g009.State = g008.State 
g008.State = g007.State 
g007.State = g006.State 
g006.State = g005.State 
g005.State = g004.State 
g004.State = g003.State 
g003.State = g002.State 
g002.State = g001.State 
g001.State = g000.State 
g000.State = tstate 
jpol1.State = 1
jpol2.State = 1
ShootAgain.State = 1
R1.State = 2
R2.State = 2
R3.State = 2
R4.State = 2
R5.State = 2
R6.State = 2
R7.State = 2
R8.State = 2
ll1.State = 1
ll2.State = 1
ll3.State = 1
ll4.State = 1
ll5.State = 1
JIL.State = 1
multb.State = 1
XB.State = 1
End Sub

Sub at2_Timer
Dim cstate
cstate = chk3.State 
chk3.State = chk2.State 
chk2.State = chk1.State 
chk1.State = cstate
End Sub

Sub at3_Timer
Dim astate
astate = ap3.State 
ap3.State = ap2.State 
ap2.State = ap1.State 
ap1.State = astate
End Sub


Sub Music()
Randomize
	Select Case Int(Rnd*21)+1
		Case 1 : PlaySound "Splattack - C-Side",-1
		Case 2 : PlaySound "clickbait",-1
		Case 3 : PlaySound "depth",-1
		Case 4 : PlaySound "ink",-1
		Case 5 : PlaySound "blitz",-1
		Case 6 : PlaySound "ent",-1
		Case 7 : PlaySound "rip",-1
		Case 8 : PlaySound "socks",-1
		Case 9 : PlaySound "fins",-1
		Case 10 : PlaySound "sand",-1
		Case 11 : PlaySound "surge",-1
		Case 12 : PlaySound "head",-1
		Case 13 : PlaySound "blam",-1
Case 14 : PlaySound "fiddles",-1
Case 15 : PlaySound "full",-1
Case 16 : PlaySound "ebb",-1
Case 17 : PlaySound "acid",-1
Case 18 : PlaySound "muck",-1
Case 19 : PlaySound "riptide",-1
Case 20 : PlaySound "sun",-1
Case 21 : PlaySound "coral",-1
	End Select
End Sub

Sub StopMusic()
Stopsound "Splattack - C-Side"
Stopsound "clickbait"
Stopsound "depth"
Stopsound "attract"
Stopsound "ink"
Stopsound "blitz"
Stopsound "ent"
Stopsound "rip"
Stopsound "socks"
Stopsound "fins"
Stopsound "sand"
Stopsound "surge"
Stopsound "head"
Stopsound "blam"
Stopsound "sun"
Stopsound "full"
Stopsound "fiddles"
Stopsound "riptide"
Stopsound "acid"
Stopsound "coral"
Stopsound "ebb"
Stopsound "muck"
Stopsound "rain"
Stopsound "pulse"
Stopsound "jingle"
End Sub



'MISC GATES + OTHER JUNK

Sub AbilityDown1
If no1p > -32 Then
no1.Transy = (no1.Transy - 3)
no1p = -32
End If

If qsj1p > -32 Then
qsj1.Transy = (qsj1.Transy - 3)
qsj1p = -32
End If

If sbr1p > -32 Then
sbr1.Transy = (sbr1.Transy - 3)
sbr1p = -32
End If

If sbp1p > -32 Then
sbp1.Transy = (sbp1.Transy - 3)
sbp1p = -32
End If

If spp1p > -32 Then
spp1.Transy = (spp1.Transy - 3)
spp1p = -32
End If

If rez1p > -32 Then
rez1.Transy = (rez1.Transy - 3)
rez1p = -32
End If

If rec1p > -32 Then
rec1.Transy = (rec1.Transy - 3)
rec1p = -32
End If

If int1p > -32 Then
int1.Transy = (int1.Transy - 3)
int1p = -32
End If

If sbs1p > -32 Then
sbs1.Transy = (sbs1.Transy - 3)
sbs1p = -32
End If

If mns1p > -32 Then
mns1.Transy = (mns1.Transy - 3)
mns1p = -32
End If

If ssp1p > -32 Then
ssp1.Transy = (ssp1.Transy - 3)
ssp1p = -32
End If

If run1p > -32 Then
run1.Transy = (run1.Transy - 3)
run1p = -32
End If

If rn1p > -32 Then
rn1.Transy = (rn1.Transy - 3)
rn1p = -32
End If

If sps1p > -32 Then
sps1.Transy = (sps1.Transy - 3)
sps1p = -32
End If

If qr1p > -32 Then
qr1.Transy = (qr1.Transy - 3)
qr1p = -32
End If

End Sub

Sub AbilityDown2
If no2p > -32 Then
noab2.Transy = (noab2.Transy - 3)
no2p = -32
End If

If qsj2p > -32 Then
qsj001.Transy = (qsj001.Transy - 3)
qsj2p = -32
End If

If sbr2p > -32 Then
sbr001.Transy = (sbr001.Transy - 3)
sbr2p = -32
End If

If sbp2p > -32 Then
sbp001.Transy = (sbp001.Transy - 3)
sbp2p = -32
End If

If spp2p > -32 Then
spp001.Transy = (spp001.Transy - 3)
spp2p = -32
End If

If rez2p > -32 Then
rez001.Transy = (rez001.Transy - 3)
rez2p = -32
End If

If rec2p > -32 Then
rec001.Transy = (rec001.Transy - 3)
rec2p = -32
End If

If int2p > -32 Then
int001.Transy = (int001.Transy - 3)
int2p = -32
End If

If sbs2p > -32 Then
sbs001.Transy = (sbs001.Transy - 3)
sbs2p = -32
End If

If mns2p > -32 Then
mns001.Transy = (mns001.Transy - 3)
mns2p = -32
End If

If ssp2p > -32 Then
ssp001.Transy = (ssp001.Transy - 3)
ssp2p = -32
End If

If run2p > -32 Then
run002.Transy = (run002.Transy - 3)
run2p = -32
End If

If rn2p > -32 Then
rn2.Transy = (rn2.Transy - 3)
rn2p = -32
End If

If sps2p > -32 Then
sps001.Transy = (sps001.Transy - 3)
sps2p = -32
End If

If qr2p > -32 Then
qr001.Transy = (qr001.Transy - 3)
qr2p = -32
End If
End Sub



Sub AbilityDown3
If no3p > -32 Then
noab3.Transy = (noab3.Transy - 3)
no3p = -32
End If

If qsj3p > -32 Then
qsj002.Transy = (qsj002.Transy - 3)
qsj3p = -32
End If

If sbr3p > -32 Then
sbr002.Transy = (sbr002.Transy - 3)
sbr3p = -32
End If

If sbp3p > -32 Then
sbp002.Transy = (sbp002.Transy - 3)
sbp3p = -32
End If

If spp3p > -32 Then
spp002.Transy = (spp002.Transy - 3)
spp3p = -32
End If

If rez3p > -32 Then
rez002.Transy = (rez002.Transy - 3)
rez3p = -32
End If

If rec3p > -32 Then
rec002.Transy = (rec002.Transy - 3)
rec3p = -32
End If

If int3p > -32 Then
int002.Transy = (int002.Transy - 3)
int3p = -32
End If

If sbs3p > -32 Then
sbs002.Transy = (sbs002.Transy - 3)
sbs3p = -32
End If

If mns3p > -32 Then
mns002.Transy = (mns002.Transy - 3)
mns3p = -32
End If

If ssp3p > -32 Then
ssp002.Transy = (ssp002.Transy - 3)
ssp3p = -32
End If

If run3p > -32 Then
run004.Transy = (run004.Transy - 3)
run3p = -32
End If

If rn3p > -32 Then
rn3.Transy = (rn3.Transy - 3)
rn3p = -32
End If

If sps3p > -32 Then
sps002.Transy = (sps002.Transy - 3)
sps3p = -32
End If

If qr3p > -32 Then
qr002.Transy = (qr002.Transy - 3)
qr3p = -32
End If
End Sub



Sub Spinner001_Spin
If TTL = 0 And levelup = 0 Then
PlaySound "shot",0,1,AudioPan(Spinner001),0.25,0,0,1,AudioFade(Spinner001)
AddScore(3)
else If TTL = 0 And levelup = 1 Then
AddScore(300)
Reroll()
Randomize
	Select Case Int(Rnd*16)+1
		Case 1 : PlaySound "gear1"
Case 1 : PlaySound "gear1"
Case 2 : PlaySound "gear2"
Case 3 : PlaySound "gear3"
Case 4 : PlaySound "gear4"
Case 5 : PlaySound "gear5"
Case 6 : PlaySound "gear6"
Case 7 : PlaySound "gear7"
Case 8 : PlaySound "gear8"
Case 9 : PlaySound "gear9"
Case 10 : PlaySound "gear10"
Case 11 : PlaySound "gear11"
Case 12 : PlaySound "gear12"
Case 13 : PlaySound "gear13"
Case 14 : PlaySound "gear14"
Case 15 : PlaySound "gear15"
Case 16 : PlaySound "gear16"
End Select
End If
End If
End Sub

Sub Reroll
If ap1.State = 1 And ap2.State = 1 And ap3.state = 1 Then
ap1.State = 1
ap2.State = 0
ap3.state = 0
Else
If ap1.State = 1 And ap2.State = 0 And ap3.state = 0 Then
ap1.State = 0
ap2.State = 1
ap3.state = 0
Else
If ap1.State = 0 And ap2.State = 1 And ap3.state = 0 Then
ap1.State = 0
ap2.State = 0
ap3.state = 1
Else
If ap1.State = 0 And ap2.State = 0 And ap3.state = 1 Then
ap1.State = 1
ap2.State = 0
ap3.state = 0
End If
End If
End If
End If

Randomize
	Select Case Int(Rnd*14)+1
		Case 1 : a1 = 1
AbilityDown1()
If qsj1p = -32 Then
qsj1.Transy = (qsj1.Transy + 3)
qsj1p = -29
End If
		Case 2 : a1 = 2
AbilityDown1()
If qr1p = -32 Then
qr1.Transy = (qr1.Transy + 3)
qr1p = -29
End If
		Case 3 : a1 = 3
AbilityDown1()
If sbs1p = -32 Then
sbs1.Transy = (sbs1.Transy + 3)
sbs1p = -29
End If
		Case 4 : a1 = 4
AbilityDown1()
If rn1p = -32 Then
rn1.Transy = (rn1.Transy + 3)
rn1p = -29
End If
		Case 5 : a1 = 5
AbilityDown1()
If spp1p = -32 Then
spp1.Transy = (spp1.Transy + 3)
spp1p = -29
End If
		Case 6 : a1 = 6
AbilityDown1()
If sbp1p = -32 Then
sbp1.Transy = (sbp1.Transy + 3)
sbp1p = -29
End If
		Case 7 : a1 = 7
AbilityDown1()
If sps1p = -32 Then
sps1.Transy = (sps1.Transy + 3)
sps1p = -29
End If
		Case 8 : a1 = 8
AbilityDown1()
If mns1p = -32 Then
mns1.Transy = (mns1.Transy + 3)
mns1p = -29
End If
		Case 9 :  a1 = 9
AbilityDown1()
If rec1p = -32 Then
rec1.Transy = (rec1.Transy + 3)
rec1p = -29
End If
		Case 10 : a1 = 10
AbilityDown1()
If rez1p = -32 Then
rez1.Transy = (rez1.Transy + 3)
rez1p = -29
End If
		Case 11 : a1 = 11
AbilityDown1()
If sbr1p = -32 Then
sbr1.Transy = (sbr1.Transy + 3)
sbr1p = -29
End If
		Case 12 : a1 = 12
AbilityDown1()
If ssp1p = -32 Then
ssp1.Transy = (ssp1.Transy + 3)
ssp1p = -29
End If
		Case 13 : a1 = 13
AbilityDown1()
If run1p = -32 Then
run1.Transy = (run1.Transy + 3)
run1p = -29
End If
		Case 14 : a1 = 14
AbilityDown1()
If int1p = -32 Then
int1.Transy = (int1.Transy + 3)
int1p = -29
End If
	End Select





Randomize
	Select Case Int(Rnd*14)+1
	Case 1 : a2 = 1
AbilityDown2()
If qsj2p = -32 Then
qsj001.Transy = (qsj001.Transy + 3)
qsj2p = -29
End If
		Case 2 : a2 = 2
AbilityDown2()
If qr2p = -32 Then
qr001.Transy = (qr001.Transy + 3)
qr2p = -29
End If
		Case 3 : a2 = 3
AbilityDown2()
If sbs2p = -32 Then
sbs001.Transy = (sbs001.Transy + 3)
sbs2p = -29
End If
		Case 4 : a2 = 4
AbilityDown2()
If rn2p = -32 Then
rn2.Transy = (rn2.Transy + 3)
rn2p = -29
End If
		Case 5 : a2 = 5
AbilityDown2()
If spp2p = -32 Then
spp001.Transy = (spp001.Transy + 3)
spp2p = -29
End If
		Case 6 : a2 = 6
AbilityDown2()
If sbp2p = -32 Then
sbp001.Transy = (sbp001.Transy + 3)
sbp2p = -29
End If
		Case 7 : a2 = 7
AbilityDown2()
If sps2p = -32 Then
sps001.Transy = (sps001.Transy + 3)
sps2p = -29
End If
		Case 8 : a2 = 8
AbilityDown2()
If mns2p = -32 Then
mns001.Transy = (mns001.Transy + 3)
mns2p = -29
End If
		Case 9 :  a2 = 9
AbilityDown2()
If rec2p = -32 Then
rec001.Transy = (rec001.Transy + 3)
rec2p = -29
End If
		Case 10 : a2 = 10
AbilityDown2()
If rez2p = -32 Then
rez001.Transy = (rez001.Transy + 3)
rez2p = -29
End If
		Case 11 : a2 = 11
AbilityDown2()
If sbr2p = -32 Then
sbr001.Transy = (sbr001.Transy + 3)
sbr2p = -29
End If
		Case 12 : a2 = 12
AbilityDown2()
If ssp2p = -32 Then
ssp001.Transy = (ssp001.Transy + 3)
ssp2p = -29
End If
		Case 13 : a2 = 13
AbilityDown2()
If run2p = -32 Then
run002.Transy = (run002.Transy + 3)
run2p = -29
End If
		Case 14 : a2 = 14
AbilityDown2()
If int2p = -32 Then
int001.Transy = (int001.Transy + 3)
int2p = -29
End If
	End Select	
		
	
Randomize
	Select Case Int(Rnd*14)+1
		Case 1 : a3 = 1
AbilityDown3()
If qsj3p = -32 Then
qsj002.Transy = (qsj002.Transy + 3)
qsj3p = -29
End If
		Case 2 : a3 = 2
AbilityDown3()
If qr3p = -32 Then
qr002.Transy = (qr002.Transy + 3)
qr3p = -29
End If
		Case 3 : a3 = 3
AbilityDown3()
If sbs3p = -32 Then
sbs002.Transy = (sbs002.Transy + 3)
sbs3p = -29
End If
		Case 4 : a3 = 4
AbilityDown3()
If rn3p = -32 Then
rn3.Transy = (rn3.Transy + 3)
rn3p = -29
End If
		Case 5 : a3 = 5
AbilityDown3()
If spp3p = -32 Then
spp002.Transy = (spp002.Transy + 3)
spp3p = -29
End If
		Case 6 : a3 = 6
AbilityDown3()
If sbp3p = -32 Then
sbp002.Transy = (sbp002.Transy + 3)
sbp3p = -29
End If
		Case 7 : a3 = 7
AbilityDown3()
If sps3p = -32 Then
sps002.Transy = (sps002.Transy + 3)
sps3p = -29
End If
		Case 8 : a3 = 8
AbilityDown3()
If mns3p = -32 Then
mns002.Transy = (mns002.Transy + 3)
mns3p = -29
End If
		Case 9 :  a3 = 9
AbilityDown3()
If rec3p = -32 Then
rec002.Transy = (rec002.Transy + 3)
rec3p = -29
End If
		Case 10 : a3 = 10
AbilityDown3()
If rez3p = -32 Then
rez002.Transy = (rez002.Transy + 3)
rez3p = -29
End If
		Case 11 : a3 = 11
AbilityDown3()
If sbr3p = -32 Then
sbr002.Transy = (sbr002.Transy + 3)
sbr3p = -29
End If
		Case 12 : a3 = 12
AbilityDown3()
If ssp3p = -32 Then
ssp002.Transy = (ssp002.Transy + 3)
ssp3p = -29
End If
		Case 13 : a3 = 13
AbilityDown3()
If run3p = -32 Then
run004.Transy = (run004.Transy + 3)
run3p = -29
End If
		Case 14 : a3 = 14
AbilityDown3()
If int3p = -32 Then
int002.Transy = (int002.Transy + 3)
int3p = -29
End If
End Select
AbilityTimer.Enabled = 0
AbilityTimer.Enabled = 1
End Sub

Sub AbilityTimer_Timer()
ap1.State = 1
ap2.State = 1
ap3.state = 1

If a1 = a2 And a2 = a3 Then
three.State = 2
two.State = 0
Else
If a1 = a2 And not a2 = a3 Then
two.State = 2
three.State = 0
Else
If not a1 = a2 And a2 = a3 Then
two.State = 2
three.State = 0
Else
If a1 = a3 And not a2 = a3 Then
two.State = 2
three.State = 0
Else
three.State = 0
two.State = 0
End If
End If
End If
End If
PlaySound "scrub"
tacount = 0
AddScore(33333)
levelup = 0
ll1.State = 0
ll2.State = 0
ll3.State = 0
ll4.State = 0
ll5.State = 0
Sp1.State = 0
SP2.State = 0
SP3.State = 0
Sp1.Timerenabled = 0
SP2.Timerenabled = 0
SP3.Timerenabled = 0
AbilityTimer.enabled = 0
End Sub

Sub Snail

If ll1.State = 1 And ll2.State = 1 And ll3.State = 1 And ll4.State = 1 And ll5.State = 1 Then
levelup = 1
Sp1.State = 1
Sp1.Timerenabled = 1
IF Tacount = 0 Then
PlaySound "zawoo"
tacount = 1
End If
Else
levelup = 0
End If
End Sub

Sub MultiR
If Multi = 1 Then
RightFlipper.Rotatetoend
PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
ReleaseTimer.Enabled = True
End If
End Sub

Sub ReleaseTimer_Timer
jpo2.Kick -70, 50
playsound "jump"
ReleaseTimer.Enabled = False
ReleaseTimer001.Enabled = True
End Sub


Sub ReleaseTimer001_Timer
RightFlipper.RotatetoStart
PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
LeftFlipper.Rotatetoend
ReleaseTimer001.Enabled = False
ReleaseTimer002.Enabled = True
End Sub

Sub ReleaseTimer002_Timer
jpo1.Kick 55, 50
playsound "jump"
ReleaseTimer002.Enabled = False
ReleaseTimer003.Enabled = True
End Sub

Sub ReleaseTimer003_Timer
LeftFlipper.RotatetoStart
ReleaseTimer003.Enabled = False
Multi = 0
MultiBIP = True
IF TTL = 0 Then
Music
end If
IF jpol1.State = 0 Then
jpo1.Enabled = False
End If
If jpol2.State = 0 Then
jpo2.Enabled = False
End If
End Sub


Sub Gate_Hit
PlaySound SoundFX("gate4",DOFContactors), 0,1,AudioPan(Gate),0,0,0,1,AudioFade(Gate)
ss = 0
Multir
If ShootAgain.State = 2 Then
BallSaver.Enabled = 1
End If
End Sub

Sub Loop_Hit
If TTL = 0 Then
PlaySound "squid"
AddScore(8888)
End If
End Sub

Sub LS2_Slingshot
If TTL = 0 Then
AddScore(33)
PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05
PlaySound SoundFX("blast",DOFContactors), 0,1, -0.05,0.05
End If
End Sub

Sub SKS_Hit
PlaySound SoundFX("gate4",DOFContactors), 0,1,AudioPan(SKS),0,0,0,1,AudioFade(SKS)
If ShootAgain.State = 2 Then
BallSaver.Enabled = 1
End If
If TTL = 0 And ss = 1 Then
AddScore(33333)
AddBonus(50000)
PlaySound SoundFX("h3",DOFContactors), 0,1,AudioPan(SKS),0,0,0,1,AudioFade(SKS)
ss = 0
Multir
End If
End Sub

Sub QR_Hit
PlaySound SoundFX("gate4",DOFContactors), 0,1,AudioPan(QR),0,0,0,1,AudioFade(QR)
If TTL = 0 And QRr = True Then
AddScore(10000)
AddBonus(26000)
PlaySound "ROLL",0,1,AudioPan(QR),0.25,0,0,1,AudioFade(QR)
QRr = False
Qr.timerenabled = 1
If BIP = 1 Then
	gaugeanimation.UpdateInterval = 1
	gaugeanimation.Play SeqClockleftOn,360,1
End If
End If
End Sub

Sub Qr_Timer
QRr = True
QR.Timerenabled = 0
End Sub


Sub RG_Hit
PlaySound"fx_metalrolling"
PlaySound SoundFX("gate4",DOFContactors), 0,1,AudioPan(RG),0,0,0,1,AudioFade(RG)
If TTL = 0 Then
AddScore(5000)
AddBonus(25000)
PlaySound "bomb"
animation.UpdateInterval = 8
    animation.Play SeqDownOn,75,0
jpol2.State = 1
jpo2.Enabled = True
DF1.RotateToEnd
JIL.State = 1
End If
End Sub

Sub TG_Hit
PlaySound"fx_metalrolling"
PlaySound SoundFX("gate4",DOFContactors), 0,1,AudioPan(TG),0,0,0,1,AudioFade(TG)
If TTL = 0 Then
AddScore(10000)
AddBonus(25000)
hitbasket(20)
PlaySound "bomb"
PlaySound "baskethit"
animation.UpdateInterval = 8
    animation.Play SeqDownOn,75,0
jpol1.State = 1
jpo1.Enabled = True
DF1.RotateToEnd
JIL.State = 1
End If
End Sub

Sub Trigger001_Hit
If LUP = 0 And TTL = 0 Then
LUP = 1
StopMusic
PlaySound "jingle",-1
For each xx in GI:xx.State = 0: Next
tl1.State = 0
tl2.State = 0
tl3.State = 0
tl4.State = 0
tl5.State = 0
tl6.State = 0
tl7.State = 0
tl9.State = 0
tl10.State = 0
tl11.State = 0
tl12.State = 0
tl13.State = 0
tl14.State = 0
BL1.State = 0
BL2.State = 0
BL3.State = 0
BB1.State = 0
BB1.State = 0
Claml.State = 1
Claml2.State = 1
Claml3.State = 1
End If
End Sub

Sub Kicker002_Hit
LUP = 0
If TTL = 0 Then
StopMusic
Music()
For each xx in GI:xx.State = 1: Next
tl1.State = 1
tl2.State = 1
tl3.State = 1
tl4.State = 1
tl5.State = 1
tl6.State = 1
tl7.State = 1
tl9.State = 1
tl10.State = 1
tl11.State = 1
tl12.State = 1
tl13.State = 1
tl14.State = 1
BL1.State = 1
BL2.State = 1
BL3.State = 1
BB1.State = 1
BB1.State = 1
Claml.State = 0
Claml2.State = 0
Claml3.State = 0
End If
End Sub

Sub jpo1_Hit
If TTL = 0 Then
PlaySound "kicker_enter_center"
jpo1.Timerenabled = 1
End If
End Sub

Sub jpo1_Timer
PlaySound "jump"
AddBonus(25000)
animation.UpdateInterval = 5
  animation.Play SeqDiagUpRightOn,30,1
jpo1.Kick 70, 25
PlaySound "kicker"
jpol1.State = 0
jpo1.Enabled = False
jpol1.State = 0
jpo2.Enabled = False
jpol2.State = 0
DF1.RotateToStart
JIL.State = 0
jpo1.Timerenabled = 0
End Sub

Sub jpo2_Hit
If TTL = 0 Then
PlaySound "kicker_enter_center"
jpo2.Timerenabled = 1
End If
End Sub

Sub jpo2_Timer
PlaySound "jump"
AddBonus(25000)
animation.UpdateInterval = 5
  animation.Play SeqDiagUpLeftOn,30,1
jpo2.Kick -70, 25
PlaySound "kicker"
jpol2.State = 0
jpo2.Enabled = False
jpo1.Enabled = False
jpol1.State = 0
DF1.RotateToStart
JIL.State = 0
jpo2.Timerenabled = 0
End Sub


Sub JI_Hit
PlaySound SoundFX("gate4",DOFContactors), 0,1,AudioPan(JI),0,0,0,1,AudioFade(JI)
If TTL = 0 Then
AddBonus(25000)
AddScore(10000)
DF1.RotateToStart
JIL.State = 0
ss = 0
MultiR
If Multi = 0 Then
jpol2.State = 0
jpol1.State = 0
jpo2.Enabled = False
jpo1.Enabled = False
End If
If ShootAgain.State = 2 Then
BallSaver.Enabled = 1
End If
End If
End Sub

Sub MG_Hit
PlaySound SoundFX("gate4",DOFContactors), 0,1,AudioPan(MG001),0,0,0,1,AudioFade(MG)
End Sub



Sub ump_Hit
If TTL = 0 Then
PlaySound "jump"
End If
End Sub

Sub xob
If BKO = 1 And RKO = 1 And TKO = 1 Then
XB.State = 2
End If
End Sub

Sub LL_Hit
If TTL = 0 Then
AddScore(3333)
PlaySound "squid",0,1,AudioPan(LL),0.25,0,0,1,AudioFade(LL)
'PlaySound SoundFX("gate4",DOFContactors), 0,1,AudioPan(SKS),0,0,0,1,AudioFade(SKS)
End If
End Sub

Sub LO2_Hit
If TTL = 0 Then
AddScore(13333)
PlaySound "PAIN",0,1,AudioPan(LO2),0.25,0,0,1,AudioFade(LO2)
'PlaySound SoundFX("gate4",DOFContactors), 0,1,AudioPan(SKS),0,0,0,1,AudioFade(SKS)
ll2.State = 1
Snail
End If
End Sub

Sub LO1_Hit
If TTL = 0 Then
AddScore(3333)
PlaySound "squid",0,1,AudioPan(LO1),0.25,0,0,1,AudioFade(LO1)
'PlaySound SoundFX("gate4",DOFContactors), 0,1,AudioPan(SKS),0,0,0,1,AudioFade(SKS)
ll1.State = 1
Snail
End If
End Sub

Sub RO_Hit
If TTL = 0 Then
AddScore(13333)
PlaySound "PAIN",0,1,AudioPan(RO),0.25,0,0,1,AudioFade(RO)
'PlaySound SoundFX("gate4",DOFContactors), 0,1,AudioPan(SKS),0,0,0,1,AudioFade(SKS)
ll5.State = 1
Snail
End If
End Sub

Sub LI_Hit
If TTL = 0 Then
AddScore(3333)
PlaySound "squid",0,1,AudioPan(LI),0.25,0,0,1,AudioFade(LI)
'PlaySound SoundFX("gate4",DOFContactors), 0,1,AudioPan(SKS),0,0,0,1,AudioFade(SKS)
ll3.State = 1
Snail
End If
End Sub

Sub RI_Hit
If TTL = 0 Then
AddScore(3333)
PlaySound "squid",0,1,AudioPan(RI),0.25,0,0,1,AudioFade(RI)
'PlaySound SoundFX("gate4",DOFContactors), 0,1,AudioPan(SKS),0,0,0,1,AudioFade(SKS)
ll4.State = 1
Snail
End If
End Sub

Sub S1_Slingshot
PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05
If TTL = 0 Then
AddScore(33)

End If
End Sub

Sub S2_Slingshot
PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 
If TTL = 0 Then
AddScore(33)
 
End If
End Sub

Sub S3_Slingshot
PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 
If TTL = 0 Then
AddScore(33)

End If
End Sub

'TARGETS

Dim Dr1 
Dr1 = 0

Dim Dr2 
Dr2 = 0

Dim Dr3 
Dr3 = 0

Dim Dr4 
Dr4 = 0

Dim Dr5 
Dr5 = 0

Dim Dr6 
Dr6 = 0

Dim Dr7 
Dr7 = 0

Dim Dr8 
Dr8 = 0

Dim Dr9 
Dr9 = 0

Dim Dr10 
Dr10 = 0



Sub T1_Hit()
T1.IsDropped = True
Dr1 = 1
DropTargs()

End Sub

Sub T2_Hit()
T2.IsDropped = True
Dr2 = 1
DropTargs()

End Sub

Sub T3_Hit()
T3.IsDropped = True
Dr3 = 1
DropTargs()

End Sub

Sub T4_Hit()
T4.IsDropped = True
Dr4 = 1
DropTargs()

End Sub

Sub T5_Hit()
T5.IsDropped = True
Dr5 = 1
DropTargs()

End Sub

Sub T6_Hit()
T6.IsDropped = True
Dr6 = 1
DropTargs()

End Sub

Sub T7_Hit()
T7.IsDropped = True
Dr7 = 1
DropTargs()

End Sub

Sub T8_Hit()
T8.IsDropped = True
Dr8 = 1
DropTargs()

End Sub

Sub T9_Hit()
T9.IsDropped = True
Dr9 = 1
DropTargs()

End Sub

Sub T10_Hit()
T10.IsDropped = True
Dr10 = 1
DropTargs()

End Sub


Sub DropTargs()
If TTL = 0 Then
PlaySound "light",0,1,AudioPan(T5),0.25,0,0,1,AudioFade(T5)
AddScore(2525)
AddBonus(5000)
If Dr1 = 1 And Dr2 = 1 And Dr3 = 1 And Dr4 = 1 And Dr5 = 1 And Dr6 = 1 And Dr7 = 1 And Dr8 = 1 And Dr9 = 1 And Dr10 = 1 Then
Dr1 = 0
Dr3 = 0
Dr4 = 0
Dr5 = 0
Dr6 = 0
Dr7 = 0
Dr8 = 0
Dr9 = 0
Dr10 = 0
Dr2 = 0
TowerMove()
DrTimer.enabled = 1

End If
End If
End Sub

Sub DrTimer_Timer()
PlaySound SoundFX("fx_resetdrop",DOFContactors), 0,1,AudioPan(T5),0,0,0,1,AudioFade(T5)
T1.IsDropped = False 
T2.IsDropped = False  
T3.IsDropped = False 
T4.IsDropped = False 
T5.IsDropped = False  
T6.IsDropped = False  
T7.IsDropped = False 
T8.IsDropped = False  
T9.IsDropped = False  
T10.IsDropped = False
DrTimer.enabled = 0
End Sub

Sub TowerMove()
PlaySound "tower"
animation.UpdateInterval = 8
	animation.Play SeqRadarLeftOn,20,1
If CHK1.State = 2 Then
CHK1.State = 1
CHK2.State = 2
AddScore(10000)
AddBonus(25000)
Elseif CHK2.State = 2 Then
CHK2.State = 1
CHK3.State = 2
AddScore(30000)
AddBonus(50000)
Elseif CHK3.State = 2 Then
CHK3.State = 1
CHK1.State = 2
AddScore(70000)
AddBonus(100000)
TKO = 1
xob
End If
End Sub

Sub RT1_Hit
If R1.State = 0 And TTL = 0 Then
R1.State = 2
Else
If R1.State = 2 And TTL = 0 Then
R1.State = 0
End If
End If
BTurf()
Twar()
End Sub

Sub RT2_Hit
If R2.State = 0 And TTL = 0 Then
R2.State = 2
Else
If R2.State = 2 And TTL = 0 Then'
R2.State = 0
End If
End If
BTurf()
Twar()
End Sub

Sub RT3_Hit
If R3.State = 0 And TTL = 0 Then
R3.State = 2
Else
If R3.State = 2 And TTL = 0 Then
R3.State = 0 
End If
End If
BTurf()
Twar()
End Sub

Sub RT4_Hit
If R4.State = 0 And TTL = 0 Then
R4.State = 2
Else
If R4.State = 2 And TTL = 0 Then
R4.State = 0 
End If
End If
BTurf()
Twar()
End Sub

Sub BTurf()
If R1.State = 2 and R2.State = 2 and R3.State = 2 and R4.State = 2 And TTL = 0 Then 
R1.State = 1
R2.State = 1
R3.State = 1
R4.State = 1
PlaySoundAtBall SoundFX("DUH",DOFContactors)
AddScore(50000)
AddBonus(50000)
End If
End Sub

Sub RT5_Hit
If R5.State = 0 And TTL = 0 Then
R5.State = 2
Else
If R5.State = 2 And TTL = 0 Then
R5.State = 0 
End If
End If
TTurf()
Twar()
End Sub

Sub RT6_Hit
If R6.State = 0 And TTL = 0 Then
R6.State = 2
Else
If R6.State = 2 And TTL = 0 Then
R6.State = 0 
End If
End If
TTurf()
Twar()
End Sub

Sub RT7_Hit
If R7.State = 0 And TTL = 0 Then
R7.State = 2
Else
If R7.State = 2 And TTL = 0 Then
R7.State = 0 
End If
End If
TTurf()
Twar()
End Sub

Sub RT8_Hit
If R8.State = 0 And TTL = 0 Then
R8.State = 2
Else
If R8.State = 2 And TTL = 0 Then
R8.State = 0 
End If
End If
TTurf()
Twar()
End Sub

Sub TTurf()
If R5.State = 2 and R6.State = 2 and R7.State = 2 and R8.State = 2 And TTL = 0 Then 
R6.State = 1
R7.State = 1
R8.State = 1
R5.State = 1
PlaySoundAtBall SoundFX("DUH",DOFContactors)
AddScore(50000)
AddBonus(50000)
End If
End Sub

Sub Twar()
If TTL = 0 Then
PlaySoundAtBall SoundFX("target",DOFContactors)
PlaySoundAtBall SoundFX("DING",DOFContactors)
	AddScore(4321)
AddBonus(5000)
If R1.State  = 1 And R2.State  = 1 And R3.State  = 1 And R4.State  = 1 And R5.State  = 1 And R6.State  = 1 And R7.State  = 1 And R8.State  = 1 Then
XB.State = 2
End If
End If
End Sub

Sub GT_Hit
If TTL = 0 Then
	AddScore(13333)
AddBonus(25000)
PlaySoundAtBall SoundFX("target",DOFContactors)
IF XB.State = 2 Then
PlaySoundAtBall SoundFX("s2s",DOFContactors)
ShootAgain.State = 1
XB.State = 0
R6.State = 0
R7.State = 0
R8.State = 0
R5.State = 0
R1.State = 0
R2.State = 0
R3.State = 0
R4.State = 0
BallSaver.Enabled = 0
End If'
If multb.State = 1 Then 
AddScore(87777)
End If
If multb.State = 2 Then
StopMusic
animation.UpdateInterval = 8
animation.Play SeqCircleInOn,50,3
RightFlipper.RotatetoStart
LeftFlipper.Rotatetostart
multb.State = 1
Multi = 1
jpo1.Enabled = False
jpo2.Enabled = False
PlaySound "TAC"
End If
End If 
End Sub


Sub GT001_Hit
If TTL = 0 Then
	AddScore(13333)
AddBonus(25000)
hitbasket(3)
PlaySoundAtBall SoundFX("target",DOFContactors)
Playsound "ClamHounou_Enemy"
End If 
End Sub




'Lights

Sub SP1_Timer()
SP1.State = 0
SP2.State = 1
SP1.TimerEnabled = 0
SP2.TimerEnabled = 1
End Sub

Sub SP2_Timer()
SP2.State = 0
SP3.State = 1
SP2.TimerEnabled = 0
SP3.TimerEnabled = 1
End Sub

Sub SP3_Timer()
SP3.State = 0
SP1.State = 1
SP3.TimerEnabled = 0
SP1.TimerEnabled = 1
End Sub





'*****GI Lights On
dim xx


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
PlaySound SoundFX("blast",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
IF TTL = 0 Then
	AddScore(33)
    
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End If
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0:gi1.State = 1:Gi2.State = 1
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
PlaySound SoundFX("blast",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
IF TTL = 0 Then
	AddScore(33)
    
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End If
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0:gi3.State = 1:Gi4.State = 1
    End Select
    LStep = LStep + 1
End Sub


'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / Splatoon3.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / Splatoon3.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function


'*****************************************
'   rothbauerw's Manual Ball Control
'*****************************************

Dim BCup, BCdown, BCleft, BCright
Dim ControlBallInPlay, ControlActiveBall
Dim BCvel, BCyveloffset, BCboostmulti, BCboost

BCboost = 1				'Do Not Change - default setting
BCvel = 4				'Controls the speed of the ball movement
BCyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
BCboostmulti = 3		'Boost multiplier to ball veloctiy (toggled with the B key) 

ControlBallInPlay = false

Sub StartBallControl_Hit()
	Set ControlActiveBall = ActiveBall
	ControlBallInPlay = true
	ss = 1
End Sub

Sub StopBallControl_Hit()
	ControlBallInPlay = false
End Sub	

Sub BallControlTimer_Timer()
	If EnableBallControl and ControlBallInPlay then
		If BCright = 1 Then
			ControlActiveBall.velx =  BCvel*BCboost
		ElseIf BCleft = 1 Then
			ControlActiveBall.velx = -BCvel*BCboost
		Else
			ControlActiveBall.velx = 0
		End If

		If BCup = 1 Then
			ControlActiveBall.vely = -BCvel*BCboost
		ElseIf BCdown = 1 Then
			ControlActiveBall.vely =  BCvel*BCboost
		Else
			ControlActiveBall.vely = bcyveloffset
		End If
	End If
End Sub


'********************************************************************
'      JP's VP10 Rolling Sounds (+rothbauerw's Dropping Sounds)
'********************************************************************

Const tnob = 5 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
		StopSound("swimming" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    For b = 0 to UBound(BOT)
        ' play the rolling sound for each ball
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			PlaySound("swimming" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
				StopSound("swimming" & b)
                rolling(b) = False
            End If
        End If

        ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
			PlaySound "drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
        If BOT(b).X < Splatoon3.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Splatoon3.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Splatoon3.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub



'************************************
' What you need to add to your table
'************************************

' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc


'******************************************
' Explanation of the rolling sound routine
'******************************************

' sounds are played based on the ball speed and position

' the routine checks first for deleted balls and stops the rolling sound.

' The For loop goes through all the balls on the table and checks for the ball speed and 
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.

' The sound is played using the VOL, AUDIOPAN, AUDIOFADE and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the AUDIOPAN & AUDIOFADE functions will change the stereo position
' according to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub



Sub Splatoon3_Exit()
	
End Sub