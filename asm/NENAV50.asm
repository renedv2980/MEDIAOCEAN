*          DATA SET NENAV50    AT LEVEL 023 AS OF 06/02/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T31850E                                                                  
*INCLUDE GETBROAD                                                               
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*                                                               *               
* LEVEL 13 - TVQDATES FROM DEMTABS                              *               
*===============================================================*               
*                                                                               
T31850   TITLE 'NENAV50 - UNIV/PROG RECORD RETRIEVAL - FRONTRUNNER'             
T31850   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV50**,R8                                                    
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         GOTO1 VALIMED                                                          
         CLC   SVRCVEL,=X'0082'       WIZARD UNIT DATA                          
         BE    ROUT100                                                          
         CLC   SVRCVEL,=X'0084'       DEMO LOOKUP                               
         BE    ROUT200                                                          
         CLC   SVRCVEL,=X'0086'       PACKAGE ADD                               
         BE    ROUT300                                                          
         SPACE 3                                                                
*--GET WIZARD UNIT DATA                                                         
ROUT100  BAS   RE,WIZDATA                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*--LOOK UP DEMOS                                                                
ROUT200  BAS   RE,GETDEMS                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*--ADD A PACJAGE RECORD                                                         
ROUT300  BAS   RE,ADDPKG                                                        
         B     EXIT                                                             
         SPACE 3                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
         EJECT                                                                  
*  ROUTINE BUILDS DEFAULT WIZARD UNIT DATA                                      
*                                                                               
*  R2 -> WIZCLIENT TABLE                                                        
*  R4 -> WIZ CLIENT                                                             
*  R5 -> LENGTH OF DATA                                                         
*                                                                               
*                                                                               
*                                                                               
WIZDATA  NTR1                                                                   
         TM    SVINDS1,X'80'        IS THIS A UNIVERSE DEMO REQUEST             
         BO    WIZDT100             YES                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVCLI                                                   
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BNE   WIZDATAX                                                         
*                                                                               
         L     R2,AIO1                                                          
         USING CLTHDR,R2                                                        
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO1                                    
******   BAS   RE,CHKSEC           CHECK CLIENT SECURITY                        
*                                                                               
         TM    SVINDS1,X'08'        BYPASS UNIVERSE VALIDATION                  
         BO    WIZDT100             YES                                         
*                                                                               
*  VLIDATE THAT UNIVERSE EXISTS                                                 
*                                                                               
         BAS   RE,GETUNIV                                                       
WIZDT100 BAS   RE,WIZIT                                                         
WIZDATAX XIT1                                                                   
         SPACE 2                                                                
*                                                                               
* EXPECTS R2->SVWIZ CLIENT LIST                                                 
*                                                                               
WIZIT    NTR1                                                                   
         TM    SVINDS1,X'80'        UNIVERSE ONLY LOOKUP                        
         BO    WIZD100                                                          
*                                                                               
         LHI   R1,X'62'            NO-SEND HEADER                               
         BAS   RE,SENDH                                                         
* PASS CLIENT NAME                                                              
         LA    R5,20                                                            
         LA    R6,CNAME            POINT TO CLIENT NAME                         
         LA    R6,19(R6)           POINT R6 TO END OF CLIENT NAME               
WIZD20   CLI   0(R6),X'40'                                                      
         BH    WIZD30                                                           
         BCTR  R6,0                                                             
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BNZ   WIZD20                                                           
         DC    H'0'                NO PRISONERS                                 
WIZD30   LHI   R1,X'02'                                                         
         LA    R4,CNAME                                                         
         BAS   RE,SENDD            R5 HAS LENGTH                                
* PASS THE SUB-LINE LIMIT                                                       
         OC    CLTSLLMT,CLTSLLMT                                                
         BZ    WIZD40                                                           
         SR    R5,R5                                                            
         LHI   R1,X'11'                                                         
         LA    R4,CLTSLLMT                                                      
         BAS   RE,SENDD                                                         
* PASS THE PROFILES                                                             
WIZD40   BAS   RE,READPROF         READ THE PROFILES                            
         LHI   R6,X'04'                                                         
         LA    R3,WORK2                                                         
         BAS   RE,SENDPRF                                                       
         LHI   R6,X'05'                                                         
         LA    R3,WORK2+16                                                      
         BAS   RE,SENDPRF                                                       
         LHI   R6,X'06'                                                         
         LA    R3,WORK2+32                                                      
         BAS   RE,SENDPRF                                                       
         CLI   SVFRSW,C'F'                                                      
         BNE   WIZD45                                                           
         LHI   R6,X'12'                                                         
         LA    R3,APPROF                                                        
         BAS   RE,SENDPRF                                                       
*                                                                               
*  SEND UNIVERSE NAD CATEGORIES                                                 
*                                                                               
WIZD45   L     R2,AIO2                                                          
         USING DEMREC,R2                                                        
         LA    R6,OVERAREA          NAD DEMOS                                   
*********USING OVEREL,R6                                                        
         CLI   0(R6),X'DD'          ARE THERE ANY NADS                          
         BNE   WIZDEX                                                           
*                                                                               
* CUNIVERSE NO LONGER PART OF COMSCORE PROJECT - SCHT 8/17                      
*                                                                               
*        BE    WIZD47                                                           
*        CLI   0(R6),NUCDELQ        CUNIVERSE?                                  
*        BNE   WIZDEX                                                           
WIZD47   LHI   R1,X'83'             UNIVERSE NADS                               
         BAS   RE,SENDH                                                         
*                                                                               
WIZD50   CLI   0(R6),X'DD'          CHECK END OF DEMOS                          
         BNE   WIZDEX                                                           
*                                                                               
* CUNIVERSE NO LONGER PART OF COMSCORE PROJECT - SCHT 8/17                      
*                                                                               
*        CLI   0(R6),NUCDELQ        CUNIVERSE?                                  
*        BNE   WIZDEX                                                           
*                                                                               
*        USING NUCELDD,R6                                                       
*        LHI   R1,X'06'                                                         
*        LA    R4,NUCDCAT          COMSCORE DEMO CATEGORY                       
*        BAS   RE,SENDD                                                         
*        LHI   R1,X'07'                                                         
*        LA    R4,NUCDAMT          DEMO AMOUNT                                  
*        BAS   RE,SENDD                                                         
*        B     WIZD60                                                           
*                                                                               
OEL      USING OVEREL,R6                                                        
WIZD55   LHI   R1,X'02'                                                         
         LA    R4,OEL.OVERDEMO      NAD NUMBER                                  
         BAS   RE,SENDD                                                         
         LHI   R1,X'03'                                                         
         LA    R4,OEL.OVERDEMO+2    DEMO CATEGORY                               
         BAS   RE,SENDD                                                         
         LHI   R1,X'04'                                                         
         LA    R4,OEL.OVERAMNT      DEMO AMOUNT                                 
         BAS   RE,SENDD                                                         
WIZD60   AHI   R6,OVERELNQ                                                      
         B     WIZD50                                                           
         DROP  R2,OEL                                                           
*                                                                               
*  INITIALIZE DEMO BLOCK                                                        
WIZD100  L     RE,AIO2                                                          
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         L     R2,AIO2                                                          
         USING DEMREC,R2                                                        
         MVC   PREL(3),=X'350902'                                               
         MVC   PVEL(3),=X'33F302'                                               
         MVC   PUEL(3),=X'31F344'                                               
*                                                                               
         BAS   RE,UNIVSET           SET DEMOS UP AS UNIVERSES                   
         BAS   RE,GETUNIV           SET UNIVERSE IN DEMO RECORD                 
         BAS   RE,SETDB                                                         
*                                                                               
         TM    SVINDS1,SVI1NTIQ                                                 
         JZ    WIZD140                                                          
         GOTO1 VDEMAND,DMCB,BLOCK,0                                             
*                                                                               
WIZD140  GOTO1 VDEMOUT,DMCB,(C'L',QDEMOS),BLOCK,WORK2                           
         LA    RE,WORK2                                                         
WIZD150  BAS   RE,SENDUNV           PASS UNIVERSE VALUES TO THE PC              
*                                                                               
WIZDEX   DS    0H                 EXIT WIZARD                                   
         XIT1                                                                   
         DROP  R2                                                               
         SPACE 3                                                                
* READ PASS N0,N1,N2 PROFILES                                                   
READPROF NTR1                                                                   
         XC    KEY,KEY             GET USER PROFILE INTO NBUSE                  
         MVC   KEY(4),=C'S0N0'                                                  
         MVC   KEY+4(2),QAGY                                                    
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(3),QCLT                                                    
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),SVOFFC                                                 
         GOTO1 VGETPROF,DMCB,KEY,WORK2,VDATAMGR    N0 PROFILE                   
         MVI   KEY+3,C'1'                     GET N1 PROFILE                    
         GOTO1 VGETPROF,DMCB,KEY,WORK2+16,VDATAMGR                              
         MVI   KEY+3,C'2'                     GET N2 PROFILE                    
         GOTO1 VGETPROF,DMCB,KEY,WORK2+32,VDATAMGR                              
         MVC   N1PROF,WORK2                                                     
         MVC   N2PROF,WORK2+16                                                  
         MVC   N3PROF,WORK2+32                                                  
*  SET APPLICATION PROFILE (FRONTRUNNER)                                        
         MVC   KEY+2(2),=CL2'FR'              APPLICATION PROFILE               
         GOTO1 VGETPROF,DMCB,KEY,APPROF,VDATAMGR                                
         XIT1                                                                   
         SPACE 3                                                                
*                                                                               
* R3->PROFILE STRING                                                            
* R1->MAP CODE                                                                  
SENDPRF  NTR1                                                                   
         LA    R4,WORK             OUTPUT AREA                                  
         LA    R2,16               # OF PROFILES                                
SENDP10  LA    R5,1                                                             
         CLI   0(R3),X'40'         SEND BLANKS                                  
         BE    SENDP15                                                          
         CLI   0(R3),C'*'          SEND ASTERISK                                
         BE    SENDP15                                                          
         CLI   0(R3),C'A'          SEND ALPHAS                                  
         BNL   SENDP15                                                          
* ASSUME NUMERIC IF LESS THAN C'A'                                              
         EDIT  (B1,0(R3)),(2,WORK+20),ALIGN=LEFT,ZERO=NOBLANK                   
         MVC   WORK(3),WORK+20                                                  
         LR    R5,R0               R0 = # OF SIGNIFICANT CHARACTERS             
         B     SENDP20                                                          
*                                                                               
SENDP15  MVC   WORK(1),0(R3)                                                    
*                                                                               
SENDP20  LR    R1,R6             R1 GETS TRASHED BY SENDD                       
         BAS   RE,SENDD                                                         
         LA    R3,1(R3)                                                         
         BCT   R2,SENDP10                                                       
         XIT1                                                                   
         EJECT                                                                  
         SPACE 3                                                                
*                                                                               
* CHECK SECURITY FOR CLIENT                                                     
*                                                                               
CHKSEC   NTR1                                                                   
         L     R2,AIO1                                                          
         USING CLTHDR,R2                                                        
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCCLT,QCLT                                                      
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),TWAACCS                                                
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,ASECBLK                                                  
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),ACOMFACS                                   
         CLI   0(R1),0                                                          
         BE    CHKSECEX                                                         
*                                                                               
         MVC   ERROR,=AL2(178)      ACCESS TO CLIENT NOT AUTHORIZED             
         XC    ERRORMSG,ERRORMSG                                                
         GOTO1 SENDMSG                                                          
*                                                                               
CHKSECEX XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
* GET PROGRAM RECORDS PASS BACK REQUESTED DEMOS                                 
*                                                                               
GETDEMS  NTR1                                                                   
         TM    SVINDS1,X'60'        CHECK FOR LIMITED RETRIEVAL                 
         BNZ   *+8                                                              
         OI    SVINDS1,X'60'        SET TO PASS HUTS/VPHS                       
         GOTO1 VDATCON,DMCB,(2,SVSDATE),(3,VPHSTDAT)                            
         GOTO1 VDATCON,DMCB,(2,SVEDATE),(3,VPHENDAT)                            
         MVC   PERIOD2B,SVSDATE                                                 
         MVC   PERIOD3B,VPHSTDAT                                                
*                                                                               
         MVC   SVPRGCD,SVWIZCLT                                                 
         MVI   SVWIZCLT,X'40'                                                   
         CLI   SVPRGCD,0                                                        
         BE    GETDMEX                                                          
*                                                                               
*  CLEAR AND INITIALIZE DEMO BUILD AREA                                         
         L     RE,AIO2                                                          
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         L     R2,AIO2                                                          
         USING DEMREC,R2                                                        
         MVC   PREL(3),=X'350902'                                               
         MVC   PVEL(3),=X'33F302'                                               
         MVC   PUEL(3),=X'31F344'                                               
*                                                                               
         BAS   RE,VPHSET            SET DEMOS TO VPH'S                          
         BAS   RE,READPROF          GET PROFILES                                
         BAS   RE,READSTA           GET POSTING TYPE FROM STATION               
                                                                                
*******  BAS   RE,GETUNIV           GET UNIVERSE VALUES                         
GETDM20  BAS   RE,GETPROG           GET VPH VALUES                              
         CLC   KEY(11),KEYSAVE      NO PROGRAM FOR PERIOD                       
         BNE   GETDM180             GET NEXT RECORD                             
         BAS   RE,GETHUT            GET HUT VALUES                              
         BAS   RE,GETUNIV           GET UNIVERSE VALUES                         
*                                                                               
         TM    SVINDS1,X'40'        WERE VPH'S REQUESTED                        
         BZ    GETDM60                                                          
         BAS   RE,SETDB                                                         
         GOTO1 VDEMOUT,DMCB,(C'L',QDEMOS),BLOCK,WORK2                           
*                                                                               
*   SEND DATA TO THE PC                                                         
GETDM60  BAS   RE,SENDVPH                                                       
*                                                                               
*   GET NEXT PERIOD                                                             
*                                                                               
         BAS   RE,BUMPDATE                                                      
         CLC   PERIOD3B,VPHENDAT                                                
         BNH   GETDM20                                                          
*                                                                               
*  ALL VPH'S HAVE BEEN PASSED FOR THIS PROGRAM                                  
*  RESTART DATES AND GO TO NEXT PROGRAM CODE                                    
*                                                                               
GETDM180 MVC   PERIOD3B,VPHSTDAT                                                
         GOTO1 VDATCON,DMCB,(3,PERIOD3B),(2,PERIOD2B)                           
*                                                                               
*   GET NEXT PROGRAM                                                            
         LA    RE,SVWIZCLT                                                      
*                                                                               
GETDM190 CLI   0(RE),0                                                          
         BE    GETDMEX                                                          
         CLI   0(RE),X'40'                                                      
         BNE   GETDM200                                                         
         LA    RE,6(RE)                                                         
         B     GETDM190                                                         
GETDM200 MVC   SVPRGCD,0(RE)                                                    
         MVI   0(RE),X'40'                                                      
         B     GETDM20                                                          
*                                                                               
GETDMEX  XIT1                                                                   
         SPACE 3                                                                
*                                                                               
*  GET HUT VALUES                                                               
*                                                                               
*              ROUTINE TO LOOK UP HUTS FOR PROGRAM/PERIOD                       
         SPACE 3                                                                
GETHUT   NTR1                                                                   
* HAD TO BE REMOVED PRVENTED CALCULATION OF CERTAIN VPHS                        
***      TM    SVINDS1,X'20'        WERE HUTS REQUESTED                         
***      BZ    GETHUTEX                                                         
*                                                                               
         L     R2,AIO2                                                          
         USING DEMREC,R2                                                        
         LA    R4,WORK2                                                         
         USING GETHUTD,R4                                                       
         XC    GHBLOCK,GHBLOCK                                                  
*                                                                               
*  GET DAY AND TIME FROM PROGRAM RECORD                                         
         L     R3,AIO4              POINTS TO PROGRAM RECORD                    
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'92',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
         USING NPGEL92,R3                                                       
         MVC   GHREPDAY,NPGRDAY                                                 
         MVC   GHMILTIM,NPGTIME                                                 
         DROP  R3                                                               
*                                                                               
         MVI   GHSCHEME,X'FE'      PRESET FOR YEAR RECORDS                      
         MVI   GH52,C'Y'           AND FOR 52 WEEK HUTS                         
***      CLI   N1PROF+3,0          PROFILE OPTION ON 52 WEEKS                   
***      BE    *+10                                                             
***      MVC   GH52,N1PROF+3                                                    
         MVC   GHDATE,PERIOD2B     USE ACTUAL DATE FOR WEEKLIES                 
         CLI   PLANPERT,C'W'                                                    
         BE    GETHUT3                                                          
         MVC   DUB(2),PERIOD3B     PRESET TO 15TH OF MONTH                      
         MVI   DUB+2,15                                                         
         CLI   PLANPERT,C'Q'       UNLESS THIS IS QUARTERLY                     
         BNE   GETHUT2                                                          
         ZIC   R1,PERIOD3B+1       PICK UP QUARTER (1-4)                        
*******  BCTR  R1,0                (0-3)                                        
*******  MH    R1,=H'3'            (0,3,6,9)                                    
*******  LA    R1,2(R1)            (2,5,8,11)                                   
         STC   R1,DUB+1                                                         
         SPACE 1                                                                
GETHUT2  GOTO1 VDATCON,DMCB,(3,DUB),(2,GHDATE)                                  
         SPACE 1                                                                
GETHUT3  MVC   GHAVE,PLANHTAV                                                   
         MVC   GHYEAR,PLANHTYR                                                  
         ZIC   R1,GHYEAR            CHECK FOR Y2K                               
         CH    R1,=H'50'                                                        
         BH    *+12                                                             
         LA    R1,100(R1)                                                       
         STCM  R1,1,GHYEAR                                                      
         MVC   GHNYEARS,PLANHTNO                                                
         MVI   GHBKTYPE,C'A'                                                    
         MVC   GHFLAVOR,N2PROF+2   HUT FLAVOR COMES FROM N2 PROFILE             
         CLI   PLANHTFL,0          BUT CAN BE OVERRIDDEN                        
         BE    *+10                                                             
         MVC   GHFLAVOR,PLANHTFL                                                
         CLI   PLANHTSC,0          IS THERE AN AGENCY SCHEME                    
         BE    GETHUT4                                                          
         MVC   GHSCHEME,PLANHTSC   THEN USE THIS                                
         MVC   GHAGYMED,BAGYMD     PASS AGENCY/MEDIA                            
         MVC   GHDEFDEM,N1PROF+4   OPTION TO DEFAULT TO DDS                     
         SPACE 1                                                                
GETHUT4  MVC   GHCOMFCS,ACOMFACS                                                
         MVC   GHNETWK,VNETWEEK                                                 
         GOTO1 VGETHUT,DMCB,(R4)                                                
         MVC   PREL(3),=XL3'350902'                                             
         MVC   HUT,GHHUT                                                        
         OC    PLANHTPO,PLANHTPO   POSSIBLE PERCENT OVERRIDE                    
         BZ    GETHUT6                                                          
         SR    R1,R1                                                            
         ICM   R1,3,HUT            YES - SO APPLY                               
         LH    R0,PLANHTPO                                                      
         MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STCM  R1,3,HUT                                                         
         SPACE 1                                                                
GETHUT6  OC    SHARE,SHARE         COMPUTE RATING                               
         BZ    GETHUTEX                                                         
         SR    R0,R0                                                            
         ICM   R0,3,HUT                                                         
         SR    R1,R1                                                            
         ICM   R1,3,SHARE                                                       
         MR    R0,R0                                                            
         D     R0,=F'500'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STCM  R1,3,RATING                                                      
GETHUTEX XIT1                                                                   
         EJECT                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO LOOK UP PROGRAM DETAILS FROM SPOT FILE                
         SPACE 3                                                                
GETPROG  NTR1                                                                   
         L     R4,AIO2                                                          
         USING DEMREC,R4                                                        
*                                                                               
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPGRECD,R3                                                       
         MVC   NPGKTYP,=XL2'0D20'                                               
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,BMKT                                                     
         MVC   NPGKPROG,SVPRGCD                                                 
         MVC   NPGKEND,PERIOD2B                                                 
*                                                                               
         CLI   PLANPERT,C'W'                                                    
         BE    GETPROG3                                                         
         MVC   DUB(2),PERIOD3B     PRESET TO 1ST OF MONTH                       
         MVI   DUB+2,1                                                          
         CLI   DUB+1,9             EXCEPT SEPTEMBER USE 19TH                    
         BNE   *+8                 (THIS WAS GOOD FOR 1988)                     
         MVI   DUB+2,19            (MAY NEED TO REVISE LATER)                   
         CLI   PLANPERT,C'Q'       IF THIS IS QUARTERLY                         
         BNE   GETPROG2               USE START OF QUARTER                      
         ZIC   R1,PERIOD3B+1       PICK UP QUARTER MONTH (2,5,8,11)             
         BCTR  R1,0                (0-3)                                        
******   MH    R1,=H'3'            (0,3,6,9)                                    
******   LA    R1,1(R1)            (1,4,7,10)                                   
         STC   R1,DUB+1                                                         
         CLI   DUB+1,10            IF 4TH QUARTER                               
         BNE   GETPROG2            LISA                                         
         MVI   DUB+1,11            SAYS USE NOV 1                               
         MVI   DUB+2,1                                                          
         SPACE 1                                                                
GETPROG2 GOTO1 VDATCON,DMCB,(3,DUB),(2,NPGKEND)                                 
         SPACE 1                                                                
GETPROG3 GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(11),KEYSAVE     SHOULD MATCH ON PROGRAM                      
         BNE   GETPRGEX                                                         
         CLC   CURKEY,KEY           SEE IF PROGRAM RECORD CHANGED               
         BE    GETPROG5                                                         
         MVI   PROGBRK,C'Y'                                                     
         MVC   CURKEY,KEY                                                       
         SPACE 1                                                                
GETPROG4 GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO4                                    
         TM    SVINDS1,X'40'        WERE VPHS'S REQUESTED                       
         BZ    GETPRGEX             DONT PROCESS PROGRAM RECORDS                
*                                                                               
GETPROG5 L     R3,AIO4                                                          
         MVC   PVEL(3),=X'33F302'                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'92',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,12(R1)                                                        
         USING NPGELEM,R5                                                       
*                                                                               
         MVC   PROGNTI,NPGPPNO                                                  
         MVC   PRROT,NPGROT                                                     
*                                                                               
         XC    VPHS,VPHS                                                        
         LA    R2,NPGVPHS          MULTIPLY THESE BY 10                         
         LA    RE,VPHS                                                          
         LA    R0,34                                                            
         SPACE 1                                                                
GETPROG6 ZIC   R1,0(R2)                                                         
         MH    R1,=H'10'                                                        
         STH   R1,0(RE)                                                         
         LA    R2,1(R2)                                                         
         LA    RE,2(RE)                                                         
         BCT   R0,GETPROG6                                                      
         SPACE 1                                                                
         XC    RATING,RATING                                                    
         MVC   SHARE,NPGSHARE      CAN BE SHARE                                 
         TM    NPGSTAT,X'80'                                                    
         BNO   *+16                                                             
         MVC   RATING,NPGSHARE     OR RATING                                    
         XC    SHARE,SHARE                                                      
         DROP  R5                                                               
         SPACE 1                                                                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'93',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   GETPROG8                                                         
         L     R5,12(R1)                                                        
         USING NPGEL93,R5                                                       
*                                                                               
         XC    VPHS,VPHS                                                        
         ZIC   R1,1(R5)                                                         
         SHI   R1,NPG2OVRL+1       ELEMENT OVERHEAD LENGTH (+1 FOR EX)          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   VPHS(0),NPG2VPHS                                                 
         MVC   VPHS+L'NPG2VPHS(L'NPG2VPH2),NPG2VPH2    M12-14, ET. AL.          
         DROP  R5                                                               
*                                                                               
*  MOVE IN NAD VPH OVERRIDE ELEMENTS                                            
         LA    R6,OVERAREA         AND MOVE TO N(AD)OVER AREA                   
OEL      USING OVEREL,R6                                                        
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'DD',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   GETPROG8                                                         
         L     R5,12(R1)                                                        
         USING NPGELDD,R5                                                       
         B     GETPRG7A                                                         
*                                                                               
GETPROG7 ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         CLI   0(R5),X'DD'                                                      
         BNE   GETPROG8                                                         
*                                                                               
GETPRG7A CLI   NPGDCAT,0           IS DEMO NAD                                  
         BE    GETPROG7            NO,GET NEXT OVERRIDE                         
         CLI   NPGDCAT+1,C'V'      IS DEMO NAD                                  
         BNE   GETPROG7            NO,GET NEXT OVERRIDE                         
         MVC   OEL.OVEREL,=X'DD0C'                                              
         MVC   OEL.OVERDEMO,NPGDCAT                                             
         MVC   OEL.OVERAMNT,NPGDAMT                                             
         MVI   OEL.OVERFLG,X'80'                                                
         MVI   OEL.OVERPREC,X'40'                                               
         LA    R6,12(R6)                                                        
GETPRG7B B     GETPROG7                                                         
         DROP  OEL                                                              
         SPACE 1                                                                
*                                                                               
*   GET THE BOOK ELEMENT                                                        
GETPROG8 GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'5D',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         MVC   PBEL,0(RE)                                                       
*                                                                               
GETPRGEX XIT1  XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
*   GET UNIVERSE INFO FOR DEMO LOOKUP                                           
GETUNIV  NTR1                                                                   
         L     R4,AIO2                                                          
         USING DEMREC,R4                                                        
         LA    R5,BLOCK                                                         
         USING GUVD,R5                                                          
         XC    GUVBLOCK,GUVBLOCK                                                
         MVC   GUVAGY,QAGY                                                      
         SPACE 1                                                                
         XC    WORK2(250),WORK2                                                 
         LA    R1,UNIVS                                                         
         ST    R1,GUVAOUT                                                       
         MVI   GUVTYPE,2           (HUNDREDS)                                   
         MVC   GUVAREC,AIO3                                                     
         MVC   GUVCMFCS,ACOMFACS                                                
*                                                                               
         MVC   GUVCODE,SVUNIV       MOVE UNIVERSE CODE                          
         OC    SVUNIV,SVUNIV        WAS UNIVERSE PASSED                         
         BNZ   *+10                 GET DEFAULT UNIVERSE                        
         MVC   GUVDATE,SVUNIVDT     GET NTI UNIVERSE                            
*                                                                               
         GOTO1 VGETNUN,DMCB,GUVBLOCK                                            
*                                                                               
         CLI   GUVERROR,0                                                       
         BE    GTUNIV30                                                         
         MVC   ERROR,=AL2(240)      UNIVERSE NOT FOUND                          
         XC    ERRORMSG,ERRORMSG                                                
         GOTO1 SENDMSG                                                          
         B     GETUNVEX                                                         
         DROP  R5                                                               
*--SET UP UNIVERSE ELEMENT                                                      
GTUNIV30 MVC   PUEL(3),=X'31F344'                                               
*-POSITION OVERRIDE AREA FOR NAD UNIVERSE                                       
         OC    SVUNIV,SVUNIV        WAS UNIVERSE PASSED                         
         BZ    GTUNIV80             NO THEN NO NAD OVERRIDE                     
*                                                                               
         L     R2,AIO3              UNIVERSE RECORD                             
         LA    R5,OVERAREA                                                      
OEL      USING OVEREL,R5                                                        
GTUNIV40 CLI   0(R1),0                                                          
         BE    GTUNIV45                                                         
         LA    R1,12(R1)                                                        
         B     GTUNIV40                                                         
*                                                                               
GTUNIV45 GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'DD',(R2)),0                        
         CLI   12(R1),0                                                         
         BNE   GTUNIV80                                                         
         L     RE,12(R1)                                                        
         USING NUNELDD,RE                                                       
         B     GTUNIV60                                                         
*                                                                               
GTUNIV50 ZIC   RF,1(RE)             GET NEXT ELEMENT                            
         AR    RE,RF                                                            
         CLI   0(RE),X'DD'                                                      
         BNE   GTUNIV80                                                         
*                                                                               
GTUNIV60 MVC   OEL.OVEREL(2),=X'DD0C'                                           
         MVC   OEL.OVERDEMO,NUNDCAT                                             
         MVC   OEL.OVERAMNT,NUNDAMT                                             
         MVI   OEL.OVERFLG,X'80'                                                
         MVI   OEL.OVERPREC,X'42'                                               
         AHI   R5,OVERELNQ                                                      
         B     GTUNIV50                                                         
*                                                                               
GTUNIV80 CLI   POSTTYP,C'C'                                                     
         BNE   GTUN100                                                          
         LA    R2,UNIVS                                                         
         LA    R3,60                                                            
         SPACE 1                                                                
GTUNIV90 L     R1,0(R2)            ADJUST UNIVS FOR NON NETWORK                 
         M     R0,=F'10'                                                        
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R3,GTUNIV90                                                      
*                                                                               
* RETURN CUNIVERSE VALUES                                                       
*                                                                               
GTUN100  L     R2,AIO3              UNIVERSE RECORD                             
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'DE',(R2)),0                        
         CLI   12(R1),0                                                         
         BNE   GETUNVEX                                                         
         L     RE,12(R1)                                                        
         USING NUCELDD,RE                                                       
         B     GTUN160                                                          
*                                                                               
GTUN150  ZIC   RF,1(RE)             GET NEXT ELEMENT                            
         AR    RE,RF                                                            
         CLI   0(RE),NUCDELQ                                                    
         BNE   GETUNVEX                                                         
*                                                                               
GTUN160  MVC   OEL.OVEREL(NUCDLENQ),NUCDEMEL                                    
         AHI   R5,OVERELNQ                                                      
         B     GTUN150                                                          
         DROP  R4,OEL                                                           
*                                                                               
GETUNVEX XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   BUILD DBLOCK                                                                
*                                                                               
SETDB    NTR1                                                                   
         LA    R3,BLOCK                                                         
         USING DBLOCKD,R3                                                       
*                                                                               
         TM    SVINDS1,SVI1NTIQ                                                 
         JO    SETDB20                                                          
*                                                                               
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK FOR EVN                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'EVN'                                                   
*        MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'V'                                                    
*        MVI   DBSELMED,C'T'                                                    
         MVC   DBSELAGY,QAGY                                                    
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBSELSTA+4,C'N'                                                  
*                                                                               
         BAS   RE,GETTVQ           CHECK TVQ                                    
*  SET FOR TVQ LOOKUP                                                           
         XC    DBEXTEND,DBEXTEND                                                
         L     RF,AIO3                                                          
         USING TVQBLOCK,RF                                                      
         CLI   GOTUTYPE,1                                                       
         BNE   SETDB10                                                          
         MVC   BTUEXT,=C'UFIL'                                                  
         MVC   BTUEX2,BTUDB+4                                                   
         LA    RE,BTUEXT                                                        
         ST    RE,DBEXTEND                                                      
         DROP  RF                                                               
*                                                                               
SETDB10  L     R1,AIO2              BUIILT DEMO RECORD                          
         ST    R1,DBAREC                                                        
         LA    R1,22(R1)                                                        
         ST    R1,DBAQUART          START OF RECORD                             
         J     SETDBX                                                           
*                                                                               
SETDB20  XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBAREC,AIO                                                       
         MVC   DBSELAGY,QAGY                                                    
         MVI   DBFUNCT,DBGETNTI                                                 
         MVC   DBSELSTA,=C'FOX T'                                               
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         GOTO1 VDATCON,DMCB,(2,SVUNIVDT),(0,WORK)                               
         GOTO1 VNETWEEK,DMCB,WORK,VGETDAY,VADDAY                                
         MVC   DBSELBK(1),DMCB+4     YEAR NO.                                   
         MVC   DBSELBK+1(1),DMCB     WEEK NO.                                   
*                                                                               
SETDBX   XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* READ FOR USER DEFINED ADJUSTMENT FACTORS - TVQ                                
*                                                                               
GETTVQ   NTR1                                                                   
         LA    R3,BLOCK                                                         
         USING DBLOCKD,R3                                                       
         L     R5,AIO3                                                          
         USING TVQBLOCK,R5                                                      
         MVI   GOTUTYPE,0                                                       
         XC    BTUEXT,BTUEXT                                                    
         XC    BTUEX1,BTUEX1                                                    
         XC    BTUEX2,BTUEX2                                                    
*                                                                               
         LA    RE,QDEMOS                                                        
BTUR10   CLI   0(RE),X'FF'                                                      
         BE    BTUXIT                                                           
         CLI   0(RE),171                                                        
         BE    BTUR12                                                           
         LA    RE,3(RE)                                                         
         B     BTUR10                                                           
*                                                                               
BTUR12   OC    SVTVQBK,SVTVQBK      ANY BOOK INPUTTED                           
         BZ    BTUXIT               NO EXIT                                     
*                                                                               
         OC    PROGNTI,PROGNTI                                                  
         BZ    BTUXIT                                                           
         MVI   DBMODE,DBMDSEQ                                                   
         MVC   BTUDB,DBLOCK                                                     
         LA    R3,BTUDB                                                         
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBMODE,DBMFRST                                                   
         MVI   DBFUNCT,DBGETNTI                                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBAREC,AIO2                                                      
         MVC   DBSELPRG,PROGNTI                                                 
         MVC   DBSELSTA,BNET                                                    
         CLI   NTISTA,X'40'                                                     
         BNH   *+10                                                             
         MVC   DBSELSTA,NTISTA                                                  
         MVC   DBSELSTA+4(1),POSTTYP                                            
         CLC   DBSELSTA+3(2),=C'PN'                                             
         BNE   *+8                                                              
         MVI   DBSELSTA+3,C' '                                                  
         CLI   DBSELSTA+4,C'T'                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'N'                                                  
         CLI   DBSELSTA+4,C'S'                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'M'                                                  
         MVI   DBBTYPE,C'U'                                                     
         MVI   DBSELDAY,0                                                       
         XC    DBSELTIM(4),DBSELTIM                                             
         MVI   DBSELDUR,X'FF'      ALL DURATIONS                                
         MVI   DBSELDAY,0                                                       
         XC    DBSELTIM(4),DBSELTIM                                             
         MVI   DBPRGDUR,C'Y'                                                    
         MVI   DBBEST,C'A'                                                      
         MVI   DBSELDUR,X'FF'                                                   
         GOTO1 VDATCON,DMCB,(2,SVTVQBK),(0,WORK+3)                              
         GOTO1 VNETWEEK,DMCB,WORK+3,VGETDAY,VADDAY                              
         MVC   DBSELBK(1),4(R1)                                                 
         MVC   DBSELBK+1(1),8(R1)                                               
*                                                                               
         L     R2,ACOMFACS                                                      
         USING COMFACSD,R2                                                      
         GOTO1 CDEMTABS,DMCB,NETVQCAL                                           
         ICM   RE,15,0(R1)   R2->MONTH/DATES TABLE                              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,4(R1)            LENGTH OF TABLE ENTRY                     
         USING NETVQD,RE                                                        
*                                                                               
BTUR2    CLI   0(RE),X'FF'         NOT FOUND - ALLOW A MISS                     
         BE    *+14                                                             
         XC    DBSELBK,DBSELBK     BK IS HI, LK UP LATEST                       
         B     BTUR4                                                            
         CLC   DBSELBK,NETVQEWK    CURRENT OUTSIDE RANGE                        
         BH    BTUR3               GET NEXT                                     
         CLC   DBSELBK,NETVQSWK    TOTALLY OUTSIDE RANGE - ALLOW MISS           
         BL    BTUR4               (BK IS LOW)                                  
         MVC   DBSELBK(2),NETVQMTH FOUND THE EQUATE - USE IT                    
         B     BTUR4                                                            
BTUR3    AR    RE,RF                                                            
         B     BTUR2                                                            
         DROP  RE                                                               
**       LA    RE,TVQDATES         YES - CONVET TO TVQ BOOK                     
**BTUR2    CLI   0(RE),X'FF'         NOT FOUND - ALLOW A MISS                   
**         BE    BTUR4                                                          
**         CLC   DBSELBK,2(RE)       CURRENT OUTSIDE RANGE                      
**         BH    BTUR3               GET NEXT                                   
**         CLC   DBSELBK,0(RE)       TOTALLY OUTSIDE RANGE - ALLOW MISS         
**         BL    BTUR4                                                          
**         MVC   DBSELBK(2),4(RE)    FOUND THE EQUATE - USE IT                  
**         B     BTUR4                                                          
**BTUR3    LA    RE,6(RE)                                                       
**         B     BTUR2                                                          
*                                                                               
BTUR4    XC    DBAQUART,DBAQUART                                                
         XC    BTUAQ,BTUAQ                                                      
         LA    RE,BTUIO                                                         
         ST    RE,DBAREC                                                        
         L     RF,=F'1000'                                                      
         XCEF                                                                   
         DROP  R3                                                               
*                                                                               
         GOTO1 VDEMAND,BTUDMCB,BTUDB,BTUHK                                      
*                                                                               
         LA    RF,BTUDB                                                         
         USING DBLOCK,RF                                                        
         OC    DBAQUART(4),DBAQUART                                             
         BNZ   BTUXIT                                                           
         CLI   GOTUTYPE,1                                                       
         BNE   BTUXIT                                                           
         MVC   DBAQUART(4),BTUAQ                                                
         B     BTUXIT                                                           
BTUHK    LA    RF,BTUDB                                                         
         MVC   BTUAQ,8(RF)                                                      
         MVI   GOTUTYPE,1                                                       
         BR    RE                                                               
BTUXIT   XIT1                                                                   
         DROP  RF                                                               
NETVQCAL EQU   37                                                               
         EJECT                                                                  
*                                                                               
*  READ STATION RECORD CHECK POSTING TYPE                                       
*                                                                               
READSTA  NTR1                                                                   
         MVC   AIO,AIO3                                                         
         LA    R5,KEY                                                           
         USING STAREC,R5                                                        
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),BNET                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,QAGY                                                     
         MVC   STAKCLT(6),=6X'F0'                                               
*                                                                               
         GOTO1 AIOCALL,DMCB,STA+FIL+HIGH,AIO3                                   
         CLC   KEY(9),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         MVC   POSTTYP,STYPE                                                    
         MVC   NTISTA,SNTISTA                                                   
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
* SET DEMOS UP AS VPHS                                                          
*                                                                               
VPHSET   NTR1                                                                   
         LA    RE,QDEMOS                                                        
VPHS10   CLI   0(RE),X'FF'                                                      
         BE    VPHSEX                                                           
         MVI   1(RE),C'V'                                                       
         LA    RE,3(RE)                                                         
         B     VPHS10                                                           
VPHSEX   XIT1                                                                   
         SPACE 2                                                                
*                                                                               
* SET DEMOS UP AS UNIVERSES                                                     
*                                                                               
UNIVSET  NTR1                                                                   
         LA    RE,QDEMOS                                                        
UNVS10   CLI   0(RE),X'FF'                                                      
         BE    UNVSEX                                                           
         MVI   1(RE),C'U'                                                       
         LA    RE,3(RE)                                                         
         B     UNVS10                                                           
UNVSEX   XIT1                                                                   
         EJECT                                                                  
*                                                                               
* BUMP DATE TO NEXT VPH LOOKUP PERIOD                                           
*                                                                               
BUMPDATE NTR1                                                                   
         CLI   PLANPERT,C'M'                                                    
         BE    BMPDT020                                                         
         CLI   PLANPERT,C'W'                                                    
         BE    BMPDT060                                                         
*  BUMP DATE TO NEXT QUARTER                                                    
         MVC   DUB(3),PERIOD3B                                                  
         ZIC   RE,DUB+1                                                         
         AH    RE,=H'3'                                                         
         STC   RE,DUB+1                                                         
         CLI   DUB+1,12                                                         
         BH    BMPDT100             BUMP TO NEXT YEAR                           
         MVC   PERIOD3B,DUB                                                     
         B     BMPDTEX                                                          
*  BUMP DATE TO NEXT MONTH                                                      
BMPDT020 MVC   DUB(3),PERIOD3B                                                  
         ZIC   RE,DUB+1                                                         
         AH    RE,=H'1'                                                         
         STC   RE,DUB+1                                                         
         CLI   DUB+1,12                                                         
         BH    BMPDT100             BUMP TO NEXT YEAR                           
         MVC   PERIOD3B,DUB                                                     
         B     BMPDTEX                                                          
*  BUMP DATE TO NEXT WEEK                                                       
BMPDT060 GOTO1 VDATCON,DMCB,(3,PERIOD3B),(0,DUB)                                
         LA    R0,7                                                             
         GOTO1 VADDAY,DMCB,DUB,CALCDATE,(R0)                                    
         GOTO1 VDATCON,DMCB,(0,CALCDATE),(3,PERIOD3B)                           
         B     BMPDTEX                                                          
*  BUMP YEAR IF NECESSARY                                                       
BMPDT100 ZIC   R6,DUB                                                           
         AH    R6,=H'1'                                                         
         STC   R6,DUB                                                           
         ZIC   R6,DUB+1                                                         
         SH    R6,=H'12'                                                        
         STC   R6,DUB+1                                                         
         MVC   PERIOD3B,DUB                                                     
         BE    BMPDTEX                                                          
*                                                                               
BMPDTEX  GOTO1 VDATCON,DMCB,(3,PERIOD3B),(2,PERIOD2B)                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* BUMP DATE TO NEXT VPH LOOKUP PERIOD                                           
*  PROGRAM RECORD IN AIO4                                                       
*  SVINDS1 X'60' = BOTH VPH'S AND HUTS                                          
*          X'40' = VPH'S ONLY                                                   
*          X'20' = HUTS ONLY                                                    
*                                                                               
SENDVPH  NTR1                                                                   
         L     R3,AIO4                                                          
         USING NPGRECD,R3                                                       
*                                                                               
         CLI   PROGBRK,C'Y'         CHECK FOR NEW PROGRAM                       
         BNE   SNDVP200             NO PASS 87 ELEMENT                          
*                                                                               
         LHI   R1,X'85'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
         LHI   R1,X'01'                                                         
         LA    R4,NPGKPROG          PROGRAM CODE                                
         BAS   RE,SENDD                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PERIOD3B),(2,DUB)                                
         LHI   R1,X'02'                                                         
         LA    R4,DUB               PERIOD DATE                                 
         BAS   RE,SENDD                                                         
*                                                                               
*  SEND INFO FROM 92 ELEMENT OF THE PROGRAM RECORD                              
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'92',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,12(R1)                                                        
         USING NPGEL92,R2                                                       
*                                                                               
*  OUTPUT THE ROTATION                                                          
         LHI   R1,X'03'                                                         
         LA    R4,NPGROT                                                        
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE DAY                                                               
         LA    R4,DAYTBL                                                        
SNDVP110 CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   NPGRDAY,0(R4)                                                    
         BE    SNDVP115                                                         
         LA    R4,5(R4)                                                         
         B     SNDVP110                                                         
SNDVP115 LA    R4,1(R4)                                                         
         LHI   R1,X'04'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE TIME                                                              
         XC    WORK,WORK                                                        
         GOTO1 VUNTIME,DMCB,NPGTIME,WORK                                        
         LHI   R1,X'05'                                                         
         LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE PROGRAM NAME                                                      
         LHI   R1,X'06'                                                         
         LA    R4,NPGNAME                                                       
         BAS   RE,SENDD                                                         
*                                                                               
* CHECK FOR HUT ONLY INFORMATION                                                
         TM    SVINDS1,X'40'       DO YOU WANT VPH INFORMATION                  
         BZ    SNDVP200            NO DONT SEND 86 ELEMENT                      
*                                                                               
*  THE FOLLOWING FIELDS ARE WITHIN THE 86 ELEMENT                               
*                                                                               
         LHI   R1,X'86'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
*  OUTPUT THE RATING/SHARE                                                      
         LHI   R1,X'08'             SET AS A RATING                             
         TM    NPGSTAT,X'80'        IS VALUE A RATING                           
         BO    *+8                                                              
         LHI   R1,X'09'             SET AS A SHARE                              
         LA    R4,NPGSHARE                                                      
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE NTI CODE                                                          
         LHI   R1,X'03'                                                         
         XC    DUB,DUB                                                          
         MVC   DUB+2(2),NPGPPNO                                                 
         LA    R4,DUB                                                           
         BAS   RE,SENDD                                                         
*                                                                               
         DROP  R2                                                               
*                                                                               
         LHI   R1,X'02'                                                         
         LA    R4,NPGKEND           END DATE                                    
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R6,QDEMOS            REQUESTED DEMOS                             
         LA    R4,WORK2                                                         
SNDVP40  CLI   0(R6),X'FF'          CHECK END OF DEMOS                          
         BE    SNDVP45                                                          
*                                                                               
*  GET REQUESTED VPH'S TO THE RIGHT PRECISION                                   
         SR    R0,R0                                                            
         L     R1,0(R4)                                                         
         D     R0,=F'10'                                                        
         ST    R1,0(R4)                                                         
*                                                                               
         LHI   R1,X'0A'                                                         
         BAS   RE,SENDD                                                         
         LA    R6,3(R6)                                                         
         LA    R4,4(R4)                                                         
         B     SNDVP40                                                          
*                                                                               
SNDVP45  L     R2,AIO2                                                          
         USING DEMREC,R2                                                        
         LA    R6,NPG2#VPHS        NUMBER OF VPH VALUES IN 1ST SET              
         LA    R4,VPHS                                                          
SNDVP50  LHI   R1,X'0B'                                                         
         BAS   RE,SENDD             VPH VALUES                                  
         LA    R4,2(R4)                                                         
         BCT   R6,SNDVP50                                                       
*                                                                               
         LA    R6,NPG2#VPH2        NUMBER OF VPH VALUES IN 2ND SET              
*                                  LEAVE A GAP FOR THE 2 MISSING VPHS           
         AHI   R4,4                BUMP TO NEXT LIST OF VPH'S                   
SNDVP55  DS    0H                                                               
         LHI   R1,X'0B'            CORRESPONDS TO NPG2VPH2                      
         BAS   RE,SENDD             M12-14 (SCHT 6/18) ET. AL.                  
         LA    R4,2(R4)                                                         
         BCT   R6,SNDVP55                                                       
*                                                                               
         DROP  R2                                                               
*                                                                               
*  OUTPUT THE NEW/TIER/PROGRAMTYPE/CONTENT FRONTRUNNER ONLY                     
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'03',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   SNDVP100                                                         
         L     R2,12(R1)                                                        
         USING NPGEL03,R2                                                       
*                                                                               
         OC    NPPRNEW,NPPRNEW                                                  
         BZ    SNDVP70              NEW PROGRAM                                 
         LHI   R1,X'04'                                                         
         LA    R4,NPPRNEW                                                       
         BAS   RE,SENDD                                                         
*                                                                               
SNDVP70  OC    NPPRGTYP,NPPRGTYP                                                
         BZ    SNDVP75              PROGRAM TYPE                                
         MVC   DUB(2),NPPRGTYP                                                  
         MVC   DUB+2(4),NPPRGSTP                                                
         OC    DUB(6),SPACES                                                    
         LHI   R1,X'05'                                                         
         LA    R4,DUB                                                           
         BAS   RE,SENDD                                                         
*                                                                               
SNDVP75  OC    NPPRGRAT,NPPRGRAT                                                
         BZ    SNDVP80              CONTENT                                     
         LHI   R1,X'06'                                                         
         LA    R4,NPPRGRAT                                                      
         BAS   RE,SENDD                                                         
*                                                                               
SNDVP80  OC    NPTIER,NPTIER                                                    
         BZ    SNDVP100             TIER                                        
         LHI   R1,X'07'                                                         
         LA    R4,NPTIER                                                        
         BAS   RE,SENDD                                                         
         DROP  R2                                                               
*                                                                               
*  SEND INFO FROM 93 ELEMENT OF THE PROGRAM RECORD                              
SNDVP100 GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'93',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   SNDVP150                                                         
         L     R2,12(R1)                                                        
         USING NPGEL93,R2                                                       
*                                                                               
         OC    NPG2STD,NPG2STD                                                  
         BZ    SNDVP120             START DATE                                  
         LHI   R1,X'01'                                                         
         LA    R4,NPG2STD                                                       
         BAS   RE,SENDD                                                         
*                                                                               
SNDVP120 OC    NPG2DYPA,NPG2DYPA                                                
         BZ    SNDVP150             DAYPART CODE                                
         LHI   R1,X'0F'                                                         
         LA    R4,NPG2DYPA                                                      
         BAS   RE,SENDD                                                         
         DROP  R2                                                               
*                                                                               
*  PASS PROGRAM NAD INFO                                                        
*                                                                               
SNDVP150 GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'DD',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   SNDVP180                                                         
         L     R2,12(R1)                                                        
         USING NPGELDD,R2                                                       
*                                                                               
SNDVP160 CLI   NPGDCAT,0            CHECK FOR NAD                               
         BE    SNDVP170                                                         
         CLI   NPGDMOD,C'V'         CHECK FOR VPH                               
         BNE   SNDVP170                                                         
         LHI   R1,X'0C'                                                         
         LA    R4,NPGDCAT           NAD CATEGORY                                
         BAS   RE,SENDD                                                         
         LHI   R1,X'0D'                                                         
         LA    R4,NPGDNUM           NAD DEMO NUMBER                             
         BAS   RE,SENDD                                                         
         LHI   R1,X'0E'                                                         
         LA    R4,NPGDAMT           NAD DEMO AMOUNT                             
         BAS   RE,SENDD                                                         
*                                                                               
*  GET NEXT DD ELEMENT                                                          
*                                                                               
SNDVP170 ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         CLI   0(R2),X'DD'                                                      
         BE    SNDVP160                                                         
*                                                                               
*  OUTPUT THE PRECISION FACTOR                                                  
SNDVP180 GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'5D',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                 MUST BE THERE                               
         L     R6,12(R1)                                                        
         MVI   BYTE,1               1 DECIMAL INDICATOR                         
         CLC   5(2,R6),=XL2'5901'   2 DECIMAL PRECISION                         
         BL    *+8                                                              
         MVI   BYTE,2               2 DECIMAL INDICATOR                         
         LA    R4,BYTE                                                          
         LHI   R1,X'10'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*  PASS CPROG INFO                                                              
*                                                                               
SNDVP190 GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'DE',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   SNDVP200                                                         
         L     R2,12(R1)                                                        
         USING NPGCELDD,R2                                                      
*                                                                               
SNDVP195 LHI   R1,17                                                            
         LA    R4,NPGCCAT           CPROG DEMO CATEGORY                         
         BAS   RE,SENDD                                                         
         LHI   R1,18                                                            
         LA    R4,NPGCAMT           CPROG DEMO AMOUNT                           
         BAS   RE,SENDD                                                         
*                                                                               
*  GET NEXT DD ELEMENT                                                          
*                                                                               
         ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         CLI   0(R2),NPGCELQ                                                    
         BE    SNDVP195                                                         
*                                                                               
*  IF PROGRAM DOES NOT CHANGE JUST PASS PERIOD AND HUT VALUE IN 86 ELEM         
*                                                                               
SNDVP200 TM    SVINDS1,X'20'        DO YOU WANT HUTS                            
         BZ    SNDVPEX                                                          
         LHI   R1,X'87'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
         TM    SVINDS1,X'40'        HUTS ONLY BEING PASSED                      
         BO    SNDVP220             NO                                          
         CLI   PROGBRK,C'Y'         CHECK FOR NEW PROGRAM                       
         BNE   SNDVP220             DONT PASS PROGRAM CODE                      
*                                                                               
*        LHI   R1,X'01'                                                         
*        LA    R4,NPGKPROG          PROGRAM CODE (HUT ONLY REQUESTS)            
*        BAS   RE,SENDD                                                         
*                                                                               
SNDVP220 GOTO1 VDATCON,DMCB,(3,PERIOD3B),(2,DUB)                                
         LHI   R1,X'02'                                                         
         LA    R4,DUB               PERIOD DATE                                 
         BAS   RE,SENDD                                                         
*                                                                               
         L     R2,AIO2                                                          
         USING DEMREC,R2                                                        
         LHI   R1,X'03'                                                         
         LA    R4,HUT               HUT                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNDVPEX  MVI   PROGBRK,C'N'                                                     
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*  SEND UNIVERSE INFORMATION TO THE PC                                          
SENDUNV  NTR1                                                                   
*                                                                               
         L     R2,AIO2                                                          
         USING DEMREC,R2                                                        
*                                                                               
         TM    SVINDS1,X'10'        CHECK IF 86 ELEMENT NEEDED                  
         BO    SNDUN50                                                          
*                                                                               
         LHI   R1,X'86'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
*  PASS THE DEMO CATEGORY INFORMATION                                           
*                                                                               
         LA    R4,QDEMOS            REQUESTED DEMOS                             
SNDUN30  CLI   0(R4),X'FF'          CHECK END OF DEMOS                          
         BE    SNDUN50                                                          
         CLI   0(R4),0              CHECK FOR NAD                               
         BE    SNDUN40                                                          
         LHI   R1,X'0C'                                                         
         BAS   RE,SENDD             NAD CATEGORY                                
SNDUN40  LA    R4,2(R4)                                                         
         LHI   R1,X'0D'                                                         
         BAS   RE,SENDD             DEMO CATEGORY                               
         LA    R4,1(R4)                                                         
         B     SNDUN30                                                          
*                                                                               
*                                                                               
SNDUN50  LHI   R1,X'83'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R6,44                                                            
         LA    R4,UNIVS                                                         
SNDUN60  LHI   R1,X'01'                                                         
         BAS   RE,SENDD             UNIVERSE VALUES                             
         LA    R4,4(R4)                                                         
         BCT   R6,SNDUN60                                                       
*                                                                               
         LA    R6,QDEMOS            REQUESTED DEMOS                             
         LA    R4,WORK2                                                         
SNDUN80  CLI   0(R6),X'FF'          CHECK END OF DEMOS                          
         BE    SNDUN100                                                         
*                                                                               
*  GET REQUESTED UNIVERSES TO THE RIGHT PRECISION                               
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         L     R1,0(R4)                                                         
         M     R0,=F'100'                                                       
         ST    R1,0(R4)                                                         
*                                                                               
         LHI   R1,X'05'                                                         
         BAS   RE,SENDD                                                         
         LA    R6,3(R6)                                                         
         LA    R4,4(R4)                                                         
         B     SNDUN80                                                          
*                                                                               
SNDUN100 LA    R6,OVERAREA          NAD DEMOS                                   
OEL      USING OVEREL,R6                                                        
SNDUN120 CLI   0(R6),X'DD'          CHECK END OF DEMOS                          
         BNE   SNDUNEX                                                          
         LHI   R1,X'02'                                                         
         LA    R4,OEL.OVERDEMO      NAD NUMBER                                  
         BAS   RE,SENDD                                                         
         LHI   R1,X'03'                                                         
         LA    R4,OEL.OVERDEMO+2    DEMO CATEGORY                               
         BAS   RE,SENDD                                                         
         LHI   R1,X'04'                                                         
         LA    R4,OEL.OVERAMNT      DEMO AMOUNT                                 
         BAS   RE,SENDD                                                         
         LA    R6,12(R6)                                                        
         B     SNDUN120                                                         
*                                                                               
SNDUNEX  XIT1                                                                   
         DROP  R2,OEL                                                           
         EJECT                                                                  
* BUILD AND ADD A PACKAGE RECORD                                                
*                                                                               
ADDPKG   NTR1                                                                   
         BAS   RE,READPROF          GET PROFILES                                
         BAS   RE,CHKCLI            CHECK FROZEN CLIENT                         
         BAS   RE,GETPACK           GET HIGHEST PACKAGE NUMBER                  
         BAS   RE,BLDPACK                                                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* READ CLIENT CHECK TO SEE IF IT IS FROZEN                                      
*                                                                               
CHKCLI   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CLTHDR,R3                                                        
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 CLIENT MUST EXIST                           
         GOTO1 AIOCALL,DMCB,SPT+FILE+GET,AIO1                                   
         L     R3,AIO1                                                          
*                                                                               
*  TEST IF CLIENT FROZEN                                                        
         TM    COPT2,COP2FRZ                                                    
         BZ    CHKCLIEX                                                         
         MVC   ERROR,=AL2(160)      CLIENT FROZEN ERROR                         
         XC    ERRORMSG,ERRORMSG                                                
         GOTO1 SENDMSG                                                          
CHKCLIEX XIT                                                                    
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO GET NEXT PACKAGE NUMBER (NUMBER RETURNED IN PACK)              
*                                                                               
GETPACK  NTR1                                                                   
         LA    R4,KEY                                                           
         USING NPRECD,R4                                                        
         XC    KEY,KEY                                                          
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,BAGYMD                                                     
         MVC   NPKCLT,BCLT                                                      
         MVC   NPKNET,SVNET                                                     
         MVC   NPKEST,BEST                                                      
         LA    R0,PASSDEL+UNT+DIR+HIGH                                          
GETPACK2 GOTO1 AIOCALL,DMCB,(R0)                                                
         CLC   KEY(NPKPACK-NPKEY),KEYSAVE                                       
         BNE   GETPACK4                                                         
         LA    R0,PASSDEL+UNT+DIR+SEQ                                           
         B     GETPACK2                                                         
         SPACE                                                                  
GETPACK4 LA    R4,KEYSAVE                                                       
         ZIC   R1,NPKPACK          GET HIGHEST NUMBER SO FAR                    
         LA    R1,1(R1)                                                         
         STC   R1,PACK                                                          
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO CREATE PACKAGE FROM PUP PLANNING RECORD                            
*                                                                               
BLDPACK  NTR1                                                                   
         L     R4,AIO1                                                          
         USING NPRECD,R4                                                        
         LR    RE,R4                                                            
         LA    RF,2000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR I/O AREA                               
*                                                                               
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,BAGYMD                                                     
         MVC   NPKCLT,BCLT                                                      
         MVC   NPKNET,SVNET                                                     
         MVC   NPKEST,BEST                                                      
         MVC   NPKPACK,PACK        SET PACKAGE NUMBER IN KEY                    
*                                                                               
         MVC   NPAKEL(2),=XL2'013C'                                             
         MVI   NPKRLEN,60+27+1                                                  
*                                                                               
*--MOVE OUT PACKAGE RECORD INFO                                                 
         L     R6,ANETBLK                                                       
         USING BUYUPLDD,R6                                                      
*                                                                               
         MVC   NPAKNAME,RUPCOMT1                                                
         MVC   NPAKDP,SV2DAYPT                                                  
         MVC   NPAKHUTA,PLANHTAV                                                
         MVC   NPAKUNCD,SVUNIV                                                  
         MVC   NPAKHUTS,PLANHTSC                                                
         MVI   NPAKHTYP,C'A'                                                    
         MVC   NPAKHPCT,PLANHTPO                                                
         MVC   NPAKHUTF,PLANHTFL                                                
         MVC   NPAKMAST,BPRD                                                    
*******  MVC   NPAKZONE,NPLNZONE                                                
*******  MVC   NPAKGCPM,NPLNGCPM                                                
         MVC   NPAKCOST,RUPNACT                                                 
         MVC   NPAKINT,RUPNINT                                                  
         OC    NPAKINT,NPAKINT                                                  
         BNZ   PD200                                                            
         CLI   N2PROF+15,C'Y'       CHECK FOR TBL INTEGRATION                   
         BNE   PD200                                                            
         OI    NPAKCNTL,X'80'       SET INTEGRATION AS TBL                      
*                                                                               
PD200    GOTO1 AIOCALL,DMCB,UNT+FIL+ADDREC,AIO1                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
                                                                                
         EJECT                                                                  
SPTFILE  DC    CL8'SPTFILE'                                                     
UNTFILE  DC    CL8'UNTFILE'                                                     
CTFILE   DC    CL8'CTFILE'                                                      
*                                                                               
DAYTBL   DC    X'01',CL4'MON '                                                  
         DC    X'02',CL4'TUE '                                                  
         DC    X'03',CL4'WED '                                                  
         DC    X'04',CL4'THU '                                                  
         DC    X'05',CL4'FRI '                                                  
         DC    X'06',CL4'SAT '                                                  
         DC    X'07',CL4'SUN '                                                  
         DC    X'00',CL4'M-F '                                                  
         DC    X'08',CL4'M-SU'                                                  
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
       ++INCLUDE DENADCATS                                                      
         EJECT                                                                  
       ++INCLUDE DETVQCDAT                                                      
         EJECT                                                                  
       ++INCLUDE NENAVWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
NDAYS    DS    F                                                                
BUYDSTR  DS    A                                                                
BUYDEND  DS    A                                                                
SPOTNUM  DS    H                                                                
INVSDATE DS    CL6                                                              
INVHDPRD DS    X                   INVOICE HEADER PRD                           
INVHDPR2 DS    X                                                                
INVHDEST DS    X                                                                
RELAFDAY DS    X                                                                
RELAFTIM DS    XL2                                                              
CALCDATE DS    CL6                                                              
VPHSTDAT DS    CL3                                                              
VPHENDAT DS    CL3                                                              
*****LASTKEY  DS    CL13                LAST PROGRAM RECORD SENT                
CURKEY   DS    CL13                CURRENT PROGRAM RECORD                       
ALDATE   DS    XL2                 ALLOC DATE                                   
LADATE   DS    XL2                 LAST ALLOC DATE                              
V10301   DS    CL4                                                              
EDSAVE   DS    XL17                                                             
ALLCLSW  DS    CL1                  ALL CLIENT SWITCH                           
PROGBRK  DS    CL1                  PROGRAM BREAK SWITCH                        
POSTTYP  DS    CL1                  POSTING TYPE                                
PRROT    DS    CL1                  CURRENT PROGRAM ROTATOR                     
PACK     DS    CL1                  NEW PACKAGE NUMBER                          
NTISTA   DS    CL4                  NTI STATION                                 
PERIOD3B DS    CL3                  3 BYTE CURRENT LOOKUP DATE                  
PERIOD2B DS    CL2                  2 BYTE CURRENT LOOKUP DATE                  
PROGNTI  DS    CL2                  NTI NUMBER                                  
*                                                                               
N1PROF   DS    CL16                 N1 PROFILE                                  
N2PROF   DS    CL16                 N2 PROFILE                                  
N3PROF   DS    CL16                 N3 PROFILE                                  
APPROF   DS    CL16                 APPLICATION PROFILE                         
*                                                                               
***BUYDATA1 DS    CL78                                                          
***BUYDATA2 DS    CL78                                                          
***BUYDATA3 DS    CL78                                                          
***BUYDATA4 DS    CL78                                                          
***BUYDATAL EQU   *-BUYDATA1                                                    
***BUYDATAX EQU   *                                                             
         ORG                                                                    
*                                                                               
* DEMO RECORD BUILD LAYOUT                                                      
*                                                                               
DEMREC   DSECT                                                                  
PIO      DS    0C                  AREA TO BUILD EVN RECORDS                    
EVNKEY   DS    CL22                (PHONY KEY)                                  
         SPACE 1                                                                
PUEL     DS    0CL243              UNIVERSE ELEMENT                             
         DS    XL3                 X'31F344'                                    
UNIVS    DS    CL240               UNIVERSES                                    
         SPACE 1                                                                
PVEL     DS    0CL243              VPH ELEMENT                                  
         DS    XL3                 X'33F302'                                    
VPHS     DS    CL240               VPHS                                         
         SPACE 1                                                                
PREL     DS    0CL9                RATING/HUT/SHARE ELEMENT                     
         DS    XL3                 X'350902'                                    
RATING   DS    XL2                 RATING                                       
HUT      DS    XL2                 HUT                                          
SHARE    DS    XL2                 SHARE                                        
         SPACE 1                                                                
PBEL     DS    CL7                 BOOK ELEMENT                                 
         SPACE 1                                                                
OVERAREA DS    0CL1                40 OVERRIDE ELEMENTS FOR NAD DEMOS           
*                                  PLUS 10 OVERRIDES PLUS EOR                   
OVEREL   DS    0CL12               OVERRIDE ELEMENT                             
         DS    CL2                 X'DD0C'                                      
         DS    CL1                 X'00'                                        
OVERDEMO DS    CL3                 CATEGORY/MODIFIER/DEMO NUMBER                
OVERFLG  DS    CL1                 NAD DEMOS X'80'                              
OVERPREC DS    CL1                 PRECISION                                    
OVERAMNT DS    CL4                                                              
OVERSPRE DS    CL3                 SPARE TO MATCH NUCDLENQ                      
OVERELNQ EQU   *-OVEREL                                                         
         DS    CL592                                                            
OVARLENE EQU   *-OVEREL                                                         
*                                                                               
NOVERS   DS    CL140              NEW OVERRIDE ELEMS 20*7(NUOVEL)               
         DS    CL2                                                              
         SPACE 2                                                                
*                                                                               
*   TVQ EXTENSION BLOCK                                                         
*                                                                               
TVQBLOCK DSECT                                                                  
GOTUTYPE DS    CL1                                                              
*                                                                               
         DS    0F                                                               
BTUEXT   DS    CL4                                                              
BTUEX1   DS    A(0)                                                             
BTUEX2   DS    A(0)                                                             
*                                                                               
BTUAQ    DS    F                                                                
BTUDMCB  DS    10F                                                              
BTUDB    DS    XL256                                                            
         DS    CL4                                                              
BTUIO    DS    1000C                                                            
         SPACE 2                                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENDPT                                                       
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENREAS                                                      
       ++INCLUDE SPGENUNIV                                                      
       ++INCLUDE NEGETNUND                                                      
       ++INCLUDE NEGETHUTD                                                      
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE NAVDSECTS                                                      
*                                                                               
NETVQD   DSECT                                                                  
NETVQSWK DS    XL2                 START WEEK                                   
NETVQEWK DS    XL2                 END WEEK                                     
NETVQMTH DS    XL2                 SCHEDULE MONTH                               
NETVQQ   EQU   *-NETVQD                                                         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023NENAV50   06/02/20'                                      
         END                                                                    
