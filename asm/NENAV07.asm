*          DATA SET NENAV07    AT LEVEL 094 AS OF 10/14/20                      
*PHASE T31807B                                                                  
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31807   TITLE 'F NAV07 - STEWARD/MATCHMAKER - BUY UPLOAD OVERLAY'              
T31807   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV07**,RA                                                    
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     R7,ATWA                                                          
         USING TWAD,R7                                                          
*                                                                               
         USING TSARD,TSARBLK                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVC   TSAREC,ANETBLK                                                   
*                                                                               
*  RESTORE THE TSAR BLOCK                                                       
*                                                                               
         CLI   SVRSTSAV,C'S'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSARES        RESTORE TSAR                                
         BAS   RE,CALLTSAR                                                      
         MVI   SVRSTSAV,C'R'                                                    
*                                                                               
         CLI   SVGLOBSW,C'B'                                                    
         BE    ROUT040                                                          
*                                                                               
*  CHECK FOR PRODUCT ONLY CHANGE                                                
*                                                                               
         LHI   R0,RUPPALEN                                                      
         CH    R0,TSRECL                                                        
         BE    CHAPROD                                                          
*                                                                               
* DID WE GO TO THE BUY ALREADY                                                  
*                                                                               
*SVGLOBSW MEANINGS  NULL=FIRST PASS GET FIRST TSAR RECORD CALL NBUY             
*                   C'B'=SEND DATA TO THE PC                                    
*                   C'E'=NO MARE TSAR RECORDS EXIT                              
*                                                                               
*                                                                               
*  SEND DATA TO THE BUY (FIRST TIME THROUGH)                                    
*                                                                               
         GOTO1 VALIMED                                                          
         MVI   TSACTN,TSAGET        GET FIRST RECORD FROM TSAR                  
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         BAS   RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                 MUST BE ONE RECORD                          
*                                                                               
         BAS   RE,CALLBUY                                                       
         B     EXIT                                                             
*                                                                               
*  PROCESS RETURNED BUY INFORMATION                                             
*  SEND NEXT TSAR RECORD TO THE BUY                                             
*                                                                               
ROUT040  BAS   RE,SENDATA           SEND OUT DATA TO FALINK                     
*                                                                               
         L     RF,ANETBLK                                                       
         MVC   0(1,RF),SVTSRKEY                                                 
         MVI   TSACTN,TSARDH        READ HIGH                                   
         BAS   RE,CALLTSAR          REPOSITION POINTER                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TSACTN,TSANXT        READ NEXT TSAR RECORD                       
         BAS   RE,CALLTSAR                                                      
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,CALLBUY                                                       
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
         EJECT                                                                  
*=================================================================*             
* PUT DATA IN AIO TO GLOBBER AND PASS CONTROL TO SPOT BUY         *             
*=================================================================*             
         SPACE 1                                                                
CALLBUY  NTR1                                                                   
         MVI   SVXFROV,X'07'       RETURN CONTROL TO THIS OVLY                  
         MVI   SVGLOBSW,C'B'       SET SWITCH TO UNITS BOUGHT                   
*                                                                               
         CLI   SVRSTSAV,C'R'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSASAV        SAVE TSAR                                   
         BAS   RE,CALLTSAR                                                      
         MVI   SVRSTSAV,C'S'                                                    
*                                                                               
*  TEST THE GLOBBER CALLS                                                       
*                                                                               
CB20     L     R3,ANETBLK                                                       
         USING BUYUPLDD,R3                                                      
         MVC   SVTSRKEY,0(R3)                                                   
         MVC   RUPDEMS,QDEMOS                                                   
         MVC   RUPPRSID,SVPRSMID   PRISMA ID                                    
*                                                                               
         L     RF,ATWA             COMSCORE DEMO NAME LIST                      
         AHI   RF,QNTDMS-TWAD                                                   
         MVC   RUPCDEMS,0(RF)                                                   
*                                                                               
***      XC    DMCB(24),DMCB                                                    
***      L     RE,ATWA                                                          
***      MVC   DMCB+10(2),2(RE)    TERMINAL NUMBER                              
***      MVI   DMCB+8,1            PAGE NUMBER                                  
***      MVI   DMCB+9,0                                                         
***      MVC   DMCB+20(2),=C'L='                                                
***      MVC   DMCB+22(2),=X'03E8'  WRITE 1000 BYTES                            
***      MVC   WORK(4),DMCB+8       SAVE FOR GLOBBER CALL                       
***      GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R3),0                      
***      CLI   8(R1),0             BLOW ON ANY ERROR HERE                       
***      BE    *+6                                                              
***      DC    H'0'                                                             
* DO WSSVR CALL YTO PASS INFO TO THE BUY SYSTEM                                 
         LA    RE,WSVRBLK                                                       
         USING FAWSSVRD,RE                                                      
         MVC   FAWSTOKN,=CL4'NNAV'                                              
         MVI   FAWSACTN,FAWSUSVE                                                
         MVC   FAWSADR,ANETBLK                                                  
         LHI   R0,RUPLEN3                                                       
         STCM  R0,3,FAWSLEN                                                     
******   MVC   FAWSLEN,=H'2000'                                                 
         GOTOR VWSSVRS,FAWSSVRD                                                 
         LA    RE,WSVRBLK                                                       
         USING FAWSSVRD,RE                                                      
         CLI   FAWSRTN,FAWSROK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
* PASS PAGE AND MONITOR NUMBER THROUGH GLOBBER                                  
         MVC   WORK,=CL4'NNAV'                                                  
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,4,GLVBUY1                            
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING GLVXFRSY,R6                                                      
         MVC   GLVXFRSY,=C'NET'                                                 
         MVC   GLVXFRPR,=C'NNA'                                                 
         MVC   GLVXTOSY,=C'NET'                                                 
         MVC   GLVXTOPR,=C'NBU'                                                 
* SET DIALOGUE PARAMETERS                                                       
         OI    GLVXFLG1,GLV1SEPS+GLV1SEPD                                       
         OC    SVSESSNS,SVSESSNS    FIRST TIME                                  
         BZ    CALLB050                                                         
         MVC   GLVXSESR(2),SVSESSNS                                             
         OI    GLVXFLG1,GLV1SIDR+GLV1SIDE                                       
         B     CALLB100                                                         
* GET CURRENT SESSION NUMBER                                                    
CALLB050 OI    GLVXFLG1,GLV1SIDR                                                
         L     R3,ACOMFACS                                                      
         USING COMFACSD,R3                                                      
         ICM   RF,15,CSWITCH                                                    
         MVC   DMCB(4),=X'FFFFFFFF'                                             
         GOTO1 (RF),DMCB                                                        
         L     R1,DMCB                                                          
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING UTLD,R1                                                          
         MVC   SVSESSNS(1),TSESSION                                             
         MVC   GLVXSESR(2),SVSESSNS                                             
         DROP  R1,R3                                                            
*                                                                               
*******  OI    GLVXFLG1,GLV1SEPS+GLV1SIDR                                       
CALLB100 GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,24,GLVXCTL                           
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
CBX      B     EXIT                                                             
*                                                                               
GLBERR   DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* SEND DRAFT BUY INFO TO THE PC                                   *             
*=================================================================*             
         SPACE 1                                                                
SENDATA  NTR1                                                                   
*                                                                               
         XC    DEMOVER(DEMOVERL),DEMOVER                                        
*                                                                               
         L     R3,AIO3                                                          
         USING DRAFRECD,R3                                                      
*                                                                               
         LHI   R1,X'31'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
*  CHECK IF ERROR RETURNED                                                      
*                                                                               
         CLI   BUYERRSW,C'Y'                                                    
         BNE   SND040                                                           
         LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
*                                                                               
         LA    R4,ERMSSEQN                                                      
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,ERNUMBR                                                       
         LHI   R1,X'39'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         GOTO1 VGETMSG,DMCB+12,(ERNUMBR,WORK),(X'FF',DMCB),(7,0)                
         LA    R4,WORK+8                                                        
         LHI   R1,X'3A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,ERMXTRA                                                       
         LHI   R1,X'3B'                                                         
         BAS   RE,SENDD                                                         
         MVI   BUYERRSW,C'N'                                                    
         B     SNDEX                                                            
*                                                                               
SND040   LA    R4,DRFSEQN           SEQUENCE NUMBER                             
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         CLC   DRFNUM(3),=CL3'DEL'  CHECK FOR DELETE                            
         BE    SNDEX                                                            
*                                                                               
         TM    DRFSTAT,X'80'        TEST FOR REFRESH ACTION                     
         BO    SND120                                                           
*                                                                               
         LA    R4,DRFDATE           BUY DATE                                    
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
****     LA    R4,DRFDAY            DAYS                                        
****     LHI   R1,X'03'                                                         
****     BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFTIME           START TIME                                  
         LHI   R1,X'04'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFTIME+2         END TIME                                    
         LHI   R1,X'05'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFLEN            UNIT LENGTH                                 
         LHI   R1,X'06'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFINT            INTEGRATION                                 
         LHI   R1,X'07'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFPNAM           PROGRAM NAME                                
         LHI   R1,X'08'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),DRFNTI                                                 
         LA    R4,FULL              NTI CODE                                    
         LHI   R1,X'09'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFBTYP           BUY TYPE                                    
         LHI   R1,X'0A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFMEDTP          MEDIA/POSTING TYPES                         
         LHI   R1,X'13'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         CLC   VERSION,=X'05030006'   VERSION  5.03.00.06 AND HIGHER            
         BL    SNDB95                                                           
         LA    R4,DRFDEMBS          DEMO BASE                                   
         LHI   R1,X'1E'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNDB95   CLC   VERSION,=X'0100005E'   VERSION  1.00.00.94 AND HIGHER            
         BL    SNDB100                                                          
         LA    R4,DRFROT            ROTATION                                    
         LHI   R1,X'14'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNDB100  LA    R4,DRFSLN            SUB LINE NUMBER                             
         LHI   R1,X'0E'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SND120   MVC   DEM1HOVR,=C'NNNN'    DEFAULT OVERRIDE TO NO                      
*                                                                               
         LA    R4,DRFHOMSH          HOMES SHARE                                 
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+8                                                              
         MVI   DEM1HOVR,C'Y'                                                    
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'0F'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFHOMHT          HOMES HUT                                   
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+8                                                              
         MVI   DEM1HOVR+1,C'Y'                                                  
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'10'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFHOMRT          HOMES RATING                                
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+8                                                              
         MVI   DEM1HOVR+2,C'Y'                                                  
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'11'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFHOMIM          HOMES IMPRESSIONS                           
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+8                                                              
         MVI   DEM1HOVR+3,C'Y'                                                  
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'12'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R6,QDEMOS                                                        
         LA    R4,DRFDEMS                                                       
         LA    R2,25                                                            
         LA    RF,DEM1OVER                                                      
         ST    RF,FULL                                                          
*                                                                               
SND150   CLI   1(R6),0                                                          
         BE    SND180                                                           
*                                                                               
         MVC   0(3,RF),=C'NNN'      DEFAULT OVERRIDE TO NO                      
*                                                                               
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+12                                                             
         L     RF,FULL                                                          
         MVI   0(RF),C'Y'                                                       
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'0B'             VPH                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,2(R4)                                                         
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+12                                                             
         L     RF,FULL                                                          
         MVI   1(RF),C'Y'                                                       
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'0C'             GRP                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,2(R4)                                                         
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+12                                                             
         L     RF,FULL                                                          
         MVI   2(RF),C'Y'                                                       
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'0D'             IMP                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R6,3(R6)                                                         
         L     RF,FULL              NEXT OVERRIDE INDICATOR GROUP               
         AHI   RF,3                                                             
         ST    RF,FULL                                                          
*                                                                               
         BCT   R2,SND150                                                        
*                                                                               
SND180   CLI   DRFDEMO2,C'Y'        CHECK SECOND DEMO COLUMN                    
         BNE   SND250                                                           
*                                                                               
         MVC   DEM2HOVR,=C'NNNN'                                                
*                                                                               
         LA    R4,DRFHOMS2          HOMES SHARE COLUMN 2                        
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+8                                                              
         MVI   DEM2HOVR,C'Y'                                                    
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'1A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFHOMH2          HOMES HUT COLUMN 2                          
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+8                                                              
         MVI   DEM2HOVR+1,C'Y'                                                  
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'1B'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFHOMR2          HOMES RATING COLUMN 2                       
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+8                                                              
         MVI   DEM2HOVR+2,C'Y'                                                  
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'1C'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFHOMI2          HOMES IMPRESSIONS COLUMN 2                  
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+8                                                              
         MVI   DEM2HOVR+3,C'Y'                                                  
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'1D'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R6,QDEMOS                                                        
         LA    R4,DRFDEMS2                                                      
         LA    R2,25                                                            
         LA    RF,DEM2OVER          OVERRIDE INDICATOR                          
         ST    RF,FULL                                                          
*                                                                               
SND200   CLI   1(R6),0                                                          
         BE    SND250                                                           
*                                                                               
         MVC   0(3,RF),=C'NNN'      DEFAULT OVERRIDE TO NO                      
*                                                                               
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+12                                                             
         L     RF,FULL                                                          
         MVI   0(RF),C'Y'                                                       
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'17'             VPH COLUMN 2                                
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,2(R4)                                                         
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+12                                                             
         L     RF,FULL                                                          
         MVI   1(RF),C'Y'                                                       
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'18'             GRP COLUMN 2                                
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,2(R4)                                                         
         TM    0(R4),X'80'          OVERRIDE?                                   
         JZ    *+12                                                             
         L     RF,FULL                                                          
         MVI   2(RF),C'Y'                                                       
         NI    0(R4),X'FF'-X'80'                                                
         LHI   R1,X'19'             IMP COLUMN 2                                
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R6,3(R6)                                                         
         L     RF,FULL             NEXT OVERRIDE INDICATOR GROUP                
         AHI   RF,3                                                             
         ST    RF,FULL                                                          
*                                                                               
         BCT   R2,SND200                                                        
*                                                                               
SND250   OC    DRFRESLT,DRFRESLT                                                
         BZ    SND300                                                           
         LA    R4,DRFRESLT          RESULT CODE                                 
         LHI   R1,X'16'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SND300   J     SNDEX               *** BACKED OUT OVERRIDE INDICATORS           
*                                                                               
         LA    R6,QDEMOS           SEND OVERRIDE INDICATORS                     
         LA    R2,25                                                            
         LA    R4,DEM1OVER                                                      
*                                                                               
SND302   CLI   1(R6),0                                                          
         BE    SND304                                                           
*                                                                               
         LHI   R1,31                                                            
         BAS   RE,SENDD                                                         
         AHI   R4,1                                                             
         LHI   R1,32                                                            
         BAS   RE,SENDD                                                         
         AHI   R4,1                                                             
         LHI   R1,33                                                            
         BAS   RE,SENDD                                                         
*                                                                               
         AHI   R4,1                                                             
         AHI   R6,3                                                             
         BCT   R2,SND302                                                        
*                                                                               
SND304   LA    R4,DEM1HOVR         HOME OVERRIDE INDICATORS                     
         LHI   R1,35                                                            
         BAS   RE,SENDD                                                         
         LA    R4,DEM1HOVR+1                                                    
         LHI   R1,36                                                            
         BAS   RE,SENDD                                                         
         LA    R4,DEM1HOVR+2                                                    
         LHI   R1,37                                                            
         BAS   RE,SENDD                                                         
         LA    R4,DEM1HOVR+3                                                    
         LHI   R1,38                                                            
         BAS   RE,SENDD                                                         
*                                                                               
         CLI   DRFDEMO2,C'Y'       CHECK SECOND DEMO COLUMN                     
         BNE   SNDEX                                                            
*                                                                               
         LA    R6,QDEMOS                                                        
         LA    R2,25                                                            
         LA    R4,DEM2OVER                                                      
*                                                                               
SND306   CLI   1(R6),0             NON-HOME DEMOS                               
         BE    SND308                                                           
*                                                                               
         LHI   R1,43                                                            
         BAS   RE,SENDD                                                         
         AHI   R4,1                                                             
         LHI   R1,44                                                            
         BAS   RE,SENDD                                                         
         AHI   R4,1                                                             
         LHI   R1,45                                                            
         BAS   RE,SENDD                                                         
*                                                                               
         AHI   R4,1                                                             
         AHI   R6,3                                                             
         BCT   R2,SND306                                                        
*                                                                               
SND308   LA    R4,DEM2HOVR         HOME OVERRIDE INDICATORS                     
         LHI   R1,46                                                            
         BAS   RE,SENDD                                                         
         LA    R4,DEM2HOVR+1                                                    
         LHI   R1,47                                                            
         BAS   RE,SENDD                                                         
         LA    R4,DEM2HOVR+2                                                    
         LHI   R1,48                                                            
         BAS   RE,SENDD                                                         
         LA    R4,DEM2HOVR+3                                                    
         LHI   R1,49                                                            
         BAS   RE,SENDD                                                         
*                                                                               
SNDEX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*  THE FOLLOWING AREA OF CODE DEALS WITH A SPECIAL                              
*  STEWARD OPTION THAT ALLOWS THE USER TO DO HIGH                               
*  SPEED CHANGES OF THE PRODUCT CODE. TO ACCOMPLISH                             
*  THIS THE PRODUCT VALIDATION ROUTINE WAS COPIED                               
*  INTO THIS PROGRAM.                                                           
*                                                                               
*                                                                               
                                                                                
CHAPROD  GOTO1 VALIMED                                                          
*                                                                               
*  GET SECURITY AGENCY AND PASSWORD                                             
*                                                                               
         L     R4,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(,R4)                                        
         GOTO1 (RF),DMCB,0                                                      
*                                                                               
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
         MVC   SECAGY,FATAGYSC     SAVE SECURITY AGENCY                         
         TM    FATFLAG,X'08'                                                    
         BZ    *+10                                                             
         MVC   SVPASSWD,FAPASSWD   SAVE PERSONAL ID                             
         DROP  RE                                                               
*                                                                               
         MVI   TSACTN,TSAGET        GET FIRST RECORD FROM TSAR                  
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         BAS   RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                 MUST BE ONE RECORD                          
         B     CHAPR050                                                         
*                                                                               
*  PROCESS RETURNED BUY INFORMATION                                             
*  SEND NEXT TSAR RECORD TO THE BUY                                             
*                                                                               
CHAPR020 MVI   TSACTN,TSANXT        READ NEXT TSAR RECORD                       
         BAS   RE,CALLTSAR                                                      
         BNE   EXIT                                                             
         MVI   PRODCHSW,0                                                       
*                                                                               
*  GET BUY VALIDATE PRODUCT WRITE BUY BACK                                      
*                                                                               
CHAPR050 L     R4,AIO1              POINTS TO BUY RECORD                        
         ZIC   RE,SEQNUM                                                        
         LA    RE,1(RE)                                                         
         STCM  RE,1,SEQNUM                                                      
         BAS   RE,CHKLOCK                                                       
         OC    ERROR,ERROR                                                      
         BNZ   CHAPR100                                                         
         BAS   RE,GETPAK                                                        
         OC    ERROR,ERROR                                                      
         BNZ   CHAPR100                                                         
         BAS   RE,GETBUY                                                        
         OC    ERROR,ERROR                                                      
         BNZ   CHAPR100                                                         
         BAS   RE,READPROF          READ PROFILE                                
         BAS   RE,CHKPRINP                                                      
         BE    CHAPR060                                                         
         BAS   RE,CHKOLD                                                        
         OC    ERROR,ERROR                                                      
         BNZ   CHAPR100                                                         
         BAS   RE,VALPROD           VALIDATE PRODUCT                            
         OC    ERROR,ERROR                                                      
         BNZ   CHAPR100                                                         
         MVI   PRODCHSW,C'P'                                                    
CHAPR060 BAS   RE,DISASS            SET UP ASSIGNED COST FOR VAIDATION          
         OC    ERROR,ERROR                                                      
         BNZ   CHAPR100                                                         
         OC    FLDH,FLDH            CHECK IF THERE IS ANY INPUT                 
         BZ    CHAPR075                                                         
         BAS   RE,VALASS            VALIDATE ASSIGNED COST                      
         OC    ERROR,ERROR                                                      
         BNZ   CHAPR100                                                         
         MVI   PRODCHSW,C'A'                                                    
CHAPR075 BAS   RE,VALLOCK           VALIDATE LOCK ASSIGNED COST                 
         OC    ERROR,ERROR                                                      
         BNZ   CHAPR100                                                         
         MVI   PRODCHSW,C'L'                                                    
         BAS   RE,WRITBUY                                                       
CHAPR100 BAS   RE,SNDPROD                                                       
         B     CHAPR020                                                         
         EJECT                                                                  
*==================================================================*            
* CHECK LOCK RECORD STATUS *                                                    
*==================================================================*            
CHKLOCK  NTR1                                                                   
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
         MVI   BILLOCK,C'N'                                                     
*                                                                               
         L     R4,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(,R4)                                        
         GOTO1 (RF),DMCB,0                                                      
*                                                                               
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
         L     R4,ACOMFACS                                                      
         LA    R3,WORK                                                          
         USING LKKEYD,R3                                                        
         XC    WORK,WORK                                                        
*                                                                               
         MVC   LOCKSE,FASYS                                                     
*****    MVC   SECAGYA,FATAGYSC     SAVE SECURITY AGENCY                        
         TM    FATFLAG,X'08'                                                    
         BZ    *+10                                                             
         MVC   SVPASSWD,FAPASSWD   SAVE PERSONAL ID                             
         DROP  RE                                                               
         SPACE                                                                  
         MVC   LOCKAGY,QAGY                                                     
         MVC   LOCKRTY,=CL2'UN'                                                 
         MVC   LOCKKEY(3),RUPCLI                                                
         OI    LOCKKEY+2,X'40'                                                  
         XC    LOCKKEY+3(7),LOCKKEY+3                                           
         SPACE                                                                  
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QLOCKET                                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R2,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         SPACE                                                                  
         PRINT GEN                                                              
         GOTO1 (R2),(R1),(C'W',WORK),(R4)                                       
         CLI   DMCB+4,0            ANY ERRORS                                   
         BNE   CKLOCERR                                                         
*                                                                               
*  CHECK STATION LEVEL LOCKS                                                    
*                                                                               
         MVC   LOCKKEY+3(4),NET                                                 
         GOTO1 (R2),(R1),(C'W',WORK),(R4)                                       
         PRINT NOGEN                                                            
         SPACE                                                                  
         CLI   DMCB+4,0            ANY ERRORS                                   
         BNE   CKLOCERR                                                         
*                                                                               
*  CHECK SOON BILLING LOCKS                                                     
*                                                                               
         MVC   LOCKAGY,QAGY                                                     
         MVC   LOCKRTY,=CL2'NB'                                                 
         XC    LOCKKEY(10),LOCKKEY                                              
* CHECK CLIENT LEVEL LOCK                                                       
         MVC   LOCKKEY(3),RUPCLI                                                
         OI    LOCKKEY+2,X'40'                                                  
         GOTO1 (R2),(R1),(C'T',WORK),(R4)                                       
         CLI   DMCB+4,0            ANY ERRORS                                   
         BE    *+12                                                             
         MVI   BILLOCK,C'Y'                                                     
         B     CKLOCEX                                                          
CKLOCEX  B     EXIT                                                             
         DROP  R3                                                               
*--LOCK  ERROR                                                                  
CKLOCERR LA    RE,WORK2                                                         
         USING ERRDATA,RE                                                       
         MVI   BUYERRSW,C'Y'                                                    
         MVI   ERROR,CLLOKERR                                                   
         MVC   ERNUMBR,ERROR                                                    
         XC    ERMXFLD(60),ERMXFLD                                              
         MVC   ERMXFLD(4),=CL4'LOCK'                                            
         B     EXIT                                                             
         SPACE                                                                  
         DROP  RE                                                               
         EJECT                                                                  
*                                                                               
*  READ THE BUY RECORD MOVE INTO AIO1                                           
*                                                                               
GETPAK   NTR1                                                                   
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
         CLC   RUPCLI(13),PAKCLI                                                
         BE    GETPAKEX                                                         
         L     R4,AIO1                                                          
         USING NPRECD,R4                                                        
         XC    ERROR,ERROR                                                      
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
*                                                                               
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,BAGYMD                                                     
         GOTO1 VCLPACK,DMCB,RUPCLI,NPKCLT                                       
         MVC   NPKNET,RUPNET                                                    
         LA    R6,RUPEST                                                        
         MVI   BYTE,3                                                           
         BAS   RE,GETBINRY                                                      
         MVC   NPKEST,BYTE                                                      
         LA    R6,RUPPACK                                                       
         MVI   BYTE,3                                                           
         BAS   RE,GETBINRY                                                      
         MVC   NPKPACK,BYTE                                                     
*                                                                               
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         CLC   KEY(19),KEYSAVE                                                  
         BE    GETPAK50                                                         
         DC    H'0'                                                             
*                                                                               
GETPAK50 GOTO1 AIOCALL,DMCB,UNT+FIL+GET,AIO                                     
         L     R4,AIO                                                           
         MVC   PAKDPT,NPAKDP                                                    
         MVC   PAKCNTL,NPAKCNTL                                                 
         MVC   PAKSTAT,NPAKSTAT                                                 
GETPAKEX B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
*  READ THE BUY RECORD MOVE INTO AIO1                                           
*                                                                               
GETBUY   NTR1                                                                   
         L     R4,AIO1                                                          
         USING NURECD,R4                                                        
         XC    ERROR,ERROR                                                      
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
*                                                                               
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,BAGYMD                                                    
         GOTO1 VCLPACK,DMCB,RUPCLI,NUKPCLT                                      
         MVC   NUKPNET,RUPNET                                                   
         MVC   NUKPPROG,RUPPCODE                                                
         GOTO1 VDATCON,DMCB,(4,RUPBDATE),(2,NUKPDATE)                           
         LA    R6,RUPEST                                                        
         MVI   BYTE,3                                                           
         BAS   RE,GETBINRY                                                      
         MVC   NUKPEST,BYTE                                                     
         MVI   BYTE,1               DEFAULT LINE NUMBER                         
         OC    RUPSLINE,RUPSLINE                                                
         BZ    GETBUY30                                                         
         LA    R6,RUPSLINE                                                      
         MVI   BYTE,3                                                           
         BAS   RE,GETBINRY                                                      
GETBUY30 MVC   NUKPSUB,BYTE                                                     
         MVC   NUKPDP,PAKDPT                                                    
*                                                                               
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         CLC   KEY(19),KEYSAVE                                                  
         BE    GETBUY50                                                         
         MVI   ERROR,NOTFOUND                                                   
         B     PRDI                                                             
*                                                                               
GETBUY50 GOTO1 AIOCALL,DMCB,UNT+FIL+GET,AIO                                     
         L     R4,AIO                                                           
*                                                                               
*  CHECK THE STATUS TO SEE IF CHANGES ARE ALLOWED TO THE UNIT                   
*                                                                               
         CLI   RUPREAS,X'40'        WAS REASON CODE INPUTTED                    
         BH    GETBUY70                                                         
         TM    NUPACKST,X'02'       CHECK IF REASON CODE REQUIRED               
         BZ    GETBUY70                                                         
         MVI   ERROR,AUDITERR                                                   
         B     PRDI                                                             
*                                                                               
GETBUY70 TM    PAKCNTL,X'08'        CHECK IF CABLE LOCKED                       
         BZ    *+12                                                             
         MVI   ERROR,UCBLKERR                                                   
         B     PRDI                                                             
*                                                                               
         TM    PAKSTAT,X'20'        CHECK IF PACKAGE LOCKED                     
         BZ    *+12                                                             
         MVI   ERROR,PAKLERR                                                    
         B     PRDI                                                             
*                                                                               
*        TM    CLIOPTN2,X'08'       CHECK IF CLIENT FROZEN                      
*        BZ    *+12                                                             
*        MVI   ERROR,CLIFRERR                                                   
*        B     PRDI                                                             
*                                                                               
GETBUYEX B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*  ROUTINE CONVERTS A VARIABLE LENGTH NUMERIC                                   
*  FIELD INTO BINARY                                                            
*  R6 - ADDRESS OF INPUT FIELD                                                  
*  BYTE - LENGTH OF THE FIELD                                                   
*                                                                               
*  OUTPUT                                                                       
*  BYTE - BINARY NUMBER                                                         
*                                                                               
GETBINRY NTR1                                                                   
         SR    RF,RF                                                            
         ZIC   RE,BYTE                                                          
         LR    R1,R6                                                            
*                                                                               
GTBI040  CLI   0(R1),X'40'                                                      
         BNH   GTBI080                                                          
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,GTBI040                                                       
*                                                                               
GTBI080  BCTR  RF,0                                                             
         EX    RF,FLDPACK                                                       
         CVB   R0,DUB                                                           
         STCM  R0,1,BYTE                                                        
         B     EXIT                                                             
         SPACE 1                                                                
FLDPACK  PACK  DUB,0(0,R6)                                                      
         EJECT                                                                  
*                                                                               
*  CHECK IF PRODUCT CHANGE REQUESTED                                            
*                                                                               
CHKPRINP NTR1                                                                   
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
*                                                                               
         CLI   RUPOPRDL,0                                                       
         BNE   CHKPRCEX                                                         
         CLI   RUPNPRDL,0                                                       
CHKPRCEX B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
*  CHECK OLD PROD INFO AGAINST THE UNIT MAKE SURE IT HAS NOT CHANGED            
*                                                                               
CHKOLD   NTR1                                                                   
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
*                                                                               
         XC    OLDPRDS,OLDPRDS                                                  
* WAS THERE AN OLD PRODUCT CODE                                                 
         CLI   RUPOPRDL,0                                                       
         BE    CHKO200                                                          
*                                                                               
         XC    PRDSCRN,PRDSCRN                                                  
         MVC   PRDSCRN+5(1),RUPOPRDL                                            
         ZIC   RE,RUPOPRDL                                                      
         LA    RE,8(RE)                                                         
         STCM  RE,1,PRDSCRN                                                     
         MVC   PRDSCRN+8(7),RUPOPRD                                             
         GOTO1 VSCANNER,DMCB,PRDSCRN,(1,WORK),C',=,*'                           
         CLI   4(R1),0                                                          
         BE    PRDINV              INVALID INPUT                                
         LA    R2,WORK             POINT R2 AT BLOCK                            
*  CHECK ALPHA PRODUCT EDIT                                                     
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'19',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   CHKO050                                                          
         L     R3,12(R1)                                                        
         B     CHKO300                                                          
*                                                                               
CHKO050  CLI   0(R2),2                                                          
         BE    *+12                                                             
         CLI   0(R2),3                                                          
         BNE   PRDINV              PRODUCT CODE IS 2 OR 3 ALPHA CHARS           
         ZIC   RF,0(R2)                                                         
         LA    R1,12(R2)                                                        
         BAS   RE,PRDFORM          TEST IF PRODUCT IN RIGHT FORMAT              
         BNZ   PRDINV                                                           
         MVC   THREE,12(R2)                                                     
         BAS   RE,VALPRD           CHECK INPUT                                  
         CLI   ERROR,0                                                          
         BNE   PRDI                                                             
         MVC   OLDPRDS(1),BYTE     PRODUCT NUMBER                               
         SPACE                                                                  
         CLI   1(R2),0             TEST FOR SECOND PRODUCT                      
         BE    CHKO200                                                          
*                                                                               
         CLI   1(R2),2                                                          
         BE    *+12                                                             
         CLI   1(R2),3                                                          
         BNE   PRDINV                                                           
         ZIC   RF,1(R2)                                                         
         LA    R1,22(R2)                                                        
         BAS   RE,PRDFORM          TEST IF PRODUCT IN RIGHT FORMAT              
         BNZ   PRDINV                                                           
         MVC   THREE,22(R2)                                                     
         BAS   RE,VALPRD                                                        
         CLI   ERROR,0                                                          
         BNE   PRDI                                                             
         MVC   OLDPRDS+1(1),BYTE                                                
*                                                                               
CHKO200  MVI   ERROR,STCHAERR                                                   
         CLC   NUPRD,OLDPRDS       TEST IF RECORD CHANGED                       
         BNE   PRDI                                                             
         CLC   NUPRD2,OLDPRDS+1     TEST IF RECORD CHANGED                      
         BNE   PRDI                                                             
         XC    ERROR,ERROR                                                      
         B     EXIT                                                             
*  ALPHA CHECK                                                                  
*  R2 = WORK(INPUT PRODUCT CODES)                                               
*  R3 = 19 ELEMENT                                                              
CHKO300  CLI   0(R2),2                                                          
         BE    *+12                                                             
         CLI   0(R2),3                                                          
         BNE   PRDINV              PRODUCT CODE IS 2 OR 3 ALPHA CHARS           
*                                                                               
         CLI   1(R2),0             TEST FOR SECOND PRODUCT                      
         BE    CHKO320                                                          
         CLI   1(R2),2                                                          
         BE    *+12                                                             
         CLI   1(R2),3                                                          
         BNE   PRDINV                                                           
*                                                                               
CHKO320  MVI   ERROR,STCHAERR                                                   
         USING NUPDED,R3                                                        
         MVC   UNITPRDA,SPACES                                                  
         MVC   UNITPRDA(3),NUPDEPR                                              
         CLI   NUPDELEN,11          CHECK FOR PIGGYBACK                         
         BL    *+10                                                             
         MVC   UNITPRDA+3(3),NUPDEPR+7                                          
         CLC   UNITPRDA(3),12(R2)   TEST IF RECORD CHANGED                      
         BNE   PRDI                                                             
         CLC   UNITPRDA+3(3),22(R2)   TEST IF RECORD CHANGED                    
         BNE   PRDI                                                             
         XC    ERROR,ERROR                                                      
         B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
*                                                                               
* PRODUCT CODE(S) (SYNTAX IS CODE*CODE)                                         
*                                                                               
VALPROD  NTR1                                                                   
         XC    ALPHAPRD,ALPHAPRD                                                
*--CHECK FOR MORE THEN 2 PRODUCTS                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'14',(R4)),0                        
         CLI   12(R1),0                                                         
         BE    EXIT                                                             
*                                                                               
*--CHECK FOR MORE THEN 2 PRODUCTS (ALPHA ELEMENT)                               
         XC    UNITPRDA,UNITPRDA                                                
         MVI   UNIT19SW,0                                                       
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'19',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   PRD1A                                                            
         L     RE,12(R1)                                                        
         USING NUPDED,RE                                                        
         TM    NUPDEIND,X'C0'       CHECK COPYSPLIT OR TRIGGYBACK               
         BNZ   EXIT                 IF ON EXIT                                  
         MVC   UNITPRDA(3),NUPDEPR                                              
         CLI   NUPDELEN,11          CHECK FOR PIGGYBACK                         
         BL    *+10                                                             
         MVC   UNITPRDA+3(3),NUPDEPR+7                                          
         MVI   UNIT19SW,C'Y'                                                    
         DROP  RE                                                               
*                                                                               
*  CHECK IF UNIT PAID                                                           
PRD1A    MVI   UNPAYSW,C'N'                                                     
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'12',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   *+8                                                              
         MVI   UNPAYSW,C'Y'                                                     
*                                                                               
*  CHECK IF UNIT BILLED                                                         
*                                                                               
         CLI   BILLOCK,C'Y'         CHECK FOR SOON BILLING                      
         BNE   *+12                                                             
         MVI   UNBILLSW,C'Y'                                                    
         B     PRD1F                                                            
         MVI   UNBILLSW,C'N'                                                    
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'10',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   PRD1D                                                            
         L     RE,12(R1)                                                        
         USING NUBILD,RE                                                        
*                                                                               
PRD1B    TM    NUBILST,X'20'                                                    
         BO    *+12                                                             
         MVI   UNBILLSW,C'Y'                                                    
         B     PRD1F                                                            
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),X'10'                                                      
         BE    PRD1B                                                            
*                                                                               
* CHECK NEW BILLING RECORDS FOR BILLED DATA                                     
PRD1D    L     R5,AIO4                                                          
         USING NETBLOCK,R5                                                      
         XCEF  (R5),4000                                                        
*                                                                               
         MVC   NBSELAGY,QAGY                                                    
         MVC   NBACOM,ACOMFACS                                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF         RF=A(COMFACS)                                
         MVC   NBDM,CDATAMGR                                                    
         MVC   NBCALLOV,CCALLOV                                                 
         MVC   NBDATCON,CDATCON                                                 
         MVC   NBGETDAY,CGETDAY                                                 
         MVC   NBADDAY,CADDAY                                                   
         MVC   NBHEXOUT,CHEXOUT                                                 
         MVC   NBHELLO,CHELLO                                                   
         MVC   NBDEMADR,CDEMADDR                                                
         MVC   NBDEMAIN,CDEMAINT                                                
         MVC   NBDEMAND,CDEMAND                                                 
         MVC   NBDEMEL,CDEMEL                                                   
         MVC   NBDEMMTH,CDEMOMTH                                                
         MVC   NBDEMOUT,CDEMOUT                                                 
         MVC   NBGTPROF,CGETPROF                                                
         DROP  RF                                                               
*                                                                               
         CLI   UNBILLSW,C'Y'        IS UNIT SET TO BILLED                       
         BE    PRD1F                                                            
         XC    DMWORK(96),DMWORK                                                
         LA    RE,DMWORK                                                        
         STCM  RE,15,NBABILRD       ADDRESS OF BILL DSECT IN NETBLOCK           
         USING NBLBILLD,RE                                                      
         ST    R4,NBLUNAIO          ADDRESS OF UNIT RECORD                      
         OI    NBLFUNC,NBLBLD                                                   
         PRINT GEN                                                              
         GOTO1 VBILLRDR,DMCB,NETBLOCK                                           
         PRINT NOGEN                                                            
         LA    RE,DMWORK                                                        
         TM    NBLFUNC,NBLBILD                                                  
         BZ    *+8                                                              
         MVI   UNBILLSW,C'Y'                                                    
         DROP  R5                                                               
*                                                                               
*  MOVE PRODUCT INTO SCREEN FORMAT                                              
*                                                                               
PRD1F    L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         CLI   RUPNPRDL,0                                                       
         BE    PRD1H                                                            
         MVC   FLDH+5(1),RUPNPRDL                                               
         ZIC   RE,RUPNPRDL                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FLD(0),RUPNPRD                                                   
         LA    RE,9(RE)                                                         
         STCM  RE,1,FLDH                                                        
*                                                                               
PRD1H    MVC   UNITPRDS(1),NUPRD   SAVE CURRENT UNIT PRODUCTS                   
         MVC   UNITPRDS+1(1),NUPRD2                                             
         MVI   NUPRD,0                                                          
         MVI   NUPRD2,0                                                         
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'19',(R4))                          
         NI    NUUNST2,X'FF'-X'20' FROZEN PROD. ALLOCATION BIT OFF              
         MVI   FREEZESW,C'N'                                                    
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   PRD2                                                             
****     CLI   UNACTSW,C'A'        TEST FOR ACTION ADD                          
****     BE    PRD1H               YES-GET DEFAULT                              
*                                  NO INPUT MEANS NO PRODUCTS ON CHANGE         
                                                                                
         OC    UNITPRDS,UNITPRDS   TEST IF UNIT HAD BEEN ALLOCATED              
         BNZ   *+14                UNIT HAD NO PRODUCTS-NO UNALLOCATION         
         OC    UNITPRDA,UNITPRDA   TEST IF UNIT HAD BEEN ALLOC ALPHA            
         BZ    PRD15               UNIT HAD NO PRODUCTS-NO UNALLOCATION         
         OI    NUACTWHY,X'40'      BRAND/COST CHANGE                            
         BAS   RE,CHKTRA                                                        
         BNE   PRD1J                                                            
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   *+12                                                             
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
         OI    NUCMLFLG,X'40'      SET ALLOC. CHANGE FLAG FOR TRAFFIC           
*                                                                               
PRD1J    CLI   UNPAYSW,C'Y'        TEST IF UNIT IS PAID                         
         BE    PRD1L               YES - ERROR CANNOT UNALLOCATE PAID           
         CLI   UNBILLSW,C'Y'       TEST IF UNIT IS PAID                         
         BNE   PRD15               NO-OK TO UNALLOCATE                          
PRD1L    MVI   ERROR,UNALLERR                                                   
         B     PRDI                                                             
         SPACE                                                                  
PRD2     CLI   FLD,C'-'            TEST FOR DASH STARTING FIELD                 
         BNE   PRD3                NO                                           
         MVI   FREEZESW,C'Y'       YES-SET FLAG                                 
         BAS   RE,REMOVE                                                        
         BE    PRDI                NOTHING ELSE IN FIELD                        
         SPACE 1                                                                
PRD3     GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),C',=,*'                              
         CLI   4(R1),0                                                          
         BE    PRDI                INVALID INPUT                                
         LA    R2,WORK             POINT R2 AT BLOCK                            
         CLI   0(R2),2                                                          
         BE    *+12                                                             
         CLI   0(R2),3                                                          
         BNE   PRDI                PRODUCT CODE IS 2 OR 3 ALPHA CHARS           
         ZIC   RF,0(R2)                                                         
         LA    R1,12(R2)                                                        
         BAS   RE,PRDFORM          TEST IF PRODUCT IN RIGHT FORMAT              
         BNZ   PRDI                                                             
         MVC   THREE,12(R2)                                                     
         BAS   RE,VALPRD           CHECK INPUT                                  
         CLI   ERROR,0                                                          
         BNE   PRDI                                                             
         MVC   NUPRD,BYTE          PRODUCT NUMBER                               
         SPACE                                                                  
PRD4     CLI   1(R2),0             TEST FOR SECOND PRODUCT                      
         BNE   PRD5                YES-PROCESS                                  
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   PRD10                                                            
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         TM    NUSDST3,X'40'       CHECK FOR COPY-SPLIT BIT                     
         BNZ   PRDI                IF C-SPLIT NO SECOND PROD ERROR              
         B     PRD10                                                            
*                                                                               
PRD5     CLI   1(R2),2                                                          
         BE    *+12                                                             
         CLI   1(R2),3                                                          
         BNE   PRDI                                                             
         ZIC   RF,1(R2)                                                         
         LA    R1,22(R2)                                                        
         BAS   RE,PRDFORM          TEST IF PRODUCT IN RIGHT FORMAT              
         BNZ   PRDI                                                             
         MVC   THREE,22(R2)                                                     
         BAS   RE,VALPRD                                                        
         CLI   ERROR,0                                                          
         BNE   PRDI                                                             
         MVC   NUPRD2,BYTE                                                      
         SPACE                                                                  
PRD10    CLI   FREEZESW,C'Y'       TEST TO FREEZE ALLOCATION                    
         BNE   *+8                                                              
         OI    NUUNST2,X'20'       YES                                          
*                                                                               
* CHECK ALPHA PRODUCT CHANGE BEFORE 1 BYTE TEST                                 
         CLI   UNIT19SW,C'Y'                                                    
         BNE   PRD10C                                                           
         CLC   UNITPRDA(3),ALPHAPRD                                             
         BNE   PRD10E                                                           
         CLC   UNITPRDA+3(3),ALPHAPRD+3                                         
         BE    PRD18                                                            
*                                                                               
PRD10C   CLC   NUPRD,OLDPRDS       TEST FOR ALLOCATION CHANGE                   
         BNE   PRD10E              NO                                           
         CLC   NUPRD2,OLDPRDS+1    TEST FOR ALLOCATION CHANGE                   
         BE    PRD18               NO                                           
PRD10E   OC    NUAFFTIM,NUAFFTIM   SEE IF AFFID SEEDED                          
         BZ    *+12                                                             
         MVI   ERROR,AFFIDLCK                                                   
         B     PRDI                                                             
         OI    NUACTWHY,X'40'      BRAND/COST CHANGE                            
         CLI   UNPAYSW,C'Y'        TEST IF UNIT PAID                            
         BE    PRD10G              NO                                           
         CLI   N2PROFLE+13,C'I'    CAN PROD CHANGE ON BILLED UNIT               
         BE    *+12                NO                                           
         CLI   N2PROFLE+13,C'C'    CAN PROD CHANGE ON BILLED UNIT               
         BNE   PRD11               NO                                           
         CLI   UNBILLSW,C'Y'       TEST IF UNIT BILLED                          
         BNE   PRD11               YES-STOP CHANGE                              
PRD10G   MVI   ERROR,ALLCHERR      YES-ERROR TO CHANGE ALLOC.                   
         B     PRDI                                                             
         SPACE 1                                                                
PRD11    BAS   RE,CHKTRA                                                        
         BNE   PRD18                                                            
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   *+12                                                             
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
         OI    NUCMLFLG,X'40'      SET ALLOC. CHANGE FLAG FOR TRAFFIC           
         B     PRD18                                                            
*                                                                               
PRD15    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   PRD18                                                            
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         TM    NUSDST3,X'40'       CHECK FOR COPY-SPLIT                         
         BNZ   PRDI                IF C-SPLIT AND NO PROD ERROR                 
         B     PRD18                                                            
         SPACE                                                                  
PRDI     LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
         MVI   BUYERRSW,C'Y'                                                    
         MVC   ERNUMBR,ERROR                                                    
         XC    ERMXFLD(60),ERMXFLD                                              
         MVC   ERMXFLD(7),=CL7'PRODUCT'                                         
         B     PRD18                                                            
*  BUILD ALPHA PRODUCT ELEMENT                                                  
*                                                                               
*  CHECK IF 19 ELEMENT SHOULD BE CREATED (COMMENTED OUT)                        
*                                                                               
****PRD18    LA    RE,OVAGYTAB                                                  
****PRD18A   CLC   0(2,RE),QAGY                                                 
****         BNE   PRD18B                                                       
****         OC    2(2,RE),2(RE)        IS THERE A CLIENT RESTRICTION           
****         BZ    PRD20                                                        
****         CLC   2(2,RE),NUKCLT                                               
****         BE    PRD20                                                        
****PRD18B   LA    RE,4(RE)                                                     
****         CLI   0(RE),0                                                      
****         BE    PRDX                                                         
****         B     PRD18A                                                       
*                                                                               
PRD18    CLI   BUYERRSW,C'Y'                                                    
         BE    PRDX                                                             
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'19',(R4))                          
PRD21    OC    ALPHAPRD,ALPHAPRD                                                
         BZ    PRDX                                                             
*                                                                               
         XC    WORK,WORK                                                        
PRD22    LA    R3,WORK                                                          
         USING NUPDED,R3                                                        
         MVC   NUPDEEL(2),=X'190A'                                              
         MVC   NUPDEPR,ALPHAPRD                                                 
         OC    ALPHAPRD+3(3),ALPHAPRD+3                                         
         BZ    PRD25                                                            
         MVI   NUPDELEN,17                                                      
         MVC   NUPDEPR+7(3),ALPHAPRD+3                                          
PRD25    GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(R4),WORK,0                           
         B     PRDX                                                             
*                                                                               
         SPACE                                                                  
PRDX     B     EXIT                                                             
         DROP  R5,R6,RE                                                         
*                                                                               
PRDINV   MVI   ERROR,INVERR                                                     
         B     PRDI                                                             
*                                                                               
*  OVAGYTAB  BYTE 1   AGENCY                                                    
*  OVAGYTAB  BYTE 2-3 CLIENT IF RESTRICTED                                      
OVAGYTAB DC    CL2'*B',XL2'0000'     DDSB                                       
         DC    CL2'SJ',XL2'0000'     SJR                                        
         DC    CL2'*1',XL2'0000'     DDS1                                       
         DC    CL2'DR',XL2'BCDE'     SMGTEST/PG5                                
         DC    CL2'DU',XL2'BCDE'     MVNYN/PG5                                  
         DC    X'00'                                                            
*                                                                               
* PRDFORM-CHECKS FOR ALPHA IN FIRST PRODUCT FIELD, AND ALPHA-NUMERIC            
* IN THE REST.                                                                  
*        RF=LENGTH OF FIELD                                                     
*        R1=FIELD                                                               
*                                                                               
PRDFORM  NTR1                                                                   
         BCTR  RF,0                                                             
*--CHECK FIRST CHARACTER FOR ALPHA                                              
         CLI   0(R1),X'C1'                                                      
         BL    PRDBFRM                                                          
         CLI   0(R1),X'E9'                                                      
         BH    PRDBFRM                                                          
         LA    R1,1(R1)                                                         
*                                                                               
PRDFM20  CLI   0(R1),X'F0'                                                      
         BL    PRDFM30                                                          
         CLI   0(R1),X'F9'                                                      
         BNH   PRDFM40                                                          
         B     PRDBFRM                                                          
PRDFM30  CLI   0(R1),X'C1'                                                      
         BL    PRDBFRM                                                          
         CLI   0(R1),X'E9'                                                      
         BH    PRDBFRM                                                          
PRDFM40  LA    R1,1(R1)                                                         
         BCT   RF,PRDFM20                                                       
*                                                                               
         SR    R1,R1               SET GOOD RETURN                              
PRDBFRM  LTR   R1,R1                                                            
         B     EXIT                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK INPUT BRAND CODE AGAINST CLIENT LIST (AT ENTRY           
* THREE CONTAINS INPUT, AT EXIT BRAND NUMBER IN BYTE)                           
*                                                                               
VALPRD   NTR1                                                                   
* FIRST VALIDATE THE ALPHA CODE                                                 
         XC    KEY,KEY             NOW VALIDATE BRAND ESTIMATE                  
         LA    R3,KEY                                                           
         USING PRDHDR,R3                                                        
         MVC   PLSTTYPE(2),=XL2'0DF1'                                           
         MVC   PLSTAM,NUKAM                                                     
         MVC   PLSTCLT,NUKCLT                                                   
         MVC   PLSTPRD,THREE       PRODUCT CODE                                 
         CLC   THREE,=C'POL'       TEST FOR POOL                                
         BE    *+14                YES                                          
         CLC   THREE,=C'AAA'       TEST FOR AAA                                 
         BNE   *+12                NO                                           
         MVI   ERROR,INVERR        NOT A VALID ALLOCATION                       
         B     VALPRDX                                                          
*                                                                               
*******  CLC   THREE,=CL3'POL'                                                  
*******  BNE   *+8                                                              
*******  MVI   PLSTXFF,X'FF'                                                    
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(9),KEYSAVE      TEST IF PRODUCT FOUND                        
         BE    *+12                YES                                          
*******  MVI   FERN,PRDERR                                                      
         MVI   ERROR,PRDERR                                                     
         B     VALPRDX                                                          
*                                                                               
         MVC   BYTE,PLSTBPRD+1                                                  
*                                                                               
         OC    ALPHAPRD(3),ALPHAPRD IS THIS THE 2ND PRODUCT                     
         BNZ   *+14                                                             
         MVC   ALPHAPRD(3),THREE                                                
         B     *+10                                                             
         MVC   ALPHAPRD+3(3),THREE                                              
         B     VALPRD15                                                         
         DROP  R3                                                               
* NOW RETRIEVE ONE BYTE PRODUCT CODE IF AVAILABLE                               
* *************   NO LONGER USED   ******************                           
         L     RE,AIO2                                                          
         USING CLTHDR,RE                                                        
         LA    R0,220              COUNTER                                      
         LA    RF,CLIST                                                         
         MVI   BYTE,0                                                           
         CLC   THREE,=C'POL'       TEST FOR POOL                                
         BE    *+14                YES                                          
         CLC   THREE,=C'AAA'       TEST FOR AAA                                 
         BNE   VALPRD2             NO                                           
         MVI   ERROR,INVERR        NOT A VALID ALLOCATION                       
         B     VALPRDX                                                          
         SPACE                                                                  
VALPRD2  OC    0(4,RF),0(RF)       TEST FOR E-O-L.                              
         BZ    VALPRD8             YES                                          
         CLC   THREE,0(RF)         COMPARE INPUT VS ENTRY                       
         BE    VALPRD10            FOUND IT                                     
         LA    RF,4(RF)                                                         
         BCT   R0,VALPRD2                                                       
         SPACE                                                                  
* CHECK SECOND TABLE                                                            
         LA    R0,35                                                            
         LA    RF,CLIST2           POINT TO PRODUCT LIST                        
VALPRD5  OC    0(4,RF),0(RF)       TEST FOR END-OF-LIST                         
         BZ    VALPRD8             YES                                          
         CLC   THREE,0(RF)                                                      
         BE    VALPRD10                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,VALPRD5                                                       
*                                                                               
VALPRD8  MVI   ERROR,PRDERR                                                     
         B     VALPRDX                                                          
         SPACE                                                                  
VALPRD10 MVC   BYTE,3(RF)          EXTRACT PRODUCT NUMBER                       
         SPACE                                                                  
VALPRD15 XC    KEY,KEY             NOW VALIDATE BRAND ESTIMATE                  
         LA    R3,KEY                                                           
         USING ESTHDRD,R3                                                       
         MVI   EKEYTYPE,X'00'                                                   
         MVC   EKEYAM,NUKAM                                                     
         MVC   EKEYCLT,NUKCLT                                                   
         MVC   EKEYPRD,THREE                                                    
         MVC   EKEYEST,NUKEST                                                   
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(L'EKEY),KEYSAVE TEST IF ESTIMATE FOUND                       
         BE    VALPRDX             YES                                          
         MVI   ERROR,PRESTERR                                                   
         SPACE 1                                                                
VALPRDX  B     EXIT                                                             
         DROP  R3,RE                                                            
         EJECT                                                                  
* SUB-ROUTINE TO REMOVE A LEADING CHARACTER FROM FLD                            
* ON EXIT, CC=EQ FOR NOTHING LEFT IN FIELD, NEQ FOR SOMETHING THERE             
*                                                                               
REMOVE   ZIC   R1,FLDH+5                                                        
         SH    R1,=H'1'                                                         
         BZR   RE                                                               
         STC   R1,FLDH+5                                                        
         EX    R1,MOVEFLD          SHIFT FLD OVER TO THE LEFT                   
         LA    RF,FLD(R1)          POINT TO LAST CHAR POSITION                  
         MVI   0(RF),C' '                                                       
         CLI   FLDH+5,0                                                         
         BR    RE                                                               
*                                                                               
MOVEFLD  MVC   FLD(0),FLD+1                                                     
         EJECT                                                                  
*          DATA SET NEBUY35    AT LEVEL 033 AS OF 09/27/00                      
* ROUTINE TO CHECK THE TRAFFIC ELEMENTS TO SEE IF THE                           
* STATUS BITS SHOULD BE CHANGED                                                 
*                                                                               
CHKTRA   NTR1                                                                   
         CLC   HOLDCLT,NUKCLT       CHECK CLIENT BREAK                          
         BE    CHKTR050                                                         
         BAS   RE,READPROF          READ PROFILE                                
         MVC   HOLDCLT,NUKCLT       CHECK CLIENT BREAK                          
CHKTR050 CLI   N2PROFLE+12,C'Y'                                                 
         BNE   *+8                                                              
         BAS   RE,CLRBLBD          CLEAR BILLBOARD INFORMATION                  
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   CHKTRBEX                                                         
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
         OC    NUCML1(16),NUCML1                                                
         BNZ   CHKTRGEX                                                         
         OC    NUCMLBSN(16),NUCMLBSN                                            
         BNZ   CHKTRGEX                                                         
*                                                                               
CHKTRBEX CR    RB,RE               NOT EQUAL CONDITION                          
         B     CHKTREX                                                          
*                                                                               
CHKTRGEX CR    RB,RB               EQUAL CONDITION                              
CHKTREX  B     EXIT                                                             
         DROP  RE                                                               
         SPACE 3                                                                
*-- CLEAR THE BILLBOARD INFORMATION IN THE TRAFFIC                              
*-- ELEMENTS (21,23).                                                           
CLRBLBD  NTR1                                                                   
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   BILBD100                                                         
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
         XC    NUCMLBSL(17),NUCMLBSL                                            
         DROP  RE                                                               
*                                                                               
BILBD100 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'23',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   BILBDEX                                                          
         L     RE,12(R1)                                                        
         USING NUFDCEL,RE                                                       
         XC    NUFDCBSL(17),NUFDCBSL                                            
*                                                                               
BILBDEX  B     EXIT                                                             
         DROP  RE                                                               
         SPACE 3                                                                
*-- REDA NEW CLIENT RECORD EXTRACT OFFICE CODE READ N2 PROFILE                  
READPROF NTR1                                                                   
         L     R4,AIO1              POINT R4 TO UNIT RECORD                     
         CLC   HOLDCLT,NUKCLT       CHECK CLIENT BREAK                          
         BE    CLIPREX                                                          
         MVC   HOLDCLT,NUKCLT                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),NUKPCLT                                                 
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 CLIENT RECORD MUST EXIST                    
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO2                                    
         L     RE,AIO2                                                          
         USING CLTHDR,RE                                                        
*                                                                               
         MVC   CLIOPTN2,COPT2                                                   
         MVC   CLCRPPRD,CPRPRD                                                  
*                                                                               
*  READ THE PROFILE                                                             
*                                                                               
CLIPROF  XC    KEY,KEY             GET USER PROFILE INTO NBUSER                 
         MVC   KEY(4),=C'S0N2'                                                  
         MVC   KEY+4(2),QAGY                                                    
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(3),NUKPCLT                                                 
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),COFFICE                                                
         GOTO1 VGETPROF,DMCB,KEY,N2PROFLE,VDATAMGR                              
CLIPREX  B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
* SET UP THE INPUTTED ASSIGNED COST IF FLDH AND FLD                             
* THIS IS TO SET UP THE INPUTTED FOR THE VALASS ROUTINE                         
*                                                                               
DISASS   NTR1                                                                   
         L     R6,AIO4                                                          
         USING NETBLOCK,R6                                                      
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
         XC    ERROR,ERROR                                                      
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         CLI   RUPNASSL,0                                                       
         BNE   *+12                                                             
         CLI   RUPOASSL,0          CHECK FOR ANY INPUT                          
         BE    DISASEX                                                          
         CLC   RUPOASS,NUASSIGN     MAKE SURE OLD ASSGN COST IS VALID           
         BNE   DISASER                                                          
         MVC   NBASSIGN,RUPOASS     SET NETBLOCK FOR VALIDATION                 
*                                                                               
         XC    FLD,FLD                                                          
         XC    FLDH,FLDH                                                        
         CLI   RUPNASSL,0          CHECK FOR INPUT                              
         BE    DISASEX                                                          
         ICM   R2,15,RUPNASS                                                    
         BNZ   DISAS40             NON-ZERO                                     
         MVI   FLD,C'0'            YES-FORCE OUT ZERO                           
         B     DISAS50                                                          
         SPACE                                                                  
DISAS40  BAS   R4,EDTMIN                                                        
DISAS50  MVC   FLDH+5(1),RUPNASSL                                               
*                                                                               
         SR    RE,RE                                                            
         LA    RF,FLD                                                           
         LA    R1,9                                                             
DISAS70  CLI   0(RF),X'40'                                                      
         BNH   DISAS80                                                          
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,DISAS70                                                       
DISAS80  STCM  RE,1,FLDH+5                                                      
DISASEX  B     EXIT                                                             
*                                                                               
DISASER  LA    RE,WORK2                                                         
         USING ERRDATA,RE                                                       
         MVI   ERROR,STCHAERR                                                   
         MVC   ERNUMBR,ERROR                                                    
         XC    ERMXFLD(60),ERMXFLD                                              
         MVC   ERMXFLD(13),=CL13'ASSIGNED COST'                                 
         MVI   BUYERRSW,C'Y'                                                    
         B     DISASEX                                                          
         DROP  R5,R6,RE                                                         
         SPACE 2                                                                
* DISPLAY ASSIGNED COST VALUE                                                   
EDTMIN   LR    R0,R2               SAVE COST VALUE                              
         SRDA  R2,32               PREPARE DIVIDEND                             
         D     R2,=F'100'          SEPARATE DOLLARS AND PENNIES                 
         LTR   R2,R2               TEST REMAINDER (PENNIES)                     
         BNZ   EDTMIN2             YES                                          
         EDIT  (R3),(11,FLD),ALIGN=LEFT,MINUS=YES                               
         B     EDTMINX                                                          
         SPACE                                                                  
EDTMIN2  LR    R2,R0               RESTORE COST VALUE W PENNIES                 
         EDIT  (R2),(12,FLD),2,ALIGN=LEFT,MINUS=YES                             
         SPACE                                                                  
EDTMINX  BR    R4                                                               
         EJECT                                                                  
* ASSIGNED COST                                                                 
*                                                                               
VALASS   NTR1                                                                   
         L     R6,AIO4                                                          
         USING NETBLOCK,R6                                                      
         XC    HALF,HALF                                                        
         MVC   FULL,NUASSIGN                                                    
         MVC   HALF(1),NUUNITST                                                 
         NI    HALF,X'08'          SAVE ASSIGNED COST INPUT BIT SETTING         
         XC    NUASSIGN,NUASSIGN                                                
         NI    NUUNITST,X'FF'-X'88' TURN OFF MINUS UNIT,ASSGND CST BIT          
         MVI   MINUSSW,NO          INITIALIZE MINUS COST SWITCH                 
*                                                                               
         MVI   ELCODE,X'02'                                                     
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(ELCODE,(R4)),0                       
         CLI   12(R1),0            SET CC ON EXIT                               
         BNE   ASS2                                                             
         L     R3,12(R1)                                                        
         USING NUSDRD,R3                                                        
         MVC   SVSTA,NUSDST3                                                    
         NI    SVSTA,X'80'         SAVE OVERRIDE SETTING                        
         NI    NUSDST3,X'FF'-X'80' RESET OVERRIDE BIT                           
         DROP  R3                                                               
         SPACE 1                                                                
ASS2     CLI   FLD,C'*'            TEST FOR FROZEN                              
         BNE   ASS3                                                             
* REMOVE FROZEN INDICATOR FROM FIELD                                            
         MVC   FLD(8),FLD+1                                                     
         MVI   FLD+8,X'40'                                                      
         ZIC   R3,FLDH+5                                                        
         BCTR  R3,0                                                             
         STC   R3,FLDH+5                                                        
*                                                                               
ASS3     CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    ASSX                                                             
         CLI   FLD,C'^'            TEST FOR REMOVE                              
         BE    ASSX                                                             
         ZIC   R1,FLDH+5                                                        
         LA    RE,FLD-1(R1)        LOOK AT LAST CHARACTER                       
         CLI   0(RE),C'-'          TEST FOR A DASH                              
         BNE   ASS4                                                             
         MVI   0(RE),C' '                                                       
         MVI   MINUSSW,YES                                                      
         SH    R1,=H'1'                                                         
         BZ    ASSR                ITS AN ERROR - ONLY DASH IN FIELD            
         SPACE 1                                                                
ASS4     LR    R0,R1               DATA LENGTH                                  
         GOTO1 VCASHVAL,DMCB,FLD,(R0)                                           
         CLI   0(R1),X'FF'                                                      
         BE    ASSR                                                             
         CLI   COMMAND,C'S'                                                     
         BE    ASSX                                                             
         ICM   RE,15,4(R1)         GET AMOUNT                                   
         CLI   MINUSSW,YES         TEST FOR MINUS AMOUNT                        
         BNE   *+6                                                              
         LNR   RE,RE               YES-FORCE COST NEGATIVE                      
         STCM  RE,15,NUASSIGN                                                   
         OI    NUUNITST,X'08'      ASSIGNED COST INPUT                          
*-- CHECK FOR SUBSIDIARY 01 ELEMENT                                             
*                                                                               
         MVI   ELCODE,X'02'                                                     
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(ELCODE,(R4)),0                       
         CLI   12(R1),0            SET CC ON EXIT                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
         USING NUSDRD,R3                                                        
         CLC   FULL,NUASSIGN                                                    
         BNE   ASS5                                                             
         OC    NUASSIGN,NUASSIGN    IF ZEROCHECK STATUS                         
         BNZ   ASS5A                                                            
         MVC   HALF+1(1),NUUNITST    SAVE CURRENT STATUS                        
         NI    HALF+1,X'08'                                                     
         CLC   HALF(1),HALF+1        DID ASSIGNED COST STATUS CHANGE            
         BE    *+12                                                             
ASS5     OI    NUSDST3,X'80'       ASSIGNED COST OVERRIDE BIT SET               
         B     ASS8                                                             
ASS5A    OC    NUSDST3,SVSTA       SET TO ORIGINAL SETTING                      
         B     ASS8                                                             
*                                                                               
ASS8     CLC   NUASSIGN,NBASSIGN   TEST FOR CHANGE IN ASSIGNED COST             
         BE    ASSX                                                             
         SPACE                                                                  
ASS10    OI    NUACTWHY,X'20'      ASSIGNED COST CHANGE INDICATOR               
         SPACE                                                                  
ASSX     GOTO1 VCALCSHP,DMCB,AIO1,ACOMFACS,CLCRPPRD                             
         B     EXIT                                                             
         SPACE 1                                                                
ASSR     LA    RE,WORK2                                                         
         USING ERRDATA,RE                                                       
         MVI   ERROR,INVERR                                                     
         MVC   ERNUMBR,ERROR                                                    
         XC    ERMXFLD(60),ERMXFLD                                              
         MVC   ERMXFLD(13),=CL13'ASSIGNED COST'                                 
         MVI   BUYERRSW,C'Y'                                                    
         B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
* CHECK TO SEE IF ASSIGNED COST SHOULD BE UNLOCKED                              
*                                                                               
VALLOCK  NTR1                                                                   
         XC    ERROR,ERROR                                                      
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
         TM    RUPSTAT,X'40'                                                    
         BZ    VALLOCEX                                                         
* UNLOCK ASSIGNED COST                                                          
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R4)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         NI    NUSDST3,X'7F'       TURN OFF FROZEN ASSIGNED COST BIT            
VALLOCEX B     EXIT                                                             
         DROP  R5,RE                                                            
         EJECT                                                                  
* WRITE THE BUY RECORD BACK                                                     
*                                                                               
WRITBUY  NTR1                                                                   
         L     R5,ANETBLK                                                       
         USING BUYUPLDD,R5                                                      
*  UPDATE ACTIVITY ELEMENT                                                      
         GOTO1 VDATCON,DMCB,(5,0),(3,THREE)                                     
         XC    WORK,WORK                                                        
         LA    RE,WORK             BUILD ACTIVITY ELEMENT                       
         USING NUACTD,RE                                                        
         MVI   NUACTEL,X'99'                                                    
         MVI   NUACTLEN,22                                                      
         MVC   NUACTADT,THREE      ADD DATE                                     
         MVC   NUACTCDT,THREE      LAST ACTIVITY DATE                           
         MVC   NUACTAID,SVPASSWD   ADD PERSONAL ID                              
         MVC   NUACTCID,SVPASSWD   LAST PERSONAL ID                             
         MVC   NUACTAGD,SECAGY     SECURITY AGENCY                              
         MVC   NUACTRSN(3),RUPREAS    REASON CODE                               
         MVI   NUACTRSN+3,X'40'    BLANK FILL                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'99',(R4)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RF,12(R1)           COPY THE 3RD STATUS BIT                      
         LA    RE,WORK             RESET RE TO WORK                             
         MVC   NUACTACD,2(RF)      COPY OVER ADD AUTH. CODE                     
         MVC   NUACTADT,4(RF)      AND THE CREATION DATE                        
         CLI   1(RF),13                                                         
         BL    WRTB100                                                          
         MVC   NUACTAID,12(RF)     COPY OVER ADD PERSONAL ID                    
         MVC   NUACTAGD,16(RF)     AND SECURITY AGENCY                          
         SPACE                                                                  
WRTB100  GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'99',(R4))                          
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(R4),WORK,0                           
*                                                                               
*  WRITE BACK THE BUY RECORD                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(20),NUKEY                                                    
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIOCALL,DMCB,UNT+FIL+GET+UPDATE,AIO3                             
         GOTO1 AIOCALL,DMCB,UNT+FIL+PUT,AIO1                                    
         B     EXIT                                                             
         DROP  R5,RE                                                            
         EJECT                                                                  
*=================================================================*             
* SEND DRAFT BUY INFO TO THE PC                                   *             
*=================================================================*             
         SPACE 1                                                                
SNDPROD  NTR1                                                                   
*                                                                               
         LHI   R1,X'31'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
*  CHECK IF ERROR RETURNED                                                      
*                                                                               
         CLI   BUYERRSW,C'Y'                                                    
         BNE   SNP040                                                           
         LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
*                                                                               
         LA    R4,SEQNUM                                                        
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,ERNUMBR                                                       
         LHI   R1,X'39'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         GOTO1 VGETMSG,DMCB+12,(ERNUMBR,WORK),(X'FF',DMCB),(7,0)                
         LA    R4,WORK+8                                                        
         LHI   R1,X'3A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,ERMXFLD                                                       
         LHI   R1,X'3B'                                                         
         BAS   RE,SENDD                                                         
         MVI   BUYERRSW,C'N'                                                    
         XC    ERROR,ERROR                                                      
         B     SNPEX                                                            
*                                                                               
SNP040   LA    R4,SEQNUM            SEQUENCE NUMBER                             
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNPEX    B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
*=================================================================*             
* COMMON CALL TO TSAR TEST                                        *             
*=================================================================*             
CALLTSAR LR    R0,RE                                                            
***********                                                                     
*****    LA    R1,SVTSSVRS                                                      
*****    CLI   0(R1),0                                                          
*****    BE    *+12                                                             
*****    LA    R1,1(R1)                                                         
*****    B     *-12                 RESTORE TSAR                                
*****    MVC   0(1,R1),TSACTN                                                   
***********                                                                     
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
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
CALBUYEX XIT1                                                                   
         EJECT                                                                  
UNTFILE  DC    CL8'UNTFILE'                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DENADCATS                                                      
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
SVBDELEM DS    CL67                                                             
ALDATE   DS    XL2                 ALLOC DATE                                   
LADATE   DS    XL2                 LAST ALLOC DATE                              
V10301   DS    CL4                                                              
EDSAVE   DS    XL17                                                             
BILLOCK  DS    CL1                                                              
PRODCHSW DS    CL1                                                              
*                                                                               
OLDPRDS  DS    CL2                                                              
UNPAYSW  DS    CL1                                                              
UNBILLSW DS    CL1                                                              
PRDSCRN  DS    CL15                                                             
HOLDCLT  DS    CL2                                                              
N2PROFLE DS    CL16                                                             
THREE    DS    CL3                                                              
FREEZESW DS    CL1                                                              
UNITPRDS DS    CL2                                                              
UNITPRDA DS    CL6                                                              
ALPHAPRD DS    CL6                                                              
UNIT19SW DS    CL1                  YES 19 ELEMENT EXISTS                       
SEQNUM   DS    CL1                                                              
SVSTA    DS    XL1                                                              
MINUSSW  DS    CL1                                                              
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
*                                                                               
CLIOPTN2 DS    CL1                                                              
CLCRPPRD DS    CL3                                                              
*                                                                               
PAKCLI   DS    CL3                                                              
PAKEST   DS    CL3                                                              
PAKNET   DS    CL4                                                              
PAKNUM   DS    CL3                                                              
PAKDPT   DS    CL1                                                              
PAKCNTL  DS    CL1                                                              
PAKSTAT  DS    CL1                                                              
*                                                                               
SECAGY   DS    CL2                                                              
SVPASSWD DS    CL2                                                              
*                                                                               
DEMOVER  DS    0C                                                               
DEM1HOVR DS    CL4                 HOMES DEMO OVERRIDES 1                       
DEM2HOVR DS    CL4                 HOMES DEMO OVERRIDES 2                       
DEM1OVER DS    CL75                DEMO OVERRIDES 1                             
DEM2OVER DS    CL75                DEMO OVERRIDES 2                             
DEMOVERL EQU   *-DEMOVER                                                        
*                                                                               
WSVRBLK  DS    XL(FAWSSVRL)        ** WSSVR CONTROL BLOCK                       
*                                                                               
         DS    0D                                                               
         ORG                                                                    
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
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENREAS                                                      
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE NETBILLRD                                                      
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE FAUTL                                                          
       PRINT ON                                                                 
       ++INCLUDE NAVDSECTS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094NENAV07   10/14/20'                                      
         END                                                                    
