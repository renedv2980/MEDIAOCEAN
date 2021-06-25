*          DATA SET NENAV06    AT LEVEL 073 AS OF 11/08/18                      
*          DATA SET NENAV06    AT LEVEL 054 AS OF 05/24/00                      
*PHASE T31806B                                                                  
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31806   TITLE 'NENAV06 - STEWARD/MATCHMAKER - BUY MAINT OVERLAY'               
T31806   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV06**                                                       
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         USING TSARD,TSARLOC                                                    
*                                                                               
* DID WE GO TO THE BUY ALREADY                                                  
         CLI   SVGLOBSW,C'B'                                                    
         BE    ROUT040                                                          
         CLI   SVGLOBSW,C'E'                                                    
         BE    EXIT                                                             
* ROUTE THE ELEMENTS TO THE RIGHT ROUTINES                                      
         CLC   SVRCVEL,=X'0010'                                                 
         BE    ROUT020                                                          
         SPACE 3                                                                
*                                                                               
*  INITIAL DOWNLOAD ROUTINES                                                    
*                                                                               
*--SEND DEMO, NAD PREFIX, DAY, LENGTH INFO                                      
ROUT020  GOTO1 VALIMED                                                          
         BAS   RE,CALLBUY                                                       
         B     EXIT                                                             
*                                                                               
*  PROCESS RETURNED BUY INFORMATION                                             
*                                                                               
*--SEND DEMO, NAD PREFIX, DAY, LENGTH INFO                                      
ROUT040  BAS   RE,SENDATA                                                       
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
         MVI   SVXFROV,X'06'       RETURN CONTROL TO THIS OVLY                  
         MVI   SVGLOBSW,C'B'       SET SWITCH TO UNITS BOUGHT                   
*                                                                               
*  TEST THE GLOBBER CALLS                                                       
*                                                                               
         L     R3,ANETBLK                                                       
         USING BUYDRFTD,R3                                                      
*        MVC   REQCLI,=CL3'BGM'                                                 
*        MVC   REQEST,=CL3'002'                                                 
*        MVC   REQNET,=CL4'NBC '                                                
*        MVC   REQPACK,=CL3'001'                                                
*        MVC   REQPCODE,=CL6'ALF   '                                            
*        MVC   REQBDATE,=CL6'990607'                                            
*        MVC   REQWEEKS,=CL6'3 '                                                
*        MVC   REQPWEEK,=CL6'5  '                                               
*        MVC   REQREAS,=CL3'2  '                                                
         MVC   RDRDEMS,QDEMOS                                                   
*********                                                                       
******   XC    DMCB(24),DMCB                                                    
******   L     RA,ATWA                                                          
******   MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
******   MVI   DMCB+8,2            PAGE NUMBER                                  
******   MVI   DMCB+9,0                                                         
******   MVC   DMCB+20(2),=C'L='                                                
******   MVC   DMCB+22(2),=X'03E8'  WRITE 1000 BYTES                            
******   MVC   WORK(4),DMCB+8       SAVE FOR GLOBBER CALL                       
******   GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R3),0                      
******   CLI   8(R1),0             BLOW ON ANY ERROR HERE                       
******   BE    *+6                                                              
******   DC    H'0'                                                             
*                                                                               
* DO WSSVR CALL YTO PASS INFO TO THE BUY SYSTEM                                 
         LA    RE,WSVRBLK                                                       
         USING FAWSSVRD,RE                                                      
         MVC   FAWSTOKN,=CL4'NNAV'                                              
         MVI   FAWSACTN,FAWSUSVE                                                
         MVC   FAWSADR,ANETBLK                                                  
         MVC   FAWSLEN,=H'2000'                                                 
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
         L     R3,AIO3                                                          
         USING DRAFRECD,R3                                                      
*                                                                               
*  CHECK IF ERROR RETURNED                                                      
*                                                                               
         CLI   BUYERRSW,C'Y'                                                    
         BNE   SNDB30                                                           
         LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
*                                                                               
         LHI   R1,X'31'                                                         
         BAS   RE,SENDH                                                         
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
         B     SNDBEX                                                           
*                                                                               
SNDB30   MVI   TSACTN,TSARES        RESTORE THE TSAR BUFFER                     
         BAS   RE,CALLTSAR                                                      
*                                                                               
         MVI   TSACTN,TSAGET        GET FIRST RECORD FROM TSAR                  
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         BAS   RE,CALLTSAR                                                      
         BE    SNDB50                                                           
         B     SNDBEX                                                           
SNDB40   MVI   TSACTN,TSANXT                                                    
         BAS   RE,CALLTSAR                                                      
         BNE   SNDBEX                                                           
*                                                                               
SNDB50   LHI   R1,X'31'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R4,DRFNUM            NUMBER OF UNITS                             
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFDATE           BUY DATE                                    
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFDAY            DAYS                                        
         LHI   R1,X'03'                                                         
         BAS   RE,SENDD                                                         
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
SNDB100  MVI   BYTE,1               DEMO PRECISION                              
         TM    DRFSTAT,X'40'                                                    
         BZ    *+8                                                              
         MVI   BYTE,2                                                           
         LA    R4,BYTE                                                          
         LHI   R1,X'15'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFHOMSH          HOMES SHARE                                 
         LHI   R1,X'0F'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFHOMHT          HOMES HUT                                   
         LHI   R1,X'10'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFHOMRT          HOMES RATING                                
         LHI   R1,X'11'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFHOMIM          HOMES IMPRESSIONS                           
         LHI   R1,X'12'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    DRFRESLT,DRFRESLT                                                
         BZ    SNDB140                                                          
         LA    R4,DRFRESLT          RESULT CODE                                 
         LHI   R1,X'16'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SNDB140  LA    R6,QDEMOS                                                        
         LA    R4,DRFDEMS                                                       
         LA    R2,25                                                            
*                                                                               
SNDB150  CLI   1(R6),0                                                          
         BZ    SNDB180                                                          
*                                                                               
         LHI   R1,X'0B'             VPH                                         
         BAS   RE,SENDD                                                         
         LA    R4,2(R4)                                                         
*                                                                               
         LHI   R1,X'0C'             GRP                                         
         BAS   RE,SENDD                                                         
         LA    R4,2(R4)                                                         
*                                                                               
         LHI   R1,X'0D'             IMP                                         
         BAS   RE,SENDD                                                         
         LA    R4,4(R4)                                                         
         LA    R6,3(R6)                                                         
         BCT   R2,SNDB150                                                       
*                                                                               
SNDB180  CLI   DRFDEMO2,C'Y'        CHECK SECOND DEMO COLUMN                    
         BNE   SNDB40                                                           
*                                                                               
         LA    R4,DRFHOMS2          HOMES SHARE COLUMN 2                        
         LHI   R1,X'1A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFHOMH2          HOMES HUT COLUMN 2                          
         LHI   R1,X'1B'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFHOMR2          HOMES RATING COLUMN 2                       
         LHI   R1,X'1C'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,DRFHOMI2          HOMES IMPRESSIONS COLUMN 2                  
         LHI   R1,X'1D'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R6,QDEMOS                                                        
         LA    R4,DRFDEMS2                                                      
         LA    R2,25                                                            
*                                                                               
SND200   CLI   1(R6),0                                                          
         BZ    SNDB40                                                           
*                                                                               
         LHI   R1,X'17'             VPH COLUMN 2                                
         BAS   RE,SENDD                                                         
         LA    R4,2(R4)                                                         
*                                                                               
         LHI   R1,X'18'             GRP COLUMN 2                                
         BAS   RE,SENDD                                                         
         LA    R4,2(R4)                                                         
*                                                                               
         LHI   R1,X'19'             IMP COLUMN 2                                
         BAS   RE,SENDD                                                         
         LA    R4,4(R4)                                                         
         LA    R6,3(R6)                                                         
         BCT   R2,SND200                                                        
*                                                                               
         B     SNDB40               GET NEXT RECORD                             
*                                                                               
SNDBEX   B     EXIT                                                             
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
CALLTSAR LR    R0,RE                                                            
         GOTO1 VTSAR,TSARLOC                                                    
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
LCLHOLD  DS    CL50                                                             
*                                                                               
WSVRBLK  DS    XL(FAWSSVRL)        ** WSSVR CONTROL BLOCK                       
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
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENREAS                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAUTL                                                          
       PRINT ON                                                                 
       ++INCLUDE NAVDSECTS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073NENAV06   11/08/18'                                      
         END                                                                    
