*          DATA SET NENAV07A   AT LEVEL 059 AS OF 05/01/02                      
*PHASE T31807A                                                                  
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31807   TITLE 'NENAV07 - STEWARD/MATCHMAKER - BUY UPLOAD OVERLAY'              
T31807   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV07**                                                       
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         USING TSARD,TSARBLK                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVC   TSAREC,ANETBLK                                                   
*                                                                               
* DID WE GO TO THE BUY ALREADY                                                  
*                                                                               
*SVGLOBSW MEANINGS  NULL=FIRST PASS GET FIRST TSAR RECORD CALL NBUY             
*                   C'B'=SEND DATA TO THE PC                                    
*                   C'E'=NO MARE TSAR RECORDS EXIT                              
*                                                                               
         CLI   SVGLOBSW,C'B'                                                    
         BE    ROUT040                                                          
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
         MVI   TSACTN,TSARES        RESTORE TSAR                                
         BAS   RE,CALLTSR2                                                      
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
         MVI   TSACTN,TSASAV        SAVE TSAR                                   
         BAS   RE,CALLTSAR                                                      
*                                                                               
*  TEST THE GLOBBER CALLS                                                       
*                                                                               
         L     R3,ANETBLK                                                       
         USING BUYUPLDD,R3                                                      
         MVC   SVTSRKEY,0(R3)                                                   
         MVC   RUPDEMS,QDEMOS                                                   
*                                                                               
         XC    DMCB(24),DMCB                                                    
         L     RA,ATWA                                                          
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         MVI   DMCB+8,1            PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=X'03E8'  WRITE 1000 BYTES                            
         MVC   WORK(4),DMCB+8       SAVE FOR GLOBBER CALL                       
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R3),0                      
         CLI   8(R1),0             BLOW ON ANY ERROR HERE                       
         BE    *+6                                                              
         DC    H'0'                                                             
* PASS PAGE AND MONITOR NUMBER THROUGH GLOBBER                                  
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,4,GLVBUY1                            
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'NET'                                                 
         MVC   GLVXFRPR,=C'NNA'                                                 
         MVC   GLVXTOSY,=C'NET'                                                 
         MVC   GLVXTOPR,=C'NBU'                                                 
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,24,GLVXCTL                           
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
CBX      B     EXIT                                                             
*                                                                               
GLBERR   DC    H'0'                                                             
         DROP  R3,R1                                                            
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
         LA    R4,DRFSLN            SUB LINE NUMBER                             
         LHI   R1,X'0E'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SND120   LA    R4,DRFHOMSH          HOMES SHARE                                 
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
         LA    R6,QDEMOS                                                        
         LA    R4,DRFDEMS                                                       
         LA    R2,25                                                            
*                                                                               
SND150   CLI   1(R6),0                                                          
         BZ    SNDEX                                                            
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
         BCT   R2,SND150                                                        
*                                                                               
SNDEX    B     EXIT                                                             
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
CALLTSAR LR    R0,RE                                                            
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR TEST                                        *             
*=================================================================*             
         SPACE 1                                                                
CALLTSR2 LR    R0,RE                                                            
         PRINT GEN                                                              
         GOTO1 VTSAR,TSARBLK                                                    
         PRINT NOGEN                                                            
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
       ++INCLUDE DDTSARD                                                        
       PRINT ON                                                                 
       ++INCLUDE NAVDSECTS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059NENAV07A  05/01/02'                                      
         END                                                                    
