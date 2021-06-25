*          DATA SET SPREPFXCBX AT LEVEL 049 AS OF 01/30/98                      
*PHASE SPFX02C                                                                  
SPFX02C  TITLE 'SPFX02A - CABLE CONVERSTION - READ SJ CABLE FILE'               
         SPACE 1                                                                
SPFX02   CSECT                                                                  
         DS    6000C                                                            
         ORG   SPFX02                                                           
         SPACE 1                                                                
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02,RB,RC                                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX2                                                              
*                                                                               
YES      CR    RB,RB               SET CC EQUAL                                 
         B     XIT                                                              
*                                                                               
NO       LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*=========================================================*                     
* REQFRST PROCESSING                                      *                     
*=========================================================*                     
         SPACE 1                                                                
FX2      DS    0H                                                               
*                                                                               
         LA    R1,CABLETAB                                                      
FX3      XC    3(2,R1),3(R1)       CLEAR COUNTER                                
         LA    R1,L'CABLETAB(R1)                                                
         CLI   0(R1),X'FF'                                                      
         BNE   FX3                                                              
*                                                                               
         CLI   QAREA+49,C'0'       TEST ANY MAX COUNT                           
         BL    FX4                                                              
         PACK  MAXCOUNT,QAREA+49(6)                                             
*                                                                               
FX4      OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(6),=C'ST0000'                                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,ADSTAT                   
         L     R6,ADSTAT                                                        
         CLC   0(2,R6),=C'ST'                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         B     FX22                                                             
*                                                                               
FX20     GOTO1 DATAMGR,DMCB,=C'DMRSEQ'                                          
         L     R8,ADSTAT                                                        
         CLC   0(2,R8),=C'ST'                                                   
         BNE   FX100                                                            
*                                                                               
         USING STARECD,R8                                                       
*                                                                               
FX22     DS    0H                                                               
         CLC   STAKAGY,QAGY                                                     
         BNE   FX20                                                             
         CLC   STAKCLT,=C'000'     CLIENT RECORDS IGNORED                       
         BNE   FX20                                                             
*                                                                               
         CLC   STAKCALL,=C'7000'   BL                                           
         BL    *+14                                                             
         CLC   STAKCALL,=C'7500'                                                
         BNH   FX20                SKIP HEADENDS 7000-7500                      
*                                                                               
         AP    MYCOUNT,=P'1'                                                    
         MVC   P(5),STAKCALL                                                    
         MVC   P+5(24),SSYSNAME                                                 
         OC    SSYSNETS(16),SSYSNETS                                            
         BNZ   FX30                                                             
         MVC   P,SPACES                                                         
         B     FX20                                                             
*                                                                               
FX30     XC    SCBL24(77),SCBL24        CLEAR NEW FIELDS                        
         MVC   STAKLEN(2),=Y(SCBLSQNQ)  SET NEW RECORD LENGTH                   
         L     RE,ADSTAT                                                        
         SR    RF,RF                                                            
         ICM   RF,3,15(RE)                                                      
         LA    RF,4(RF)            SET LEN FOR PUT                              
         SLL   RF,16                                                            
         SH    RE,=H'4'                                                         
         ST    RF,0(RE)                                                         
*                                                                               
         LA    R1,SSYSNETS                                                      
         LA    R4,P+129                                                         
         ST    R4,SAVEP129                                                      
         SH    R4,=H'98'           START PRINTING AT P+31                       
         LA    R6,CABLETAB         POINT TO DECODE TABLE                        
*                                                                               
FX32     LA    R5,X'80'            SET TM FLAG                                  
         LA    R0,8                SET LOOP COUNT                               
*                                                                               
FX34     EX    R5,*+8                                                           
         B     *+8                                                              
         TM    0(R1),0                                                          
         BZ    FX38                                                             
         C     R4,SAVEP129         TEST PAST END OF PRINT LINE                  
         BNH   FX36                                                             
         L     R4,SAVEP129                                                      
         LA    R4,132(R4)                                                       
         ST    R4,SAVEP129                                                      
         SH    R4,=H'98'           START AT PN+31                               
*                                                                               
FX36     MVC   0(3,R4),0(R6)       MOVE NETWORK TO PRINT                        
         ICM   RE,3,3(R6)          BUMP COUNTER                                 
         LA    RE,1(RE)                                                         
         STCM  RE,3,3(R6)                                                       
         LA    R4,4(R4)                                                         
*                                                                               
FX38     LA    R6,L'CABLETAB(R6)                                                
         SRL   R5,1                                                             
         BCT   R0,FX34                                                          
*                                                                               
         LA    R1,1(R1)                                                         
         LA    R0,SSYSNETS+16                                                   
         CR    R1,R0                                                            
         BL    FX32                                                             
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         BAS   RE,GENEL                                                         
         LA    R1,FILEOUT                                                       
         L     R0,ADSTAT                                                        
         SH    R0,=H'4'                                                         
         PUT   (1),(0)                                                          
*                                                                               
         MVI   LINE,0              FORCE NO PAGE BREAKS                         
         CP    MYCOUNT,MAXCOUNT                                                 
         BH    FX100                                                            
         B     FX20                                                             
*                                                                               
         EJECT                                                                  
FX100    MVC   P(14),=C'RECORD COUNT ='                                         
         OI    MYCOUNT+7,X'0F'                                                  
         UNPK  P+15(6),MYCOUNT                                                  
         GOTO1 REPORT                                                           
* PRINT NETWORK COUNTERS                                                        
         MVI   FORCEHED,C'Y'                                                    
         LA    R1,CABLETAB                                                      
*                                                                               
FX110    MVC   P(3),0(R1)                                                       
         SR    RE,RE                                                            
         ICM   RE,3,3(R1)                                                       
         CH    RE,=H'100'                                                       
         BL    FX112                                                            
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+4(5),DUB                                                       
         GOTO1 REPORT                                                           
FX112    LA    R1,L'CABLETAB(R1)                                                
         CLI   0(R1),X'FF'                                                      
         BNE   FX110                                                            
*                                                                               
         CLOSE FILEOUT                                                          
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
* GENERATE NEW STATION MASTER FIELDS                                            
* FIRST DO TOP 24     INTO SCBL24                                               
* THEN DO NON TOP 24  INTO SCBLSEQ                                              
         SPACE 1                                                                
GENEL    NTR1                                                                   
         LA    R1,SSYSNETS                                                      
         LA    R6,CABLETAB         POINT TO DECODE TABLE                        
         L     R7,=X'01000000'     SET INITIAL VALUE                            
*                                                                               
         L     R4,ADBUY                                                         
         LA    R0,4                                                             
         XC    0(256,R4),0(R4)                                                  
         LA    R4,256(R4)                                                       
         BCT   R0,*-10                                                          
*                                                                               
GEN2     LA    R5,X'80'            SET TM FLAG                                  
         LA    R0,8                SET LOOP COUNT                               
*                                                                               
GEN4     TM    5(R6),X'01'         TEST TOP 24                                  
         BZ    GEN8                                                             
         SRL   R7,1                POSITION TO NEXT BIT                         
*                                                                               
         EX    R5,*+8              TEST NETWORK ACTIVE                          
         B     *+8                                                              
         TM    0(R1),0                                                          
         BZ    GEN8                                                             
*                                                                               
         ST    R7,FULL             SAVE CURRENT BIT                             
         OC    SCBL24,FULL+1       'OR' INTO PREVIOUS                           
*                                                                               
GEN8     LA    R6,L'CABLETAB(R6)                                                
         SRL   R5,1                                                             
         BCT   R0,GEN4                                                          
*                                                                               
         LA    R1,1(R1)                                                         
         LA    R0,SSYSNETS+15                                                   
         CR    R1,R0                                                            
         BL    GEN2                                                             
         EJECT                                                                  
* GENERATE NON-TOP24 FIELD                                                      
*                                                                               
         LA    R1,SSYSNETS                                                      
         LA    R6,CABLETAB         POINT TO DECODE TABLE                        
         LA    R7,SCBLSEQ                                                       
*                                                                               
GEN22    LA    R5,X'80'            SET TM FLAG                                  
         LA    R0,8                SET LOOP COUNT                               
*                                                                               
GEN24    TM    5(R6),X'01'         TEST TOP 24                                  
         BO    GEN28                                                            
         EX    R5,*+8                                                           
         B     *+8                                                              
         TM    0(R1),0                                                          
         BZ    GEN28                                                            
*                                                                               
         LA    RE,CABLETAB         COMPUTE                                      
         SR    RE,R6                                                            
         LPR   RE,RE                                                            
         SRDL  RE,32                                                            
         D     RE,=F'6'                                                         
         LTR   RE,RE                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         STC   RF,0(R7)            SET CABLENUM IN FIELD                        
         LA    R7,1(R7)                                                         
*                                                                               
GEN28    LA    R6,L'CABLETAB(R6)                                                
         SRL   R5,1                                                             
         BCT   R0,GEN24                                                         
*                                                                               
         LA    R1,1(R1)                                                         
         LA    R0,SSYSNETS+15                                                   
         CR    R1,R0                                                            
         BL    GEN22                                                            
GENX     XIT1                                                                   
*                                                                               
MYCOUNT  DC    PL4'0'                                                           
MAXCOUNT DC    PL4'9999999'                                                     
SEQNUM   DC    PL4'0'                                                           
SAVEP129 DS    A                                                                
         LTORG                                                                  
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,LRECL=512,BLKSIZE=8000,X        
               MACRF=PM                                                         
         EJECT                                                                  
       ++INCLUDE SPCBLLST                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049SPREPFXCBX01/30/98'                                      
         END                                                                    
