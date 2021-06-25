*          DATA SET SPREPRZ02  AT LEVEL 091 AS OF 05/01/02                      
*PHASE SPRZ02A                                                                  
SPRZ02   TITLE 'MARKET-STATION-PRODUCT LIST'                                    
*                                                                               
SPRZ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPRZ02                                                         
***      ST    R3,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,ESTFRST                                                     
         BE    RZ00                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RZEX                                                             
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   ALLSW,C'N'                                                       
         CLC   QEST,=C'ALL'                                                     
         BNE   EXIT                                                             
         MVC   QEST(3),=C'NO '                                                  
         MVI   ALLSW,C'Y'                                                       
*                                                                               
EXIT     XMOD1                                                                  
XIT      XIT1                                                                   
*                                                                               
* RELO     DC    A(0)                                                           
*                                                                               
RZ00     DS    0H                                                               
         LA    R7,LIST                                                          
         SR    R5,R5                                                            
         LA    R3,KEY                                                           
         USING BUYRECD,R3                                                       
*                                                                               
* INITIALIZE BUYREC KEY                                                         
         XC    KEY,KEY                                                          
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(3),BAGYMD   BRING AGENCY/MEDIA/CLIENT                    
         MVI   SAVEKEY+3,1         PRODUCT NO.                                  
         MVC   KEY,SAVEKEY                                                      
*                                                                               
         CLC   QMKT(4),=C'ALL '                                                 
         BE    RZ10                DO FOR ALL MARKETS                           
         PACK  DUB(8),QMKT(4)      CONVERT MARKET NUMBER TO DECIMAL             
         CVB   R4,DUB                                                           
         OR    R4,R4                                                            
         BZ    *+12                DO FOR PARTICULAR MARKET                     
         STCM  R4,3,BMKT                                                        
         B     ONLYMKT                                                          
*                                                                               
* DO FOR ALL MARKETS                                                            
RZ10     GOTO1 HIGH                                                             
         CLI   KEY+3,X'FF'         IF PROD=X'FF' OR !A/M/CLT                    
         BE    RZEREQ               THEN STOP                                   
         CLC   KEY(3),SAVEKEY                                                   
         BNE   RZEREQ                                                           
* VERIFY THE DATA AS NEEDED                                                     
         BAS   RE,VLEST            SET ACTIVE OR INACTIVE                       
         TM    FLAG,ACTIV                                                       
         BZ    RZ10                IF INACTIVE, READ NEXT RECORD                
         BAS   RE,MVDT00                                                        
         MVC   BUYKEST(LACTIVE),XFF       FORCE NEXT STATION                    
         B     RZ10                                                             
*                                                                               
* DO FOR PARTICULAR MARKET                                                      
ONLYMKT  DS    0H                                                               
RZ30     GOTO1 HIGH                                                             
         CLI   KEY+3,X'FF'                                                      
         BE    RZEREQ                                                           
         CLC   KEY(3),SAVEKEY                                                   
         BNE   RZEREQ                                                           
         BAS   RE,VLEST                                                         
         TM    FLAG,ACTIV                                                       
         BZ    RZ40                IF INACTIVE, READ NEXT RECORD                
*                                                                               
RZ32     DS    0H                                                               
         CLC   BUYMSTA(2),BMKT                                                  
         BNE   *+8                                                              
         BAS   RE,MVDT00                                                        
         MVC   BUYKEST(LACTIVE),XFF                                             
         B     RZ40                                                             
*                                                                               
         CLC   BUYMSTA(2),BMKT     IF (BUYKMKT > MKT)                           
         BL    RZ35                  THEN INCREMENT PRODUCT                     
         ZIC   RE,BUYKPRD                 FOR NEXT READHI                       
         LA    RE,1(RE)                                                         
         STCM  RE,1,BUYKPRD                                                     
         XC    KEY+4(9),KEY+4      CLEAR REST OF KEY                            
         B     RZ40                                                             
*                                                                               
RZ35     MVC   BUYMSTA(2),BMKT     IF (BUYMKT < MKT) THEN OVERWRITE             
*                                     BUYKMKT FOR NEXT READHI                   
         XC    KEY+6(7),KEY+6      CLEAR REST OF KEY                            
RZ40     DS    0H                                                               
         B     RZ30                                                             
*                                                                               
* MOVE DATA TO THE LIST                                                         
MVDT00   DS    0H                                                               
         TM    FLAG,SPILL          IF SPILL, DISREGARD                          
         BNZ   MVDTXIT                                                          
*                                                                               
         LA    R5,1(R5)            INCREMENT COUNTER OF RECORD                  
         MVC   0(5,R7),BUYMSTA     MKT/STATION                                  
         LA    R7,5(R7)                                                         
         L     R6,ADCLT            FIND ALPHA CODE OF PRD                       
         USING CLTHDR,R6                                                        
         LA    R0,220              LENGTH OF CLIST                              
         LA    RF,CLIST                                                         
         CLC   BUYKPRD,3(RF)                                                    
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         BCT   R0,*-14                                                          
         MVC   0(3,R7),0(RF)       PRODUCT CODE                                 
         LA    R7,3(R7)                                                         
MVDTXIT  BR    RE                                                               
         EJECT                                                                  
*                                                                               
* CHECK THE STATE OF ESTIMATE - ACTIVE OR INACTIVE                              
VLEST    DS    0H                                                               
         NTR1                                                                   
         XC    FLAG,FLAG                                                        
         CLI   KEY+10,X'80'        IF SPILL, SET THE FLAG                       
         BNE   *+8                                                              
         OI    FLAG,SPILL                                                       
*                                                                               
         CLI   BEST,0                                                           
         BNE   VLEST02                                                          
         LA    RF,ESTLST           LOOK UP ESTLST                               
         SR    RE,RE                                                            
         ICM   RE,1,KEY+9                                                       
         AR    RF,RE                                                            
         CLI   0(RF),0                                                          
         BZ    VLEST01             NOT IN THE ESTLIST                           
VLEST00  MVC   BUYKEST(LACTIVE),XFF  ESTIMATE ACTIVE                            
         B     VLXITEQ                                                          
VLEST01  MVC   BUYKEST+1(LINACT),XFF  ESTIMATE INACTIVE                         
         B     VLXITNEQ                                                         
VLEST02  DS    0H                                                               
         CLI   BESTEND,0           IF BESTEND=0                                 
         BNZ   VLEST03               THEN CHECK IF BEST=EST                     
         CLC   BUYKEST,BEST                                                     
         BE    VLEST00                                                          
         B     VLEST01                                                          
VLEST03  CLC   BUYKEST,BEST        CHECK FOR (BEST <= EST <= BESTEND)           
         BL    VLEST01                                                          
         CLC   BUYKEST,BESTEND                                                  
         BH    VLEST01                                                          
         B     VLEST00                                                          
*                                                                               
VLXITEQ  OI    FLAG,ACTIV                                                       
VLXITNEQ B     XIT                                                              
         DROP  R3                                                               
*                                                                               
RZEREQ   DS    0H                                                               
         LA    RE,LIST             COMPUTE TOTAL DATA LNG. IN THE LIST          
         SR    R7,RE                                                            
         ST    R7,LENDATA                                                       
*                                                                               
         LA    RE,8                RECORD LENGTH                                
         L     R3,LENDATA          LENGTH OF THE TABLE                          
         SR    R2,R2                                                            
         DR    R2,RE                                                            
         OR    R2,R2               TEST REMAINDER                               
         BZ    *+8                                                              
         LA    R3,1(R3)            NUMBER OF ENTRIES                            
*                                                                               
         LA    R7,LIST                                                          
         GOTO1 XSORT,DMCB,(X'00',(R7)),(R3),8,8,0                               
*                                                                               
* PUT DATAS FROM THE LIST TO SEQUENTIAL FILE                                    
         LA    R2,SEQFIL                                                        
         OPEN  ((2),OUTPUT)                                                     
         BAS   RE,PUTSEQ                                                        
         CLOSE ((2))                                                            
         GOTO1 AENDREQ                                                          
         B     EXIT                                                             
*                                                                               
RZEX     B     EXIT                                                             
*                                                                               
SEQFIL   DCB   DDNAME=SEQFIL,DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=80,    +        
               MACRF=PM                                                         
*                                                                               
PUTSEQ   NTR1                                                                   
         CLI   FIRST,1                                                          
         BNE   PUTSEQ2                                                          
         MVI   FIRST,0                                                          
         MVI   P,C' '                                                           
         MVC   P+1(159),P                                                       
         MVC   P(2),=C'RS'                                                      
         CLC   QAREA2+20(2),=C'  '                                              
         BE    *+10                                                             
         MVC   P(2),QAREA2+20  REP FROM SECOND REQ. AREA                        
         LA    R1,SEQFIL                                                        
         PUT   (1),P                                                            
*                                                                               
PUTSEQ2  LA    R7,LIST                                                          
PUTLP    DS    0H                                                               
         GOTO1 MSUNPK,DMCB,(R7),QMKT,QSTA                                       
         CLI   ALLSW,C'Y'                                                       
         BNE   *+10                                                             
         MVC   QEST(3),=C'ALL'                                                  
         MVC   QPRD,5(R7)                                                       
         MVC   QAREA(2),=C'RS'     OVERWRITE PHASE ID                           
         CLC   QAREA2+20(2),=C'  '                                              
         BE    *+10                                                             
         MVC   QAREA(2),QAREA2+20  REP FROM SECOND REQ. AREA                    
         MVI   QCONTREQ,C' '                                                    
*----->  MVC   P(80),QAREA         TESTING                                      
*        GOTO1 REPORT                                                           
         LA    R7,8(R7)                                                         
*                                                                               
         LA    R1,SEQFIL                                                        
         PUT   (1),QAREA                                                        
*                                                                               
         BCT   R3,PUTLP                                                         
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
ALLSW    DC    C'N'                                                             
FLAG     DC    XL1'00'                                                          
ACTIV    EQU   X'01'                                                            
SPILL    EQU   X'02'                                                            
FIRST    DC    X'01'                                                            
SAVEKEY  DS    CL13                                                             
LENDATA  DS    F                                                                
CNTREC   DC    XL1'00'                                                          
XFF      DC    4X'FF'                                                           
LINACT   EQU   3                                                                
LACTIVE  EQU   4                                                                
LIST     DS    960000X             MARKET-STATION-PRODUCT LIST                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPWORKD                                                     
