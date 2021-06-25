*          DATA SET PPREPNT02  AT LEVEL 035 AS OF 04/23/03                      
*PHASE PPNT02A                                                                  
*                                                                               
         TITLE 'PPNT02 - PRINTPAK NVTEXT'                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 04/19/01 QOPT1=I FOR I/O COM AND QOPT1=C FOR CONTRACT COM                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPNT02   CSECT                                                                  
         NMOD1 0,PPNT02                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LR    R9,RC                                                            
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPNTWRK,R8                                                       
*                                                                               
         LA    RE,SPACEND                                                       
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BNE   FINI                                                             
*                                                                               
         MVI   RCSUBPRG,0          DEFUALT IS NVTEXT LISTING HEADER             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    P(132),P                                                         
         MVC   PAGE,=H'1'                                                       
         MVC   FLTLINE,SPACES                                                   
*                                  SET UP KEY FOR READ                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVC   KEYWORK,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(3),KEYWORK                                                   
         MVI   KEY+3,X'41'                                                      
*                                                                               
         CLI   QOPT1,C'I'          I/O COMMENT?                                 
         BNE   *+12                                                             
         MVI   KEY+3,X'42'         I/O COMMENT RECORD CODE                      
         MVI   RCSUBPRG,10         I/OCOM LISTING HEADER                        
         CLI   QOPT1,C'C'          CONTRACT COMMENT?                            
         BNE   *+12                                                             
         MVI   KEY+3,X'44'         CONTRACT COMMENT RECORD CODE                 
         MVI   RCSUBPRG,20         CONCOM LISTING HEADER                        
*                                                                               
         MVI   ONESW,0                                                          
         CLC   QUESTOR,=CL12'AUTOREQ'            TURNAROUND?                    
         BNE   PRC                                                              
         MVI   ONESW,1                                                          
         CLC   QPAY(3),SPACES                                                   
         BE    *+10                                                             
         MVC   KEY+4(3),QPAY                                                    
         B     PRC10                                                            
*                                                                               
PRC      CLI   QPAY,C' '           FILTER CAN NO START WITH SPACE               
         BE    PRC10                                                            
         MVC   FLTLINE(6),=C'FILTER'                                            
         LA    R4,QPAY                                                          
         LA    R5,FLTLINE+8                                                     
         ZAP   DUB,=P'1'                                                        
*                                                                               
PRC2     CLI   0(R4),C' '                                                       
         BE    PRC4                                                             
         CLI   0(R4),C'*'                                                       
         BE    PRC4                                                             
         OI    DUB+7,X'0F'                                                      
         UNPK  0(1,R5),DUB                                                      
         MVI   1(R5),C'='                                                       
         TM    0(R4),X'40'         TEST NEGATIVE                                
         BO    *+12                                                             
         MVI   2(R5),C'-'                                                       
         LA    R5,1(R5)                                                         
         MVC   2(1,R5),0(R4)                                                    
         OI    2(R5),C' '                                                       
         MVI   3(R5),C','                                                       
         LA    R5,4(R5)                                                         
*                                                                               
PRC4     LA    R4,1(R4)                                                         
         AP    DUB,=P'1'                                                        
         CP    DUB,=P'6'                                                        
         BNH   PRC2                                                             
         BCTR  R5,0                                                             
         MVI   0(R5),C' '          BLANK LAST COMMA                             
*                                                                               
PRC10    LA    R4,DMRDHI           READ HIGH                                    
         BAS   RE,READ                                                          
         B     PR02                                                             
*                                  READ SEQ                                     
PRCS     LA    R4,DMRSEQ                                                        
         BAS   RE,READ                                                          
*                                  COMPARE KEYS   UNEQ = END                    
PR02     CLC   KEY(3),KEYWORK                                                   
         BNE   PROX                                                             
*                                                                               
         CLI   QOPT1,C'I'          I/O COMMENT?                                 
         BNE   PRO2AB                                                           
         CLI   KEY+3,X'42'                                                      
         BNE   PROX                                                             
         B     PRO2AD                                                           
*                                                                               
PRO2AB   CLI   QOPT1,C'C'          CONTRACT COMMENT?                            
         BNE   PRO2AC                                                           
         CLI   KEY+3,X'44'                                                      
         BNE   PROX                                                             
         B     PRO2AD                                                           
*                                                                               
PRO2AC   CLI   KEY+3,X'41'         SEE IF NV TEXT RECORD                        
         BNE   PROX                                                             
*                                                                               
PRO2AD   CLI   ONESW,1             SEE IF TURNAROUND                            
         BNE   PRO2A                                                            
         CLC   KEY+4(3),QPAY       CNG 12/18/87  4(6) TO 4(3)                   
         BNE   PROX                                                             
         B     PRO2X               SKIP FILTER TEST                             
*                                                                               
PRO2A    DS    0H                                                               
         CLC   QPAY(3),SPACES      CNG 12/18/87  (6) TO (3)                     
         BE    PRO2X               NO FILTERS                                   
         LA    R5,2                LEFT ALIGN KEY+4 IN WORK                     
         LA    R6,KEY+4                                                         
         XC    WORK(6),WORK                                                     
*                                                                               
PRO2B    CLI   0(R6),C' '                                                       
         BNE   PRO2D               NON SPACE ENCOUNTERED                        
         LA    R6,1(R6)                                                         
         BCTR  R5,0                                                             
         B     PRO2B                                                            
*                                                                               
PRO2D    EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R6)                                                    
         LA    R6,QPAY                                                          
         LA    R4,WORK                                                          
         LA    R5,3                CNG 12/17/87   R5,6 TO R5,3                  
*                                                                               
PRO2F    CLI   0(R6),C'*'          ANY CHAR IS OK                               
         BE    PRO2J                                                            
         CLI   0(R6),C' '                                                       
         BE    PRO2J                                                            
         TM    0(R6),X'40'         TEST NEGATIVE FILTER                         
         BZ    PRO2H                                                            
         CLC   0(1,R4),0(R6)                                                    
         BE    PRO2J                                                            
         B     PRCS                BYPASS                                       
*                                                                               
PRO2H    MVC   DUB(1),0(R6)                                                     
         OI    DUB,X'40'                                                        
         CLC   DUB(1),0(R4)                                                     
         BE    PRCS                BYPASS                                       
*                                                                               
PRO2J    LA    R6,1(R6)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,PRO2F                                                         
*                                                                               
PRO2X    DS    0H                  CLEAR REC AREA / GET REC                     
         BAS   R6,CLR                                                           
         LA    R4,GETREC                                                        
         LA    R3,REC                                                           
         BAS   RE,GET                                                           
*                                                                               
* COUNT COMMENT LINES  - SO THAT ALL CAN BE PRINTED ON ONE PAGE                 
*                                                                               
         SR    R2,R2                                                            
         LA    R3,33(R3)                                                        
         CLI   0(R3),X'40'                                                      
         BNE   PR030                                                            
         B     PR03                                                             
         MVI   ELCODE,X'40'                                                     
         BAS   R6,NEXTEL                                                        
         BNE   PR030                                                            
PR03     BCT   R2,*-12             COUNTING NUMBER OF COMMENT ELEMS             
PR030    LPR   R2,R2               GET POSITIVE COUNT                           
         SR    RE,RE                                                            
         IC    RE,LINE                                                          
         AR    RE,R2                                                            
         STC   RE,BYTE                                                          
         CLC   BYTE(1),MAXLINES    ENOUGH TO PRINT THIS COMMENT?                
         BNH   *+8                                                              
         MVI   LINE,99                                                          
         LA    R3,REC+33                                                        
*                                                                               
* PRINT COMMENTS                                                                
*                                                                               
         CLC   KEY+4(3),=X'FFFFFF'                                              
         BNE   *+14                                                             
         MVC   P+1(11),=C'ALL CLIENTS'                                          
         B     PR030A                                                           
         CLI   KEY+4,X'FF'                                                      
         BNE   *+20                                                             
         MVC   P+1(9),=C'OFFICE = '                                             
         MVC   P+10(1),KEY+5                                                    
         B     PR030A                                                           
         MVC   P+1(3),KEY+4        CLIENT CODE = X'XXXXXX'                      
*                                                                               
PR030A   CLI   0(R3),X'40'         NO MORE COMMENT ELEMS?                       
         BNE   FINI                                                             
PR04     SR    R6,R6                                                            
         IC    R6,1(R3)            ELEM LENGTH                                  
         AHI   R6,-3               2 FROM ELEM OVERHEAD AND 1 FOR EX            
*                                                                               
         CLI   2(R3),C'+'          ANY SPACING OPTIONS?                         
         BE    PR07                                                             
PR05     DS    0H                                                               
         EX    R6,MOVE                                                          
PR06     BAS   RE,PRNTIT                                                        
*                                  NEXT ELEMENT                                 
PR06A    DS    0H                                                               
         MVI   ELCODE,X'40'                                                     
         BAS   R6,NEXTEL                                                        
         BNE   PR08                                                             
         B     PR04                                                             
*                                  PRINT COMMENT WITH SPACING                   
PR07     CLI   3(R3),C'1'          SKIP 1 LINE?                                 
         BL    PR05                                                             
         CLI   3(R3),C'3'          SKIP 3 LINES?                                
         BH    PR05                HIGHER IS NOT RIGHT, BUT CONTINUE            
         MVC   FULL(1),3(R3)                                                    
         PACK  DUB,FULL(1)                                                      
         CVB   R5,DUB                                                           
PR070    BAS   RE,PRNTIT           PRINTING BLANK LINES                         
         BCT   R5,PR070                                                         
PR071    AHI   R6,-2               TWO FOR +1, +2 OR +3                         
         BM    PR06A               BRANCH ON MINUS                              
         EX    R6,SPCEMV                                                        
         B     PR06                                                             
*                                                                               
PR08     MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     PRCS                                                             
*                                                                               
PROX     MVC   KEY,KEYWORK                                                      
         B     FINI                RESTORE PPG KEY                              
*                                                                               
MOVE     MVC   P+16(0),2(R3)       AFTER ELEM CODE AND LENGTH                   
SPCEMV   MVC   P+16(0),4(R3)       AFTER ELEM CODE, LENGTH, +1/+2/+3            
*                                                                               
FINI     XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
READ     NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    RDEX                                                             
         MVC   P+1(17),=C'DISK ERROR - READ'                                    
         GOTO1 REPORT                                                           
RDEX     XIT                                                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GET      NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',                X        
               KEY+27,(R3),(0,DMWORK)                                           
         CLI   DMCB+8,0                                                         
         BE    GTEX                                                             
         MVC   P+1(16),=C'DISK ERROR - GET'                                     
         GOTO1 REPORT                                                           
GTEX     XIT                                                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLR      LA    RE,REC              CLEAR REC AREA                               
         LA    RF,1500                                                          
         XCEF                                                                   
         BR    R6                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLC   ELCODE,0(R3)                                                     
         BCR   8,R6                                                             
         CLI   0(R3),0                                                          
         BNE   NEXTEL                                                           
         LTR   R6,R6                                                            
         BR    R6                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DS    0F                                                               
PRNTIT   NTR1                                                                   
         MVC   HEAD5(40),FLTLINE                                                
         GOTO1 REPORT                                                           
PRNTITX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPNTWRK  DSECT                                                                  
*                                                                               
KEYWORK  DS    CL32                                                             
FLTLINE  DS    CL40                                                             
ELCODE   DS    CL1                                                              
ONESW    DS    CL1                                                              
REC      DS    CL1500                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPWORKD                                                        
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035PPREPNT02 04/23/03'                                      
         END                                                                    
