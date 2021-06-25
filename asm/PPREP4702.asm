*          DATA SET PPREP4702  AT LEVEL 029 AS OF 04/22/03                      
*PHASE PP4702C                                                                  
*                                                                               
         TITLE 'PP4702 - PRINTPAK COMMENT LISTING'                              
*                                                                               
PP4702   CSECT                                                                  
         NMOD1 0,PP4702                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LR    R9,RC                                                            
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP47WRK,R8                                                       
         LA    R0,5                                                             
         LA    R1,SPACEND                                                       
         XC    0(250,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
         EJECT                                                                  
         CLI   MODE,PROCREQ                                                     
         BNE   FINI                                                             
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
         MVI   KEY+3,X'40'                                                      
         MVI   ONESW,0                                                          
         CLC   QUESTOR,=CL12'AUTOREQ'         SEE IF TURNAROUND                 
         BNE   PRC                                                              
         MVI   ONESW,1                                                          
         CLC   QPAY(6),SPACES                                                   
         BE    *+10                                                             
         MVC   KEY+4(6),QPAY                                                    
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
*                                  READ HIGH                                    
PRC10    LA    R4,DMRDHI                                                        
         BAS   RE,READ                                                          
         B     PR02                                                             
*                                  READ SEQ                                     
PRCS     LA    R4,DMRSEQ                                                        
         BAS   RE,READ                                                          
*                                  COMPARE KEYS   UNEQ = END                    
PR02     CLC   KEY(3),KEYWORK                                                   
         BNE   PROX                                                             
         CLI   KEY+3,X'40'         COMMENT RECORD                               
         BNE   PROX                                                             
         CLI   ONESW,1             SEE IF TURNAROUND                            
         BNE   PRO2A                                                            
         CLC   KEY+4(6),QPAY                                                    
         BNE   PROX                                                             
         B     PRO2X               SKIP FILTER TEST                             
*                                                                               
PRO2A    DS    0H                                                               
         CLC   QPAY(6),SPACES                                                   
         BE    PRO2X               NO FILTERS                                   
         LA    R5,5                LEFT ALIGN KEY+4 IN WORK                     
         LA    R6,KEY+4                                                         
         XC    WORK(6),WORK                                                     
*                                                                               
PRO2B    CLI   0(R6),C' '                                                       
         BNE   PRO2D              NON SPACE ENCOUNTERED                         
         LA    R6,1(R6)                                                         
         BCTR  R5,0                                                             
         B     PRO2B                                                            
*                                                                               
PRO2D    EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R6)                                                    
         LA    R6,QPAY                                                          
         LA    R4,WORK                                                          
         LA    R5,6                                                             
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
PRO2X    DS    0H                                                               
*                                  CLEAR REC AREA / GET REC                     
         BAS   R6,CLR                                                           
         LA    R4,GETREC                                                        
         LA    R3,REC                                                           
         BAS   RE,GET                                                           
*                                  COUNT COMMENT LINES  - SO THAT ALL           
*                                       CAN BE PRINTED ON ONE PAGE              
         SR    R2,R2                                                            
         LA    R3,33(R3)                                                        
         CLI   0(R3),X'40'                                                      
         BNE   PR030                                                            
         B     PR03                                                             
         MVI   ELCODE,X'40'                                                     
         BAS   R6,NEXTEL                                                        
         BNE   PR030                                                            
PR03     BCT   R2,*-12                                                          
PR030    LPR   R2,R2                                                            
         SR    RE,RE                                                            
         IC    RE,LINE                                                          
         AR    RE,R2                                                            
         STC   RE,BYTE                                                          
         CLC   BYTE(1),MAXLINES                                                 
         BNH   *+8                                                              
         MVI   LINE,99                                                          
         LA    R3,REC+33                                                        
*                                  PRINT COMMENTS                               
         MVC   P+1(6),KEY+4                                                     
         CLI   0(R3),X'40'                                                      
         BNE   FINI                                                             
PR04     SR    R6,R6                                                            
         IC    R6,1(R3)                                                         
         SH    R6,=H'3'                                                         
         CLI   2(R3),C'+'                                                       
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
*                                                                               
*                                  PRINT COMMENT WITH SPACING                   
PR07     CLI   3(R3),C'1'                                                       
         BL    PR05                                                             
         CLI   3(R3),C'3'                                                       
         BH    PR05                                                             
         MVC   FULL(1),3(R3)                                                    
         PACK  DUB,FULL(1)                                                      
         CVB   R5,DUB                                                           
PR070    BAS   RE,PRNTIT                                                        
         BCT   R5,PR070                                                         
PR071    SH    R6,=H'2'                                                         
         BM    PR06A                                                            
         EX    R6,SPCEMV                                                        
         B     PR06                                                             
*                                                                               
*                                                                               
PR08     MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     PRCS                                                             
         SPACE 3                                                                
PROX     MVC   KEY,KEYWORK                                                      
         B     FINI                RESTORE PPG KEY                              
*                                                                               
MOVE     MVC   P+16(0),2(R3)                                                    
SPCEMV   MVC   P+16(0),4(R3)                                                    
         SPACE 3                                                                
FINI     XMOD1 1                                                                
         EJECT                                                                  
*                                  READ COMMENT RECS                            
READ     NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    RDEX                                                             
         MVC   P+1(17),=C'DISK ERROR - READ'                                    
         GOTO1 REPORT                                                           
RDEX     XIT                                                                    
         SPACE 2                                                                
*                                  GET COMMENT RECS                             
GET      NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',                X        
               KEY+27,(R3),(0,DMWORK)                                           
         CLI   DMCB+8,0                                                         
         BE    GTEX                                                             
         MVC   P+1(16),=C'DISK ERROR - GET'                                     
         GOTO1 REPORT                                                           
GTEX     XIT                                                                    
*                                  CLEAR REC AREA                               
CLR      LA    R3,REC                                                           
         LA    R4,4                                                             
         XC    0(250,R3),0(R3)                                                  
         LA    R3,250(R3)                                                       
         BCT   R4,*-10                                                          
         BR    R6                                                               
*                                  GET NEXT ELEMENT                             
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
PRNTIT   NTR1                                                                   
         MVC   HEAD5(40),FLTLINE                                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
PP47WRK  DSECT                                                                  
KEYWORK  DS    CL32                                                             
FLTLINE  DS    CL40                                                             
ELCODE   DS    CL1                                                              
ONESW    DS    CL1                                                              
REC      DS    CL1000                                                           
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPWORKD                                                        
       ++INCLUDE PPNEWFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029PPREP4702 04/22/03'                                      
         END                                                                    
