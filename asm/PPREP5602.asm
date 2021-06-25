*          DATA SET PPREP5602  AT LEVEL 006 AS OF 02/04/15                      
*PHASE PP5602A                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'PP5602  SFM REPORT'                                             
*                                                                               
*************************   CHANGE LOG   ******************************         
*                                                                               
* BPLA 02/04/15 SUPPORT MEDIA FILTERING IN QOPT1 AND REPORT                     
*               MEDIA NAME AND VENDOR NAME                                      
*                                                                               
* SMYE 01/04/01 ADD DDS BILLING DOWNLOAD FIELDS                                 
*                                                                               
* KWAN 01/27/99 ADD MEDIA COUNTERS AND PAGYPROF CHAR COUNT TABLE                
*                                                                               
***********************************************************************         
*                                                                               
PP5602   CSECT                                                                  
         NMOD1 0,PP5602                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LR    R5,RC                                                            
         LA    R5,4095(R5)                                                      
         LA    R5,1(R5)                                                         
         USING PPFILED,RC,R5                                                    
*                                                                               
         BAS   RE,INITWK                                                        
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BNE   EXT                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   PPGKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVI   KEY+3,X'01'                                                      
         MVI   RCSUBPRG,1                                                       
AGYS     MVC   SAVEKEY,KEY                                                      
         LA    R4,DMRDHI                                                        
         BAS   RE,DIRRD                                                         
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    GETREPS                                                          
*                                                                               
         CLI   QOPT1,C' '          SEE IF FILTERING ON MEDIA                    
         BE    AGYS2                                                            
         CLC   KEY+2(1),QOPT1                                                   
         BE    AGYS2                                                            
         B     AGYX                                                             
*                                                                               
AGYS2    CLI   KEY+3,X'01'                                                      
         BNE   AGYX                                                             
         LA    R3,PAGYREC                                                       
         LA    R4,GETREC                                                        
         BAS   RE,FILERD                                                        
         IC    R7,LINE                                                          
         AH    R7,=H'4'            NEED 5 LINES                                 
         STC   R7,SAVELINE                                                      
         CLC   SAVELINE(1),MAXLINES                                             
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+1(3),KEY                                                       
         MVC   P+6(33),PAGYNAME                                                 
         MVC   P+42(7),PAGYABBR                                                 
         MVC   P+52(2),PAGYEQU                                                  
         MVC   P+70(7),=C'PROFILE'                                              
         MVC   P+79(30),PAGYPROF                                                
         MVI   P+116,C' '                                                       
         TM    PAGYSTAT,X'01'      SKIP IN DDS BILLING DOWNLOAD?                
         BNO   *+8                                                              
         MVI   P+116,C'Y'                                                       
         MVI   P+123,C' '                                                       
         TM    PAGYSTAT,X'02'      PRD LEVEL IN DDS BILLING DOWNLOAD?           
         BNO   *+8                                                              
         MVI   P+123,C'Y'                                                       
         BAS   R8,PRINTIT                                                       
         MVC   P+6(33),PAGYADDR                                                 
*                                                                   L01         
         MVC   P+50(12),=C'NATIONALITY='                            L01         
         MVC   P+62(1),PAGYNAT                                      L01         
         CLI   PAGYNAT,0            DEFAULT IS USA                  L01         
         BNE   *+10                                                 L01         
         MVC   P+62(3),=C'USA'                                      L01         
         CLI   PAGYNAT,C'C'         CHECK FOR CANADA                L01         
         BNE   *+10                                                 L01         
         MVC   P+62(3),=C'CAN'                                      L01         
*                                                                               
         MVC   P+70(09),=C'ACCOUNT #'                                           
         MVC   P+81(18),PAGYACCT                                                
         BAS   R8,PRINTIT                                                       
*                                                                               
         MVC   P+70(10),=C'MEDIA NAME'                                          
         MVC   P+82(10),PAGYMED                                                 
         BAS   R8,PRINTIT                                                       
         MVC   P+70(11),=C'VENDOR NAME'                                         
         MVC   P+82(20),PAGYVEN                                                 
         BAS   R8,PRINTIT                                                       
         BAS   R8,PRINTIT                                                       
*                                                                               
* DO PROFILE CHARACTERS TOTALS                                                  
*                                                                               
         L     R2,=A(PROFTAB)                                                   
         LA    R6,PAGYPROF                                                      
         SR    R7,R7               PROFILE CHAR POSITION COUNTER                
         LA    RE,30               PROFILE IS 30 CHAR LONG                      
         XC    FULL,FULL           USED FOR OTHER CHAR IN PROFILE               
*                                                                               
PTOT10   CLC   0(1,R2),0(R6)                                                    
         BNE   PTOT30                                                           
         LA    R2,1(R2)            BY PASS CHAR                                 
         LR    RF,R7               CALCULATE INDEX                              
         MHI   RF,2                                                             
         AR    R2,RF               ADD INDEX                                    
         ZICM  R8,0(R2),(3)                                                     
         LA    R8,1(R8)            INCREASE COUNTER                             
         STCM  R8,3,0(R2)                                                       
         LA    R6,1(R6)            NEXT CHAR IN PROFILE                         
         LA    R7,1(R7)            NEXT PROFILE CHAR POSITION                   
         L     R2,=A(PROFTAB)      POINT TO PROFILE COUNTERS TABLE              
         BCT   RE,PTOT10                                                        
         B     MTOT10              DO MEDIA TYPE TOTALS                         
*                                                                               
PTOT30   CLI   0(R2),C'Z'          LAST ENTRY IN TABLE?                         
         BE    PTOT50                                                           
         LA    R2,PTABLNQ(R2)      POINT TO NEXT ENTRY IN TABLE                 
         B     PTOT10                                                           
*                                                                               
PTOT50   L     R2,=A(PROFTAB)      CHAR IN PROFILE IN NOT IN TAB                
         L     RF,FULL                                                          
         CHI   RF,40               MAX IS 40                                    
         BH    PTOT10              NO ROOM IN OTHERPRF, IGNORE CODES            
         LA    R8,OTHERPRF                                                      
         AR    R8,RF                                                            
         MVC   0(1,R8),0(R6)       CHARS OTHER THAN THOSE IN PROF TAB           
         LA    RF,1(RF)                                                         
         ST    RF,FULL                                                          
*                                                                               
         LA    R6,1(R6)            NEXT CHAR IN PROFILE                         
         LA    R7,1(R7)            NEXT PROFILE CHAR POSITION                   
         CHI   RE,0                                                             
         BE    MTOT10                                                           
         BCTR  RE,0                                                             
         B     PTOT10                                                           
*                                                                               
* DO MEDIA TYPE TOTALS                                                          
*                                                                               
MTOT10   CLI   PAGYKMED,C'I'                                                    
         BNE   *+16                                                             
         AP    ICOUNT,=P'1'                                                     
         AP    TOTALC,=P'1'        TOTAL                                        
         CLI   PAGYKMED,C'M'                                                    
         BNE   *+16                                                             
         AP    MCOUNT,=P'1'                                                     
         AP    TOTALC,=P'1'        TOTAL                                        
         CLI   PAGYKMED,C'N'                                                    
         BNE   *+16                                                             
         AP    NCOUNT,=P'1'                                                     
         AP    TOTALC,=P'1'        TOTAL                                        
         CLI   PAGYKMED,C'O'                                                    
         BNE   *+16                                                             
         AP    OCOUNT,=P'1'                                                     
         AP    TOTALC,=P'1'        TOTAL                                        
         CLI   PAGYKMED,C'S'                                                    
         BNE   *+16                                                             
         AP    SCOUNT,=P'1'                                                     
         AP    TOTALC,=P'1'        TOTAL                                        
         CLI   PAGYKMED,C'T'                                                    
         BNE   *+16                                                             
         AP    TCOUNT,=P'1'                                                     
         AP    TOTALC,=P'1'        TOTAL                                        
         CLI   PAGYKMED,C'L'       SOCIAL                                       
         BNE   *+16                                                             
         AP    LCOUNT,=P'1'                                                     
         AP    TOTALC,=P'1'        TOTAL                                        
*                                                                               
*                                                                               
*                                                                               
AGYX     MVC   KEY+3(4),=4X'FF'                                                 
         B     AGYS                                                             
*                                                                               
*                                                                               
EXT      XMOD1 1                                                                
*                                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*        DATAMANAGER SUB ROUTINE                                                
*                                                                               
***********************************************************************         
*                                                                               
DIRRD    NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    ENDRD                                                            
DMER     MVC   P+1(28),=C'DISK ERROR ON PRINT FILE'                             
         GOTO1 REPORT                                                           
ENDRD    XIT                                                                    
*                                                                               
***********************************************************************         
*                                                                               
FILERD   NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,(R3),    X        
               (0,DMWORK)                                                       
         CLI   DMCB+8,0                                                         
         BNE   DMER                                                             
         B     ENDRD                                                            
*                                                                               
***********************************************************************         
*                                                                               
         EJECT                                                                  
GETREPS  DS    0H                  USED TO BE REPORTS AND ERRRO MSGS            
*                                                                               
         BAS   RE,MEDCOUNT         SUMMARY OF MEDIA COUNTERS                    
         BAS   RE,PROFCNT          SUMMARY OF PROFILE COUNTERS                  
*                                                                               
         MVC   KEY,PPGKEY                                                       
         LA    R4,DMRDHI                                                        
         BAS   RE,DIRRD                                                         
         LA    R3,PAGYREC                                                       
         LA    R4,GETREC                                                        
         BAS   RE,FILERD                                                        
         B     EXT                                                              
*                                                                               
*                                                                               
PRINTIT  GOTO1 REPORT                                                           
         BR    R8                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                                                                               
INITWK   NTR                                                                    
*                                                                               
         ZAP   ICOUNT,=P'0'        MEDIA COUNTERS                               
         ZAP   LCOUNT,=P'0'                                                     
         ZAP   MCOUNT,=P'0'                                                     
         ZAP   NCOUNT,=P'0'                                                     
         ZAP   OCOUNT,=P'0'                                                     
         ZAP   SCOUNT,=P'0'                                                     
         ZAP   TCOUNT,=P'0'                                                     
         ZAP   TOTALC,=P'0'        TOTAL                                        
*                                                                               
         L     R4,=A(PROFTAB)      POINT TO PROFILE COUNTS TABLE                
*                                                                               
         XC    0(PTABLNQ,R4),0(R4) FIRST ENTRY:  X'00' (NULL)                   
         LA    R4,PTABLNQ(R4)                                                   
         XC    0(PTABLNQ,R4),0(R4)                                              
         MVI   0(R4),X'40'         SECOND ENTRY: X'40' (SPACE)                  
         LA    R4,PTABLNQ(R4)                                                   
*                                                                               
         LA    R6,10               0,1,2,3,4,5,6,7,8,9                          
         LA    R7,240              240=X'F0' WHICH IS "0"                       
         BAS   R8,INITWK80                                                      
*                                                                               
         LA    R6,9                A,B,C,D,E,F,G,H,I                            
         LA    R7,193              193=X'C1' WHICH IS "A"                       
         BAS   R8,INITWK80                                                      
*                                                                               
         LA    R6,9                J,K,L,M,N,O,P,Q,R                            
         LA    R7,209              209=X'D1' WHICH IS "J"                       
         BAS   R8,INITWK80                                                      
*                                                                               
         LA    R6,8                S,T,U,V,W,X,Y,Z                              
         LA    R7,226              226=X'E2' WHICH IS "S"                       
         BAS   R8,INITWK80                                                      
*                                                                               
         XC    OTHERPRF,OTHERPRF   OTHER CHAR IN PROFILE                        
*                                                                               
         B     INITWKX             DONE INITIALIZE WORK AREAS                   
*                                                                               
INITWK80 XC    0(PTABLNQ,R4),0(R4)                                              
         STC   R7,0(R4)                                                         
         LA    R7,1(R7)            NEXT DIGIT OR LETTER                         
         LA    R4,PTABLNQ(R4)                                                   
         BCT   R6,INITWK80                                                      
         BR    R8                                                               
*                                                                               
INITWKX  XIT                                                                    
*                                                                               
***********************************************************************         
*                                                                               
MEDCOUNT NTR                                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2          MEDIA COUNTERS SUMMARY                       
         BAS   R8,PRINTIT          SKIP A LINE                                  
*                                                                               
         ZAP   DOUBLE,=P'0'        TOTAL                                        
*                                                                               
         LA    R7,ICOUNT           POINT TO ICOUNT                              
         MVI   P+1,C'I'                                                         
         BAS   R4,MEDCNT30                                                      
         LA    R7,LCOUNT           POINT TO ICOUNT                              
         MVI   P+1,C'L'                                                         
         BAS   R4,MEDCNT30                                                      
         LA    R7,MCOUNT           POINT TO MCOUNT                              
         MVI   P+1,C'M'                                                         
         BAS   R4,MEDCNT30                                                      
         LA    R7,NCOUNT           POINT TO NCOUNT                              
         MVI   P+1,C'N'                                                         
         BAS   R4,MEDCNT30                                                      
         LA    R7,OCOUNT           POINT TO OCOUNT                              
         MVI   P+1,C'O'                                                         
         BAS   R4,MEDCNT30                                                      
         LA    R7,SCOUNT           POINT TO SCOUNT                              
         MVI   P+1,C'S'                                                         
         BAS   R4,MEDCNT30                                                      
         LA    R7,TCOUNT           POINT TO TCOUNT                              
         MVI   P+1,C'T'                                                         
         BAS   R4,MEDCNT30                                                      
         B     MEDCNT40                                                         
*                                                                               
MEDCNT30 EDIT  (P4,0(R7)),(10,P+9),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK           
         BAS   R8,PRINTIT                                                       
         AP    DOUBLE,0(4,R7)                                                   
         BR    R4                                                               
*                                                                               
MEDCNT40 CP    DOUBLE,TOTALC                                                    
         BE    MEDCNT50                                                         
         BAS   R8,PRINTIT                                                       
         MVC   P(25),=C'ERR: TOTALS DO NOT ADD UP'                              
         EDIT  (P8,DOUBLE),(10,P+30),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK         
         BAS   R8,PRINTIT                                                       
*                                                                               
MEDCNT50 BAS   R8,PRINTIT                                                       
         MVC   P+1(6),=C'TOTAL:'                                                
         EDIT  (P8,TOTALC),(10,P+9),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK          
         BAS   R8,PRINTIT                                                       
*                                                                               
MEDCNTX  XIT                                                                    
*                                                                               
***********************************************************************         
*                                                                               
PROFCNT  NTR                                                                    
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,3          PROFILE ANALYSIS (COLUMN 01-15)              
         BAS   R8,PRINTIT          SKIP A LINE                                  
*                                                                               
         L     R4,=A(PROFTAB)      POINT TO PROFILE COUNTERS TABLE              
         LA    R6,38               X'00', X'40', 0-9, A-Z                       
*                                                                               
PCNT50   CLI   0(R4),X'00'         X'00' (NULL)?                                
         BNE   *+14                                                             
         MVC   P+1(3),=C'X00'                                                   
         B     PCNT55                                                           
         CLI   0(R4),X'40'         X'40' (SPACE)?                               
         BNE   *+14                                                             
         MVC   P+1(3),=C'X40'                                                   
         B     PCNT55                                                           
         MVC   P+1(1),0(R4)                                                     
PCNT55   LA    R4,1(R4)            POINT TO 1ST HALF OF PROF COUNTERS           
         LA    R3,15               DO FIRST SET OF 30 COUNTERS                  
         LA    R7,P+8                                                           
PCNT60   EDIT  (B2,0(R4)),(6,0(R7)),ALIGN=RIGHT,ZERO=NOBLANK,COMMAS=YES         
         LA    R7,8(R7)                                                         
         LA    R4,2(R4)            NEXT HALF WORD TOTAL                         
         BCT   R3,PCNT60                                                        
         BAS   R8,PRINTIT                                                       
         LA    R4,30(R4)           BY PASS 2ND HALF COUNTERS                    
         BCT   R6,PCNT50                                                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,4          PROFILE ANALYSIS (COLUMN 16-30)              
         BAS   R8,PRINTIT          SKIP A LINE                                  
*                                                                               
         L     R4,=A(PROFTAB)      POINT TO PROFILE COUNTERS TABLE              
         LA    R6,38               X'00', X'40', 0-9, A-Z                       
*                                                                               
PCNT70   CLI   0(R4),X'00'         X'00' (NULL)?                                
         BNE   *+14                                                             
         MVC   P+1(3),=C'X00'                                                   
         B     PCNT75                                                           
         CLI   0(R4),X'40'         X'40' (SPACE)?                               
         BNE   *+14                                                             
         MVC   P+1(3),=C'X40'                                                   
         B     PCNT75                                                           
         MVC   P+1(1),0(R4)                                                     
PCNT75   LA    R4,31(R4)           POINT TO 2ND HALF PROF COUNTERS              
         LA    R3,15               DO SECOND SET OF 30 COUNTERS                 
         LA    R7,P+8                                                           
PCNT80   EDIT  (B2,0(R4)),(6,0(R7)),ALIGN=RIGHT,ZERO=NOBLANK,COMMAS=YES         
         LA    R7,8(R7)                                                         
         LA    R4,2(R4)            NEXT HALF WORD TOTAL                         
         BCT   R3,PCNT80                                                        
         BAS   R8,PRINTIT                                                       
         BCT   R6,PCNT70                                                        
*                                                                               
         OC    OTHERPRF,OTHERPRF   OTHER CHARS IN PROFILE?                      
         BZ    PCNTX                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,5          OTHER CHARACTERS USED IN PROFILE             
         BAS   R8,PRINTIT          SKIP A LINE                                  
*                                                                               
         LA    R6,P                                                             
         LA    R7,OTHERPRF                                                      
         SR    R4,R4               COUNTING CHARS IN OTHERPRF                   
*                                                                               
PCNT90   CLI   0(R7),X'00'                                                      
         BE    PCNT100             END OF OTHERPRF                              
         CHI   R4,20                                                            
         BH    PCNT100             END OF OTHERPRF (40 CHAR MAX)                
*                                                                               
         GOTO1 HEXOUT,DMCB,0(R7),0(R6),1                                        
*                                                                               
         LA    R6,3(R6)            HEX VERSION AND A SPACE (3 CHARS)            
         LA    R7,1(R7)            NEXT CHAR IN OTHERPRF                        
         LA    R4,1(R4)            COUNTING NUMBER OF CHAR PROCESSED            
         B     PCNT90                                                           
*                                                                               
PCNT100  BAS   R8,PRINTIT          TIME TO PRINT IT                             
*                                                                               
PCNTX    XIT                                                                    
*                                                                               
***********************************************************************         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                                                                               
SAVELINE DS    CL32                                                             
PPGKEY   DS    CL32                                                             
SAVEKEY  DS    CL32                                                             
*                                                                               
*                                                                               
ICOUNT   DS    PL4                 MEDIA COUNTERS                               
LCOUNT   DS    PL4                 MEDIA COUNTERS                               
MCOUNT   DS    PL4                                                              
NCOUNT   DS    PL4                                                              
OCOUNT   DS    PL4                                                              
SCOUNT   DS    PL4                                                              
TCOUNT   DS    PL4                                                              
TOTALC   DS    PL8                 TOTAL                                        
*                                                                               
*                                                                               
OTHERPRF DS    CL40                OTHER CHARS USED IN PROFILE                  
*                                                                               
*                                                                               
PROFTAB  CSECT                                                                  
         DS    38CL(PTABLNQ)       X'00', X'40', DIGIT 0-9, LETTER A-Z          
*                                                                               
PTABLNQ  EQU   61                  1 CHAR AND 30 HALF WORD COUNTS               
*                                                                               
         DC    X'00'                                                            
*                                                                               
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPREP5602 02/04/15'                                      
         END                                                                    
