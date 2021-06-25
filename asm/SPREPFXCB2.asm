*          DATA SET SPREPFXCB2 AT LEVEL 024 AS OF 05/11/98                      
*PHASE SPFX025                                                                  
         TITLE 'SPFX02 - FIX CABLE NETWORKS ON SNV RECS'                        
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
DMXIT    XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* REQFRST                                                                       
REQF     DS    0H                                                               
         XC    COUNT,COUNT                                                      
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R0,=F'2000000'                                                   
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         STCM  R1,15,TABADDR                                                    
         LR    R3,R1                                                            
         SR    R5,R5                                                            
*                                                                               
DMXIN10  GET   FILEIN,TEMP                                                      
         ZICM  R4,TABADDR,4                                                     
         GOTO1 BINSRCH,DMCB,(X'01',TEMP),(R4),(R5),80,(0,5),11000               
         ZICM  R1,DMCB,4                                                        
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R5,1(R5)            BUMP COUNT                                   
         B     DMXIN10                                                          
*                                                                               
DMXIN20  CLOSE FILEIN                                                           
         STCM  R5,15,TABCOUNT                                                   
*                                                                               
SNV      XC    INVKEY,INVKEY                                                    
         MVC   INVKEY(2),=X'0E03'                                               
         MVC   KEYSAVE,INVKEY                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',INVKEY,INVKEY,0               
         B     SNV05                                                            
SNVSEQ   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',INVKEY,INVKEY,0               
SNV05    CLC   INVKEY(2),KEYSAVE                                                
         BH    SNVX                                                             
*                                                                               
SNV10    DS    0H                                                               
         LA    R3,INVKEY                                                        
         USING SNVKEY,R3                                                        
         MVC   INVDA,SNVDDA                                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',INVDA,ADBUY,DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ADBUY                                                         
         MVI   ELCODE,X'F1'                                                     
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         BNE   SNV12                                                            
         CLC   =X'62050B',2(R6)    ADDED MONDAY                                 
         BE    SNVSEQ              THEN SKIP                                    
*                                                                               
SNV12    L     R3,ADBUY                                                         
         TM    5(R3),X'F0'                                                      
         BNO   SNVSEQ                                                           
*  POINT IT TO TWO BEFORE STATION                                               
         LA    R2,3(R3)                                                         
         MVC   BAGYMED,2(R3)                                                    
         MVI   ELCODE,X'40'                                                     
         LR    R6,R3                                                            
         BAS   RE,GETEL            SKIP FIRST ELEM                              
         BNE   SNVSEQ                                                           
SNV20    BAS   RE,NEXTEL                                                        
         BNE   SNV30                                                            
         USING SNVIDELD,R6                                                      
         MVC   NETNUM,SNVIDNWK                                                  
         MVC   MYNET,SNVIDNWK                                                   
         ST    R6,NETADDR                                                       
         GOTO1 GETSTA                                                           
         MVC   SNVIDNWK,MYNET                                                   
         B     SNV20                                                            
*                                                                               
SNV30    DS    0H                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    SNV32                                                            
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFIL',INVDA,ADBUY,DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
SNV32    B     SNVSEQ                                                           
*        GOTO1 HEXOUT,DMCB,0(R3),P+2,13,=C'TOG'                                 
*        MVC   P+27(18),=C'**CHANGED ELEM ***'                                  
*        GOTO1 HEXOUT,DMCB,0(R6),P+47,35,=C'TOG'                                
*        GOTO1 REPORT                                                           
*        B     SNV20                                                            
*                                                                               
SNVX     GOTO1 AENDREQ                                                          
         DROP  R3,R6                                                            
*                                                                               
*==================================================================             
*                                                                               
GETSTA   NTR1                                                                   
* BUILD ARGS AND CALL BINSRCH                                                   
         IC    R7,NETNUM            GET NETWORK BYTE                            
         N     R7,=X'0000007F'      BYTE MARKET/STATION                         
         STC   R7,NETNUM                                                        
*                                                                               
         OC    NETNUM,NETNUM                                                    
         BZ    EXIT                                                             
*   THIS PART JUMPS INTO CABLE TABLE ENTRY                                      
         LA    R4,CABLETAB                                                      
         SR    R5,R5                                                            
         SR    R8,R8                                                            
         SR    R6,R6                                                            
         LA    R8,L'CABLETAB                                                    
         MR    R6,R8               PRODUCT IN 6 AND 7                           
         SR    R7,R8                                                            
*                                                                               
         AR    R7,R4               R7 POINTS TO CABLE ENTRY IN TABLE            
         CLC   3(1,R7),NETNUM                                                   
         BE    FNDTAB                                                           
*                                                                               
******* DID NOT FIND NETWORK NUMBER IN CABLE TABLE ***********                  
*                                                                               
BADNET   DS    0H                                                               
         GOTO1 HEXOUT,DMCB,0(R3),P+2,60,=C'TOG'                                 
         GOTO1 REPORT                                                           
         MVC   P(42),=C'DID NOT FIND NETWORK IN CABTABLE -NUMBER :'             
         GOTO1 HEXOUT,DMCB,NETNUM,P+44,1,=C'TOG'                                
         GOTO1 REPORT                                                           
         CR    RE,RF                                                            
         B     DMXIT                                                            
*                                                                               
*   NOW TEST IF TOP 24 STATION ?                                                
FNDTAB   TM    5(R7),X'40'                                                      
         BZ    NON24                                                            
         B     TOP24                                                            
*                                                                               
*================================================================*              
* TOP 24 HAS '40'BIT TURNED ON, SO SUBTRACT '40' FROM 5(TABLEENTRY)             
* TO GET THE RANKING AMONG THE TOP 24 NETWORKS                                  
* EXAMPLE .. 5(AE NETWORK) = '41',  41 - '40'= 1                                
*         .. 5(ESP NETWORK)= '47',  47 - '40'= 7                                
*                                                                               
TOP24    ZIC   RE,5(R7)    RE NOW HAS 5(NETWORK TABLE ENTRY)                    
         N     RE,=X'000000BF'                                                  
         STC   RE,BYTE                                                          
*        GOTO1 HEXOUT,DMCB,0(R3),P+2,13,=C'TOG'                                 
*        MVC   P+29(16),=C'***   TOP 24 ***'                                    
*        ZICM  R6,NETADDR,4                                                     
*        GOTO1 HEXOUT,DMCB,0(R6),P+47,35,=C'TOG' ELEMENT                        
*        GOTO1 REPORT                                                           
*                                                                               
         NI    MYNET,X'80'   TAKE OFF LAST 7 BITS OF NET FIELD                  
         OC    MYNET,BYTE                                                       
         CR    RE,RE                                                            
         XIT1                                                                   
         EJECT                                                                  
*================================================================*              
* NON TOP 24 REQUIRES THE SEQUENCES IN SCBLSEQ IN INPUT TABLE    *              
* R3==> BUYREC                                                                  
NON24    SR    R4,R4                                                            
         ICM   R4,7,2(R2)                                                       
         N     R4,=X'000FFFFF'                                                  
         SRL   R4,7                                                             
         XC    KEY,KEY             BUILD THE SEARCH KEY                         
         ZICM  R6,NETADDR,4                                                     
*                                                                               
         EDIT  (R4),(4,KEY),FILL=0                                              
         MVC   KEY+4(L'BAGYMED),BAGYMED                                         
         ZICM  R4,TABADDR,4                                                     
         ZICM  R8,TABCOUNT,4                                                    
         GOTO1 BINSRCH,DMCB,(X'00',KEY),(R4),(R8),80,(0,5),8000                 
         ZICM  R1,DMCB,4                                                        
         CLI   DMCB,X'01'                                                       
         BNE   GOTIT                                                            
*                                                                               
* DID NOT FIND THE RECORD IN THE INPUT FILE   *******                           
* ===== EX. SJ0021 =================================*                           
*                                                                               
         GOTO1 HEXOUT,DMCB,0(R3),P+2,24,=C'TOG'                                 
         GOTO1 HEXOUT,DMCB,0(R6),P+49,35,=C'TOG'                                
         GOTO1 REPORT                                                           
         MVC   P(32),=C'DID NOT FIND NETWORK IN FILE  >>'                       
         MVC   P+35(4),KEY                                                      
         GOTO1 HEXOUT,DMCB,KEY+4,P+41,1,=C'TOG'                                 
         GOTO1 REPORT                                                           
         CR    RE,RF                                                            
         B     DMXIT                                                            
*                                                                               
* RECORD MUST EXIST IF VALID SO NOW SEARCH SEQUENCE NUMBERS                     
GOTIT    LA    R4,64      64 BYTES IN SEQUENCE                                  
         LA    R8,7(R1)   FIRST SEQUENCE NUMBER                                 
         LA    R5,1                                                             
         ZIC   RE,NETNUM                                                        
*                                                                               
NON10    CLC   NETNUM,0(R8)    4B                                               
         BE    FOUND                                                            
         LA    R5,1(R5)                                                         
         LA    R8,1(R8)                                                         
         BCT   R4,NON10                                                         
*                                                                               
*   DID NOT FIND THE NETWORK NUMBER IN THE SEQ 64 BYTE FIELD IN FILE *          
*   IF DID NOT FIND SEQ NUM JUST PRINT OUT                                      
*                                                                               
         GOTO1 HEXOUT,DMCB,0(R3),P+2,24,=C'TOG'                                 
         GOTO1 HEXOUT,DMCB,0(R6),P+49,35,=C'TOG'                                
         GOTO1 REPORT                                                           
         MVC   P(24),=C'DID NOT FIND IN SEQ64 >>'                               
         MVC   P+25(4),KEY                                                      
         GOTO1 HEXOUT,DMCB,NETNUM,P+33,1,=C'TOG'                                
         GOTO1 REPORT                                                           
         CR    RE,RF                                                            
         B     DMXIT                                                            
*                                                                               
FOUND    AH    R5,=H'24'                     ADD 24 TO ANY NON24 NET            
         STC   R5,BYTE                                                          
*                                                                               
*        GOTO1 HEXOUT,DMCB,0(R3),P+2,13,=C'TOG'                                 
*        MVC   P+27(11),=C'** N24 ****'                                         
*        MVC   P+38(4),KEY                                                      
*        MVC   P+42(3),=C'***'                                                  
*        GOTO1 HEXOUT,DMCB,0(R6),P+47,35,=C'TOG'                                
*        GOTO1 REPORT                                                           
*                                                                               
         NI    MYNET,X'80'   TAKE OFF LAST 7 BITS OF NET FIELD                  
         OC    MYNET,BYTE                                                       
         CR    RE,RE                                                            
GETSTAX  XIT1                                                                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
REQL     MVC   P(17),=C'NUMBER OF RECORDS'                                      
         EDIT  COUNT,(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         GETEL R6,42,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
FILEIN   DCB   DDNAME=TEMPIN,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      X        
               EODAD=DMXIN20                                                    
*                                                                               
NETNUM   DS    X                                                                
TESTREC  DC    XL22'E20000010000F00A850000000000000000000000E2D1'               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
ELCODE   DS    X                                                                
COUNT    DS    F                                                                
TEMP     DS    CL80                                                             
TABADDR  DS    A                                                                
TABCOUNT DS    F            MARKT/STAT                                          
BAGYMED  DS    CL1                                                              
NETADDR  DS    A                                                                
MYNET    DS    X                                                                
INVDA    DS    XL4                                                              
INVKEY   DS    CL50                                                             
INVKEYSV DS    CL50                                                             
*                                                                               
       ++INCLUDE SPCBLLST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSNV                                                       
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PIND2    DS    CL1                                                              
         DS    CL6                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SPREPFXCB205/11/98'                                      
         END                                                                    
