*          DATA SET SPSFM24    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T21724A                                                                  
         TITLE 'T21724  -XRATE- EXCHANGE RATES RECORDS'                         
T21724   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21724                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       LA    R6,SVKEY                                                         
         USING XRTKEY,R6                                                        
         XC    SVKEY,SVKEY                                                      
         MVI   XRTKTYP,XRTKTYPQ                                                 
         MVI   XRTKSUB,XRTKSUBQ                                                 
         MVC   XRTKAGY,AGENCY                                                   
*                                                                               
         LA    R2,RATYERH          YEAR                                         
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK2                                                              
         GOTO1 ANY                                                              
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    EINV                                                             
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,RATYER                                                       
         CVB   RE,DUB                                                           
         STC   RE,XRTKYR                                                        
*                                                                               
VK2      MVC   KEY(13),SVKEY                                                    
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING XRTRECD,R6                                                       
         ZIC   RE,XRTKYR                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LRTYER,DUB                                                       
         OI    LRTYERH+6,X'80'                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   ELCODE,MXRELCDQ     REMOVE ALL ELEMENTS                          
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM           BUILD NEW ELEMENTS                           
         LA    R6,ELEM                                                          
         USING MXREL,R6                                                         
         MVI   MXRELCD,MXRELCDQ    BUILD MONTHLY EXCHANGE RATES ELEM            
         MVI   MXRELLN,MXRELLNQ                                                 
         LA    R2,RATJANH          R2=A(FIELD HEADER)                           
         LA    R3,MXRMNTHS         R3=A(MONTHLY RATE IN ELEMENT)                
         LA    R5,12               12 MONTHS                                    
*                                                                               
VR2      SR    R7,R7               R7=L'FIELD                                   
         ICM   R7,1,5(R2)                                                       
         BZ    EMIS                                                             
         MVI   BYTE,0                                                           
         MVC   WORK(5),=C'00000'                                                
         LA    R1,8(R2)                                                         
*                                                                               
VR3      CLI   0(R1),C'.'          TEST DECIMAL POIINT                          
         BNE   VR4                                                              
         CLI   BYTE,0              YES-TEST ALREADY ENCOUNTERED                 
         BNE   EINV                                                             
         MVI   BYTE,1                                                           
         LA    RE,WORK+1                                                        
         LA    RF,5                UP TO 4 DECIMAL PLACES                       
         B     VR8                                                              
*                                                                               
VR4      CLI   0(R1),C'0'          TEST NUMERIC                                 
         BL    EINV                                                             
         CLI   0(R1),C'9'                                                       
         BH    EINV                                                             
         CLI   BYTE,0              TEST BEFORE DEIMAL POINT                     
         BNE   VR6                                                              
         CLI   WORK,C'0'           YES-ONLY 1 NON-ZERO DIGIT BEFORE             
         BNE   EINV                    DECIMAL POINT                            
         MVC   WORK(1),0(R1)                                                    
         B     VR8                                                              
*                                                                               
VR6      MVC   0(1,RE),0(R1)                                                    
         LA    RE,1(RE)                                                         
         BCT   RF,VR8              UP TO 4 DECIMAL PLACES                       
         B     EINV                                                             
*                                                                               
VR8      LA    R1,1(R1)            NEXT CHARACTER                               
         BCT   R7,VR3                                                           
*                                                                               
         PACK  DUB,WORK(5)                                                      
         CVB   RE,DUB                                                           
         SRDL  RE,16                                                            
         LTR   RE,RE               TEST VALUE NOT TOO BIG                       
         BNZ   EINV                                                             
         STCM  RF,12,0(R3)         STORE RATE IN ELEMENT                        
         LA    R3,2(R3)                                                         
         BCT   R5,*+8              NEXT MONTH                                   
         B     VR10                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   VR2                                                              
         DC    H'0'                                                             
*                                                                               
VR10     GOTO1 ADDELEM                                                          
*                                                                               
VRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R2,RATJANH          CLEAR SCREEN                                 
         BAS   RE,CLRSCRN                                                       
         L     R6,AIO                                                           
         USING XRTRECD,R6                                                       
         SR    R0,R0                                                            
         LA    R6,XRTFSTEL         FIND EXCHANGE RATES ELEMENT                  
*                                                                               
DR2      CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),MXRELCDQ                                                   
         BE    DR4                                                              
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DR2                                                              
*                                                                               
         USING MXREL,R6                                                         
DR4      LA    R2,RATJANH          FORMAT THE MONTHLY RATES                     
         LA    R3,12                                                            
         LA    R4,MXRMNTHS                                                      
*                                                                               
DR6      EDIT  (2,0(R4)),(6,8(R2)),4,FILL=0                                     
         OI    6(R2),X'80'                                                      
         BCT   R3,*+8              NEXT MONTH                                   
         B     DRX                                                              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         LA    R4,2(R4)                                                         
         B     DR6                                                              
*                                                                               
DRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R2,LRTSELH                                                       
         BAS   RE,CLRSCRN          CLEAR THE SCREEN                             
         LA    R6,WORK                                                          
         XC    WORK,WORK                                                        
         USING XRTRECD,R6                                                       
         MVC   XRTKTYP,XRTKTYPQ                                                 
         MVC   XRTKSUB,XRTKSUBQ                                                 
         MVC   XRTKAGY,AGENCY                                                   
         CLC   XRTKEY(XRTKYR-XRTKEY),KEY   TEST FIRST TIME                      
         BE    LR2                                                              
         MVC   KEY(13),SVKEY                                                    
*                                                                               
LR2      GOTO1 HIGH                                                             
         B     LR6                                                              
*                                                                               
LR4      GOTO1 SEQ                                                              
*                                                                               
LR6      CLC   KEY(XRTKYR-XRTKEY),KEYSAVE                                       
         BNE   LRX                                                              
         CLI   MODE,LISTRECS                                                    
         BNE   LR20                                                             
         MVC   LISTAR,SPACES                                                    
         LA    R6,KEY                                                           
         ZIC   RE,XRTKYR                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LISTAR(2),DUB                                                    
         MVC   DMDSKADD,KEY+14     SET DISK ADDRESS FOR LISTMON                 
         GOTO1 LISTMON                                                          
         B     LR4                                                              
*                                                                               
LR20     CLI   MODE,PRINTREP                                                    
         BE    LR4                                                              
         DC    H'0'                                                             
*                                                                               
LRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* ROUTINE TO CLEAR THE SCREEN                                                   
* FROM FIELD AT R2                                                              
*                                                                               
CLRSCRN  NTR1                                                                   
         SR    RE,RE                                                            
*                                                                               
CS2      IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    CS4                                                              
         EX    RE,CSCLC                                                         
         BE    CS4                                                              
         EX    RE,CSOC                                                          
         BZ    CS4                                                              
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS4      LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS2                                                              
         B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
* ERROR EXITS                                                                   
*                                                                               
EMIS     MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMFBD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMEBD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENXRT                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPSFM24   05/01/02'                                      
         END                                                                    
