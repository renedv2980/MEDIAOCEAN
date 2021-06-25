*          DATA SET GEKWX08    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TF2008A,+0                                                               
         TITLE '$KWX MK3 - COPY AND OVERCOPY ACTIONS'                           
         PRINT NOGEN                                                            
KWX08    CSECT                                                                  
         NMOD1 0,**KWX08*,R9                                                    
         L     RC,0(R1)                                                         
         USING KWX08+4096,R9       R9 = 2ND BASE                                
         USING TWAD,RA             RA = TWA                                     
         USING GWS,RC              RC = GWS                                     
*                                                                               
T001     ST    RB,ABASE2                                                        
         ST    R9,A2NDBAS2                                                      
         ST    RD,AREGSAV2                                                      
         B     T010                                                             
         EJECT                                                                  
*              FURTHER PARAMETER CHECKS ON PRE-PROCESSED PARAMETERS             
*                                                                               
T010     CLI   FXID,0              NAME REQUIRED FOR OVERCOPY                   
         BNE   T020                                                             
         CLI   ACTION,OVC                                                       
         BNE   T030                                                             
         MVI   FERN,MISSING                                                     
         MVI   FNDX,2                                                           
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
*                                                                               
T020     MVC   WORK(2),TWAUSRID    NAME MUST BE THIS USER/INITS                 
         MVI   WORK+2,C'K'                                                      
         MVC   WORK+3(3),LASTINIT                                               
         CLI   SAVMODE,FORMAT                                                   
         BE    *+10                                                             
         MVC   WORK+2(4),LASTINIT                                               
         CLC   ID(6),WORK                                                       
         BE    T030                                                             
         MVI   FERN,INVNOWNR                                                    
         MVC   FNDX,FXID                                                        
         B     ERROR                                                            
         EJECT                                                                  
*              CHECK WHETHER BOOK EXISTS                                        
*                                                                               
T030     LA    R2,FRMBOOK          R2 = A(SOURCE USER INDEX)                    
         MVI   BYTE,MESSAGE        BYTE = THE OTHER MODE                        
         CLI   SAVMODE,FORMAT                                                   
         BE    *+12                                                             
         LA    R2,MSGBOOK                                                       
         MVI   BYTE,FORMAT                                                      
         MVC   WORK(20),DMCB       SAVE SOURCE PARM SET                         
         CLC   ID,0(R2)                                                         
         BNE   T035                                                             
         MVI   FERN,COPYSELF       DESTINATION=SOURCE NAME                      
         MVC   FNDX,FXID                                                        
         B     ERROR                                                            
*                                                                               
T035     GOTO1 ,PARAS,(BYTE,ID)    IF NO ID GIVEN FIND NEXT AVAILABLE           
         L     RF,AFINDBK                                                       
         CLI   FXID,0                                                           
         BNE   T040                                                             
         BASR  RE,RF                                                            
         BZ    ERROR                                                            
         B     T125                ID NOW CONTAINS NEXT AVAILABLE               
*                                                                               
T040     BASR  RE,RF               IF ID GIVEN DOES BOOK EXIST                  
         TM    DMCB+8,X'6F'                                                     
         BNZ   ERROR                                                            
         TM    DMCB+8,X'10'                                                     
         BNO   T045                                                             
         CLI   ACTION,OVC          IT MUST IF OVERCOPY                          
         BNE   T125                                                             
         MVI   FERN,BOOKNXST                                                    
         MVC   FNDX,FXID                                                        
         B     ERROR                                                            
*                                                                               
T045     CLI   ACTION,COP          AND MUST NOT IF COPY                         
         BNE   T125                                                             
         MVI   FERN,BOOKXIST                                                    
         MVC   FNDX,FXID                                                        
         B     ERROR                                                            
         EJECT                                                                  
*              SET UP PARMS FOR COPY SEQUENCE                                   
*                                                                               
T125     MVC   DMCB(20),WORK       RANDOM READ FOR ZERO ON SOURCE               
         L     R4,DMCB+12                                                       
         XC    0(4,R4),0(R4)                                                    
         LA    R0,=C'RAN'                                                       
         ST    R0,DMCB                                                          
         GOTO1 ADATAMGR,DMCB                                                    
         CLI   DMCB+8,0                                                         
         BNE   DMERROR                                                          
*                                                                               
T130     MVC   PARAS(20),DMCB      USE COMMON REC AREA BUT THE OTHER            
         LA    R3,FRMNDEX          USER INDEX & BUFFER                          
         L     R5,ATIA                                                          
         CLI   SAVMODE,MESSAGE                                                  
         BE    *+12                                                             
         LA    R3,MSGNDEX                                                       
         L     R5,ABUFFER                                                       
         STM   R3,R5,PARAS+8                                                    
         USING UKRECD,R3                                                        
         XC    UKINDEX,UKINDEX                                                  
         MVC   UKKEY,ID                                                         
         MVI   UKFLAG,X'1A'        SAME COMMENTS/RETENTION/LIBRARY TYPE         
         CLC   UKKEY,0(R2)         IF OLD=NEW SAME ATTRIBUTES IN TOTO           
         BNE   *+14                                                             
         MVC   UKFLAG,UKFLAG-UKRECD(R2)                                         
         OI    UKFLAG,1            AND DUPS OK                                  
*                                                                               
T135     LA    R0,=C'OPE'          OPEN DEST BOOK                               
         ST    R0,PARAS                                                         
         GOTO1 ADATAMGR,PARAS                                                   
         CLI   PARAS+8,0                                                        
         BNE   DMERROR                                                          
T140     LA    R0,=C'REA'          COPY HEADER REC WITH ACCESS SETTINGS         
         ST    R0,DMCB             INITIALIZED UNLESS SOURCE=DEST               
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BNE   DMERROR                                                          
         USING HDRD,R4                                                          
         CLC   UKKEY,0(R2)                                                      
         BE    *+8                                                              
         MVI   HDACCS,X'FF'                                                     
         DROP  R4                                                               
         LA    R0,=C'ADD'                                                       
         ST    R0,PARAS                                                         
         LA    R1,PARAS                                                         
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BNE   DMERROR                                                          
*                                                                               
T145     LA    R1,DMCB             COPY REMAINING RECORDS                       
         BASR  RE,RF                                                            
         TM    8(R1),X'80'                                                      
         BO    T150                                                             
         CLI   8(R1),0                                                          
         BNE   DMERROR                                                          
         LA    R1,PARAS                                                         
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    T145                                                             
         B     DMERROR                                                          
*                                                                               
T150     LA    R0,=C'CLO'          CLOSE DEST FILE                              
         ST    R0,PARAS                                                         
         LA    R1,PARAS                                                         
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BNE   DMERROR                                                          
*                                                                               
T160     MVC   KWXHEAD(36),=C'BOOK COPY IS NNN - ENTER NEXT ACTION'             
         UNPK  KWXHEAD+13(1),UKKEY+7(1)                                         
         OI    KWXHEAD+13,X'F0'                                                 
         ZIC   R0,UKKEY+6                                                       
         SRDL  R0,4                                                             
         SLL   R0,4                                                             
         SLDL  R0,4                                                             
         STH   R0,HALF                                                          
         OC    HALF,=C'00'                                                      
         MVC   KWXHEAD+14(2),HALF                                               
         CLC   UKKEY,0(R2)                                                      
         BNE   OKXIT                                                            
         SPACE 3                                                                
*                                  DESTINATION=SOURCE NOT ALLOWED FOR           
*                                  NOW - BLOWS UP IF USED IN FORMAT             
*                                  MODE                                         
         SPACE 3                                                                
         LA    R0,=C'PUR'          PURGE SOURCE IF KEY IS SAME                  
         ST    R0,DMCB                                                          
         L     R3,DMCB+8                                                        
         MVC   UKFILNO,=H'1'                                                    
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BNE   DMERROR                                                          
*                                                                               
T165     LA    R2,SAVF             AND JIGGLE FOR BUFFER SAVE                   
         LA    R3,SAVM                                                          
         CLI   SAVMODE,FORMAT                                                   
         BE    *+12                                                             
         LA    R2,SAVM                                                          
         LA    R3,SAVF                                                          
         MVC   WORK(L'SAVF),0(R3)                                               
         OC    BUFSTAT,BYTE                                                     
         GOTO1 ACLOSE,PARAS,(BYTE,0)                                            
         BZ    ERROR                                                            
         MVC   0(L'SAVF,R2),0(R3)                                               
         MVC   0(L'SAVF,R3),WORK                                                
         B     OKXIT                                                            
         EJECT                                                                  
*              EXITS BACK TO ROOT                                               
*                                                                               
ERROR    SR    R0,R0               CC = EQU FOR ERROR                           
         B     EXIT                                                             
*                                                                               
MOREXIT  LNR   RB,RB               CC = NEG FOR MORE INPUT                      
*                                                                               
OKXIT    LTR   RB,RB               CC = POS OK COMPLETED                        
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
*              DATA MANAGER ERRORS                                              
*                                                                               
DMERROR  MVI   FERN,RECNFND                                                     
         TM    8(R1),X'10'                                                      
         BO    ERROR                                                            
         MVI   FERN,ENDFILE                                                     
         TM    8(R1),X'80'                                                      
         BO    ERROR                                                            
         MVI   FERN,DISKERR                                                     
         TM    8(R1),X'40'                                                      
         BO    ERROR                                                            
         MVI   FERN,INVALID                                                     
         B     ERROR                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
* NESTED INCLUDES                                                               
* DMWRKRD                                                                       
* GEKWXDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE GEKWXDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003GEKWX08   05/01/02'                                      
         END                                                                    
