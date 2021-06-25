*          DATA SET GEKWX02    AT LEVEL 002 AS OF 05/24/96                      
*PHASE TF2002A,+0                                                               
         TITLE '$KWX MK3 - MODE AND HELP ACTIONS'                               
KWX02    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**KWX02*,R9                                                    
         L     RC,0(R1)                                                         
         USING KWX02+4096,R9       R9 = 2ND BASE                                
         USING TWAD,RA             RA = TWA                                     
         USING GWS,RC              RC = GWS                                     
*                                                                               
T001     ST    RB,ABASE2                                                        
         ST    R9,A2NDBAS2                                                      
         ST    RD,AREGSAV2                                                      
         B     T010                                                             
         EJECT                                                                  
*              HANDLE MODE ACTION                                               
*                                                                               
T010     CLI   ACTION,MOD          MODE=FORMAT/MESSAGE                          
         BNE   T020                                                             
         CLI   FXNEW,0             NEWL + NEW CONTAIN L+VALUE OF RHS            
         BNE   T012                                                             
         MVI   FERN,MISSING                                                     
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
T012     ZIC   R1,NEWL             CHECK FOR F/M                                
         BCTR  R1,0                                                             
         MVI   BYTE,MESSAGE                                                     
         MVI   FERN,INVALID                                                     
         EX    R1,MESSCLC                                                       
         BE    T014                                                             
         MVI   BYTE,FORMAT                                                      
         EX    R1,FORMCLC                                                       
         BE    T014                                                             
         MVI   FERN,INVALID                                                     
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
T014     CLC   BYTE,SAVMODE        CHECK FOR NO CHANGE OF MODE                  
         BNE   *+12                                                             
         MVI   FERN,MODALRDY                                                    
         B     ERROR                                                            
         MVC   SAVMODE,BYTE                                                     
         MVI   KWXDATAH,0                                                       
         LA    R2,KWXDATAH                                                      
         ST    R2,ATWADIFF                                                      
         XC    SAVEREST,SAVEREST   CLEAR ALL BOOK-RELATED VALUES                
         XC    BUFFSAVE,BUFFSAVE                                                
         XC    KWXBOOK,KWXBOOK                                                  
         OI    KWXBOOKH+6,X'80'                                                 
         MVC   KWXHEAD(35),=C'ACTION COMPLETED - NOW SPECIFY BOOK'              
         B     OKXIT                                                            
*                                                                               
MESSCLC  CLC   NEW(0),=C'MESSAGE'                                               
FORMCLC  CLC   NEW(0),=C'FORMAT'                                                
         EJECT                                                                  
*              HANDLE HELP WITH BOOK OR FORM PARAMETER                          
*                                                                               
T020     SR    R0,R0               NOT MORE THAN ONE PARAM                      
         ZIC   R1,FXID             BOOK                                         
         AR    R0,R1                                                            
         IC    R1,FXFORMID         FORMAT                                       
         AR    R0,R1                                                            
         IC    R1,FXREF            REF                                          
         AR    R0,R1                                                            
         BZ    T060                NONE                                         
         CH    R0,=H'2'                                                         
         BNH   T025                                                             
         MVI   SYNTAX,C'Y'                                                      
         MVI   FERN,INCPARMS                                                    
         MVI   FNDX,2                                                           
         B     ERROR                                                            
*                                                                               
T025     CLI   FXID,0              BOOK - WHAT IS MY CURRENT BOOK               
         BE    T027                                                             
         LA    R4,FRMBOOK                                                       
         CLI   SAVMODE,FORMAT                                                   
         BE    *+8                                                              
         LA    R4,MSGBOOK                                                       
         OC    0(L'FRMBOOK,R4),0(R4)                                            
         BNZ   T030                                                             
         MVI   FERN,NOBOOK         ISNT ONE                                     
         B     ERROR                                                            
*                                                                               
T027     CLI   FXFORMID,0          FORMAT - WHAT IS MY CURRENT FORMAT           
         BE    T060                                                             
         CLI   SAVMODE,FORMAT                                                   
         BNE   *+12                                                             
         MVI   FERN,INVMODE        NO MEANING IN FORMAT MODE                    
         B     ERROR                                                            
         OC    FRMRECHI,FRMRECHI                                                
         BNZ   *+12                                                             
         MVI   FERN,NOFORMAT       ISNT ONE                                     
         B     ERROR                                                            
         MVC   KWXHEAD(22),=C'YOUR CURRENT FORMAT IS'                           
         LA    R3,KWXHEAD+23                                                    
         OC    FRMBOOK,FRMBOOK                                                  
         BNZ   *+14                                                             
         MVC   0(11,R3),=C'0 (DEFAULT)'                                         
         B     OKXIT                                                            
         LA    R4,FRMBOOK                                                       
*&&US                                                                           
         CLC   0(2,R4),=H'43'      TCH1                                         
*&&                                                                             
*&&UK                                                                           
         CLC   SAVTKWID,0(R4)                                                   
*&&                                                                             
         BE    T042                                                             
         B     T032                                                             
*                                                                               
T030     MVC   KWXHEAD(20),=C'YOUR CURRENT BOOK IS'                             
         LA    R3,KWXHEAD+21                                                    
         CLC   TWAUSRID,0(R4)                                                   
         BE    T040                                                             
*                                                                               
T032     LA    R7,KEY              READ IF REC FOR NAME                         
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),0(R4)                                                
         GOTO1 ADATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,KEY,WORK                 
         CLI   DMCB+8,0                                                         
         MVI   FERN,INVALID                                                     
         BNE   ERROR                                                            
         LA    R6,CTIDATA                                                       
         SR    R5,R5                                                            
*                                                                               
T034     CLI   0(R6),0                                                          
         BE    ERROR                                                            
         CLI   0(R6),X'02'                                                      
         BE    *+14                                                             
         IC    R5,1(R6)                                                         
         AR    R6,R5                                                            
         B     T034                                                             
         USING CTDSCD,R6                                                        
         MVC   0(10,R3),CTDSC                                                   
         DROP  R6                                                               
         LA    R3,9(R3)                                                         
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C'.'                                                       
         LA    R3,2(R3)                                                         
         CLI   FXFORMID,0                                                       
         BE    T041                                                             
         B     T045                                                             
*                                                                               
T040     CLI   FXID,0              DONT SHOW INITS IN NAME IF SAME AS           
         BE    T042                USER'S (BOOK) OR IF 'ZZZ' (FORM)             
         CLC   LASTINIT(4),2(R4)                                                
         BE    T050                                                             
T041     MVC   0(4,R3),2(R4)                                                    
         LA    R3,4(R3)                                                         
         B     T050                                                             
*                                                                               
T042     CLC   3(3,R4),=C'ZZZ'                                                  
         BE    T050                                                             
T045     MVC   0(3,R3),3(R4)       DISPLAY  INITS                               
         LA    R3,3(R3)                                                         
*                                                                               
T050     UNPK  FULL,6(2,R4)        DISPLAY 123 FROM X'2310'                     
         MVC   0(1,R3),FULL+3                                                   
         OI    0(R3),X'F0'                                                      
         MVC   1(2,R3),FULL+1                                                   
         B     OKXIT                                                            
         EJECT                                                                  
*              HANDLE HELP WITH REF OR NO PARAMETER                             
*                                                                               
T060     XC    PARAS(12),PARAS     SET UP FULL, PROTECTED SCREEN FOR            
         GOTO1 ASETSCRN,PARAS      TEXT                                         
         MVI   FRMSTAT,0                                                        
         LA    R2,KWXDATAH                                                      
         SR    R1,R1                                                            
T062     OI    1(R2),X'20'                                                      
         OI    6(R2),X'A0'                                                      
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BNE   T062                                                             
*                                                                               
T065     LA    R2,KWXDATAH                                                      
         CLI   FXREF,0                                                          
         BE    T070                                                             
         L     RF,AACTAB                                                        
         USING ACTIOND,RF                                                       
         LA    R0,ACTNFULL-ACTIOND                                              
         MVC   KWXDATA(33),=C'VALID ACTIONS IN MESSAGE MODE ARE'                
         CLI   SAVMODE,MESSAGE                                                  
         BE    *+10                                                             
         MVC   KWXDATA+17(7),=C'FORMAT '                                        
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
*                                                                               
T068A    CLI   0(RF),0             LOOP FOR AN ACTION                           
         BE    T069X                                                            
         CLI   0(R2),0                                                          
         BE    T069X                                                            
         MVC   BYTE,SAVMODE                                                     
         NC    BYTE,ACTNSTAT                                                    
         BZ    T068B               NOT FOR THIS MODE                            
         TM    ACTNSTAT,DDSONLY                                                 
         BZ    *+12                                                             
         TM    TWAAUTH,X'80'                                                    
         BNO   T068B                                                            
         MVC   8(2,R2),ACTNSHRT                                                 
         IC    R1,ACTNLEN                                                       
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   13(0,R2),ACTNFULL                                                
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
T068B    IC    R1,0(RF)                                                         
         AR    RF,R1                                                            
         B     T068A                                                            
*                                                                               
T069X    MVC   KWXHEAD(29),=C'ACTION COMPLETED - ENTER NEXT'                    
         B     OKXIT                                                            
*                                                                               
T070     LA    RF,HELPTEXT         NO PARAM                                     
T072     CLI   0(RF),X'FF'                                                      
         BE    T069X                                                            
         MVC   8(78,R2),0(RF)                                                   
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    RF,78(RF)                                                        
         B     T072                                                             
*                                                                               
HELPTEXT DS    0C                                                               
         DC    CL39'THIS PROGRAM ENABLES YOU TO CREATE AND '                    
         DC    CL39'MAINTAIN ''BOOKS'' OF MESSAGE TEXT, AND'                    
         DC    CL39'SEND YOUR MESSAGES TO OTHER USERS OF TH'                    
         DC    CL39'E SYSTEM FROM THESE BOOKS. '                                
         DC    CL39'TO SEND A MESSAGE, USE THE FOLLOWING SE'                    
         DC    CL39'QUENCE OF ACTIONS -'                                        
*                                                                               
         DC    CL39'1. ADD A BOOK AND STATE THE FORMAT YOU '                    
         DC    CL39'INTEND TO USE, VIA A ''BOOK'' ACTION;  '                    
         DC    CL39'.  EG.    BOOK,ADD,FORM=ORD,COMMENT=MY '                    
         DC    CL39'1ST MESSAGE'                                                
         DC    CL39'2. ADD ONE OR MORE SCREENFULS OF MESSAG'                    
         DC    CL39'E TEXT TO YOUR BOOK VIA ''ADD'' ACTIONS.'                   
         DC    CL39'3. IF REQUIRED, CHECK THROUGH YOUR MESS'                    
         DC    CL39'AGE VIA ''DISPLAY'' ACTIONS AND MAKE'                       
         DC    CL39'.  CHANGES TO IT VIA ''CHANGE'', ''INSERT'''                
         DC    CL39', ''DELETE'' AND ''REPLACE'' ACTIONS.'                      
         DC    CL39'4. SEND THE MESSAGE TO ONE OR MORE ADDR'                    
         DC    CL39'ESSEES VIA A ''SEND'' ACTION.'                              
         DC    CL39'5. IF YOU WISH, ALLOW OTHERS TO READ/CO'                    
         DC    CL39'PY OR CHANGE YOUR BOOK VIA AN'                              
         DC    CL78'.  ''AUTHORIZE'' ACTION.'                                   
         DC    CL78' '                                                          
*                                                                               
         DC    CL39'FOR FURTHER DETAILS OF THE FACILITIES A'                    
         DC    CL39'VAILABLE, USE THE ACTION ''HELP,REF'', '                    
         DC    CL39'WHICH WILL DISPLAY A REFERENCE LIST OF '                    
         DC    CL39'ALL THE ACTIONS AVAILABLE TO YOU,'                          
         DC    CL39'INDICATING VIA PARENTHESES THOSE PARAME'                    
         DC    CL39'TERS THAT ARE OPTIONAL. EACH ACTION HAS'                    
         DC    CL39'A STANDARD 2-CHARACTER ABBREVIATED NAME'                    
         DC    CL39' AND PARAMETER KEYWORDS MAY BE '                            
         DC    CL39'ABBREVIATED TO THE FIRST CHARACTER.'                        
         DC    CL39' '                                                          
         DC    X'FF'                                                            
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
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
*              DSECT TO COVER AN ENTRY IN ACTION TABLE ACTAB                    
*                                                                               
ACTIOND  DSECT                                                                  
ACTNLEN  DS    CL1       B         LENGTH OF TABLE ENTRY                        
ACTNOV   DS    CL1       B         OVERLAY NUMBER                               
ACTNNUM  DS    CL1       B         ACTION NUMBER (EQUATED)                      
ACTNSTAT DS    CL1       X         ACTION STATUS                                
*                                  X'80' = FORMAT   (COMPATIBLE MODE)           
*                                  X'40' = MESSAGE  (DITTO)                     
*                                  X'20' = BOOK     (REQUIRES BOOK)             
*                                  X'10' = WRITE    (UPDATES)                   
*                                  X'08' = PRINT    (WRITES TO PQ)              
*                                                                               
*                                                                               
ACTNSHRT DS    CL2       C         2-CHARACTER ACTION CODE                      
ACTNFULL DS    0C        C         FULL ACTION SYNTAX (FOR HELP)                
*                                                                               
         SPACE 2                                                                
* NESTED INCLUDES                                                               
* CTGENFILE                                                                     
* GEKWXDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE GEKWXDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002GEKWX02   05/24/96'                                      
         END                                                                    
