*          DATA SET BUFIL16    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T50216A                                                                  
         TITLE 'T50216 - BUDGET CONTROL LFM - DATA TYPE COPY SCREEN'            
T50216   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FI16**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R5,ANODBLK          R5=A(NODIO BLOCK)                            
         USING NODBLKD,R5                                                       
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
*                                                                               
         GOTO1 VSETADD                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY FIELDS                          
         BE    VKEY                                                             
         CLI   MODE,VALREC                                                      
         BE    COPY                COPY DATA TYPE RECORDS                       
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE KEY FIELDS                                                           
*                                                                               
VKEY     BAS   RE,CLRNAME                                                       
         GOTO1 VVALCLT,PARAS,DCYFCLTH,0                                         
         MVC   FRCLT,CLTCODE       SAVE 'FROM' CLIENT CODE                      
         MVC   DCYFCLN,CLTNAM                                                   
         OI    DCYFCLNH+6,X'80'    XMIT NAME BACK                               
*                                                                               
VKEY2    GOTO1 VVALPRD,PARAS,DCYFPRDH,0                                         
         MVC   FRPRD,PRDCODE       SAVE 'FROM' PRODUCT CODE                     
         MVC   DCYFPRN,PRDNAM                                                   
         OI    DCYFPRNH+6,X'80'                                                 
*                                                                               
VKEY4    GOTO1 VVALPLAN,PARAS,DCYFPLAH,0                                        
         MVC   FRPLAN,PLANCODE                                                  
         MVC   DCYFPLN,PLANNAM                                                  
         OI    DCYFPLNH+6,X'80'                                                 
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVC   FRPLNKEY,NDLVKEY    SAVE 'FROM' PLAN'S DIRECTORY KEY             
*                                                                               
         XC    CLTVALS,CLTVALS                                                  
         XC    PRDVALS,PRDVALS                                                  
         XC    PLANVALS,PLANVALS                                                
*                                                                               
VKEY6    GOTO1 VVALCLT,PARAS,DCYTCLTH,(C'D',FRCLT)                              
         MVC   TOCLT,CLTCODE                                                    
         MVC   DCYTCLN,CLTNAM                                                   
         OI    DCYTCLNH+6,X'80'                                                 
*                                                                               
VKEY8    GOTO1 VVALPRD,PARAS,DCYTPRDH,(C'D',FRPRD)                              
         MVC   TOPRD,PRDCODE                                                    
         MVC   DCYTPRN,PRDNAM                                                   
         OI    DCYTPRNH+6,X'80'                                                 
*                                                                               
VKEY10   GOTO1 VVALPLAN,PARAS,DCYTPLAH,0                                        
         MVC   TOPLAN,PLANCODE                                                  
         MVC   DCYTPLN,PLANNAM                                                  
         OI    DCYTPLNH+6,X'80'                                                 
         L     R3,NDLEVPTR                                                      
         MVC   TOPLNKEY,NDLVKEY    SAVE 'TO' PLAN'S DIRECTORY KEY               
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLC   TOPLNKEY,FRPLNKEY   TEST 'TO PLAN'='FROM PLAN'                   
         BE    SPERR                                                            
         TM    PLANIND,BUPLNDAT    TEST IF 'TO PLAN' HAS DATA                   
         BZ    VKEYX               NO                                           
         MVC   XTRA(13),=C'PLAN HAS DATA'                                       
         B     SPERR                                                            
*                                                                               
VKEYX    OI    WHENOK,X'01'        BYPASS GENCON MAINTENANCE                    
         B     XIT                                                              
         EJECT                                                                  
* COPY DATA TYPE RECORDS (VALREC PROCESSING)                                    
*                                                                               
COPY     L     R2,AIO3             R2=A(DISK ADDRESS LIST)                      
         SR    R3,R3               R3=N'DATA TYPE RECORDS                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         MVC   BUKEY,FRPLNKEY                                                   
         MVI   BUDSUB,BUDSUBQ                                                   
         GOTO1 HIGH                                                             
         B     COPY2                                                            
*                                                                               
COPY1    GOTO1 SEQ                                                              
*                                                                               
COPY2    CLC   BUKEY(BUDTYP-BUKEY),KEYSAVE    TEST SAME PLAN/DTYPE REC          
         BNE   COPY4                                                            
         MVC   0(4,R2),BUKDA       EXTRACT DISK ADDRESS                         
         LA    R2,4(R2)            NEXT DISK ADDRESS                            
         LA    R3,1(R3)            INCREMENT RECORD COUNT                       
         LA    RF,MAXDTYPS         TEST FOR COPY LIMIT                          
         CR    R3,RF                                                            
         BNH   COPY1                                                            
*                                                                               
         MVC   CONHEAD(L'MANYMSG),MANYMSG                                       
         LA    R2,DCYFCLTH         TOO MANY RECORD TO COPY                      
         ST    R2,ACURFORC                                                      
         B     COPYX                                                            
*                                                                               
COPY4    LTR   R3,R3               TEST FOR ANY RECORDS TO COPY                 
         BP    COPY6               YES                                          
         MVC   CONHEAD(L'NODTMSG),NODTMSG                                       
         LA    R2,DCYFCLTH                                                      
         ST    R2,ACURFORC                                                      
         B     COPYX                                                            
*                                                                               
COPY6    STC   R3,NFRDT            SAVE RECORD COUNT                            
         L     R2,AIO3             POINT R2 TO START OF LIST                    
         XC    KEY,KEY                                                          
*                                                                               
COPY8    LA    R4,KEY              FETCH DISK ADDRESS                           
         MVC   BUKDA,0(R2)                                                      
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         CLC   BUKEY(BUSUBKEY-BUKEY),FRPLNKEY                                   
         BE    *+6                 RIGHT PLAN                                   
         DC    H'0'                                                             
         CLI   BUDSUB,BUDSUBQ                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUKEY(BUSUBKEY-BUKEY),TOPLNKEY  'TO' PLAN KEY                    
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),0(R4)  EXTRACT NEW KEY FROM RECORD                  
*                                                                               
COPY10   LA    R4,KEY              MAKE KEY ADDRESSABLE                         
         L     R6,AIO                                                           
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   BUKEY,KEYSAVE       TEST IF NEW KEY FOUND                        
         BE    COPY12              YES                                          
*                                                                               
         GOTO1 ADDREC                                                           
         B     COPY15                                                           
*                                                                               
COPY12   MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            SET TO REPLACE RECORD                        
         GOTO1 PUTREC                                                           
         TM    BUKCSTA,X'80'       TEST IF DIRECTORY ENTRY DELETED              
         BZ    COPY15              NO                                           
         NI    BUKCSTA,X'FF'-X'80' YES-UNDELETE DIRECTORY POINTER               
         GOTO1 WRITE                                                            
*                                                                               
COPY15   NI    DMINBTS,X'FF'-X'08' TURN OFF PASS DELETE BIT                     
         LA    R2,4(R2)            NEXT DISK ADDRESS                            
         BCT   R3,COPY8                                                         
*                                                                               
COPY20   LA    R2,CONHEAD                                                       
         ZIC   R0,NFRDT                                                         
         EDIT  (R0),(3,0(R2)),ALIGN=LEFT                                        
         AR    R2,R0                                                            
         MVC   1(L'COPYMSG,R2),COPYMSG                                          
         LA    R2,CONACTH                                                       
         ST    R2,ACURFORC                                                      
*                                                                               
COPYX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO CLEAR PROTECTED NAME FIELDS AT START OF VALKEY                 
*                                                                               
CLRNAME  ST    RE,SAVERE                                                        
         LA    RE,NAMETAB          RE=TABLE POINTER                             
         LA    R0,NAMES            R0=COUNTER                                   
*                                                                               
CLRNAME2 SR    R2,R2                                                            
         ICM   R2,3,0(RE)          RE=FIELD HEADER DISPLACEMENT                 
         A     R2,ATWA             R2=A(FIELD HEADER)                           
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'8'                                                         
         TM    1(R2),X'02'         TEST FOR EXTENDED HEADER                     
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         BCTR  R1,0                FOR EXECUTE                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
CLRNAME4 LA    RE,L'NAMETAB(RE)                                                 
         BCT   R0,CLRNAME2                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         SPACE 1                                                                
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* TABLE OF DISPLACEMENTS OF PROTECTED 'NAME' FIELDS                             
*                                                                               
NAMETAB  DS    0XL2                                                             
         DC    AL2(DCYFCLNH-T502FFD)                                            
         DC    AL2(DCYFPRNH-T502FFD)                                            
         DC    AL2(DCYFPLNH-T502FFD)                                            
         DC    AL2(DCYTCLNH-T502FFD)                                            
         DC    AL2(DCYTPRNH-T502FFD)                                            
         DC    AL2(DCYTPLNH-T502FFD)                                            
NAMES    EQU   (*-NAMETAB)/L'NAMETAB                                            
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
COPYMSG  DC    C'DATA TYPE RECORDS COPIED'                                      
NODTMSG  DC    C'NO DATA TYPE RECORDS TO COPY'                                  
MANYMSG  DC    C'* TOO MANY DATA TYPE RECORDS TO COPY *'                        
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER DATA TYPE SCREEN                                               
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILC6D                                                       
         EJECT                                                                  
* WORKING STORAGE VALUES                                                        
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
*                                                                               
FRCLT    DS    CL(L'CLTCODE)       FROM CLIENT                                  
FRPRD    DS    CL(L'PRDCODE)       FROM PRODUCT                                 
FRPLAN   DS    CL(L'PLANCODE)      FROM PLAN                                    
TOCLT    DS    CL(L'CLTCODE)       TO CLIENT                                    
TOPRD    DS    CL(L'PRDCODE)       TO PRODUCT                                   
TOPLAN   DS    CL(L'PLANCODE)      TO PLAN                                      
*                                                                               
NFRDT    DS    X                   N'FROM DATA TYPE RECORDS                     
*                                                                               
FRPLNKEY DS    CL(L'BUKEY)         FROM PLAN DIRECTORY KEY                      
TOPLNKEY DS    CL(L'BUKEY)         TO PLAN DIRECTORY KEY                        
*                                                                               
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MAXDTYPS EQU   255                                                              
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003BUFIL16   05/01/02'                                      
         END                                                                    
