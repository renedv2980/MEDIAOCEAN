*          DATA SET TASYSDRIVE AT LEVEL 151 AS OF 02/24/17                      
*PHASE T00A8AC                                                                  
*INCLUDE ADSCAN                                                                 
         TITLE 'TADRIVER - SYSTEM DRIVER FOR TALENT'                            
TADRIVER CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TADV**,R8,R7,R6                                              
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     RC,GLAWORKD                                                      
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         SPACE 1                                                                
         CLI   GLHOOK,GLRESOLV                                                  
         BNE   *+12                                                             
         BAS   RE,SYSRES           RESOLVING ADDRESSES                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   GLHOOK,GLINCOMP                                                  
         BNE   *+12                                                             
         BAS   RE,SYSICOMP         INTERNAL COMPUTES                            
         B     XIT                                                              
         SPACE 1                                                                
         CLI   GLHOOK,GLROUT                                                    
         BNE   XIT                                                              
         BAS   RE,SYSEXEC          EXECUTING ROUTINES                           
         B     XIT                                                              
         EJECT                                                                  
*              RESOLVING ROUTINE ADDRESSES                                      
         SPACE 3                                                                
SYSRES   NTR1                                                                   
         L     R1,=A(ROUTLIST)                                                  
         SPACE 1                                                                
SYSRES2  CLC   0(8,R1),GLLABEL                                                  
         BE    SYSRES4                                                          
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         B     SYSRES2                                                          
         SPACE 1                                                                
SYSRES4  MVC   GLAROUT,8(R1)       RETURN ADDRESS                               
         B     XIT                                                              
         EJECT                                                                  
*              INTERNAL COMPUTES                                                
         SPACE 3                                                                
SYSICOMP NTR1                                                                   
         L     R1,=A(ICMPLIST)                                                  
         SPACE 1                                                                
SYSICMP2 CLC   0(8,R1),GLLABEL                                                  
         BE    SYSICMP4                                                         
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         B     SYSICMP2                                                         
         SPACE 1                                                                
SYSICMP4 L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     RF,8(R1)            PICK UP ROUTINE                              
         BR    RF                                                               
         SPACE 1                                                                
ICMPLIST DC    CL8'LOWEST  ',A(COMPLOW)                                         
         DC    CL8'OUTCD   ',A(COMPCD)                                          
         DC    X'FF'                                                            
         SPACE 1                                                                
COMPLOW  LR    R3,R2               COMPUTE LOWEST OF 3                          
         SH    R3,=H'24'           BACK UP THREE                                
         ZAP   0(8,R2),0(8,R3)     FIRST                                        
         CP    0(8,R2),=P'0'       USE SECOND IF FIRST IS ZERO                  
         BZ    COMPLOW1                                                         
         CP    8(8,R3),=P'0'       DON'T USE SECOND IF ZERO                     
         BE    COMPLOW2                                                         
         CP    8(8,R3),0(8,R2)     OR, IF SECOND < FIRST                        
         BH    COMPLOW2                                                         
         SPACE 1                                                                
COMPLOW1 ZAP   0(8,R2),8(8,R3)     SECOND                                       
         SPACE 1                                                                
COMPLOW2 ZAP   DUB,16(8,R3)        THIRD                                        
         ZIC   R1,GLARGS           ARG1 = $N000                                 
         M     R0,=F'10'                                                        
         ZIC   R0,GLARGS+1         ARG2 = $N00                                  
         AR    R1,R0                                                            
         M     R0,=F'10000'                                                     
         CVD   R1,LOWMAXP                                                       
         SP    DUB,LOWMAXP                                                      
         BNP   COMPLOW4            MUST BE POSITIVE                             
         SPACE 1                                                                
         CP    DUB,0(8,R2)         IS THIS < CURRENT                            
         BH    COMPLOW4                                                         
         ZAP   0(8,R2),DUB                                                      
         SPACE 1                                                                
COMPLOW4 CP    0(8,R2),LOWMAXP     FINAL MUST BE LOWER THAN MAX                 
         BL    XIT                                                              
         ZAP   0(8,R2),LOWMAXP                                                  
         B     XIT                                                              
         SPACE 1                                                                
TOTLOW   DC    PL8'0'                                                           
LOWMAXP  DS    D                                                                
         SPACE 1                                                                
COMPCD   DS    0H                  AVERAGE DAYS                                 
         L     R1,0(R2)                                                         
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         OC    4(4,R2),4(R2)                                                    
         BZ    XIT                                                              
         M     R0,=F'20'                                                        
         D     R0,4(R2)                                                         
         A     R1,=F'1'                                                         
         LTR   R1,R1                                                            
         BNM   *+8                                                              
         S     R1,=F'2'                                                         
         M     R0,=F'1'                                                         
         D     R0,=F'2'                                                         
         ST    R1,0(R2)                                                         
         B     XIT                                                              
         SPACE 2                                                                
OUTLOW   TM    GLINDS,GLTOTLIN     DETAILS                                      
         BO    OUTLOW2                                                          
         AP    TOTLOW,0(8,R2)                                                   
         B     OUTLOW4                                                          
         SPACE 1                                                                
OUTLOW2  ZAP   0(8,R2),TOTLOW      TOTAL                                        
         ZAP   TOTLOW,=P'0'                                                     
         SPACE 1                                                                
OUTLOW4  MVI   GLHOOK,GLEDIT       LET DRIVER EDIT                              
         B     XIT                                                              
         SPACE 2                                                                
OUTCD    L     R4,0(R2)            EDIT WITH ONE DECIMAL PLACE                  
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         EDIT  (R4),(4,0(R3)),1,FLOAT=-                                         
         LTR   R4,R4                                                            
         BM    OUTCD2                                                           
         CH    R4,=H'1000'                                                      
         BL    XIT                                                              
         LR    R1,R4                                                            
         AH    R1,=H'5'                                                         
         M     R0,=F'1'                                                         
         D     R0,=F'10'                                                        
         EDIT  (R1),(4,0(R3))      IF TOO BIG, ZERO PLACES                      
         B     XIT                                                              
         SPACE 1                                                                
OUTCD2   CLI   0(R3),C'0'          IF NEGATIVE                                  
         BL    XIT                                                              
         LR    R1,R4                                                            
         SH    R1,=H'5'                                                         
         M     R0,=F'1'                                                         
         D     R0,=F'10'                                                        
         LR    R4,R1                                                            
         EDIT  (R4),(4,0(R3)),FLOAT=-                                           
         B     XIT                                                              
         EJECT                                                                  
*              EXECUTING ROUTINES (ROWS)                                        
         SPACE 3                                                                
SYSEXEC  NTR1                                                                   
         MVC   WORK,MYSPACES       PRESET WORK AREAS                            
         ZAP   DUB,=P'0'                                                        
         L     RF,GLAROUT                                                       
         L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         SPACE 1                                                                
         CLI   GLMODE,GLOUTPUT                                                  
         BE    SYSOUT                                                           
         L     R1,GLADTENT                                                      
         USING DRIND,R1                                                         
         MVC   MYILEN,DRINLEN      SAVE INPUT LEN & TYPE                        
         MVC   MYITYPE,DRINTYPE                                                 
         ZIC   R3,DRINLEN          PICK UP INPUT LENGTH-1 INTO R3               
         BCTR  R3,0                                                             
         L     R4,TIAREC           R4=A(RECORD)                                 
         B     DRIVEGO                                                          
         DROP  R1                                                               
         SPACE 1                                                                
SYSOUT   MVC   OUTAREA,MYSPACES    PRESET SOME FIELDS FOR OUTPUT                
         L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         CLI   DROLTYP,C'N'        NO PRINT - NOT INTERESTED                    
         BE    XIT                                                              
         MVC   MYPOSO,DROPOS                                                    
         MVC   MYOLEN,DROLEN                                                    
         DROP  R1                                                               
         SPACE 1                                                                
DRIVEGO  L     RE,=A(DRIVE2)       IS THE ROUTINE IN MAIN SECTION               
         SLL   RE,8                                                             
         SRL   RE,8                                                             
         SLL   RF,8                                                             
         SRL   RF,8                                                             
         CR    RF,RE                                                            
         BLR   RF                     YES SO GO THERE                           
         BR    RE                     NO  GO TO DRIVE2                          
         EJECT                                                                  
*              ADDRESS ROUTINES                                                 
         SPACE 2                                                                
INADD    CLI   GLARGS,C'A'                                                      
         BL    INADD1                                                           
         LA    R3,GLARGS           R3 = A(ARGUMENT)                             
         BAS   RE,SUBADDR          EXTRACT SUBSIDIARY ADDRESS PART              
         B     XIT                                                              
         SPACE 1                                                                
INADD1   MVI   ELCODE,TAADELQ      ADDRESSES                                    
         BAS   RE,GETEL                                                         
         MVC   ELEMENT(132),MYSPACES                                            
         BNE   INADD2                                                           
         USING TAADD,R4                                                         
         ZIC   R1,TAADLNES                                                      
         MH    R1,=H'30'                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),TAADADD                                               
         SPACE 1                                                                
INADD2   L     R4,TIAREC           CHECK FOR NEW ADDRESS                        
         MVI   ELCODE,TAA2ELQ      ADDRESSES                                    
         BAS   RE,GETEL                                                         
         BNE   INADD4                                                           
         USING TAA2D,R4                                                         
         MVC   ELEMENT(90),TAA2ADD1                                             
         LA    R3,ELEMENT+30                                                    
         CLI   0(R3),C'A'                                                       
         BL    INADD3                                                           
         LA    R3,ELEMENT+60                                                    
         CLI   0(R3),C'A'                                                       
         BL    INADD3                                                           
         LA    R3,ELEMENT+60                                                    
         SPACE 1                                                                
INADD3   MVC   0(25,R3),TAA2CITY                                                
         MVC   26(2,R3),TAA2ST                                                  
         MVC   29(10,R3),TAA2ZIP                                                
         SPACE 1                                                                
         CLI   TAA2LEN,TAA2LNQ                                                  
         BL    INADD3Z                                                          
         CLC   TAA2CTRY,=C'US'                                                  
         JE    INADD3Z                                                          
         GOTO1 VALCTRY,DMCB,(X'80',TAA2CTRY)                                    
         BNE   INADD3Z                                                          
         USING CTRYTABD,R1                                                      
         L     R1,TGACTRY                                                       
         ZIC   RE,CTRYDSP                                                       
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         IC    RE,CTRYLEN                                                       
         SHI   RE,CTRYDESC-CTRYTABD+1                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   40(0,R3),CTRYDESC                                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    40(0,R3),MYSPACES                                                
         DROP  R1                                                               
         SPACE 1                                                                
INADD3Z  GOTO1 SQUASHER,DMCB,(R3),132                                           
         SPACE 1                                                                
INADD4   MVC   0(120,R2),ELEMENT                                                
         CLI   GLARGS,0            ARGS 1 1-4 = LINE NUMBER                     
         BE    XIT                                                              
         ZIC   R1,GLARGS                                                        
         BCTR  R1,0                                                             
         MH    R1,=H'30'                                                        
         AR    R1,R2                                                            
         MVC   0(30,R2),0(R1)                                                   
         MVC   30(90,R2),MYSPACES                                               
         B     XIT                                                              
         EJECT                                                                  
*              ADDRESS SUBSIDIARY ROUTINES                                      
         SPACE 1                                                                
*                                  R3 = A(ARGUMENT)   S = STATE CODE            
*                                                     N = STATE NAME            
*                                                     Y = CITY                  
*                                                     Z = ZIP                   
*                                  R4 = A(RECORD)                               
SUBADDR  NTR1                                                                   
         LR    R0,R4                                                            
         MVI   ELCODE,TAADELQ      GET ADDRESS ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   SUBAD4                                                           
         USING TAADD,R4                                                         
         MVC   ADSCADD,MYSPACES                                                 
         ZIC   R1,TAADLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ADSCADD(0),TAADADD                                               
         GOTO1 SQUASHER,DMCB,ADSCADD,120                                        
         SPACE 1                                                                
         ZIC   R1,TAADLNES                                                      
         BCTR  R1,0                                                             
         MH    R1,=H'30'                                                        
         LA    R1,TAADADD(R1)                                                   
         ST    R1,DMCB                                                          
         MVI   DMCB,30                                                          
         GOTO1 =V(ADSCAN),DMCB,,(24,ADSCCITY),ADSCSTA,(9,ADSCZIP)               
         MVI   ADSCCITY+24,C' '                                                 
         MVI   ADSCZIP+9,C' '                                                   
         SPACE 1                                                                
SUBAD4   LR    R4,R0               LOOK FOR NEW STYLE ADDRESS                   
         MVI   ELCODE,TAA2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   SUBAD6                                                           
         USING TAA2D,R4                                                         
         MVC   ADSCCITY,TAA2CITY                                                
         MVC   ADSCSTA,TAA2ST                                                   
         MVC   ADSCZIP,TAA2ZIP                                                  
         MVC   ADSCCTRY,MYSPACES                                                
         CLI   TAA2LEN,TAA2LNQ                                                  
         BL    SUBAD6                                                           
         MVC   ADSCCTRY,TAA2CTRY                                                
         SPACE 1                                                                
SUBAD6   MVC   0(2,R2),ADSCCTRY                                                 
         CLI   0(R3),C'C'                                                       
         BE    XIT                                                              
         MVC   0(2,R2),ADSCSTA                                                  
         CLI   0(R3),C'S'                                                       
         BE    XIT                                                              
         MVI   2(R2),C' '                                                       
         BAS   RE,NEEDUNIT                                                      
         MVC   0(8,R2),TGTANAME                                                 
         CLI   0(R3),C'N'                                                       
         BE    XIT                                                              
         MVC   0(9,R2),ADSCZIP                                                  
         CLI   0(R3),C'Z'                                                       
         BE    XIT                                                              
         MVC   0(24,R2),ADSCCITY                                                
         B     XIT                                                              
         SPACE 1                                                                
ADSCSTA  DS    CL2                                                              
ADSCZIP  DS    CL10                                                             
ADSCCITY DS    CL25                                                             
ADSCADD  DS    CL120                                                            
ADSCCTRY DS    CL2                                                              
         EJECT                                                                  
*                                                                               
INCWST   DS    0H                                                               
         MVC   0(3,R2),TIUNIT      (TAX) UNIT                                   
         B     XIT                                                              
*&&DO                                                                           
         L     R4,TIAREC                                                        
         USING TACWD,R4                                                         
         MVI   ELCODE,TACWELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
INCWST10 BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         CLC   TACWUNIT,=C'FD '                                                 
         BE    INCWST10                                                         
         TM    TACWSTAT,TACWSWRK                                                
         BZ    INCWST10                                                         
INCWST90 MVC   0(2,R2),TACWUNIT                                                 
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
*              GENERAL ROUTINES                                                 
         SPACE 3                                                                
INNAME   BAS   RE,GETNAME          NAMES                                        
         MVC   0(36,R2),NEEDNAME                                                
         B     XIT                                                              
         SPACE 1                                                                
OUTNAME  MVC   LABLAREA(4),=C'SHORT'                                            
         MVC   NAMEAREA,0(R2)                                                   
         B     GENOUT                                                           
         SPACE 1                                                                
INSHORT  BAS   RE,GETSHORT                                                      
         MVC   0(16,R2),NEEDSHRT                                                
         B     XIT                                                              
         SPACE 1                                                                
OUTSHORT MVC   LABLAREA(5),=C'SHORT'                                            
         MVC   NAMEAREA(16),0(R2)                                               
         B     GENOUT                                                           
         SPACE 1                                                                
INFREE   MVI   ELCODE,TAFNELQ      FREE FORM NAME                               
         BAS   RE,GETEL                                                         
         MVC   0(36,R2),MYSPACES                                                
         BNE   XIT                                                              
         USING TAFND,R4                                                         
         ZIC   R1,TAFNLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R2),TAFNNAME                                                 
         SPACE 1                                                                
OUTFREE  MVC   LABLAREA(5),=C'MUSIC'                                            
         CLI   GLARGS,C'A'                                                      
         BNE   *+10                                                             
         MVC   LABLAREA(9),=C'ATTENTION'                                        
         MVC   NAMEAREA,0(R2)                                                   
         B     GENOUT                                                           
         SPACE 1                                                                
INNUM    MVC   0(20,R2),MYSPACES                                                
         MVI   ELCODE,TANUELQ      FREE FORM NUMBER                             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
INNUM2   BAS   RE,NEXTEL           FIND FREE FORM NUMBER ELM                    
         BNE   XIT                                                              
         USING TANUD,R4                                                         
         CLC   TANUTYPE,GLARGS+1                                                
         BNE   INNUM2                                                           
         ZIC   R1,TANULEN                                                       
         SH    R1,=H'4'            PASS BACK NUMBER                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TANUMBER                                                 
*                                                                               
         CLI   GLARGS+1,C'I'       HANDLE INVOICES                              
         BE    INNUM2I                                                          
         CLI   GLARGS+1,C'S'                                                    
         BE    INNUM2I                                                          
         CLI   GLARGS+1,C'<'                                                    
         BNE   INNUM3                                                           
INNUM2I  MVC   WORK(6),0(R2)                                                    
         GOTO1 TINVCON,DMCB,WORK,(R2),DATCON                                    
         B     XIT                                                              
*                                                                               
INNUM3   DS    0H                  HANDLE LIEN PAYEE'S SSN                      
*                                                                               
         CLI   GLARGS+1,C'Y'       MUST BE FOR LIEN PAYEE'S PID                 
         BNE   INNUMX                                                           
*                                                                               
         CLI   GLARGS+2,C'N'       IF NAME ONLY WANTED                          
         BNE   INNUMX                                                           
*                                                                               
         BAS   RE,NEEDW4           READ IN W4 RECORD                            
         L     R4,NEEDAREC                                                      
         BAS   RE,GETNAME          FIND NAME                                    
*                                                                               
         MVC   0(32,R2),NEEDNAME                                                
         MVC   32(1,R2),NEEDTYPE                                                
*                                                                               
INNUMX   DS    0H                                                               
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
         SPACE 2                                                                
OUTNU    CLI   GLARGS,C'E'                                                      
         BE    OUTNUE                                                           
         CLI   GLARGS,C'L'                                                      
         BE    OUTNUL                                                           
         CLI   GLARGS,C'A'                                                      
         BE    OUTNUA                                                           
         CLI   GLARGS,C'Y'         LIEN PAYEE                                   
         BE    OUTNUY                                                           
*                                                                               
         MVC   LABLAREA(6),=C'NUMBER'                                           
         MVC   NAMEAREA(20),0(R2)                                               
         B     OUTNUX                                                           
         SPACE 1                                                                
OUTNUL   MVC   LABLAREA(7),=C'ACCOUNT'                                          
         MVC   NAMEAREA(12),0(R2)                                               
         B     OUTNUX                                                           
         SPACE 1                                                                
OUTNUA   MVC   LABLAREA(8),=C'AUTH/PO '                                         
         MVC   NAMEAREA(16),0(R2)                                               
         B     OUTNUX                                                           
         SPACE 1                                                                
OUTNUE   MVC   LABLAREA(8),=C'ESTIMATE'                                         
         MVC   NAMEAREA(16),0(R2)                                               
         SPACE 1                                                                
OUTNUX   CLI   NAMEAREA,X'FE'                                                   
         BNE   *+10                                                             
         MVC   NAMEAREA(4),=C'NONE'                                             
         B     GENOUT                                                           
*                                                                               
OUTNUY   DS    0H                                                               
*                                                                               
         MVC   LABLAREA(5),=C'PAYER'                                            
*                                                                               
         CLI   GLARGS+1,C'N'       IF NAME ONLY                                 
         BNE   OUTNUY2                                                          
*                                                                               
         MVC   NAMEAREA(32),0(R2)     PASS IT ON                                
         GOTO1 SQUASHER,DMCB,NAMEAREA,33                                        
*                                                                               
         B     GENOUT                                                           
*                                                                               
OUTNUY2  DS    0H                                                               
*                                                                               
         MVC   CODEAREA(9),0(R2)   CODE                                         
         MVC   NAMEAREA,MYSPACES   INIT                                         
*                                                                               
         CLI   GLARGS+1,C'C'       SKIP IF CODE ONLY                            
         BE    OUTNUY5                                                          
         CLI   GLARGS+1,C'X'       SKIP IF CODE ONLY                            
         BE    OUTNUY5                                                          
*                                                                               
         BAS   RE,NEEDW4                                                        
*                                                                               
         MVC   NAMEAREA(32),NEEDNAME    NAME                                    
         GOTO1 SQUASHER,DMCB,NAMEAREA,33                                        
*                                                                               
OUTNUY5  DS    0H                                                               
*                                                                               
         MVC   OUTNUSSN,0(R2)      COPY CURRENT SSN                             
*                                                                               
         MVC   CODEAREA(9),MYSPACES                                             
*                                                                               
         OC    OUTNUSSN,OUTNUSSN   DONE IF NO SSN                               
         BZ    OUTNUY6                                                          
*                                                                               
         CLI   GLARGS+1,C'X'       ALWAYS SHOW PID                              
         BO    W4OUT6                                                           
         TM    TGSYSTAT,TASYSPID   SKIP IF PID REQUIRED                         
         BO    W4OUT5                                                           
*                                                                               
         MVC   CODEAREA(3),0(R2)   SOCIAL SECURITY NUMBER                       
         MVI   CODEAREA+3,C'-'                                                  
         MVC   CODEAREA+4(2),3(R2)                                              
         MVI   CODEAREA+6,C'-'                                                  
         MVC   CODEAREA+7(4),5(R2)                                              
         B     OUTNUY6                                                          
                                                                                
NUOUT5   TM    TGSYSTAT,TASYSPID   PID REQUIRED                                 
         BZ    OUTNUY6                                                          
*                                                                               
         MVC   0(9,R2),MYSPACES                                                 
*                                                                               
         GOTO1 SSNPACK,DMCB,OUTNUSSN,OUTNUPID                                   
         MVC   CODEAREA(L'OUTNUPID),OUTNUPID                                    
         MVC   0(L'OUTNUPID,R2),OUTNUPID                                        
*                                                                               
OUTNUY6  DS    0H                                                               
         B     GENOUT                                                           
*                                                                               
OUTNUSSN DS    XL(L'TGSSN)         SSN SAVEAREA                                 
OUTNUPID DS    XL(L'TGPID)         PID SAVEAREA                                 
*                                                                               
         SPACE 1                                                                
         EJECT                                                                  
*              MIDHEAD CONTROL ROUTINES                                         
         SPACE 1                                                                
MIDHDIN  DS    0H                                                               
         CLI   GLARGS,C'D'         ONLY IF WE HAD DETAIL                        
         BNE   *+12                                                             
         CLI   HADDTL,C'Y'         TEST IF WE'VE HAD A DETAIL LINE              
         BNE   XIT                                                              
         MVI   HADDTL,C'N'                                                      
         L     R1,ASPOOLD                                                       
         MVI   FORCEHED-SPOOLD(R1),C'M'  SET DIRECTLY IN SPOOLD                 
         B     XIT                 (THEREFORE NO NEED TO TURN OFF)              
         SPACE 3                                                                
*              ALL TOTALS REQUIRED                                              
         SPACE 1                                                                
TOTRQIN  DS    0H                                                               
         OI    GLINDS,GLPALTOT     TURN THEM ON                                 
         B     XIT                                                              
         SPACE 1                                                                
TOTRQOUT DS    0H                                                               
         NI    GLINDS,ALL-GLPALTOT  TURN THEM OFF                               
         B     XIT                                                              
         SPACE 3                                                                
*              SUPPRESS LINE CONTROL                                            
         SPACE 1                                                                
NOPRTIN  DS    0H                                                               
         CLI   GLARGS,C'I'         SKIP IF REPORT BY INVOICE ONLY               
         BE    *+10                                                             
         MVC   0(1,R2),TIMODE      PASS TIMODE                                  
         B     XIT                                                              
         SPACE 1                                                                
NOPRTOUT DS    0H                                                               
         MVI   NOPRINT,C'N'        ASSUME WE WON'T SUPPRESS                     
         OI    GLINDS,GLPALTOT     SET TO FORCE TOTALS                          
         CLI   0(R2),PROCINV       SUPPRESS HIGH-LEVEL RECORDS                  
         BNE   *+12                                                             
         MVI   NOPRINT,C'Y'        SET SUPPRESS SWITCH                          
         NI    GLINDS,ALL-GLPALTOT TURN OFF FORCE TOTALS SWITCH                 
         B     XIT                                                              
         EJECT                                                                  
*              CUSTOM OUTPUT ROUTINE FOR AV REPORT                              
         SPACE 2                                                                
AVPAYOUT DS    0H                                                               
         MVI   HADDTL,C'Y'         SET WE'VE HAD A DETAIL LINE                  
         SPACE 1                                                                
         TM    GLINDS,GLTOTLIN     IF THIS ISN'T TOTALS                         
         BO    AVPAYO2                                                          
         CLI   GLARGS,C'F'         AND NOT FORCING TOTALS                       
         BE    AVPAYO2                                                          
         EDIT  (P8,0(R2)),(12,(R3)),2,MINUS=YES  PRINT 1ST ACCUM ONLY           
         B     XIT                                                              
         SPACE 1                                                                
AVPAYO2  DS    0H                  PRINT ALL NON-ZERO ACCUMS                    
         LA    R4,ACCLITS          R4=A(LITERALS)                               
         LA    RE,NACCLITS         RE=N'ACCUMS WITHOUT GROSS                    
         ZAP   TGDUB,=P'0'         INIT. RUNNING TOTAL GROSS                    
         SPACE 1                                                                
AVPAYO4  CLI   GLARGS+1,C'X'       IF NOT CALCULATING COMMISSION                
         BNE   *+12                                                             
         CLI   0(R4),C'C'          SKIP IF THIS IS COMMISSION ENTRY             
         BE    AVPAYO7                                                          
         CP    0(8,R2),=P'0'       DON'T BOTHER IF ZERO                         
         BE    AVPAYO6                                                          
         MVC   13(L'ACCLITS-1,R3),1(R4)          DISPLAY LITERAL                
         EDIT  (P8,0(R2)),(12,(R3)),2,MINUS=YES          AMOUNT                 
         LA    R3,198(R3)                                                       
         SPACE 1                                                                
         CLI   0(R4),C'N'          TEST INCLUDED IN GROSS                       
         BE    *+10                                                             
         AP    TGDUB,0(8,R2)       ADD TO TOTAL                                 
         SPACE 1                                                                
AVPAYO6  LA    R2,8(R2)            BUMP TO NEXTS                                
AVPAYO7  LA    R4,L'ACCLITS(R4)                                                 
         BCT   RE,AVPAYO4                                                       
         SPACE 1                                                                
         CLI   0(R4),X'FF'         IF NOT END OF TABLE                          
         BE    *+16                                                             
         LA    R2,TGDUB            MUST BE GROSS - SIMULATE REG OUTPUT          
         LA    RE,1                                                             
         B     AVPAYO4             GO BACK AND PROCESS                          
         SPACE 1                                                                
         CLI   GLARGS,C'F'         FINISHED - IF FORCING TOTALS                 
         BNE   *+8                                                              
         MVI   0(R3),0             SET TO SKIP A LINE                           
         B     XIT                                                              
         SPACE 2                                                                
ACCLITS  DS    0CL19               TOTAL LITERALS                               
         DC    CL19'YPAYMENT'                                                   
         DC    CL19'NAPPLIED CREDITS'                                           
         DC    CL19'NGUARANTEE CREDITS'                                         
         DC    CL19'YPENSION && HEALTH'                                         
         DC    CL19'YTAXES && HANDLING'                                         
         DC    CL19'CAGENCY COMMISSION'                                         
NACCLITS EQU   (*-ACCLITS)/L'ACCLITS                                            
         DC    CL19'NTOTAL GROSS'                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*              OFFICE RECORDS                                                   
         SPACE 3                                                                
OFIN     GOTO1 NEEDAY,DMCB,TIAGY                                                
         MVC   0(1,R2),TIOFF       INPUT                                        
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         BAS   RE,NEEDOF                                                        
         MVC   0(36,R2),NEEDNAME                                                
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
OFOUT    MVC   LABLAREA(6),=C'OFFICE'                                           
         MVC   CODEAREA(1),0(R2)                                                
         BAS   RE,GENBOTH                                                       
         BAS   RE,NEEDOF                                                        
         MVC   NAMEAREA,NEEDNAME   NAME                                         
         B     GENOUT                                                           
         SPACE 1                                                                
NEEDOF   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE OFFICE AROUND                         
         LA    R4,NEEDKEY                                                       
         USING TLOFD,R4                                                         
         MVI   TLOFCD,TLOFCDQ                                                   
         MVC   TLOFOFF,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              AGENCY RECORDS                                                   
         SPACE 3                                                                
AYIN     MVC   0(6,R2),TIAGY       INPUT                                        
         OC    TIAGY,TIAGY         IF WE DON'T HAVE AGENCY                      
         BNZ   AYIN2                                                            
         OC    TICOM,TICOM         AND WE DO HAVE COMMERCIAL                    
         BZ    AYIN2                                                            
         BAS   RE,NEEDCO           GET COMMERCIAL                               
         L     R4,NEEDAREC                                                      
         USING TLCOD,R4                                                         
         MVC   0(6,R2),TLCOAGY     AGENCY IS IN THE KEY                         
         SPACE 1                                                                
AYIN2    CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         GOTO1 NEEDAY,DMCB,TIAGY                                                
         MVC   0(36,R2),NEEDNAME                                                
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
AYOUT    MVC   LABLAREA(6),=C'AGENCY'                                           
         MVC   CODEAREA(6),0(R2)                                                
         BAS   RE,GENBOTH                                                       
         GOTO1 NEEDAY,DMCB,(R2)                                                 
         MVC   NAMEAREA,NEEDNAME   NAME                                         
         B     GENOUT                                                           
         SPACE 1                                                                
NEEDAY   NTR1                                                                   
         L     R2,0(R1)            (ADDRESS OF AGENCY CODE)                     
         XC    NEEDKEY,NEEDKEY     ENSURE AGENCY AROUND                         
         LA    R4,NEEDKEY                                                       
         USING TLAYD,R4                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         MVC   TIAGG,=C'OTHERS'    SET AGENCY GROUP                             
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAAYD,R4                                                         
         MVC   TIOFF,TAAYTPOF                                                   
         CLI   TAAYAGG,X'41'                                                    
         BL    XIT                                                              
         MVC   TIAGG,TAAYAGG                                                    
         B     XIT                                                              
         EJECT                                                                  
*              AGENCY GROUPS                                                    
         SPACE 3                                                                
AGIN     GOTO1 NEEDAY,DMCB,TIAGY                                                
         MVC   0(6,R2),TIAGG                                                    
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         BAS   RE,NEEDAG                                                        
         MVC   0(36,R2),NEEDNAME                                                
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
AGOUT    MVC   LABLAREA(12),=C'AGENCY GROUP'                                    
         MVC   CODEAREA(6),0(R2)                                                
         BAS   RE,GENBOTH                                                       
         BAS   RE,NEEDAG                                                        
         MVC   NAMEAREA,NEEDNAME   NAME                                         
         B     GENOUT                                                           
         SPACE 1                                                                
NEEDAG   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY                                                  
         LA    R4,NEEDKEY                                                       
         USING TLAGD,R4                                                         
         MVI   TLAGCD,TLAGCDQ                                                   
         MVC   TLAGAGG,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              CLIENT RECORDS                                                   
         SPACE 3                                                                
CLIN     MVC   0(6,R2),TICLI       INPUT                                        
         MVC   6(6,R2),TIAGY                                                    
         OC    TICLI,TICLI         IF WE DON'T HAVE CLIENT                      
         BNZ   CLIN6                                                            
         OC    TICOM,TICOM         AND WE DO HAVE COMMERCIAL                    
         BZ    CLIN6                                                            
         BAS   RE,NEEDCO           GET COMMERCIAL                               
         L     R4,NEEDAREC                                                      
         USING TLCOD,R4                                                         
         MVC   0(6,R2),TLCOCLI     CLIENT IS IN THE KEY                         
         SPACE 1                                                                
CLIN6    CLI   GLARGS,C'C'                                                      
         BE    XIT                                                              
         CLI   GLARGS,C'B'                                                      
         BE    XIT                                                              
         GOTO1 NEEDCL,DMCB,(R2)                                                 
         SPACE 1                                                                
LONGSHRT CLI   GLARGS,C'S'         LONG OR SHORT NAMES                          
         BE    SHORTOUT                                                         
         MVC   0(36,R2),NEEDNAME                                                
         B     XIT                                                              
         SPACE 1                                                                
SHORTOUT MVC   0(16,R2),NEEDNAME                                                
         L     R4,NEEDAREC                                                      
         CLI   0(R4),TLCLCDQ                                                    
         BNE   XIT                                                              
         BAS   RE,GETSHORT                                                      
         CLI   NEEDSHRT,C' '                                                    
         BE    XIT                                                              
         MVC   0(16,R2),NEEDSHRT                                                
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
CLOUT    MVC   LABLAREA(6),=C'CLIENT'                                           
         MVC   CODEAREA(6),0(R2)                                                
         BAS   RE,GENBOTH                                                       
         GOTO1 NEEDCL,DMCB,(R2)                                                 
         MVC   NAMEAREA,NEEDNAME   NAME                                         
         B     GENOUT                                                           
         EJECT                                                                  
NEEDCL   NTR1                                                                   
         L     R2,0(R1)            P1=A(CCCCCCAAAAAA)                           
         XC    NEEDKEY,NEEDKEY     ENSURE RECORD AROUND                         
         SPACE 1                                                                
         LA    R4,NEEDKEY                                                       
         USING TLCLD,R4                                                         
         MVI   TLCLCD,TLCLCDQ                                                   
         MVC   TLCLCLI,0(R2)                                                    
         MVC   TLCLAGY,6(R2)                                                    
         MVI   NEEDRSET,C'N'       SET DON'T RESET IF NOT FOUND                 
         BAS   RE,NEEDREC          TRY FOR AGENCY CLIENT                        
         CLI   NEEDHIT,C'Y'                                                     
         BE    XIT                                                              
         XC    TLCLAGY,TLCLAGY                                                  
         BAS   RE,NEEDREC          THEN FOR GLOBAL CLIENT                       
         B     XIT                                                              
         EJECT                                                                  
*              CLIENT GROUPS                                                    
         SPACE 3                                                                
CGIN     MVC   0(6,R2),TICLI       INPUT                                        
         MVC   6(6,R2),TIAGY                                                    
         GOTO1 NEEDCL,DMCB,(R2)                                                 
         MVC   0(6,R2),=C'OTHERS'                                               
         XC    6(6,R2),6(R2)                                                    
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TACIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TACID,R4                                                         
         CLI   TACICLG,X'41'                                                    
         BL    XIT                                                              
         MVC   0(6,R2),TACICLG                                                  
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         BAS   RE,NEEDCG                                                        
         MVC   0(36,R2),NEEDNAME                                                
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
CGOUT    MVC   LABLAREA(12),=C'CLIENT GROUP'                                    
         MVC   CODEAREA(6),0(R2)                                                
         BAS   RE,GENBOTH                                                       
         BAS   RE,NEEDCG                                                        
         MVC   NAMEAREA,NEEDNAME   NAME                                         
         B     GENOUT                                                           
         SPACE 1                                                                
NEEDCG   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY                                                  
         LA    R4,NEEDKEY                                                       
         USING TLCGD,R4                                                         
         MVI   TLCGCD,TLCGCDQ                                                   
         MVC   TLCGCLG,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              PRODUCT RECORDS                                                  
         SPACE 3                                                                
PRIN     MVC   0(6,R2),TICLI       INPUT                                        
         MVC   6(6,R2),TIAGY                                                    
         MVC   12(6,R2),TIPRD                                                   
         CLI   TIPRD,0             IF PRODUCT CODE SPECIFIED                    
         BE    PRIN2                                                            
         CLI   GLARGS,C'C'         AND INTERESTED IN NAME ONLY                  
         BE    XIT                                                              
         CLI   GLARGS,C'B'                                                      
         BE    XIT                                                              
         BAS   RE,NEEDPR           GO GET NAME NOW                              
         B     LONGSHRT                                                         
*                                                                               
PRIN2    XC    0(12,R2),0(R2)      PRODUCT CODE MISSNG                          
         MVI   12(R2),X'FE'        PASS THROUGH X'FE'                           
         CLI   GLARGS,C'C'         AND IF INTERESTED IN NAME AT ALL             
         BE    XIT                                                              
         BAS   RE,NEEDPR           GO GET NAME NOW                              
         CLI   GLARGS,C'B'         AND IF CODE AND NAME                         
         BNE   LONGSHRT                                                         
         MVC   18(36,R2),NEEDNAME  AND SAVE NAME AFTER CODES                    
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
PROUT    MVC   LABLAREA(7),=C'PRODUCT'                                          
         MVC   CODEAREA(6),12(R2)                                               
         CLI   CODEAREA,X'FE'                                                   
         BNE   *+10                                                             
         MVC   CODEAREA(4),=C'NONE'                                             
         BAS   RE,GENBOTH                                                       
         BAS   RE,NEEDPR                                                        
         MVC   NAMEAREA,NEEDNAME   NAME                                         
         B     GENOUT                                                           
         SPACE 1                                                                
NEEDPR   NTR1                                                                   
         MVC   NEEDNAME,MYSPACES                                                
         MVC   NEEDNAME(4),=C'NONE'                                             
         CLI   12(R2),X'FE'        NO PRODUCT AROUND?                           
         BNE   NEEDPR2                                                          
         OC    TIKEY,TIKEY         IF OUTPUT STAGE                              
         BNZ   NEEDPR1                                                          
         CLC   18(36,R2),MYSPACES  AND NAME WAS SET ON INPUT                    
         BNH   XIT                                                              
         MVC   NEEDNAME,18(R2)     USE IT                                       
         B     XIT                                                              
*                                  ELSE, INPUT STAGE                            
NEEDPR1  OC    TICID,TICID         AND COMMERCIAL AROUND                        
         BZ    XIT                                                              
         MVI   NEEDFNTY,TAFNTPRD   SEE IF PRODUCT IS THERE                      
         BAS   RE,NEEDCO                                                        
         MVI   NEEDFNTY,0                                                       
         B     XIT                                                              
         SPACE 1                                                                
NEEDPR2  XC    NEEDKEY,NEEDKEY     ENSURE RECORD AROUND                         
         LA    R4,NEEDKEY                                                       
         USING TLPRD,R4                                                         
         MVI   TLPRCD,TLPRCDQ      TRY FOR AGENCY PRODUCT                       
         MVC   TLPRCLI,0(R2)                                                    
         MVC   TLPRAGY,6(R2)                                                    
         MVC   TLPRPRD,12(R2)                                                   
         MVI   NEEDRSET,C'N'       SET DON'T RESET IF NOT FOUND                 
         BAS   RE,NEEDREC                                                       
         CLI   NEEDHIT,C'Y'                                                     
         BE    XIT                                                              
         XC    TLPRCLI,TLPRCLI                                                  
         XC    TLPRAGY,TLPRAGY                                                  
         BAS   RE,NEEDREC          THEN FOR GLOBAL PRODUCT                      
         B     XIT                                                              
         EJECT                                                                  
*              PRODUCT GROUPS                                                   
         SPACE 3                                                                
PGIN     MVC   0(6,R2),TICLI       INPUT                                        
         MVC   6(6,R2),TIAGY                                                    
         MVC   12(6,R2),TIPRD                                                   
         BAS   RE,NEEDPR                                                        
         MVC   12(6,R2),=C'OTHERS'                                              
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAPIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPID,R4                                                         
         CLI   TAPIPRG,X'41'                                                    
         BL    XIT                                                              
         MVC   12(6,R2),TAPIPRG                                                 
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         BAS   RE,NEEDPG                                                        
         MVC   0(36,R2),NEEDNAME                                                
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
PGOUT    MVC   LABLAREA(13),=C'PRODUCT GROUP'                                   
         MVC   CODEAREA(6),12(R2)                                               
         BAS   RE,GENBOTH                                                       
         BAS   RE,NEEDPG                                                        
         MVC   NAMEAREA,NEEDNAME   NAME                                         
         B     GENOUT                                                           
         SPACE 1                                                                
NEEDPG   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY                                                  
         LA    R4,NEEDKEY                                                       
         USING TLPGD,R4                                                         
         MVI   TLPGCD,TLPGCDQ                                                   
         MVC   TLPGAGY,0(R2)                                                    
         MVC   TLPGCLI,6(R2)                                                    
         MVC   TLPGPRG,12(R2)                                                   
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              COMMERCIAL RECORDS                                               
         SPACE 3                                                                
COIN     DS    0H                                                               
         MVC   0(12,R2),TICID      INPUT - SEE IF CODE IS AROUND                
         CLI   0(R2),0             GO GET COMMERCIAL RECORD IF IT'S NOT         
         BE    COIN2                                                            
         CLI   GLARGS,C'N'         IF ONLY NAME REQ'D THEN MUST GET REC         
         BE    COIN2                                                            
         CLI   GLARGS,C'C'         IF BOTH CODE AND NAME ARE REQ'D              
         BE    *+14                                                             
         MVI   12(R2),0                                                         
         MVC   13(4,R2),TICOM      THEN SAVE INTERNAL COMML NO. FOR O/P         
         B     XIT                                                              
COIN2    BAS   RE,NEEDCO           MAKE SURE COMMERCIAL IS AROUND               
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TACOD,R4                                                         
         MVC   0(12,R2),TACOCID                                                 
         CLI   GLARGS,C'C'                                                      
         BE    XIT                                                              
         L     R4,NEEDAREC                                                      
         BAS   RE,GETNAME                                                       
         CLI   GLARGS,C'N'                                                      
         BE    *+8                                                              
         LA    R2,12(R2)                                                        
         MVC   0(36,R2),NEEDNAME                                                
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
COOUT    MVC   LABLAREA(10),=C'COMMERCIAL'                                      
         CLI   GLARGS+1,C'A'       OPTIONALLY PRINT AD                          
         BNE   *+10                                                             
         MVC   LABLAREA(10),=C'PRINT AD. '                                      
         MVC   CODEAREA(12),0(R2)                                               
         BAS   RE,GENBOTH                                                       
         MVC   NAMEAREA,12(R2)     NAME                                         
         CLI   NAMEAREA,0          IF WE DON'T HAVE NAME                        
         BNE   GENOUT                                                           
         MVC   TICOM,13(R2)        INTERNAL NUMBER SHOULD BE AROUND             
         BAS   RE,NEEDCO           GET COMMERCIAL RECORD                        
         MVC   NAMEAREA,NEEDNAME   TAKE NAME FROM THERE                         
         B     GENOUT                                                           
         SPACE 1                                                                
*                                  SPECIAL CID FROM COMM                        
CIDOUT   MVC   TICOM,0(R2)                                                      
         BAS   RE,NEEDCO           MAKE SURE COMMERCIAL IS AROUND               
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TACOD,R4                                                         
         MVC   0(12,R3),TACOCID    COMMERCIAL ID                                
         B     XIT                                                              
         SPACE 1                                                                
NEEDCO   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     MAKE SURE COMMERCIAL IS AROUND               
         LA    R4,NEEDKEY                                                       
         USING TLCOPD,R4                                                        
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,TICOM                                                   
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              EMPLOYER RECORDS                                                 
         SPACE 3                                                                
EMIN     MVC   0(3,R2),TIEMP       INPUT                                        
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         BAS   RE,NEEDEM                                                        
         MVC   0(36,R2),NEEDNAME                                                
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
EMOUT    MVC   LABLAREA(8),=C'EMPLOYER'                                         
         MVC   CODEAREA(3),0(R2)                                                
         BAS   RE,GENBOTH                                                       
         BAS   RE,NEEDEM                                                        
         MVC   NAMEAREA,NEEDNAME   NAME                                         
         B     GENOUT                                                           
         SPACE 1                                                                
NEEDEM   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE EMPLOYER AROUND                       
         LA    R4,NEEDKEY                                                       
         USING TLEMD,R4                                                         
         MVI   TLEMCD,TLEMCDQ                                                   
         MVC   TLEMEMP,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         SPACE 1                                                                
*                                  CURRENCY                                     
CURROUT  MVC   LABLAREA(8),=C'CURRENCY'                                         
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
         EJECT                                                                  
*              CAST RECORDS - GET AGENT ADDRESS                                 
*                                                                               
AGNTIN   MVC   0(120,R2),MYSPACES                                               
         OC    TIAGT,TIAGT         DOES CAST MEMBER HAVE AN AGENT?              
         BZ    XIT                                                              
         BAS   RE,NEEDAGT          GET AGENT RECORD                             
         BNE   XIT                                                              
         L     R4,AIO                                                           
         MVI   ELCODE,TAADELQ      GET AGENT ADDRESS                            
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAADD,R4                                                         
         ZIC   R1,TAADLNES                                                      
         MHI   R1,30                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TAADADD                                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ESTIMATE RECORDS                                                 
         SPACE 3                                                                
ESIN     MVC   0(20,R2),MYSPACES    INPUT                                       
         L     R4,TIAREC                                                        
         CLI   0(R4),TLESCDQ                                                    
         BNE   XIT                                                              
         USING TLESD,R4                                                         
         MVC   0(20,R2),TLESEST                                                 
         B     XIT                                                              
         SPACE 1                                                                
*              W4 RECORDS                                                       
         SPACE 1                                                                
*              ARGUMENT 1          C=CODE                                       
*                                  N=NAME (OR ADDRESS PIECE)                    
*                                  B=BOTH                                       
*              ARGUMENT 2          P=PAYEE                                      
*              ARGUMENT 3          Y=CITY                                       
*                                  S=STATE                                      
*                                  Z=ZIP                                        
*                                  A=ALL PAYEES                                 
         SPACE 2                                                                
W4IN     MVC   0(9,R2),TISSN       INPUT                                        
         L     R4,TIAREC                                                        
         CLI   0(R4),TLCKCDQ       IF CHECK IS THERE                            
         BNE   W4IN1                                                            
         USING TLCKD,R4                                                         
         MVC   0(9,R2),TLCKSSN        USE THAT SS#                              
                                                                                
W4IN1    CLI   GLARGS+1,C'P'       (ARG2 P=PAYEE SS#)                           
         BNE   W4IN2                                                            
         CLI   GLARGS+2,C'A'       IF WANT ALL PAYEES                           
         BNE   *+10                                                             
         MVC   0(54,R2),MYSPACES   PRE CLEAR                                    
         MVI   ELCODE,TATIELQ      CHECK FOR TAX ID ELEMENT FOR CORP            
         BAS   RE,GETEL                                                         
         BNE   W4IN2                                                            
         USING TATID,R4                                                         
W4IN1D   MVC   0(9,R2),TATIID      SAVE CORP ID                                 
         CLI   GLARGS+2,C'A'       IF WANT ALL PAYEES                           
         BNE   W4IN2                                                            
         LA    R2,9(R2)            BUMP TO NEXT SLOT                            
         BAS   RE,NEXTEL                                                        
         BE    W4IN1D                                                           
         B     XIT                 MUST XIT - R2 ALREADY BUMPED                 
         SPACE 1                                                                
W4IN2    CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         BAS   RE,NEEDW4                                                        
         L     R4,NEEDAREC                                                      
         BAS   RE,GETNAME                                                       
         MVC   0(32,R2),NEEDNAME                                                
         MVC   32(1,R2),NEEDTYPE                                                
         CLI   GLARGS+2,C'A'       ARG3: S(TATE) Z(IP) CIT(Y)                   
         BL    XIT                                                              
         CLI   GLARGS+1,C'P'       IF PAYEE                                     
         BNE   W4IN4                                                            
         CLC   =C'NONE',TIAGT         MAY NEED AGENT                            
         BE    W4IN4                                                            
         BAS   RE,NEEDAGT                                                       
         SPACE 1                                                                
W4IN4    XC    0(33,R2),0(R2)      CLEAR WHAT WE DID SO FAR                     
         L     R4,NEEDAREC         R4 = A(RECORD)                               
         LA    R3,GLARGS+2         R3 = A(ARGUMENT)                             
         BAS   RE,SUBADDR          EXTRACT SUBSIDIARY ADDRESS PART              
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
W4OUT    CLI   GLARGS,C'A'                                                      
         BE    W4OUTADD                                                         
         MVC   LABLAREA(9),=C'PERFORMER'                                        
         CLI   GLARGS,C'N'                                                      
         BNE   W4OUT2                                                           
         CLI   32(R2),TAW4TYCO                                                  
         BE    W4OUT1                                                           
         CLI   32(R2),TAW4TYTR                                                  
         BE    W4OUT1                                                           
         MVC   NAMEAREA(16),16(R2)                                              
         LA    RF,NAMEAREA+17                                                   
         CLI   GLARGS+1,C'F'       FULL NAME                                    
         MVC   NAMEAREA+17(16),0(R2)                                            
         GOTO1 SQUASHER,DMCB,NAMEAREA,33                                        
         B     GENOUT                                                           
         SPACE 1                                                                
W4OUT1   MVC   NAMEAREA(32),0(R2)   CORP                                        
         B     GENOUT                                                           
         SPACE 1                                                                
W4OUT2   CLI   GLARGS,C'S'                                                      
         BE    W4OUT4                                                           
         MVC   CODEAREA(9),0(R2)   CODE                                         
         CLI   GLARGS,C'C'                                                      
         BE    W4OUT5                                                           
         CLI   GLARGS,C'X'                                                      
         BE    W4OUT6                                                           
         BAS   RE,NEEDW4                                                        
         CLI   NEEDTYPE,TAW4TYCO                                                
         BE    W4OUT3                                                           
         CLI   NEEDTYPE,TAW4TYTR                                                
         BE    W4OUT3                                                           
         MVC   NAMEAREA(16),NEEDNAME                                            
         MVC   NAMEAREA+17(16),NEEDNAME+16                                      
         GOTO1 SQUASHER,DMCB,NAMEAREA,33                                        
         B     W4OUT5                                                           
         SPACE 1                                                                
W4OUT3   MVC   NAMEAREA(32),NEEDNAME CORP NAME                                  
         B     W4OUT5                                                           
                                                                                
W4OUT4   TM    TGSYSTAT,TASYSPID                                                
         BO    W4OUT6                                                           
         MVC   CODEAREA(3),0(R2)   SOCIAL SECURITY NUMBER                       
         MVI   CODEAREA+3,C'-'                                                  
         MVC   CODEAREA+4(2),3(R2)                                              
         MVI   CODEAREA+6,C'-'                                                  
         MVC   CODEAREA+7(4),5(R2)                                              
         B     GENOUT                                                           
                                                                                
W4OUT5   TM    TGSYSTAT,TASYSPID                                                
         BZ    GENOUT                                                           
                                                                                
W4OUT6   MVC   TGSSN,0(R2)                                                      
         MVC   CODEAREA(9),MYSPACES                                             
         OC    TGSSN,TGSSN                                                      
         BZ    GENOUT                                                           
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   CODEAREA(L'TGPID),TGPID                                          
         B     GENOUT                                                           
         SPACE 1                                                                
W4OUTADD BAS   RE,NEEDW4                                                        
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAA2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAA2D,R4                                                         
         CLI   GLARGS+1,0          (1-4) IS FOR LINE 1-4                        
         BNE   W4OUTAD4                                                         
         ZIC   R1,MYOLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TAA2ADD1                                                 
         LA    R3,198(R3)                                                       
         SPACE 1                                                                
         CLI   TAA2ADD2,C'A'                                                    
         BL    W4OUTAD2                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TAA2ADD2                                                 
         LA    R3,198(R3)                                                       
         SPACE 1                                                                
W4OUTAD2 CLI   TAA2ADD3,C'A'                                                    
         BL    W4OUTAD3                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TAA2ADD3                                                 
         LA    R3,198(R3)                                                       
         SPACE 1                                                                
W4OUTAD3 MVC   WORK,MYSPACES                                                    
         MVC   WORK(25),TAA2CITY   CITY STATE ZIP                               
         MVC   WORK+26(2),TAA2ST                                                
         MVC   WORK+30(10),TAA2ZIP                                              
         CLC   WORK,MYSPACES                                                    
         BE    XIT                                                              
         GOTO1 SQUASHER,DMCB,WORK,40                                            
         ZIC   R1,MYOLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
         B     XIT                                                              
         SPACE 1                                                                
W4OUTAD4 ZIC   RF,GLARGS+1         SO ADDRESS SPECIFIC LINE                     
         BCTR  RF,0                                                             
         MH    RF,=H'30'                                                        
         LA    RF,TAA2ADDR(RF)                                                  
         ZIC   R1,MYOLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RF)                                                    
         EX    R1,*+8                                                           
         B     XIT                                                              
         OC    0(0,R3),MYSPACES    MAKE SURE PADDED WITH SPACES                 
         DROP  R4                                                               
         SPACE 1                                                                
*                                  OTHER WITHHOLDING                            
INOW     OC    TISSN,TISSN         ONLY USEFUL IF SS# IS AROUND                 
         BZ    XIT                                                              
         LR    R1,R2                                                            
         LA    R2,TISSN                                                         
         BAS   RE,NEEDW4           INSURE WE HAVE W4 RECORD                     
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         LR    R2,R1                                                            
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAOWELQ                                                   
         L     RF,=A(INOW2)                                                     
         B     DRIVEGO                                                          
         SPACE 1                                                                
*                                  WITHHOLDING                                  
INWH     OC    TISSN,TISSN         ONLY USEFUL IF SS# IS AROUND                 
         BZ    XIT                                                              
         LR    R1,R2                                                            
         LA    R2,TISSN                                                         
         BAS   RE,NEEDW4           INSURE WE HAVE W4 RECORD                     
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         LR    R2,R1                                                            
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAWHELQ                                                   
         L     RF,=A(INWH2)                                                     
         B     DRIVEGO                                                          
         SPACE 1                                                                
*                                  W4 DETAIL                                    
INW4     OC    TISSN,TISSN         ONLY USEFUL IF SS# IS AROUND                 
         BZ    XIT                                                              
         LR    R1,R2                                                            
         LA    R2,TISSN                                                         
         BAS   RE,NEEDW4           INSURE WE HAVE W4 RECORD                     
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         LR    R2,R1                                                            
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAW4ELQ                                                   
         L     RF,=A(INW42)                                                     
         B     DRIVEGO                                                          
         SPACE 1                                                                
INWX     OC    TISSN,TISSN         ONLY USEFUL IF SS# IS AROUND                 
         BZ    XIT                                                              
         LR    R1,R2                                                            
         LA    R2,TISSN                                                         
         BAS   RE,NEEDW4           INSURE WE HAVE W4 RECORD                     
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         LR    R2,R1                                                            
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAW4ELQ                                                   
         L     RF,=A(INWX2)                                                     
         B     DRIVEGO                                                          
*                                                                               
NEEDW4   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE W4 AROUND                             
         LA    R4,NEEDKEY                                                       
         USING TLW4D,R4                                                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         SPACE 1                                                                
NEEDAGT  NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE AGENT AROUND                          
         LA    R4,NEEDKEY                                                       
         USING TLANPD,R4                                                        
         MVI   TLANPCD,TLANCCDQ                                                 
         MVC   TLANCAGT,TIAGT                                                   
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              OUTPUT - CORP CODES AND NAMES                                    
         SPACE 3                                                                
CRPSOUT  LA    R5,1190(R3)         R5=AREA FOR CHOPPER (LEAVE ROOM FOR          
*                                  FINAL OUTPUT OF 6 LINES, EACH 198)           
CRPSO5   BAS   RE,NEEDW4                                                        
         L     R4,NEEDAREC                                                      
         BAS   RE,GETNAME                                                       
         MVC   0(9,R5),0(R2)                                                    
         MVC   10(32,R5),NEEDNAME                                               
         LA    R5,43(R5)           BUMP TO NEXT OUTPUT ENTRY                    
         LA    R2,9(R2)            BUMP TO NEXT INPUT ENTRY                     
         OC    0(9,R2),0(R2)                                                    
         BNZ   CRPSO5              REPEAT IF HAVE ANOTHER INPUT                 
         SPACE                                                                  
         GOTO1 MYCHOP,DMCB,1190(R3),(MYOLEN,(R3)),6,C'LEN=',6*43                
         MVC   1190(198,R3),MYSPACES    CLEAR CHOPPER AREA                      
         MVC   1190+198(60,R3),MYSPACES                                         
         B     XIT                                                              
         EJECT                                                                  
*              STAFF RECORDS                                                    
         SPACE 3                                                                
*              ARGUMENT 2          0=STAFF 1=TPC 3=MANAGER                      
         SPACE 1                                                                
STIN     CLI   GLARGS,C'U'         GETTING USER-ID?                             
         BE    STIN3                                                            
         MVC   0(8,R2),TISTAFF     SELECT STAFF CODE                            
         CLI   GLARGS+1,1                                                       
         BL    STIN2                                                            
         MVC   0(8,R2),TITPC                                                    
         BE    STIN2                                                            
         MVC   0(8,R2),TIMGR                                                    
         SPACE 1                                                                
STIN2    CLI   0(R2),X'41'         STAFF CODE MUST EXIST                        
         BL    XIT                                                              
**NO-OP  CLI   TIID,X'F0'          NEED STAFF ID AROUND                         
**08/03  BL    XIT                                                              
**       PACK  DUB,TIID                                                         
**       CVB   R1,DUB                                                           
**       STH   R1,8(R2)                                                         
         CLC   TIID,=H'00'         NEED STAFF ID AROUND                         
         BL    XIT                                                              
         MVC   8(2,R2),TIID                                                     
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         BAS   RE,NEEDST                                                        
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TASTELQ                                                   
         BAS   RE,GETEL                                                         
         USING TASTD,R4                                                         
         MVC   0(12,R2),TASTLST                                                 
         MVC   12(12,R2),TASTFST                                                
         B     XIT                                                              
STIN3    MVC   0(6,R2),MYSPACES                                                 
         L     R4,TIAREC                                                        
         USING TLSTD,R4                                                         
         L     R5,AIO              SAVE AIO                                     
         MVC   AIO,AIO2                                                         
         XC    WORK,WORK                                                        
         MVC   WORK+8(L'TLSTUSER),TLSTUSER                                      
         GOTO1 USERVAL,DMCB,(X'A0',WORK)                                        
         BNE   XIT                                                              
         MVC   0(6,R2),TGUSERID    USER-ID                                      
         ST    R5,AIO              RESTORE AIO                                  
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
STOUT    MVC   LABLAREA(3),=C'TPC'                                              
         CLI   GLARGS+1,1                                                       
         BE    STOUT1                                                           
         MVC   LABLAREA(5),=C'STAFF'                                            
         CLI   GLARGS+1,0                                                       
         BE    STOUT1                                                           
         MVC   LABLAREA(7),=C'MANAGER'                                          
         SPACE 1                                                                
STOUT1   CLI   GLARGS,C'N'                                                      
         BNE   STOUT2                                                           
         MVC   NAMEAREA(12),12(R2)       FIRST NAME                             
         MVC   NAMEAREA+13(12),0(R2)     SECOND NAME                            
         GOTO1 SQUASHER,DMCB,NAMEAREA,25                                        
         B     GENOUT                                                           
         SPACE 1                                                                
STOUT2   MVC   CODEAREA(8),0(R2)   CODE                                         
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         BAS   RE,NEEDST                                                        
         BNE   GENOUT                                                           
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TASTELQ                                                   
         BAS   RE,GETEL                                                         
         USING TASTD,R4                                                         
         MVC   NAMEAREA(12),TASTFST      FIRST NAME                             
         MVC   NAMEAREA+13(12),TASTLST   SECOND NAME                            
         GOTO1 SQUASHER,DMCB,NAMEAREA,25                                        
         B     GENOUT                                                           
*              TPC STAFF                                                        
         SPACE 3                                                                
INCLTPC  DS    0H                                                               
*                                                                               
         MVC   0(10,R2),MYSPACES   INIT OUTPUT                                  
*                                                                               
         LR    R3,R4               ADDRESS OF CLIENT RECORD                     
*                                                                               
         CLI   0(R4),TLCLCDQ       MUST BE CLIENT RECORD                        
         BNE   INCLTPCX                                                         
*                                                                               
         MVI   ELCODE,TACIELQ      FIND CLIENT INFORMATION ELEMENT              
         USING TACID,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   INCLTPCX                                                         
*                                                                               
         CLC   TACITPC,MYSPACES    SKIP IF NO TPC CODE                          
         BNH   INCLTPCX                                                         
*                                                                               
         MVC   L'TACITPC(L'TACITID,R2),TACITID  TPC USERID                      
*                                                                               
         OC    TACITID,TACITID     IF NO USER ID                                
         BNZ   *+10                                                             
         MVC   L'TACITPC(L'TACITID,R2),TGUSER  USE REQUESTOR'S                  
*                                                                               
         MVC   0(L'TACITPC,R2),TACITPC  TPC                                     
*                                                                               
         CLI   GLARGS,C'N'         DONE IF NOT NAME                             
         BNE   INCLTPCX                                                         
*                                                                               
         BAS   RE,NEEDST           FIND STAFF RECORD                            
*                                                                               
         L     R4,NEEDAREC                                                      
*                                                                               
         MVI   ELCODE,TASTELQ                                                   
         BAS   RE,GETEL                                                         
         USING TASTD,R4                                                         
*                                                                               
         MVC   0(12,R2),TASTLST    LAST  NAME                                   
         MVC   12(12,R2),TASTFST   FIRST NAME                                   
         B     INCLTPCX                                                         
*                                                                               
INCLTPCX DS    0H                                                               
         B     XIT                                                              
*                                                                               
*        TPC OUTPUT ROUTINES                                                    
*                                                                               
OUTCLTPC MVC   LABLAREA(3),=C'TPC'                                              
*                                                                               
         CLI   GLARGS,C'N'         IF NAME ONLY                                 
         BNE   OUTCLTP2                                                         
*                                                                               
         MVC   NAMEAREA(12),12(R2)       FIRST NAME                             
         MVC   NAMEAREA+13(12),0(R2)     SECOND NAME                            
*                                                                               
         GOTO1 SQUASHER,DMCB,NAMEAREA,25                                        
*                                                                               
         B     GENOUT                                                           
*                                                                               
OUTCLTP2 MVC   CODEAREA(8),0(R2)   CODE                                         
*                                                                               
         CLI   GLARGS,C'C'         DONE IF CODE ONLY                            
         BE    GENOUT                                                           
*                                                                               
         BAS   RE,NEEDST                                                        
         BNE   GENOUT                                                           
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TASTELQ                                                   
         BAS   RE,GETEL                                                         
         USING TASTD,R4                                                         
         MVC   NAMEAREA(12),TASTFST      FIRST NAME                             
         MVC   NAMEAREA+13(12),TASTLST   SECOND NAME                            
         GOTO1 SQUASHER,DMCB,NAMEAREA,25                                        
         B     GENOUT                                                           
         SPACE 1                                                                
         SPACE 1                                                                
NEEDST   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE STAFF AROUND                          
         LA    R4,NEEDKEY                                                       
         USING TLSTD,R4                                                         
         MVI   TLSTCD,TLSTCDQ                                                   
         MVC   TLSTUSER,8(R2)                                                   
         MVC   TLSTSTAF,0(R2)                                                   
         BAS   RE,NEEDREC          GET STAFF RECORD                             
         CLI   NEEDHIT,C'Y'                                                     
         BNE   NOGOOD                                                           
***NO-OP BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         BAS   RE,GETSHORT                                                      
         SPACE                                                                  
         USING TASTD,R4                                                         
         MVI   ELCODE,TASTELQ      GET STAFF ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVC   TIMGR,TASTMGR       SET MANAGER                                  
         B     XIT                                                              
         EJECT                                                                  
*              AGENT RECORDS                                                    
         SPACE 3                                                                
ANIN     MVC   0(4,R2),TIAGT       INPUT                                        
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         BAS   RE,NEEDAN                                                        
         MVC   0(36,R2),NEEDNAME                                                
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
ANOUT    MVC   LABLAREA(5),=C'AGENT'                                            
         MVC   CODEAREA(4),0(R2)                                                
         BAS   RE,GENBOTH                                                       
         BAS   RE,NEEDAN                                                        
         MVC   NAMEAREA,NEEDNAME   NAME                                         
         B     GENOUT                                                           
         SPACE 1                                                                
NEEDAN   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE AGENT AROUND                          
         LA    R4,NEEDKEY                                                       
         USING TLAND,R4                                                         
         MVI   TLANCD,TLANCDQ                                                   
         MVC   TLANAGT,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              DEAL AREA AND USER RECORDS                                       
         SPACE 3                                                                
DLIN     L     R1,ATHISEL          INPUT                                        
         USING TADLD,R1                                                         
         CLI   0(R1),TADLELQ                                                    
         BNE   XIT                                                              
         LA    R4,NEEDKEY                                                       
         USING TLARD,R4                                                         
         XC    NEEDKEY,NEEDKEY                                                  
         MVI   TLARCD,TLARCDQ                                                   
         MVC   TLARAREA,TADLAREA                                                
         CLI   GLARGS,C'A'                                                      
         BE    DLIN2                                                            
         USING TLUSD,R4                                                         
         XC    NEEDKEY,NEEDKEY                                                  
         MVI   TLUSCD,TLUSCDQ                                                   
         MVC   TLUSUSE,TADLUSE                                                  
         SPACE 1                                                                
DLIN2    BAS   RE,NEEDREC                                                       
         MVC   0(36,R2),NEEDNAME                                                
         B     XIT                                                              
         EJECT                                                                  
*              MUSIC RECORDS                                                    
         SPACE 3                                                                
MUIN     MVC   0(8,R2),TIMUSIC     INPUT                                        
         OC    0(8,R2),MYSPACES                                                 
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         BAS   RE,NEEDMU                                                        
         MVC   0(36,R2),NEEDNAME                                                
         B     XIT                                                              
         SPACE 1                                                                
*                                  OUTPUT                                       
MUOUT    MVC   LABLAREA(5),=C'MUSIC'                                            
         MVC   CODEAREA(8),0(R2)                                                
         BAS   RE,GENBOTH                                                       
         BAS   RE,NEEDMU                                                        
         MVC   NAMEAREA,NEEDNAME   NAME                                         
         B     GENOUT                                                           
         SPACE 1                                                                
INMU     OC    TIMUSIC,TIMUSIC     ONLY USEFUL IS MUSIC CODE AROUND             
         BZ    XIT                                                              
**NOP    OC    TIAGY,TIAGY         SKIP OLD STYLE MUSIC RECORDS                 
**       BZ    XIT                                                              
         XC    NEEDKEY,NEEDKEY     ENSURE MUSIC AROUND                          
         LA    R4,NEEDKEY                                                       
         USING TLMUD,R4                                                         
         MVI   TLMUCD,TLMUCDQ                                                   
         MVC   TLMUMUS,TIMUSIC                                                  
         OC    TLMUMUS,MYSPACES                                                 
         BAS   RE,NEEDREC                                                       
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAMUELQ                                                   
         L     R1,=A(INMUTAB)                                                   
         L     RF,=A(INMU2)                                                     
         B     DRIVEGO                                                          
         SPACE 1                                                                
INMUH    OC    TIMUSIC,TIMUSIC     ONLY USEFUL IF MUSIC CODE AROUND             
         BZ    XIT                                                              
         XC    NEEDKEY,NEEDKEY     ENSURE MUSIC AROUND                          
         LA    R4,NEEDKEY                                                       
         USING TLMUD,R4                                                         
         MVI   TLMUCD,TLMUCDQ                                                   
         MVC   TLMUMUS,TIMUSIC                                                  
         OC    TLMUMUS,MYSPACES                                                 
         BAS   RE,NEEDREC                                                       
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAPMELQ                                                   
         L     R1,=A(INMUHTAB)                                                  
         L     RF,=A(INMUH2)                                                    
         B     DRIVEGO                                                          
         SPACE 1                                                                
INMUT    OC    TIMUSIC,TIMUSIC     ONLY USEFUL IF MUSIC CODE AROUND             
         BZ    XIT                                                              
         XC    NEEDKEY,NEEDKEY     ENSURE MUSIC AROUND                          
         LA    R4,NEEDKEY                                                       
         USING TLMUD,R4                                                         
         MVI   TLMUCD,TLMUCDQ                                                   
         MVC   TLMUMUS,TIMUSIC                                                  
         OC    TLMUMUS,MYSPACES                                                 
         BAS   RE,NEEDREC                                                       
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAPMELQ                                                   
         L     R1,=A(INMUTTAB)                                                  
         L     RF,=A(INMUT2)                                                    
         B     DRIVEGO                                                          
         SPACE 1                                                                
INMUTL   OC    TIMUSIC,TIMUSIC     ONLY USEFUL IF MUSIC CODE AROUND             
         BZ    XIT                                                              
         XC    NEEDKEY,NEEDKEY     ENSURE MUSIC AROUND                          
         LA    R4,NEEDKEY                                                       
         USING TLMUD,R4                                                         
         MVI   TLMUCD,TLMUCDQ                                                   
         MVC   TLMUMUS,TIMUSIC                                                  
         OC    TLMUMUS,MYSPACES                                                 
         BAS   RE,NEEDREC                                                       
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAPMELQ                                                   
         L     R1,=A(INMUTTAB)                                                  
         L     RF,=A(INMUTL2)                                                   
         B     DRIVEGO                                                          
         SPACE 1                                                                
         SPACE 1                                                                
INMUC    OC    TIMUSIC,TIMUSIC     ONLY USEFUL IS MUSIC CODE AROUND             
         BZ    XIT                                                              
         XC    NEEDKEY,NEEDKEY     ENSURE MUSIC AROUND                          
         LA    R4,NEEDKEY                                                       
         USING TLMUD,R4                                                         
         MVI   TLMUCD,TLMUCDQ                                                   
         MVC   TLMUMUS,TIMUSIC                                                  
         OC    TLMUMUS,MYSPACES                                                 
         BAS   RE,NEEDREC                                                       
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         XC    0(120,R2),0(R2)                                                  
         USING TACMD,R4                                                         
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TACMELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         B     *+8                                                              
INMUC5   BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         CLC   TACMTYPE,GLARGS                                                  
         BNE   INMUC5                                                           
         ZIC   R1,TACMLEN                                                       
         SHI   R1,TACMLNQ                                                       
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TACMCOMM                                                 
         B     XIT                                                              
         SPACE 1                                                                
OUTMUC   MVC   BLOCK(120),MYSPACES                                              
         LA    R5,BLOCK                                                         
         LA    RF,2                                                             
OUTMUC5  CLI   0(R2),0                                                          
         BE    OUTMUC10                                                         
         MVC   0(60,R5),0(R2)                                                   
         OC    0(60,R5),MYSPACES                                                
         AHI   R2,60                                                            
         AHI   R5,60                                                            
         BCT   RF,OUTMUC5                                                       
*                                                                               
OUTMUC10 GOTO1 MYCHOP,DMCB,BLOCK,(MYOLEN,(R3)),60,C'LEN=',60*2                  
         B     XIT                                                              
         SPACE 1                                                                
NEEDMU   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE MUSIC AROUND                          
         LA    R4,NEEDKEY                                                       
         USING TLMUD,R4                                                         
         MVI   TLMUCD,TLMUCDQ                                                   
         MVC   TLMUMUS,0(R2)                                                    
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              VERSIONS CODE AND ID                                             
         SPACE 3                                                                
VRIN     DS    0H                                                               
         XC    0(13,R2),0(R2)      PRE-CLEAR FIRST INPUT ENTRY                  
         MVI   ELCODE,TAVRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         SR    R5,R5               R5=COUNT OF # OF VERSIONS                    
         LR    R1,R2               SAVE A(INPUT) IN R1                          
         B     VRIN7                                                            
         SPACE                                                                  
         USING TAVRD,R4                                                         
VRIN5    BAS   RE,NEXTEL                                                        
         BNE   VRINX                                                            
VRIN7    LA    R5,1(R5)            INCREMENT COUNT                              
         CH    R5,=H'19'           IF NOT PAST 19                               
         BH    VRIN10                                                           
         MVC   0(1,R2),TAVRVERS    SAVE VERSION CODE                            
         MVC   1(12,R2),TAVRCID                 ID                              
         LA    R2,13(R2)           BUMP R2                                      
         XC    0(13,R2),0(R2)      CLEAR NEXT INPUT ENTRY                       
         B     VRIN5               SEE IF ANYMORE TAVR ELS                      
         SPACE                                                                  
*                                  > 19 VERSIONS - WON'T FIT IN INPUT           
VRIN10   LR    R2,R1               RESET R2                                     
         MVI   0(R2),0             CLEAR 1ST BYTE                               
         MVC   1(4,R2),TICOM       SAVE INTERNAL COMML NO. FOR O/P              
VRINX    B     XIT                                                              
         SPACE 3                                                                
VROUT    DS    0H                                                               
         OC    0(13,R2),0(R2)          DON'T BOTHER IF NO INPUT                 
         BZ    XIT                                                              
         MVC   BLOCK(198),MYSPACES     INIT BLOCK FOR CHOPPER                   
         MVC   BLOCK+198(87),MYSPACES  MAX OF 19 ENTRIES OF 15 BYTES            
         LA    R5,BLOCK                                                         
         CLI   0(R2),0                 BRANCH IF INPUT IS INT COM NUM           
         BE    VROUT5                                                           
VROUT2   MVC   0(1,R5),0(R2)       ELSE, JUST SET CHOPPER BLOCK                 
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    VROUT3                                                           
         EDIT  (1,0(R2)),(3,0(R5)),ALIGN=LEFT                                   
VROUT3   MVC   2(12,R5),1(R2)                                                   
         LA    R5,15(R5)           BUMP TO NEXT CHOPPER ENTRY                   
         LA    R2,13(R2)           BUMP TO NEXT INPUT ENTRY                     
         OC    0(13,R2),0(R2)      REPEAT IF HAVE ANOTHER INPUT                 
         BNZ   VROUT2                                                           
         B     VROUT10                                                          
         SPACE                                                                  
         USING TAVRD,R4                                                         
VROUT5   MVC   TICOM,1(R2)         SET TICOM                                    
         BAS   RE,NEEDCO           MAKE SURE COMMERCIAL IS AROUND               
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAVRELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VROUT7   BAS   RE,NEXTEL                                                        
         BNE   VROUT10                                                          
         MVC   0(1,R5),TAVRVERS                                                 
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    VROUT8                                                           
         EDIT  TAVRVERS,(3,0(R5)),ALIGN=LEFT                                    
VROUT8   MVC   2(12,R5),TAVRCID                                                 
         LA    R5,15(R5)           BUMP TO NEXT CHOPPER ENTRY                   
         B     VROUT7                                                           
         SPACE                                                                  
VROUT10  GOTO1 MYCHOP,DMCB,BLOCK,(MYOLEN,(R3)),19,C'LEN=',19*15                 
         B     XIT                                                              
         SPACE 2                                                                
TEXT     MVI   0(R2),C' '          TEXT                                         
         B     XIT                                                              
         EJECT                                                                  
*              OTHER SYSIO CODES                                                
         SPACE 3                                                                
W4TIN    MVC   0(1,R2),TIW4TY      W4 TYPE                                      
         B     XIT                                                              
         SPACE 1                                                                
W4TOUT   MVC   LABLAREA(7),=C'W4 TYPE'                                          
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
FILTIN   ZIC   R1,GLARGS           FILTERS (ARG 1-4)                            
         LA    R1,TIFILT1-1(R1)                                                 
         MVC   0(1,R2),0(R1)                                                    
         B     XIT                                                              
         SPACE 1                                                                
FILTOUT  MVC   LABLAREA(6),=C'FILTER'                                           
         MVC   LABLAREA+7(1),GLARGS                                             
         OI    LABLAREA+7,X'F0'                                                 
         MVC   CODEAREA(1),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
CORPIN   MVC   0(9,R2),TICORP      CORPORATION                                  
         B     XIT                                                              
         SPACE 1                                                                
CORPOUT  MVC   LABLAREA(11),=C'CORPORATION'                                     
         MVC   CODEAREA(9),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
CATIN    MVC   0(3,R2),TICAT       CATEGORY                                     
         B     XIT                                                              
         SPACE 1                                                                
CATOUT   MVC   LABLAREA(8),=C'CATEGORY'                                         
         MVC   CODEAREA(3),0(R2)                                                
         SPACE 1                                                                
         CLI   GLARGS,C'N'         IF WANT NAME ONLY                            
         BNE   *+14                                                             
         MVC   CODEAREA,MYSPACES   CLEAR OUT CODE AND NAME                      
         B     *+12                                                             
         CLI   GLARGS,C'B'         IF WANT CODE AND NAME                        
         BNE   GENOUT                                                           
         MVC   NAMEAREA,MYSPACES   GET NAME FROM CATEGORY TABLE                 
         L     R4,TGACATS                                                       
         USING CATTABD,R4                                                       
CATOUT5  CLI   0(R4),X'FF'         TEST END OF TABLE                            
         BE    GENOUT                                                           
         CLC   CATCDE,0(R2)                                                     
         BE    CATOUT8                                                          
         ZIC   R1,CATLEN                                                        
         AR    R4,R1               BUMP TO NEXT CATEGORY ENTRY                  
         B     CATOUT5                                                          
         SPACE 1                                                                
CATOUT8  ZIC   RE,CATLEN           SET CATEGORY NAME                            
         SH    RE,=Y(CATNAME-CATTABD+1)                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   NAMEAREA(0),CATNAME                                              
         B     GENOUT                                                           
         SPACE 1                                                                
ONOFIN   MVC   0(3,R2),TIONOF      ON/OFF CAMERA                                
         B     XIT                                                              
         SPACE 1                                                                
ONOFOUT  MVC   LABLAREA(6),=C'CAMERA'                                           
         MVC   CODEAREA(3),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
EPIN     MVC   0(5,R2),TIEPI       EPISODE NUMBER                               
         B     XIT                                                              
         SPACE 1                                                                
EPOUT    MVC   LABLAREA(7),=C'EPISODE'                                          
         MVC   CODEAREA(5),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
GUAIN    MVC   0(4,R2),TIGUA       GUARANTEE CODE                               
         B     XIT                                                              
         SPACE 1                                                                
GUAOUT   MVC   LABLAREA(9),=C'GUARANTEE'                                        
         MVC   CODEAREA(4),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
LACTIN   MVC   0(8,R2),TILACT      LAST ACTIVE PERSON                           
         B     XIT                                                              
         SPACE 1                                                                
LACTOUT  MVC   LABLAREA(9),=C'LAST ACT.'                                        
         MVC   CODEAREA(8),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
TPCIN    MVC   0(8,R2),TITPC       TPC                                          
         B     XIT                                                              
         SPACE 1                                                                
TPCOUT   MVC   LABLAREA(3),=C'TPC'                                              
         MVC   CODEAREA(8),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
MGRIN    CLI   GLARGS,C'I'         IF WANT MANAGER OF ASSIGNER                  
         BE    MGRIN5                                                           
         CLI   GLARGS,C'P'         OR PAYER                                     
         BE    MGRIN5                                                           
         CLI   GLARGS,C'Q'         OR APPROVER                                  
         BE    MGRIN5                                                           
         MVC   0(8,R2),TIMGR       ELSE SET MGR FROM SYSIO'S STAFF REC          
         B     XIT                                                              
MGRIN5   L     R4,TIAREC                                                        
         CLI   0(R4),TLINCDQ       MUST HAVE INVOICE RECORD                     
         BNE   XIT                                                              
         MVI   ELCODE,TAINELQ      GET INVOICE DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         SPACE                                                                  
         USING TAIND,R4                                                         
         MVC   0(8,R2),TAINIST     SET TO GET STAFF REC OF ASSIGNER             
         MVC   8(2,R2),TAINIID                                                  
         CLI   GLARGS,C'I'                                                      
         BE    MGRIN10                                                          
         MVC   0(8,R2),TAINPST     OR PAYER                                     
         MVC   8(2,R2),TAINPID                                                  
         CLI   GLARGS,C'P'                                                      
         BE    MGRIN10                                                          
         MVC   0(8,R2),TAINQST     OR APPROVER DEPENDING ON ARGUMENTS           
         MVC   8(2,R2),TAINQID                                                  
MGRIN10  BAS   RE,NEEDST                                                        
         MVC   0(8,R2),TIMGR       MANAGER (USERID IS STILL AT 8(R2))           
         B     XIT                                                              
         SPACE 1                                                                
MGROUT   MVC   LABLAREA(7),=C'MANAGER'                                          
         MVC   CODEAREA(8),0(R2)                                                
         BAS   RE,GENBOTH                                                       
         BAS   RE,NEEDST           (EXPECTING USERID AT 8(R2))                  
         MVC   NAMEAREA(16),NEEDSHRT   NAME                                     
         B     GENOUT                                                           
         SPACE 1                                                                
TIMOUT   MVC   LABLAREA(4),=C'TIME'                                             
         MVC   CODEAREA,MYSPACES                                                
         OC    0(3,R2),0(R2)                                                    
         BZ    GENOUT                                                           
         MVC   WORK(6),0(R2)                                                    
         GOTO1 TIMECON,DMCB,WORK,WORK+3,(8,CODEAREA)                            
         B     GENOUT                                                           
         SPACE 1                                                                
UNITIN   MVC   0(3,R2),TIUNIT      (TAX) UNIT                                   
         CLI   GLARGS,C'N'                                                      
         BNE   XIT                                                              
         BAS   RE,NEEDUNIT                                                      
         MVC   0(8,R2),TGTANAME                                                 
         B     XIT                                                              
         SPACE 1                                                                
UNITOUT  MVC   LABLAREA(8),=C'TAX UNIT'                                         
         CLI   GLARGS,C'N'         NAME ONLY?                                   
         BNE   *+14                                                             
         MVC   NAMEAREA(8),0(R2)   MOVE OUT NAME                                
         B     GENOUT                                                           
         MVC   CODEAREA(3),0(R2)                                                
         CLI   GLARGS,C'C'         CODE ONLY?                                   
         BE    GENOUT                                                           
         BAS   RE,NEEDUNIT                                                      
         MVC   NAMEAREA(8),TGTANAME                                             
         B     GENOUT                                                           
         SPACE 1                                                                
NEEDUNIT NTR1                                                                   
         MVC   TGTANAME,=C'UNKNOWN '                                            
         GOTO1 TAXVAL,DMCB,(3,(R2))  LOOK UP TAX UNIT                           
         BE    XIT                                                              
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',(R2))                                         
         B     XIT                                                              
         SPACE 1                                                                
UNIN     MVC   0(3,R2),TIUN        UNION                                        
         B     XIT                                                              
         SPACE 1                                                                
UNOUT    MVC   LABLAREA(5),=C'UNION'                                            
         MVC   CODEAREA(3),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
USEOUT   MVC   LABLAREA(3),=C'USE'                                              
         MVC   CODEAREA(3),0(R2)                                                
         CLI   GLARGS,C'N'                                                      
         BNE   GENOUT                                                           
         MVC   CODEAREA(3),MYSPACES                                             
         MVC   NAMEAREA(16),0(R2)                                               
         B     GENOUT                                                           
         SPACE 1                                                                
LOCLIN   MVC   0(3,R2),TILOCL      LOCAL                                        
         B     XIT                                                              
         SPACE 1                                                                
LOCLOUT  MVC   LABLAREA(5),=C'LOCAL'                                            
         MVC   CODEAREA(3),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
MEDIN    GOTO1 MEDVAL,DMCB,TIMED   MEDIA - EXPAND NAME                          
         MVC   0(5,R2),TGMENAME                                                 
         B     XIT                                                              
         SPACE 1                                                                
PTYPEIN  MVC   0(7,R2),=C'REUSE  ' PAYMENT TYPE                                 
         CLI   TIPTYPE,C'R'                                                     
         BE    XIT                                                              
         MVC   0(7,R2),=C'SESSION'                                              
         CLI   TIPTYPE,C'S'                                                     
         BE    XIT                                                              
         MVC   0(7,R2),MYSPACES                                                 
         B     XIT                                                              
         SPACE 1                                                                
MEDOUT   MVC   LABLAREA(5),=C'MEDIA'                                            
         MVC   CODEAREA(5),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
BNKOUT   MVC   LABLAREA(9),=C'BANK ACCT'                                        
         MVC   CODEAREA(6),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
ACDEOUT  MVC   LABLAREA(10),=C'APPLY CODE'                                      
         MVC   CODEAREA(1),0(R2)                                                
         CLI   GLARGS,C'C'                                                      
         BE    GENOUT                                                           
         MVI   CODEAREA,C' '                                                    
         GOTO1 APPLVAL,DMCB,0(R2)                                               
         MVC   NAMEAREA(15),TGAPTEXT                                            
         B     GENOUT                                                           
         SPACE 1                                                                
*                                  INVOICE NUMBER                               
INVIN    GOTO1 TINVCON,DMCB,TIINV,(R2),DATCON                                   
         B     XIT                                                              
         SPACE 1                                                                
OUTINV   MVC   LABLAREA(7),=C'INVOICE'                                          
         MVC   CODEAREA(6),0(R2)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
DVIN     L     R4,TIAREC           ADVICE NUMBER FROM KEY                       
         USING TLDVD,R4                                                         
         MVC   0(6,R2),TLDVADV                                                  
         B     XIT                                                              
         SPACE 1                                                                
GTIN     L     R4,TIAREC           TRACKING NUMBER FROM KEY                     
         USING TLGTD,R4                                                         
         MVC   0(2,R2),TLGTTRK     (COMPLEMENTED)                               
         CLI   GLARGS,C'R'         (OK IF REVERSE OPTION)                       
         BE    XIT                                                              
         XC    0(2,R2),=X'FFFF'                                                 
         B     XIT                                                              
         SPACE 1                                                                
DSKADDIN MVC   0(4,R2),TIDSKADD    DISK ADDRESS                                 
         B     XIT                                                              
         SPACE 1                                                                
AGYINV   L     R4,TIAREC                                                        
         CLI   0(R4),TLINCDQ                                                    
         BNE   XIT                                                              
         USING TLIND,R4                                                         
         MVC   0(L'TLINAGY,R2),TLINAGY                                          
         B     XIT                                                              
         SPACE 1                                                                
HLDUSE   L     R4,TIAREC                                                        
         CLI   0(R4),TLNXCDQ                                                    
         BNE   XIT                                                              
         USING TLNXD,R4                                                         
         MVC   0(L'TLNXUSE,R2),TLNXUSE                                          
         B     XIT                                                              
         SPACE 1                                                                
ADJINVIN L     R4,TIAREC                                                        
         CLI   0(R4),TLINCDQ                                                    
         BE    ADJINV2                                                          
         CLI   0(R4),TLCKCDQ                                                    
         BE    ADJINV4                                                          
         B     XIT                                                              
         SPACE 1                                                                
         USING TLIND,R4                                                         
ADJINV2  CLC   TLINAGY,=C'999999'                                               
         BNE   XIT                                                              
         MVC   WORK(6),TLININV                                                  
         XC    WORK(6),=6X'FF'                                                  
         GOTO1 TINVCON,DMCB,WORK,(R2),DATCON                                    
         B     XIT                                                              
         SPACE 1                                                                
         USING TLCKD,R4                                                         
ADJINV4  CLC   TLCKAGY,=C'999999'                                               
         BNE   XIT                                                              
         GOTO1 TINVCON,DMCB,TLCKINV,(R2),DATCON                                 
         B     XIT                                                              
         EJECT                                                                  
*              LEADER HANDLING                                                  
         SPACE 3                                                                
LEADIN   CLI   GLARGS,C'S'                                                      
         BE    LEADINS                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLCAD,R4                                                         
         MVI   TLCACD,TLCACDQ                                                   
* NO-OP  MVC   TLCAAGY,TIAGY                                                    
         MVC   TLCACOM,TICOM                                                    
         OI    TLCASORT,X'80'      MUSICIAN                                     
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     MATCH ON AGY/COMM                            
         BNE   LEADINX             THEN, MUST BE A MUSICIAN                     
         SPACE 1                                                                
         MVI   LEADCNT+3,1         (USE THIS AS A SWITCH)                       
         CLI   GLARGS,C'L'                                                      
         BE    LEADINL                                                          
         MVC   0(9,R2),TLCASSN                                                  
         CLI   GLARGS,C'C'         C=CAST SS#                                   
         BE    LEADINC                                                          
*                                  N=NAME                                       
         BAS   RE,NEEDW4                                                        
         MVC   0(32,R2),NEEDNAME                                                
         MVC   32(1,R2),NEEDTYPE                                                
         B     LEADINX                                                          
         SPACE 1                                                                
LEADINL  GOTO1 GETREC              L=CAST LOCAL                                 
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LEADINX                                                          
         USING TACAD,R4                                                         
         MVC   0(3,R2),TACALOCL                                                 
         B     LEADINX                                                          
                                                                                
LEADINC  TM    TGSYSTAT,TASYSPID                                                
         BZ    LEADINX                                                          
         MVC   TGSSN,0(R2)                                                      
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   0(9,R2),MYSPACES                                                 
         MVC   0(L'TGPID,R2),TGPID                                              
                                                                                
LEADINX  MVC   KEY,TIKEY           RESTORE SEQUENCE                             
         GOTO1 HIGH                                                             
         B     XIT                                                              
         SPACE 1                                                                
LEADINS  MVC   0(4,R2),LEADCNT                                                  
         XC    LEADCNT,LEADCNT                                                  
         B     XIT                                                              
         SPACE 1                                                                
LEADCNT  DC    F'0'                                                             
         EJECT                                                                  
*              DATE ROUTINES                                                    
         SPACE 2                                                                
DATOUT   MVC   LABLAREA(4),=C'DATE'                                             
         B     ALLDOUT                                                          
         SPACE 1                                                                
DATOUTRV MVC   LABLAREA(4),=C'DATE'                                             
         MVC   DUB(3),0(R2)                                                     
         XC    DUB(3),=X'FFFFFF'                                                
         LA    R2,DUB                                                           
         B     ALLDOUT                                                          
         SPACE 1                                                                
AIROUT   MVC   LABLAREA(8),=C'AIR DATE'                                         
         MVI   LABLAREA+8,X'41'    TO ALIGN WITH WORK DATE                      
         B     ALLDOUT                                                          
         SPACE 1                                                                
WRKOUT   MVC   LABLAREA(9),=C'WORK DATE'                                        
         B     ALLDOUT                                                          
         SPACE 1                                                                
USEDOUT  MVC   LABLAREA(8),=C'USE DATE'                                         
         B     ALLDOUT                                                          
         SPACE 1                                                                
DUEOUT   MVC   LABLAREA(8),=C'DUE DATE'                                         
         SPACE 1                                                                
ALLDOUT  OC    0(3,R2),0(R2)                                                    
         BZ    XIT                                                              
         LA    R4,CODEAREA                                                      
         ST    R4,DMCB+4                                                        
         MVI   DMCB+4,8                                                         
         TM    DOWNOPT,GLDLACTV                                                 
         BZ    *+8                                                              
         MVI   DMCB+4,10                                                        
         GOTO1 DATCON,DMCB,(1,0(R2))                                            
         B     GENOUT                                                           
         SPACE 1                                                                
MONOUT   MVC   LABLAREA(5),=C'MONTH'                                            
         CLI   GLARGS,2                                                         
         BNE   MONOUT2             OPTION JUST TO DO MONTH                      
         MVI   DUB,1                                                            
         MVC   DUB+1(1),0(R2)                                                   
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(1,DUB),(9,WORK)                                     
         MVC   CODEAREA(3),WORK                                                 
         B     GENOUT                                                           
         SPACE 1                                                                
MONOUT2  MVC   DUB(2),0(R2)                                                     
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(1,DUB),(9,WORK)                                     
         CLI   WORK+3,C'/'                                                      
         BNE   *+10                                                             
         MVC   WORK+3(2),WORK+4                                                 
         MVC   CODEAREA(5),WORK                                                 
         B     GENOUT                                                           
         SPACE 1                                                                
QUARTOUT MVC   LABLAREA(7),=C'QUARTER'                                          
         MVC   CODEAREA,MYSPACES                                                
         UNPK  WORK(3),0(2,R2)                                                  
         ZIC   R1,1(R2)                                                         
         SLL   R1,2                                                             
         LA    R1,QUARTLST(R1)                                                  
         MVC   CODEAREA(4),0(R1)                                                
         B     GENOUT                                                           
         SPACE 1                                                                
QUARTLST DC    C'1ST.2ND.3RD.4TH.   '                                           
         SPACE 1                                                                
USECOUT  CLI   0(R2),X'FE'         USE CYCLE MMMDD/YY-MMMDD/YY                  
         BE    XIT                 (MAY BE NONE)                                
         GOTO1 DATCON,DMCB,(1,(R2)),(8,(R3))                                    
         OC    3(3,R2),3(R2)                                                    
         BZ    XIT                                                              
         CLI   3(R2),X'FE'                                                      
         BE    XIT                                                              
         MVI   8(R3),C'-'                                                       
         GOTO1 DATCON,DMCB,(1,3(R2)),(8,9(R3))                                  
         B     XIT                                                              
         SPACE 1                                                                
YEAROUT  MVC   LABLAREA(4),=C'YEAR'                                             
         MVC   WORK(1),0(R2)                                                    
         MVC   WORK+1(2),=X'0101'                                               
         GOTO1 DATCON,DMCB,(1,WORK),(20,WORK+3)                                 
         MVC   CODEAREA(4),WORK+3                                               
         B     GENOUT                                                           
         EJECT                                                                  
*              TESTING GLARGS - RETURNING OR ON TO GENOUT BELOW                 
         SPACE 3                                                                
*              ARGUMENT 1          N=NAME                                       
*                                  C=CODE                                       
*                                  B=BOTH                                       
         SPACE 1                                                                
GENBOTH  CLI   GLARGS,C'N'         NAME ONLY?                                   
         BNE   GENBOTH2                                                         
         MVC   CODEAREA,MYSPACES   CLEAR OUT CODE                               
         MVC   NAMEAREA,0(R2)      AND MOVE OUT NAME                            
         B     GENOUT                                                           
         SPACE 1                                                                
GENBOTH2 CLI   GLARGS,C'C'         CODE ONLY?                                   
         BE    GENOUT                                                           
         BR    RE                                                               
         EJECT                                                                  
*              SHARED OUTPUT ROUTINE                                            
         SPACE 2                                                                
*              AT THIS STAGE...    LABLAREA HAS PREFIX                          
*                                  CODEAREA HAS CODE                            
*                                  NAMEAREA HAS NAME                            
         SPACE 1                                                                
GENOUT   TM    GLINDS,X'40'        TOTALS ARE DEALT WITH BELOW                  
         BO    TOTOUT                                                           
         CLI   MYLTYP,C'H'         FOR HEADLINES, MOVE OUT THE LOT              
         BNE   GENOUT2                                                          
         CLI   MYCOL,95            IF ON THE LEFT HAND SIDE                     
         BL    GENOUT1                                                          
         GOTO1 SQUASHER,DMCB,OUTAREA,68                                         
         MVC   0(33,R3),OUTAREA    ELSE SQUASHER AND SHOW SOME                  
         B     XIT                                                              
         SPACE 1                                                                
GENOUT1  MVC   0(L'OUTAREA,R3),OUTAREA                                          
         B     XIT                                                              
         SPACE 1                                                                
GENOUT2  CLI   MYLTYP,C'M'         FOR MIDLINES, SQUASH FIRST                   
         BNE   GENOUT4                                                          
         GOTO1 SQUASHER,DMCB,CODENNAM,52                                        
         MVC   0(L'CODENNAM,R3),CODENNAM                                        
         B     XIT                                                              
         SPACE 1                                                                
GENOUT4  SR    R4,R4               ANY CODE TO OUTPUT                           
         CLC   CODEAREA,MYSPACES                                                
         BNH   GENOUT10                                                         
         LA    R4,12                                                            
         LA    R1,CODEAREA+11                                                   
         SPACE 1                                                                
GENOUT6  CLI   0(R1),C' '          FIGURE OUT L'CODE                            
         BH    GENOUT8                                                          
         BCTR  R1,0                                                             
         BCT   R4,GENOUT6                                                       
         SPACE 1                                                                
GENOUT8  BCTR  R4,0                (R4=L'CODE-1)                                
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),CODEAREA    MOVE OUT THE CODE                            
         LA    R4,2(R4)            (NOW HAS LENGTH+1)                           
         AR    R3,R4                                                            
         SPACE 1                                                                
GENOUT10 ZIC   R5,MYOLEN           SET TO CHOP THE NAME                         
         SR    R5,R4                                                            
         BNP   XIT                                                              
         GOTO1 MYCHOP,DMCB,(36,NAMEAREA),((R5),(R3)),4,0                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CALL CHOPPER                                          
*              PARAMETERS PASSED ARE SAME AS CHOPPER                            
         SPACE                                                                  
MYCHOP   NTR1                                                                   
         TM    GLDOWNLD,X'80'                                                   
         BNO   *+12                                                             
         LA    R1,1                ONLY 1 FOR DOWNLOADING                       
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,198          PRINT LINES ARE 198 APART                    
         GOTO1 CHOPPER,DMCB                                                     
         B     XIT                                                              
         EJECT                                                                  
*              SHARED TOTAL ROUTINE                                             
         SPACE 3                                                                
*              AT THIS STAGE...    LABLAREA HAS PREFIX                          
*                                  CODEAREA HAS CODE                            
*                                  NAMEAREA HAS NAME                            
*              INPUT               ROW1WIDE ROWWIDTH                            
         SPACE 1                                                                
TOTOUT   ZIC   R1,GLRECNO          PICK UP PRESENT RECORD NUMBER                
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,GLAINTD(R1)      GET TO DETAILS FOR THIS REPORT               
         L     R1,0(R1)                                                         
         USING GLINTD,R1                                                        
         LH    R4,GLPDISP          NOW PICK UP PRINT DISPLACEMENT               
         A     R4,GLAINTP1         AND GET ACTUAL PRINT ADDRESS                 
         LA    R4,1(R4)                                                         
         DROP  R1                                                               
         SPACE 1                                                                
         ZIC   R1,ROWWIDTH                                                      
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,CLEARP1                                                       
         EX    R1,CLEARP2                                                       
         EX    R1,CLEARP3                                                       
         CLI   ROWWIDTH,16         IF THERE IS ENOUGH ROOM                      
         BL    TOTOUT2                                                          
         MVC   BLOCK(80),MYSPACES                                               
         MVC   BLOCK(11),=C'*TOTALS FOR'                                        
         MVC   BLOCK+12(65),OUTAREA                                             
         GOTO1 SQUASHER,DMCB,BLOCK,80                                           
         LA    R1,4                                                             
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,198                                                       
         ZIC   R3,ROWWIDTH                                                      
         BCTR  R3,0                                                             
         GOTO1 CHOPPER,DMCB,(80,BLOCK),((R3),0(R4))                             
         B     TOTOUTX                                                          
         SPACE 1                                                                
CLEARP1  MVC   000(0,R4),MYSPACES                                               
CLEARP2  MVC   198(0,R4),MYSPACES                                               
CLEARP3  MVC   396(0,R4),MYSPACES                                               
         SPACE 1                                                                
TOTOUT2  L     R1,GLATOUT          PICK UP A(OUT ELEMENT)                       
         USING DROD,R1                                                          
         ZIC   R4,DROLEN           AND USE THE OUT WIDTH                        
         CLI   DROLTYP,C'P'        IF FIELD IS IN THE PRINT LINE                
         BE    TOTOUT4                                                          
         DROP  R1                                                               
         ZIC   R4,ROW1WIDE                                                      
         BCTR  R4,0                                                             
         SPACE 1                                                                
TOTOUT4  CH    R4,=H'3'                                                         
         BL    TOTOUTX                                                          
         MVC   0(3,R3),=C'ALL'                                                  
         CH    R4,=H'5'                                                         
         BL    TOTOUTX                                                          
         MVC   0(5,R3),=C'*ALL*'                                                
         CH    R4,=H'8'                                                         
         BL    TOTOUTX                                                          
         MVC   0(8,R3),=C'*TOTALS*'                                             
         SPACE 1                                                                
TOTOUTX  MVC   OUTAREA,MYSPACES                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ENSURE RECORD IS IN CORE                              
         SPACE 3                                                                
*              INPUT               NEEDKEY IS SET                               
*              OUTPUT              NEEDAREC NEEDNAME                            
         SPACE 1                                                                
NEEDREC  NTR1                                                                   
         MVC   NEEDAREC,TIAREC     SYSIO MAY HAVE THIS RECORD                   
         MVC   NEEDNAME,TINAME                                                  
         MVI   NEEDHIT,C'Y'                                                     
         L     R2,=A(NEEDTAB)                                                   
         OC    TIKEY,TIKEY         (IF WE ARE IN INPUT PHASE)                   
         BZ    NREC2                                                            
         L     R1,TIAREC                                                        
         CLC   NEEDKEY(1),0(R1)                                                 
         BE    NRECX                                                            
         CLC   NEEDKEY(1),TIKEY                                                 
         BE    NRECX                                                            
         SPACE 1                                                                
NREC2    CLI   0(R2),X'FF'                                                      
         BE    NREC4                                                            
         CLC   0(1,R2),NEEDKEY     MATCH ON FIRST BYTE OF KEY                   
         BE    NREC4                                                            
         LA    R2,4(R2)                                                         
         B     NREC2                                                            
         SPACE 1                                                                
NREC4    L     R2,0(R2)            NOW R2 HAS A(BUFFER)                         
*                                  BYTES  1-4 N'ENTRIES                         
*                                  BYTES  5-8 L'ENTRY                           
*                                  BYTES 9-12 NUMBER OF LAST ENTRY              
         LA    R4,12(R2)           BYTES 13+  THE BUFFER!                       
         L     R0,0(R2)                                                         
         SPACE 1                                                                
NREC6    CLC   NEEDKEY,0(R4)       IS MY RECORD IN THE BUFFER?                  
         BE    NREC8                                                            
         CLI   NEEDKEY,TLCOCCDQ    IF LOOKING FOR COMML PASSIVE                 
         BNE   *+14                                                             
         CLC   TICOM,TLCOCOM-TLCOD(R4)  MATCH ONLY ON INT. COMML NO.            
         BE    NREC8                                                            
         A     R4,4(R2)                                                         
         BCT   R0,NREC6                                                         
         SPACE 1                                                                
         MVI   NEEDHIT,C'N'                                                     
         MVC   KEY,NEEDKEY         NO, NOW NEED THE RECORD                      
         GOTO1 HIGH                                                             
         CLC   NEEDKEY,KEY                                                      
         BNE   *+8                                                              
         MVI   NEEDHIT,C'Y'                                                     
         SPACE 1                                                                
         L     R1,8(R2)            NO - PICK UP N'LAST ENTRY                    
         LA    R1,1(R1)                 ROUND ROBIN                             
         C     R1,0(R2)            HAVE WE GOT TO THE END OF BUFFER?            
         BNH   *+8                                                              
         LA    R1,1                YES, SO GO BACK TO THE BEGINNING             
         ST    R1,8(R2)                                                         
         BCTR  R1,0                                                             
         M     R0,4(R2)            DISPLACE INTO THE BUFFER                     
         LA    R4,12(R1,R2)                                                     
         SPACE 1                                                                
         MVC   0(32,R4),NEEDKEY    MOVE KEY TO BUFFER                           
         MVI   32(R4),X'FF'        SET NOT FOUND STATUS IN BUFFER               
         CLI   NEEDHIT,C'Y'                                                     
         BNE   NREC7               DON'T READ RECORD IF NOT FOUND               
         GOTO1 GETREC                                                           
         BAS   RE,SHRNKREC         SHRINK RECORD                                
         L     R3,AIO                                                           
         L     R2,4(R2)                                                         
         MOVE  ((R4),(R2)),(R3)    MOVE INTO OUR AREA                           
         SPACE 1                                                                
NREC7    OC    TIKEY,TIKEY         IF SYSIO IS READING RECORDS                  
         BZ    NREC10                                                           
         TM    TISTAT,TISTRDCK     THEN UNLESS READING CHECK FILE               
         BO    NREC10                                                           
         MVI   NEEDPEND,C'Y'       (SET RESET PENDING)                          
         CLI   NEEDHIT,C'N'        OR UNLESS WE DIDN'T FIND RECORD              
         BNE   *+12                                                             
         CLI   NEEDRSET,C'N'       AND DON'T WANT TO RESET YET                  
         BE    NREC10                                                           
         B     NREC9               RESET READ SEQUENCE                          
         SPACE 1                                                                
NREC8    CLI   NEEDPEND,C'Y'       FOUND IN BUFFER - TEST RESET PENDING         
         BNE   NREC10                                                           
         SPACE 1                                                                
NREC9    MVI   NEEDPEND,C'N'                                                    
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                REREAD TO ESTABLISH SEQUENCE                 
         SPACE 1                                                                
NREC10   CLI   32(R4),X'FF'        IF RECORD MARKED NOT FOUND IN BUFFER         
         BNE   *+8                                                              
         MVI   NEEDHIT,C'N'        SET SWITCH ACCORDINGLY                       
         CLI   NEEDHIT,C'Y'        IF WE FOUND RECORD                           
         BNE   NRECX                                                            
         ST    R4,NEEDAREC         PASS BACK A(RECORD)                          
         BAS   RE,GETNAME                                                       
         SPACE 1                                                                
NRECX    MVI   NEEDRSET,C'Y'       SET TO RESET NEXT TIME                       
         B     XIT                                                              
         SPACE 1                                                                
NEEDAREC DS    A                                                                
NEEDKEY  DC    XL32'00'                                                         
NEEDNAME DS    CL36                                                             
NEEDSHRT DS    CL16                                                             
NEEDHIT  DS    CL1                                                              
NEEDRSET DS    C'Y'                                                             
NEEDPEND DS    C'N'                                                             
NEEDTYPE DS    X'00'                                                            
NEEDFNTY DS    X'00'                                                            
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
GETNAME  NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDNAME                     
         XR    R3,R3                                                            
         CLI   0(R4),TLSTCDQ       SPECIAL CHECK FOR STAFF RECORDS              
         BNE   GETN010                                                          
         LR    R3,R4                                                            
         AR    R3,R2               R2 HOLDS LENGTH OF RECORD SAVED              
*                                                                               
GETN010  MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVC   NEEDNAME,MYSPACES                                                
         CLI   0(R4),TLW4CDQ                                                    
         BE    GETW4NM                                                          
         CLI   NEEDFNTY,0                                                       
         BNE   GETFNM                                                           
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         LTR   R3,R3                                                            
         BZ    GETN020                                                          
         CR    R4,R3               DON'T GO PAST LENGTH SAVED                   
         BNL   NOGOOD                                                           
         USING TANAD,R4                                                         
GETN020  ZIC   R1,TANALEN                                                       
         SH    R1,=H'3'                                                         
         CH    R1,=H'35'                                                        
         BL    *+8                                                              
         LA    R1,35                                                            
         EX    R1,*+8                                                           
         B     ITSFINE                                                          
         MVC   NEEDNAME(0),TANANAME                                             
         SPACE 1                                                                
GETFNM   MVI   ELCODE,TAFNELQ      OPTIONALLY, PASS BACK FREE NAME              
         BAS   RE,GETEL                                                         
         B     GETFNM4                                                          
         SPACE 1                                                                
GETFNM2  MVI   ELCODE,TAFNELQ                                                   
         BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
GETFNM4  MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TAFND,R4                                                         
         CLC   TAFNTYPE,NEEDFNTY   MUST MATCH ON TYPE                           
         BNE   GETFNM4                                                          
         ZIC   R1,TAFNLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     ITSFINE                                                          
         MVC   NEEDNAME(0),TAFNNAME                                             
         SPACE 1                                                                
GETW4NM  MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TAW4D,R4                                                         
         MVC   NEEDNAME(32),TAW4CRPN                                            
         MVC   NEEDTYPE,TAW4TYPE                                                
         B     ITSFINE                                                          
         SPACE 1                                                                
GETSHORT NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDSHRT                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVI   ELCODE,TASNELQ      SHORT NAME                                   
         BAS   RE,GETEL                                                         
         MVC   NEEDSHRT,MYSPACES                                                
         MVC   ELCODE,SAVEEL                                                    
         BNE   XIT                                                              
         USING TASND,R4                                                         
         ZIC   R1,TASNLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   NEEDSHRT(0),TASNAME                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*              SHRINK RECORD                                                    
* SHRINK RECORD SO MOVING A FIXED LENGTH IN "NEEDREC" WILL NOT CAUSE            
* A PROBLEM.  WE DIED ONCE WHEN STAFF WAS ASSUMED 200 BYTES LONG.               
* DELETE:                                                                       
*   TAPWELQ, TAVAELQ                                                            
*   TAACELQ - ONLY KEEP LAST ONE                                                
*----------------------------------------------------------------------         
SHRNKREC NTR1                                                                   
         L     R4,AIO                                                           
         XR    R3,R3               POINTS TO LAST TAACEL                        
         MVI   ELCODE,TAACELQ      ACTIVITY ELEMENTS                            
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SHRNK10  BAS   RE,NEXTEL                                                        
         BNE   SHRNK20                                                          
         LTR   R3,R3               FIRST ONE, MAY BE LAST ONE TOO               
         BZ    *+8                                                              
         MVI   0(R3),X'FF'         MARK ELEMENT FOR DELETION                    
         LR    R3,R4                                                            
         B     SHRNK10                                                          
*                                                                               
SHRNK20  MVI   ELCODE,X'FF'        DELETE ELEMENTS WE DON'T WANT                
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAPWELQ      PASSWORD ELEMENTS                            
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAVAELQ      AGENCY LIMITS                                
         GOTO1 REMELEM                                                          
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*              EXTRA NEED ROUTINES                                              
*----------------------------------------------------------------------         
         SPACE 3                                                                
INAY     OC    TIAGY,TIAGY        AGENCY DATA - NEED AGENCY!                    
         BZ    XIT                                                              
         GOTO1 NEEDAY,DMCB,TIAGY  INSURE WE HAVE AROUND                         
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAAYELQ                                                   
         L     R1,=A(INAYTAB)                                                   
         L     RF,=A(INAY2)                                                     
         B     DRIVEGO                                                          
         SPACE 1                                                                
INBR     L     R1,TIAREC           CHECK RECODE                                 
         CLI   0(R1),TLCLCDQ                                                    
         BE    INBRCL              (DO CLIENTS BELOW)                           
         SPACE 1                                                                
         OC    TIAGY,TIAGY         BILLING DATA - NEED AGENCY                   
         BZ    XIT                                                              
         GOTO1 NEEDAY,DMCB,TIAGY   GET AGENCY                                   
         B     INBRAYCL                                                         
         SPACE 1                                                                
INBRCL   MVC   WORK(6),TICLI       CLIENT LEVEL                                 
         MVC   WORK+6(6),TIAGY                                                  
         GOTO1 NEEDCL,DMCB,WORK    GET CLIENT                                   
         SPACE 1                                                                
INBRAYCL CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TABRELQ                                                   
         L     R1,=A(INBRTAB)                                                   
         L     RF,=A(INBR2)                                                     
         B     DRIVEGO                                                          
         SPACE 1                                                                
INCL     OC    TICLI,TICLI        CLIENT DATA - NEED CLIENT!                    
         BZ    XIT                                                              
         MVC   0(6,R2),TICLI                                                    
         MVC   6(6,R2),TIAGY                                                    
         GOTO1 NEEDCL,DMCB,(R2)   INSURE WE HAVE AROUND                         
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TACIELQ                                                   
         L     R1,=A(INCLTAB)                                                   
         L     RF,=A(INCL2)                                                     
         B     DRIVEGO                                                          
         SPACE 1                                                                
INCO     L     R1,TIAREC           IF NOT PROC. COMML OR INVOICE RECORD         
         BAS   RE,CKCOORIN                                                      
         BE    INCO10                                                           
         L     R1,TIAMAIN                                                       
         BAS   RE,CKCOORIN                                                      
         BE    INCO10                                                           
         OC    TICOM,TICOM         AND HAVE INTERNAL COMML # AROUND             
         BZ    INCO10                                                           
         BAS   RE,NEEDCO           GET THE COMMERCIAL RECORD                    
         L     R4,NEEDAREC                                                      
*                                                                               
INCO10   CLI   GLARGS,1            INTERNAL COMMERCIAL NUMBER                   
         BNE   INCO20                                                           
         MVC   0(4,R2),TICOM                                                    
         B     XIT                                                              
*                                                                               
INCO20   MVI   ELCODE,TACOELQ                                                   
         L     R1,=A(INCOTAB)                                                   
         L     RF,=A(INCO2)                                                     
         B     DRIVEGO                                                          
*                                                                               
INCS     L     R1,TIAREC           IF NOT PROC. COMML OR INVOICE RECORD         
         BAS   RE,CKCOORIN                                                      
         BE    INCS10                                                           
         L     R1,TIAMAIN                                                       
         BAS   RE,CKCOORIN                                                      
         BE    INCS10                                                           
         OC    TICOM,TICOM         AND HAVE INTERNAL COMML # AROUND             
         BZ    INCS10                                                           
         BAS   RE,NEEDCO           GET THE COMMERCIAL RECORD                    
         L     R4,NEEDAREC                                                      
INCS10   MVI   ELCODE,TACSELQ                                                   
         L     R1,=A(INCSTAB)                                                   
         L     RF,=A(INCS2)                                                     
         B     DRIVEGO                                                          
*                                                                               
CKCOORIN DS    0H                                                               
         CLI   0(R1),TLCOCDQ       IF NOT READING COMMERCIALS                   
         BER   RE                                                               
         CLI   0(R1),TLINCDQ       OR INVOICE RECORDS                           
         BER   RE                                                               
         BR    RE                  SET CC NOT EQUAL                             
*                                                                               
INCP     L     R1,TIAREC           IF DON'T HAVE COMMERCIAL RECORD              
         CLI   0(R1),TLCOCDQ                                                    
         BE    INCP10                                                           
         L     R1,TIAMAIN                                                       
         CLI   0(R1),TLCOCDQ                                                    
         BE    INCP10                                                           
         OC    TICOM,TICOM         AND HAVE INTERNAL COMML # AROUND             
         BZ    INCP10                                                           
         BAS   RE,NEEDCO           GET THE COMMERCIAL RECORD                    
         L     R4,NEEDAREC                                                      
INCP10   MVI   ELCODE,TACPELQ                                                   
         L     R1,=A(INCPTAB)                                                   
         L     RF,=A(INCP2)                                                     
         B     DRIVEGO                                                          
*                                                                               
INCC     L     R1,TIAREC           IF DON'T HAVE COMMERCIAL RECORD              
         CLI   0(R1),TLCOCDQ                                                    
         BE    INCC10                                                           
         L     R1,TIAMAIN                                                       
         CLI   0(R1),TLCOCDQ                                                    
         BE    INCC10                                                           
         OC    TICOM,TICOM         AND HAVE INTERNAL COMML # AROUND             
         BZ    INCC10                                                           
         BAS   RE,NEEDCO           GET THE COMMERCIAL RECORD                    
         L     R4,NEEDAREC                                                      
INCC10   BAS   RE,ADJTAMCS                                                      
         MVI   ELCODE,TAMCELQ                                                   
         L     R1,=A(INCCTAB)                                                   
         L     RF,=A(INCC2)                                                     
         B     DRIVEGO                                                          
*                                                                               
INMC     L     R1,TIAREC           IF DON'T HAVE COMMERCIAL RECORD              
         CLI   0(R1),TLCOCDQ                                                    
         BE    INMC10                                                           
         L     R1,TIAMAIN                                                       
         CLI   0(R1),TLCOCDQ                                                    
         BE    INMC10                                                           
         OC    TICOM,TICOM         AND HAVE INTERNAL COMML # AROUND             
         BZ    INMC10                                                           
         BAS   RE,NEEDCO           GET THE COMMERCIAL RECORD                    
         L     R4,NEEDAREC                                                      
INMC10   BAS   RE,ADJTAMCS                                                      
         MVI   ELCODE,TAMCELQ                                                   
         L     R1,=A(INMCTAB)                                                   
         L     RF,=A(INMC2)                                                     
         B     DRIVEGO                                                          
                                                                                
ADJTAMCS NTR1                                                                   
         GOTO1 ADJTAMC,DMCB,TIAREC                                              
         GOTO1 ADJTAMC,DMCB,TIAMAIN                                             
         GOTO1 ADJTAMC,DMCB,NEEDAREC                                            
         B     XIT                                                              
                                                                                
ADJTAMC  NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         USING TACOD,R4                                                         
         LR    R4,R2                                                            
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         CLI   TACOTYPE,CTYMUS                                                  
         BNE   ATAMC20                                                          
         MVC   WORK(L'TACOCID),TACOCID                                          
         DROP  R4                                                               
                                                                                
         USING TAMCD,R4                                                         
         LR    R4,R2                                                            
         MVI   ELCODE,TAMCELQ      BUILD NEW STYLE ELEMENT FROM OLD             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
ATAMC10  BAS   RE,NEXTEL                                                        
         BNE   ATAMC20                                                          
         OC    TAMCCON,TAMCCON                                                  
         BNZ   ATAMC10                                                          
         MVC   TAMCCON,WORK                                                     
         B     ATAMC10                                                          
         DROP  R4                                                               
                                                                                
         USING TACCD,R4                                                         
ATAMC20  LR    R4,R2                                                            
         MVI   ELCODE,TACCELQ                                                   
         BAS   RE,GETEL                                                         
         BE    XIT                 HAVE NEW STYLE ELEMENT, LEAVE                
                                                                                
         USING TAMCD,R4                                                         
         LR    R4,R2                                                            
         MVI   ELCODE,TAMCELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
                                                                                
         USING TACCD,R3                                                         
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TACCEL,TACCELQ                                                   
         MVI   TACCLEN,TACCLNQ                                                  
         LA    R5,TACCCON                                                       
                                                                                
ATAMC30  OC    TAMCCON,TAMCCON                                                  
         BZ    ATAMC40                                                          
                                                                                
         MVC   0(L'TACCCON,R5),TAMCCON                                          
         ZIC   R1,TACCNCON                                                      
         AHI   R1,1                                                             
         STC   R1,TACCNCON                                                      
         ZIC   R1,TACCLEN                                                       
         AHI   R1,L'TACCCON                                                     
         STC   R1,TACCLEN                                                       
                                                                                
         LA    R5,L'TACCCON(R5)                                                 
                                                                                
ATAMC40  BAS   RE,NEXTEL                                                        
         BE    ATAMC30                                                          
                                                                                
         CLI   TACCNCON,0                                                       
         BE    XIT                                                              
         GOTO1 HELLO,DMCB,(C'P',=C'TALFIL'),(R2),(R3),0                         
         B     XIT                                                              
         DROP  R3                                                               
*                                                                               
INDU     OC    TIDUC,TIDUC         ONLY USEFUL IS DUE CODE PRESENT              
         BZ    XIT                                                              
         BAS   RE,NEEDDU           INSURE WE HAVE DUE COMPANY RECORD            
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TADUELQ                                                   
         L     R1,=A(INDUTAB)                                                   
         L     RF,=A(INDU2)                                                     
         B     DRIVEGO                                                          
         SPACE 1                                                                
NEEDDU   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE DUE COMPANY RECORD AROUND             
         LA    R4,NEEDKEY                                                       
         USING TLDUD,R4                                                         
         MVI   TLDUCD,TLDUCDQ                                                   
         MVC   TLDUSSN,TISSN                                                    
         LA    R1,TLDUSSN                                                       
         BAS   RE,NEEDDUSS                                                      
         MVC   TLDUDUC,TIDUC                                                    
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         SPACE 1                                                                
INEI     OC    TIEPI,TIEPI         EPISODES NEED EPISODE NUMBER                 
         BZ    XIT                                                              
         OC    TIAGY,TIAGY         AND AGENCY - IF NO AGENCY                    
         BNZ   INEI10                                                           
         OC    TICOM,TICOM                                                      
         BZ    XIT                                                              
         BAS   RE,NEEDCO           GET COMMERCIAL RECORD                        
         L     R4,NEEDAREC                                                      
         USING TLCOD,R4                                                         
         MVC   TIAGY,TLCOAGY       SET AGENCY                                   
*                                                                               
INEI10   BAS   RE,NEEDEPI          INSURE WE HAVE EPISODE RECORD                
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAEIELQ                                                   
         L     R1,=A(INEITAB)                                                   
         L     RF,=A(INEI2)                                                     
         B     DRIVEGO                                                          
         SPACE                                                                  
NEEDEPI  NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE EPISODE RECORD AROUND                 
         LA    R4,NEEDKEY                                                       
         USING TLEPD,R4                                                         
         MVI   TLEPCD,TLEPCDQ                                                   
         MVC   TLEPAGY,TIAGY                                                    
         MVC   TLEPEPI,TIEPI                                                    
         XC    TLEPEPI,=6X'FF'                                                  
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         SPACE                                                                  
INEMAGY  OC    TIAGY,TIAGY        AGENCY EMAIL - NEED AGENCY!                   
         BZ    XIT                                                              
         GOTO1 NEEDAY,DMCB,TIAGY  INSURE WE HAVE AROUND                         
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TACMELQ                                                   
         L     R1,=A(INCMTAB)                                                   
         L     RF,=A(INCM)                                                      
         B     DRIVEGO                                                          
         SPACE 1                                                                
INEMCLI  OC    TICLI,TICLI        CLIENT EMAIL - NEED CLIENT!                   
         BZ    XIT                                                              
         MVC   WORK(6),TICLI                                                    
         MVC   WORK+6(6),TIAGY                                                  
         GOTO1 NEEDCL,DMCB,WORK   INSURE WE HAVE AROUND                         
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TACMELQ                                                   
         L     R1,=A(INCMTAB)                                                   
         L     RF,=A(INCM)                                                      
         B     DRIVEGO                                                          
*                                                                               
INEMPRD  OC    TIPRD,TIPRD         PRODUCT EMAIL - NEED PRODUCT!                
         BZ    XIT                                                              
         BAS   RE,NEEDPRD          ENSURE PRODUCT RECORD AROUND                 
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TACMELQ                                                   
         L     R1,=A(INCMTAB)                                                   
         L     RF,=A(INCM)                                                      
         B     DRIVEGO                                                          
         DROP  R4                                                               
         SPACE 1                                                                
NEEDPRD  NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE PRODUCT RECORD AROUND                 
         LA    R4,NEEDKEY                                                       
         USING TLPRD,R4                                                         
         MVI   TLPRCD,TLPRCDQ      TRY FOR AGENCY PRODUCT                       
         MVC   TLPRCLI,TICLI                                                    
         MVC   TLPRAGY,TIAGY                                                    
         MVC   TLPRPRD,TIPRD                                                    
         MVI   NEEDRSET,C'N'       SET DON'T RESET IF NOT FOUND                 
         BAS   RE,NEEDREC                                                       
         CLI   NEEDHIT,C'Y'                                                     
         BE    XIT                                                              
         XC    TLPRCLI,TLPRCLI                                                  
         XC    TLPRAGY,TLPRAGY                                                  
         BAS   RE,NEEDREC          THEN FOR GLOBAL PRODUCT                      
         B     XIT                                                              
*                                                                               
INEMATT  OC    TICOM,TICOM         ATTN EMAIL - MUST HAVE INT COMML #           
         BZ    XIT                                                              
         BAS   RE,NEEDCO           GET THE COMMERCIAL RECORD                    
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TACOD,R4                                                         
         OC    TACOATT,TACOATT    IF NO ATTN CODE, EXIT                         
         BZ    XIT                                                              
         MVC   HALF,TACOATT       SAVE ATTENTION CODE (BILL-TO)                 
         DROP  R4                                                               
*                                                                               
         BAS   RE,NEEDATT         GET ATTENTION RECORD                          
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TACMELQ                                                   
         L     R1,=A(INCMTAB)                                                   
         L     RF,=A(INCM)                                                      
         B     DRIVEGO                                                          
         SPACE 1                                                                
NEEDATT  NTR1                                                                   
         XC    NEEDKEY,NEEDKEY    GET ATTENTION RECORD                          
         LA    R4,NEEDKEY                                                       
         USING TLATD,R4                                                         
         MVI   TLATCD,TLATCDQ                                                   
         MVC   TLATAGY,TIAGY      AGENCY                                        
         MVC   TLATATT,HALF       ATTN CODE                                     
         MVI   NEEDRSET,C'N'      SET DON'T RESET IF NOT FOUND                  
         BAS   RE,NEEDREC                                                       
         DROP  R4                                                               
         B     XIT                                                              
         SPACE 1                                                                
*                                                                               
INEMSTF  CLC   TIID,=H'00'         STAFF EMAIL - MUST HAVE STAFF ID             
         BL    XIT                                                              
         BAS   RE,NEEDST           GET STAFF RECORD                             
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TACMELQ                                                   
         L     R1,=A(INCMTAB)                                                   
         L     RF,=A(INCM)                                                      
         B     DRIVEGO                                                          
         SPACE 1                                                                
INGU     OC    TIGUA,TIGUA         GUARANTEES - NEED CODES                      
         BZ    XIT                                                              
         OC    TISSN,TISSN                                                      
         BZ    XIT                                                              
         BAS   RE,NEEDGU           INSURE WE HAVE DUE COMPANY RECORD            
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAGUELQ                                                   
         L     R1,=A(INGUTAB)                                                   
         L     RF,=A(INGU2)                                                     
         B     DRIVEGO                                                          
         SPACE 1                                                                
NEEDGU   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE GUARANTEE RECORD AROUND               
         LA    R4,NEEDKEY                                                       
         USING TLGUD,R4                                                         
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,TISSN                                                    
         MVC   TLGUGUA,TIGUA                                                    
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         SPACE 1                                                                
INEP     OC    TIAGY,TIAGY         NEED ESTIMATE PROFILE RECORD                 
         BZ    XIT                                                              
         BAS   RE,NEEDEP           INSURE WE HAVE EST PROFILE                   
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TAEPELQ                                                   
         L     R1,=A(INEPTAB)                                                   
         L     RF,=A(INEP2)                                                     
         B     DRIVEGO                                                          
         SPACE 1                                                                
NEEDEP   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE WE HAVE EST PROFILE                   
         LA    R4,NEEDKEY                                                       
         USING TLCTD,R4                                                         
         MVI   TLCTCD,TLCTCDQ                                                   
         MVC   TLCTAGY,TIAGY                                                    
         MVC   TLCTCLI,TICLI       TRY CLIENT SPECIFIC                          
         MVI   NEEDRSET,C'N'       SET DON'T RESET IF NOT FOUND                 
         BAS   RE,NEEDREC                                                       
         CLI   NEEDHIT,C'Y'                                                     
         BE    XIT                                                              
         XC    TLCTCLI,TLCTCLI     THEN AGENCY OVERRIDE                         
         MVI   NEEDRSET,C'N'       SET DON'T RESET IF NOT FOUND                 
         BAS   RE,NEEDREC                                                       
         CLI   NEEDHIT,C'Y'                                                     
         BE    XIT                                                              
         XC    TLCTAGY,TLCTAGY     THEN DEFAULT RECORD                          
         BAS   RE,NEEDREC                                                       
         CLI   NEEDHIT,C'Y'                                                     
         BE    XIT                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
NEEDDUSS NTR1                                                                   
         L     R4,TIAREC                                                        
         CLI   0(R4),TLCKCDQ       IF WE HAVE A CHECK RECORD...                 
         BNE   XIT                                                              
         MVI   ELCODE,TATIELQ      CHECK FOR TAX ID ELEMENT                     
         BAS   RE,GETEL                                                         
         MVI   ELCODE,0                                                         
         BNE   XIT                                                              
         USING TATID,R4                                                         
         MVC   0(9,R1),TATIID      AND RETURN SS#                               
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
INLN     OC    TILNC,TILNC         ONLY USEFUL IS LIEN CODE PRESENT             
         BZ    XIT                                                              
         BAS   RE,NEEDLN           INSURE WE HAVE LIEN RECORD                   
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TALNELQ                                                   
         L     R1,=A(INLNTAB)                                                   
         L     RF,=A(INLN2)                                                     
         B     DRIVEGO                                                          
         SPACE 1                                                                
NEEDLN   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE LIEN RECORD AROUND                    
         LA    R4,NEEDKEY                                                       
         USING TLLND,R4                                                         
         MVI   TLLNCD,TLLNCDQ                                                   
         MVC   TLLNSSN,TISSN                                                    
         LA    R1,TLLNSSN                                                       
         BAS   RE,NEEDDUSS                                                      
         MVC   TLLNLIN,TILNC                                                    
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
INCAROL  L     RF,TIAREC                                                        
         OC    TISSN,TISSN        SSN NUMBER                                    
         BZ    XIT                                                              
         OC    TICOM,TICOM        INT COMM NUMBER                               
         BZ    XIT                                                              
         OC    TICAT,TICAT        CATEGORY                                      
         BZ    XIT                                                              
         OC    TICASEQ,TICASEQ    CAST SEQUENCE NUMBER                          
         BZ    XIT                                                              
*                                                                               
         MVC   WORK(9),TISSN                                                    
         MVC   WORK+9(4),TICOM                                                  
         MVC   WORK+13(3),TICAT                                                 
         MVC   WORK+16(2),TICASEQ                                               
*                                                                               
         GOTO1 NEEDCA,DMCB,WORK   INSURE WE HAVE AROUND                         
         CLI   NEEDHIT,C'Y'                                                     
         BNE   XIT                                                              
         L     R4,NEEDAREC                                                      
         MVI   ELCODE,TACMELQ                                                   
         L     R1,=A(INCMTAB)                                                   
         L     RF,=A(INCM)                                                      
         B     DRIVEGO                                                          
*                                                                               
NEEDCA   NTR1                                                                   
         L     R2,0(R1)            P1=A(SSSSSSSSSIIIICCCQQ)                     
         XC    NEEDKEY,NEEDKEY     ENSURE CAST AROUND                           
         LA    R4,NEEDKEY                                                       
         USING TLCAPD,R4                                                        
         MVI   TLCAPCD,TLCACCDQ    (EMPLOYEE'S COMMLS)                          
         MVC   TLCACSSN,0(R2)      SS#                                          
         MVC   TLCACCOM,9(R2)      INTERNAL COMML NUMBER                        
         MVC   TLCACCAT,13(R2)     CAST CAT                                     
         MVC   TLCACSEQ,16(R2)     SEQ NUMBER                                   
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              THIS IS THE END OF DRIVE(1)                                      
         SPACE 3                                                                
ITSFINE  SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NOGOOD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         DS    0D                  REQ'D FOR PL16                               
PL16     DC    PL16'0'                                                          
MYSPACES DC    CL198' '                                                         
MYMONTHS DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
SAVEEL   DC    X'00'                                                            
HADDTL   DC    C'N'                                                             
         LTORG                                                                  
         DROP  RB                                                               
         DROP  R8                                                               
         DROP  R7                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              THIS IS THE START OF DRIVE2                                      
         SPACE 3                                                                
         ENTRY DRIVE2                                                           
         DS    0D                                                               
DRIVE2   LR    RB,RE                                                            
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         LA    R7,2048(R8)                                                      
         LA    R7,2048(R7)                                                      
         LA    R6,2048(R7)                                                      
         LA    R6,2048(R6)                                                      
         USING DRIVE2,RB,R8,R7,R6                                               
         BRAS  RE,COLFILT          DO WE WANT THIS AT ALL                       
         BNE   XIT2                                                             
         MVI   ELTYPE,0                                                         
         BR    RF                                                               
         EJECT                                                                  
*              INPUT - ELEMENT RELATED                                          
         SPACE 3                                                                
INKEY    L     R1,=A(INKEYTAB)                                                  
         B     GENDATA                                                          
         SPACE 1                                                                
INAK     MVI   ELCODE,TAAKELQ                                                   
         L     R1,=A(INAKTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INAN     MVI   ELCODE,TAANELQ                                                   
         L     R1,=A(INANTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
         USING TAATD,R4                                                         
INAT     L     R4,ATHISEL          WE WILL BE PASSED A(ELEMENT)                 
         LTR   R4,R4               IF NOT, FORGET IT                            
         JZ    XIT                                                              
         CLI   0(R4),TACWELQ       ONLY TACW/CN & QC FOR NOW                    
         JE    INAT10                                                           
         CLI   0(R4),TAATELQ       ONLY ADDITIONAL TAX ELEMENTS                 
         JNE   XIT                                                              
                                                                                
         CLI   GLARGS,2                                                         
         BNH   INAT5                                                            
         CLI   GLARGS,8                                                         
         JNL   INAT3                                                            
         CLC   TAATUNIT,=C'QC '                                                 
         JE    XIT                                                              
         J     INAT5                                                            
                                                                                
INAT3    CLI   GLARGS,8                                                         
         BL    INAT5                                                            
         CLI   GLARGS,11                                                        
         BH    INAT5                                                            
         CLC   TAATUNIT,=C'QC '                                                 
         JNE   XIT                                                              
                                                                                
INAT5    CLI   GLARGS,1                                                         
         BE    INATUNIT                                                         
         CLI   GLARGS,2                                                         
         BE    INATPUNT                                                         
         ICM   R1,15,TAATTAX                                                    
         CLI   GLARGS,3                                                         
         BE    INATCAN                                                          
         CLI   GLARGS,4                                                         
         BE    INATPRV                                                          
         CLI   GLARGS,8                                                         
         BE    INATPRV                                                          
         CLI   GLARGS,12                                                        
         BE    INATPRV                                                          
         ICM   R1,15,TAATPP                                                     
         CLI   GLARGS,5                                                         
         BE    INATPRV                                                          
         CLI   GLARGS,9                                                         
         BE    INATPRV                                                          
         CLI   GLARGS,13                                                        
         BE    INATPRV                                                          
         ICM   R1,15,TAATEI                                                     
         CLI   GLARGS,6                                                         
         BE    INATPRV                                                          
         CLI   GLARGS,10                                                        
         BE    INATPRV                                                          
         CLI   GLARGS,14                                                        
         BE    INATPRV                                                          
         ICM   R1,15,TAATPIP                                                    
         CLI   GLARGS,7                                                         
         BE    INATPRV                                                          
         CLI   GLARGS,11                                                        
         BE    INATPRV                                                          
         CLI   GLARGS,15                                                        
         BE    INATPRV                                                          
         J     XIT                                                              
         USING TACWD,R4                                                         
INAT10   CLI   GLARGS,3                                                         
         JE    INAT20                                                           
         CLI   GLARGS,8                                                         
         JE    INAT30                                                           
         JNE   XIT                                                              
INAT20   CLC   TACWUNIT,=C'CN '                                                 
         JNE   XIT                                                              
         J     INAT40                                                           
INAT30   CLC   TACWUNIT,=C'QC '                                                 
         JNE   XIT                                                              
INAT40   ICM   R1,15,TACWTAX                                                    
         J     CVD2                                                             
         SPACE 1                                                                
INAY2    L     R1,=A(INAYTAB)      (SOME WORK DONE IN DRIVE1)                   
         B     GENIN                                                            
         SPACE 1                                                                
INBD     MVI   ELCODE,TABDELQ                                                   
         L     R1,=A(INBDTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INCH     MVI   ELCODE,TABDELQ2     CHLOE KEYWORDS                               
         L     R1,=A(INCHTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INBR2    L     R1,=A(INBRTAB)      (SOME WORK DONE IN DRIVE1)                   
         B     GENIN                                                            
         SPACE 1                                                                
INCA     MVI   ELCODE,TACAELQ                                                   
         L     R1,=A(INCATAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INCC2    MVI   ELCODE,TACCELQ                                                   
         L     R1,=A(INCCTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INMC2    L     R1,=A(INMCTAB)                                                   
         B     INMCCON                                                          
         SPACE 1                                                                
INCD     MVI   ELCODE,TACDELQ                                                   
         L     R1,=A(INCDTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INCL2    DS    0H                                                               
         L     R1,=A(INCLTAB)                                                   
         B     INCLSTAT                                                         
         SPACE 1                                                                
INCM     MVI   ELCODE,TACMELQ                                                   
         L     R1,=A(INCMTAB)                                                   
         MVC   ELTYPE,GLARGS+1                                                  
         B     GENIN                                                            
         SPACE 1                                                                
INCO2    L     R1,=A(INCOTAB)      (SOME WORK DONE IN DRIVE1)                   
         B     GENIN                                                            
         SPACE 1                                                                
INCR     MVI   ELCODE,TACRELQ                                                   
         L     R1,=A(INCRTAB)                                                   
         CLI   TIKEY,TLECCDQ                                                    
         BE    GENIN                                                            
         B     GENIN1                                                           
         SPACE 1                                                                
INCS2    L     R1,=A(INCSTAB)      (SOME WORK DONE IN DRIVE1)                   
         MVC   ELTYPE,GLARGS+1                                                  
         B     GENIN                                                            
         SPACE 1                                                                
INCP2    L     R1,=A(INCPTAB)      (SOME WORK DONE IN DRIVE1)                   
         B     GENIN                                                            
         SPACE 1                                                                
INCX     MVI   ELCODE,TACXELQ                                                   
         L     R1,=A(INCXTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INDD     MVI   ELCODE,TADDELQ                                                   
         L     R1,=A(INDDTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INDL     DS    0H                                                               
*&&DO                                                                           
         L     R4,ATHISEL                                                       
         LTR   R4,R4                                                            
         BZ    XIT2                                                             
         CLI   0(R4),TADLELQ                                                    
         BNE   XIT2                                                             
*&&                                                                             
         MVI   ELCODE,TADLELQ                                                   
         L     R1,=A(INDLTAB)                                                   
*        B     GENIN1                                                           
         B     GENIN                                                            
         SPACE 1                                                                
INDU2    L     R1,=A(INDUTAB)      (SOME WORK DONE IN DRIVE1)                   
         B     GENIN                                                            
         SPACE 1                                                                
INDV     MVI   ELCODE,TADVELQ                                                   
         L     R1,=A(INDVTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INEI2    L     R1,=A(INEITAB)      (SOME WORK DONE IN DRIVE1)                   
         B     GENIN                                                            
         SPACE 1                                                                
INEP2    L     R1,=A(INEPTAB)      (SOME WORK DONE IN DRIVE1)                   
         B     GENIN                                                            
         SPACE 1                                                                
INEUB    MVI   ELCODE,TABDELQ3     EURO BILLING KEYWORDS                        
         L     R1,=A(INEUBTAB)                                                  
         B     GENIN                                                            
         SPACE 1                                                                
INEUP    CLI   TIMODE,PROCREC      IF THIS ISN'T DETAIL RECORD                  
         BE    *+12                                                             
         CLI   MYITYPE+1,C'+'      AND FIELD IS ADDITIVE                        
         BE    XIT2                GET OUT                                      
*                                                                               
         MVI   ELCODE,TAEUELQ      EURO PAYING KEYWORDS                         
         L     R4,TIAREC                                                        
         CLI   0(R4),TLCKCDQ       IF CHECK RECORD                              
         BNE   *+8                                                              
         MVI   ELCODE,TAPDELQ        USE TAPD INSTEAD                           
         L     R1,=A(INEUPTAB)                                                  
         B     GENIN                                                            
         SPACE 1                                                                
INFN     MVI   ELCODE,TAFNELQ                                                   
         CLI   GLARGS,1            REGULAR NAME WANTED                          
         BE    INFN300                                                          
         MVC   0(36,R2),SPACES2                                                 
         USING TAFND,R4                                                         
         L     R4,TIAREC                                                        
         BAS   RE,GETEL2                                                        
         B     *+8                                                              
INFN10   BAS   RE,NEXTEL2                                                       
         BNE   XIT2                                                             
         CLC   TAFNTYPE,GLARGS+1   MATCH ON TYPE                                
         BNE   INFN10                                                           
         CLI   GLARGS,2            PAID VITA SESSION                            
         BE    INFN20                                                           
         CLI   GLARGS,3            PAID FROM CERNO                              
         BE    INFN30                                                           
         CLI   GLARGS,4            PAID VITA COMPLETION                         
         BE    INFN40                                                           
         B     XIT2                                                             
INFN20   CLC   TAFNNAME(2),=C'VS'                                               
         BE    *+10                                                             
         CLC   TAFNNAME(2),=C'RS'                                               
         BNE   XIT2                                                             
INFN25   MVC   0(36,R2),=CL36'PAID VITA SESSION'                                
         B     XIT2                                                             
INFN30   CLC   TAFNNAME(2),=C'HF'                                               
         BNE   XIT2                                                             
         MVC   0(36,R2),=CL36'PAID FROM CERNO'                                  
         B     XIT2                                                             
INFN40   CLC   TAFNNAME(2),=C'VC'                                               
         BE    *+10                                                             
         CLC   TAFNNAME(2),=C'RC'                                               
         BNE   XIT2                                                             
         MVC   0(36,R2),=CL36'PAID VITA COMPLETION'                             
         B     XIT2                                                             
INFN300  L     R1,=A(INFNTAB)                                                   
         MVC   ELTYPE,GLARGS+1                                                  
         B     GENIN                                                            
         SPACE 1                                                                
INGC     MVI   ELCODE,TAGCELQ                                                   
         L     R1,=A(INGCTAB)                                                   
         B     GENIN1                                                           
         SPACE 1                                                                
INGH     MVI   ELCODE,TAGHELQ                                                   
         L     R1,=A(INGHTAB)                                                   
         B     GENIN                                                            
*                                                                               
INGT     MVI   ELCODE,TAGTELQ                                                   
         L     R1,=A(INGTTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INGU2    L     R1,=A(INGUTAB)      (SOME WORK DONE IN DRIVE1)                   
         B     GENIN                                                            
         SPACE 1                                                                
INIF     MVI   ELCODE,TAIFELQ                                                   
         L     R1,=A(INIFTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
ININ     MVI   ELCODE,TAINELQ                                                   
         L     R1,=A(ININTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INIS     MVI   ELCODE,TAISELQ                                                   
         L     R1,=A(INISTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INKP     MVI   ELCODE,TAKPELQ                                                   
         L     R1,=A(INKPTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INLF     MVI   ELCODE,TALFELQ                                                   
         L     R1,=A(INLFTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INLN2    L     R1,=A(INLNTAB)      (SOME WORK DONE IN DRIVE1)                   
         B     GENIN                                                            
         SPACE 1                                                                
INMA     MVI   ELCODE,TAMAELQ                                                   
         L     R1,=A(INMATAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INMU2    L     R1,=A(INMUTAB)      (SOME WORK DONE IN DRIVE1)                   
         MVC   ELTYPE,GLARGS+1                                                  
*                                                                               
         CLI   GLARGS+1,C'*'       IF LOOKING FOR CLIENT NAME                   
         BNE   *+12                                                             
         MVI   ELTYPE,TAFNTCLI                                                  
         B     INMU210                                                          
         CLI   GLARGS+1,C'P'       OR PRODUCT NAME                              
         BNE   GENIN                                                            
         MVI   ELTYPE,TAFNTPRD                                                  
*                                                                               
INMU210  MVI   ELCODE,TAFNELQ      GET FROM FREE FORM ELEMENT                   
         L     R1,=A(INFNTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INMUH2   MVI   ELCODE,TAPMELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         USING TAPMD,R4                                                         
*                                                                               
         CLI   GLARGS,1            ORIGINAL MUSIC CODE?                         
         JNE   *+14                                                             
         MVC   0(L'TAPMOMUS,R2),TAPMOMUS                                        
         J     XIT                                                              
*                                                                               
         CLI   GLARGS,2            PREVIOUS AGY?                                
         JNE   *+14                                                             
         MVC   0(L'TAPMPAGY,R2),TAPMPAGY                                        
         J     XIT                                                              
*                                                                               
         CLI   GLARGS,3            ORIGINAL AGY?                                
         JNE   *+14                                                             
         MVC   0(L'TAPMOAGY,R2),TAPMOAGY                                        
         J     XIT                                                              
*                                                                               
         CLI   GLARGS,4            CURRENT AGY?                                 
         JNE   XIT                                                              
         MVC   0(L'TAPMCAGY,R2),TAPMCAGY                                        
         J     XIT                                                              
*                                                                               
INMUT2   MVI   ELCODE,TAMUELQ      GET MUSIC ELEMENT                            
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
INMUT210 BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         USING TAMUD,R4                                                         
*                                                                               
         CLC   TAMUTYPE,GLARGS     MATCH ON TYPE                                
         JNE   INMUT210                                                         
         CLC   TAMUTNUM,GLARGS+1   MATCH ON NUMBER                              
         JNE   INMUT210                                                         
         MVC   0(L'TAMUNAME,R2),TAMUNAME                                        
         J     XIT                                                              
*                                                                               
INMUTL2  MVI   ELCODE,TAMUELQ      GET MUSIC ELEMENT                            
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
INMUTL2A BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         USING TAMUD,R4                                                         
*                                                                               
         CLC   TAMUTYPE,GLARGS     MATCH ON TYPE                                
         JNE   INMUTL2A                                                         
         CLC   TAMUTNUM,GLARGS+1   MATCH ON NUMBER                              
         JNE   INMUTL2A                                                         
*                                                                               
         MVC   0(1,R2),TAMULIC     LICENSER - CODE                              
         GOTO1 LICVAL,DMCB,(X'80',0(R2))                                        
*                                                                               
         MVC   0(L'TAMUNAME,R2),TAMUNAME      AUTHOR/COMPOSER/PUBLISHER         
         MVI   L'TAMUNAME(R2),C' '                                              
         MVC   L'TAMUNAME+1(9,R2),=C'         '                                 
         OC    TGLCNAME,TGLCNAME                                                
         JZ    *+10                                                             
         MVC   L'TAMUNAME+1(9,R2),TGLCNAME    LISCENSER                         
         J     XIT                                                              
*                                                                               
INMUC2   MVI   ELCODE,TACMELQ                                                   
         L     R1,=A(INCMTAB)                                                   
         B     GENIN                                                            
*                                                                               
INNU     MVI   ELCODE,TANUELQ                                                   
         L     R1,=A(INNUTAB)                                                   
         MVC   ELTYPE,GLARGS+1                                                  
         MVI   0(R2),X'FE'         INSURE WE HAVE SOMETHING                     
         B     GENIN                                                            
         SPACE 1                                                                
INNX     MVI   ELCODE,TANXELQ                                                   
         L     R1,=A(INNXTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INOA     MVI   ELCODE,TAOAELQ                                                   
         L     R1,=A(INOATAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INOC     MVI   ELCODE,TAOCELQ                                                   
         L     R1,=A(INOCTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INOP     MVI   ELCODE,TAOPELQ                                                   
         L     R1,=A(INOPTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INO2     MVI   ELCODE,TAO2ELQ                                                   
         L     R1,=A(INO2TAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INOW2    MVC   ELTYPE,GLARGS+1     (SOME WORK DONE IN DRIVE1)                   
         L     R1,=A(INOWTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INPD     CLI   TIMODE,PROCREC      IF THIS ISN'T DETAIL RECORD                  
         BE    *+12                                                             
         CLI   MYITYPE+1,C'+'      AND FIELD IS ADDITIVE                        
         BE    XIT2                GET OUT                                      
         MVI   ELCODE,TAPDELQ                                                   
         L     R1,=A(INPDTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INPE     MVI   ELCODE,TAPEELQ                                                   
         L     R1,=A(INPETAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INPI     MVI   ELCODE,TAPIELQ                                                   
         L     R1,=A(INPITAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INPO     MVI   ELCODE,TAPOELQ                                                   
         L     R1,=A(INPOTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INRN     MVI   ELCODE,TARNELQ                                                   
         L     R1,=A(INRNTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INRP     MVI   ELCODE,TARPELQ                                                   
         L     R1,=A(INRPTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
         USING TAR1D,R4                                                         
INR1     L     R4,ATHISEL                                                       
         LTR   R4,R4                                                            
         BZ    XIT2                                                             
         CLI   0(R4),TAR1ELQ                                                    
         BNE   XIT2                                                             
         MVI   ELCODE,TAR1ELQ                                                   
         L     R1,=A(INR1TAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INSD     MVI   ELCODE,TASDELQ                                                   
         L     R1,=A(INSDTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INSI     MVI   ELCODE,TASIELQ                                                   
         L     R1,=A(INSITAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INSO     MVI   ELCODE,TASOELQ                                                   
         L     R1,=A(INSOTAB)                                                   
         L     RE,TIAREC                                                        
         CLI   0(RE),TLCKCDQ                                                    
         BE    GENIN1                                                           
         B     GENIN                                                            
         SPACE 1                                                                
INNP     MVI   ELCODE,TANPELQ                                                   
         L     R1,=A(INNPTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INST     MVI   ELCODE,TASTELQ                                                   
         L     R1,=A(INSTTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INTU     MVI   ELCODE,TATUELQ                                                   
         L     R1,=A(INTUTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
         USING TAT4D,R4                                                         
INT4     L     R4,ATHISEL                                                       
         LTR   R4,R4                                                            
         BZ    XIT2                                                             
         CLI   0(R4),TAT4ELQ                                                    
         BNE   XIT2                                                             
         CLI   GLARGS+1,1                                                       
         BE    INT4CN                                                           
         CLI   GLARGS+1,2                                                       
         BE    INT4PRV                                                          
INT4STAT MVI   ELCODE,TAT4ELQ                                                   
         L     R1,=A(INT4TAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INT4CN   CLC   TAT4UNIT(2),=C'CN'  CANADIAN FEDERAL ONLY                        
         BNE   XIT2                                                             
         B     INT4STAT                                                         
         SPACE 1                                                                
INT4PRV  CLC   TAT4UNIT(2),=C'CN'  PROVINCES ONLY                               
         BE    XIT2                                                             
         B     INT4STAT                                                         
         SPACE 1                                                                
INUL     MVI   ELCODE,TAULELQ                                                   
         L     R1,=A(INULTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INUP     MVI   ELCODE,TAUPELQ                                                   
         L     R1,=A(INUPTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INVR     MVI   ELCODE,TAVRELQ                                                   
         L     R1,=A(INVRTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INVU     MVI   ELCODE,TAVUELQ                                                   
         L     R1,=A(INVUTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
INWH2    L     R1,=A(INWHTAB)      (SOME WORK DONE IN DRIVE1)                   
         B     GENIN                                                            
         SPACE 1                                                                
INW42    L     R1,=A(INW4TAB)      (SOME WORK DONE IN DRIVE1)                   
         B     GENIN                                                            
         SPACE 1                                                                
INWX2    MVI   ELCODE,TAWXELQ                                                   
         L     R1,=A(INWXTAB)                                                   
         B     GENIN                                                            
         SPACE 1                                                                
         USING TAXTD,R4                                                         
INXT     MVI   ELCODE,TAXTELQ      EXTRA TAX ELEMENT                            
         L     R4,TIAREC                                                        
         BAS   RE,GETEL2                                                        
         B     *+8                                                              
INXT3    BAS   RE,NEXTEL2                                                       
         BNE   INXT4                                                            
         CLC   TAXTUNIT,GLARGS     MATCH TO UNIT IN GLARGS                      
         BNE   INXT3                                                            
         B     INXT5                                                            
*                                                                               
INXT4    XR    R1,R1               MAKE IT ZERO IF CAN'T FIND                   
         B     CVD2                                                             
INXT5    ICM   R1,15,TAXTTAXS      USE THE AMOUNT SAVED                         
         B     CVD2                                                             
         DROP  R4                                                               
         SPACE 1                                                                
INYE     MVI   ELCODE,TAYEELQ                                                   
         MVC   ELTYPE,GLARGS+1                                                  
         L     R1,=A(INYETAB)                                                   
         B     GENIN1                                                           
         EJECT                                                                  
*              GENERAL INPUT HANDLER                                            
         SPACE 3                                                                
*              INPUT               R2=A(WHERE DATA IS TO GO)                    
*                                  R3=L'INPUT                                   
*                                  R1=A(ELEMENT DISPLACEMENT TABLE)             
*                                  ELCODE SET                                   
*                                  R4=A(RECORD)                                 
         SPACE 1                                                                
GENIN    MVI   INMAIN,C'N'                                                      
         OC    ATHISEL,ATHISEL     IF AN ELEMENT IS BEING PASSED                
         BZ    GENIN2                                                           
         CLI   MYITYPE+1,C'+'         AND FIELD IS ADDITIVE                     
         BNE   GENIN2                                                           
         SPACE 1                                                                
GENIN1   L     R4,ATHISEL             PROCESS IF WE ARE                         
         CLC   0(1,R4),ELCODE         ADDRESSING THE REQUIRED ELEMENT           
         BNE   XIT2                                                             
         BAS   RE,ELFILT           MAY BE SOME ELEMENT FILTERING                
         BNE   XIT2                                                             
         CLI   ELTYPE,0            IF ELTYPE IS SET                             
         BE    GENDATA                                                          
         CLC   ELTYPE,2(R4)           CHECK AGAINST FIRST DATA BYTE             
         BNE   XIT2                                                             
         B     GENDATA                                                          
         SPACE 1                                                                
GENIN2   BAS   RE,GETEL2                                                        
         B     *+8                                                              
         SPACE 1                                                                
GENIN4   BAS   RE,NEXTEL2                                                       
         BE    GENIN6                                                           
         CLI   INMAIN,C'Y'         TEST WE'VE ALREADY CHECKED MAIN              
         BE    XIT2                                                             
         CLI   MYITYPE+1,C'+'      DON'T GET DETAILS OUT OF MAIN                
         BE    XIT2                                                             
         ICM   R4,15,TIAMAIN       TEST WE HAVE HIGHER LEVEL RECORD             
         BZ    XIT2                                                             
         MVI   INMAIN,C'Y'         SET WE'RE LOOKING IN MAIN                    
         B     GENIN2              AND START AGAIN                              
         SPACE 1                                                                
GENIN6   BAS   RE,ELFILT           MAY BE SOME ELEMENT FILTERING                
         BNE   GENIN4                                                           
         CLI   ELTYPE,0            IF ELTYPE IS SET                             
         BE    GENDATA                                                          
         CLC   ELTYPE,2(R4)           CHECK AGAINST FIRST DATA BYTE             
         BNE   GENIN4                                                           
         B     GENDATA                                                          
         EJECT                                                                  
*              NOW EXTRACT THE DATA FROM ELEMENT                                
         SPACE 3                                                                
GENDATA  CLI   ELCODE,TABDELQ                                                   
         BE    *+8                                                              
         CLI   ELCODE,TABDELQ2                                                  
         BNE   GENDA1                                                           
         CLI   GLARGS,13           IF LOOKING FOR AGY COMM                      
         BE    *+8                                                              
         CLI   GLARGS,15           IF LOOKING FOR CDN PST                       
         BE    *+8                                                              
         CLI   GLARGS,16           IF LOOKING FOR CDN HST                       
         BNE   GENDA1A                                                          
         USING TABDD,R4                                                         
         CLI   TABDLEN,TABDLN2Q    AND IF ELEMENT IS NOT NEW LENGTH             
         BNL   GENDA1A             AGY COMM OR BDPST DOES NOT EXIST             
         XR    R1,R1               SET AGY COMM OR BDPST TO ZERO                
         B     CVD2                                                             
*                                                                               
GENDA1   CLI   ELCODE,TADDELQ                                                   
         BNE   GENDA1A                                                          
         CLI   GLARGS,1            IF LOOKING FOR ORIGINAL DUE DATE             
         BNE   GENDA1A                                                          
         USING TADDD,R4                                                         
         CLI   TADDLEN,TADDLNQ     AND IF ELEMENT IS NOT NEW LENGTH             
         BNL   GENDA1A             ORIGINAL DUE DATE DOES NOT EXIST             
         XR    R1,R1               SET DUE DATE TO NOTHING                      
         B     CVD2                                                             
*                                                                               
GENDA1A  DS    0H                                                               
*                                                                               
         CLI   ELCODE,TANXELQ      IF TRANSFER DETAILS ELEMENT                  
         BNE   GENDA1X                                                          
*                                                                               
         CLI   GLARGS,3            AND NETWORK COMMERCIAL ID                    
         BNE   GENDA1X                                                          
*                                                                               
         USING TANXD,R4            ESTABLISH TRANSFER DETAILS ELEMENT           
*                                                                               
         TM    TANXSTAT,TANXPACK   IF NOT PACKED ADID NUMBER                    
         BO    GENDA12                                                          
*                                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES2     SO CLEAR TO SPACES                           
*                                                                               
         MVC   0(L'TANXNCID,R2),TANXNCID RETURN NCID                            
         B     XIT2                                                             
*                                                                               
GENDA12  DS    0H                  PACKED ADID                                  
*                                                                               
         GOTOR TRPACK,DMCB,(C'U',TANXNCID),0(R2)  UNPK ADID                     
*                                                                               
         B     XIT2                                                             
*                                                                               
GENDA1X  DS    0H                                                               
*                                                                               
         ZIC   RF,GLARGS           USE ARG1 TO GET TO DATA ELEMENT              
         BCTR  RF,0                                                             
         SLL   RF,1                                                             
         AR    R1,RF                                                            
         ZIC   RE,0(R1)            RE=DISP INTO ELEMENT                         
         AR    RE,R4                                                            
         ST    RE,AGINDATA                                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)       MOVE OUT BASIC DATA                          
         CLI   MYITYPE,C'C'        IF WE NEEDED CHARACTERS                      
         BNE   GENDA2                                                           
         CLI   0(R2),C' '          WAS THERE ANYTHING RETURNED?                 
         BH    GENDA2                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),0(R2)       IF NOT, CLEAR                                
         MVI   0(R2),X'FE'            AND RETURN A X'FE'                        
         SPACE 1                                                                
GENDA2   CLI   1(R1),0             SECOND BYTE IS ADDITIONAL ROUTINE#           
         BE    XIT2                                                             
         L     RE,=A(ADDROUT)      ADDITIONAL ROUTINE NEEDED                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES2     SO CLEAR TO SPACES                           
         SPACE 1                                                                
GENDA1A2 CLC   0(1,RE),ELCODE      FIRST BYTE IS ELCODE                         
         BNE   GENDA1A4                                                         
         CLC   1(1,RE),1(R1)       SECOND BYTE IS ROUTINE NUMBER                
         BE    GENDA1A6                                                         
         SPACE 1                                                                
GENDA1A4 LA    RE,8(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    XIT2                                                             
         B     GENDA1A2                                                         
         SPACE 1                                                                
GENDA1A6 L     RF,4(RE)            PICK UP EXTRA ROUTINE                        
         L     R1,AGINDATA         AND ADDRESS OF DATA                          
         BR    RF                                                               
         EJECT                                                                  
*              FILTERING ELEMENT                                                
         SPACE 3                                                                
ELFILT   NTR1                                                                   
         CLI   ELCODE,TACRELQ      CHECK ELEMENTS FOR SPECIAL HANDLING          
         BE    ELFLTCR                                                          
         CLI   ELCODE,TACWELQ                                                   
         BE    ELFLTCW                                                          
         CLI   ELCODE,TAGCELQ                                                   
         BE    ELFLTGC                                                          
         CLI   ELCODE,TAWHELQ                                                   
         BE    ELFLTWH                                                          
         CLI   ELCODE,TAW2ELQ                                                   
         BE    ELFLTW2                                                          
         CLI   ELCODE,TASDELQ                                                   
         BE    ELFLTSD                                                          
         CLI   ELCODE,TATUELQ                                                   
         BE    ELFLTTU                                                          
         B     ITSFINE2                                                         
         SPACE 1                                                                
         USING TACRD,R4                                                         
ELFLTCR  MVC   THISDATE,TACREND    FILTER ON END DATE                           
         BAS   RE,TESTDATE                                                      
         BNE   NOGOOD2                                                          
         CLI   TIFCUST,15                                                       
         BE    ELFLTCR2                                                         
         CLI   TIFCUST,9                                                        
         BNE   ITSFINE2            AND IF CUSTOM FILTER 9                       
         CLC   TACRUSE,=C'HLD'           CHECK IT'S HLD TYPE                    
         BE    ITSFINE2                                                         
         CLC   TACRUSE,=C'SHL'                                                  
         BE    ITSFINE2                                                         
         CLC   TACRUSE,=C'ADH'                                                  
         BE    ITSFINE2                                                         
         B     NOGOOD2                                                          
         SPACE 1                                                                
ELFLTCR2 TM    TACRSTAT,TACRSGUA   CUSTOM 15 FIXED GUARANTEE                    
         BO    ITSFINE2                                                         
         B     NOGOOD2                                                          
         SPACE 1                                                                
         USING TAGCD,R4                                                         
ELFLTGC  MVC   THISDATE,TAGCEND    FILTER ON END DATE                           
         BAS   RE,TESTDATE                                                      
         BNE   NOGOOD2                                                          
         B     ITSFINE2                                                         
         SPACE 1                                                                
         USING TACWD,R4                                                         
ELFLTCW  MVC   THISUNIT,TACWUNIT   FILTER CW UNIT                               
         BRAS  RE,TESTWHER                                                      
         BNE   NOGOOD2                                                          
         B     ITSFINE2                                                         
         SPACE 1                                                                
         USING TAWHD,R4                                                         
ELFLTWH  MVC   THISUNIT,TAWHUNIT   FILTER WH UNIT                               
         BRAS  RE,TESTWHER                                                      
         BNE   NOGOOD2                                                          
         B     ITSFINE2                                                         
         SPACE 1                                                                
         USING TAW2D,R4                                                         
ELFLTW2  MVC   THISUNIT,TAW2UNIT   FILTER W2 UNIT                               
         BRAS  RE,TESTWHER                                                      
         BNE   NOGOOD2                                                          
         B     ITSFINE2                                                         
         SPACE 1                                                                
         USING TASDD,R4                                                         
ELFLTSD  CLI   TASDEQU,UBSM        IF MUSIC SESSION                             
         BE    *+12                                                             
         CLI   TASDEQU,UIMS                                                     
         BNE   *+8                                                              
         L     R1,=A(INSDMTAB)     USE MUSIC SESSION TABLE INSTEAD              
*                                                                               
         CLI   TASDEQU,UBSR        IF RADIO SESSION                             
         BE    *+12                                                             
         CLI   TASDEQU,UADO        OR ADDENDUM RADIO SESSION                    
         BNE   *+8                                                              
         L     R1,=A(INSDRTAB)     USE RADIO SESSION TABLE INSTEAD              
         CR    R1,R1               ALWAYS RETURN WITH CC EQUAL                  
         XIT1  REGS=(R1)           XIT WITH POSSIBLE R1 TAB ADJUSTED            
         SPACE 1                                                                
         USING TATUD,R4                                                         
ELFLTTU  MVC   THISUNIT,TATUUNIT   FILTER TU UNIT                               
         BRAS  RE,TESTWHER                                                      
         BNE   NOGOOD2                                                          
         B     ITSFINE2                                                         
         SPACE 1                                                                
         USING TAATD,R4                                                         
ELFLTAT  MVC   THISUNIT,TAATUNIT   FILTER AT UNIT                               
         BRAS  RE,TESTWHER                                                      
         BNE   NOGOOD2                                                          
         B     ITSFINE2                                                         
         SPACE 1                                                                
         EJECT                                                                  
*              STACK ROUTINES                                                   
         SPACE 3                                                                
STACKIN  LA    R3,STACKDEF                                                      
         USING STKENTD,R3                                                       
         MVC   MYGLARGS,GLARGS                                                  
         MVI   GLARGS,0                                                         
         SPACE 1                                                                
STACKIN2 CLI   STKCON,X'FF'                                                     
         BE    XIT2                                                             
         CLI   STKEL,0             IGNORE CONTROL ENTRIES                       
         BE    STIN12                                                           
         MVC   ELCODE,STKEL        PICK UP ELEMENT CODE                         
         MVC   GLARGS(1),STKROUT           AND ROUTINE NUMBER                   
         CLI   MYGLARGS,0                                                       
         BE    STIN8                                                            
         SPACE 1                                                                
         LA    R1,STKMODTB         MODIFY ROUTINE NUMBER                        
         SPACE 1                                                                
STIN4    CLC   MYGLARGS(1),0(R1)                                                
         BNE   STIN6                                                            
         CLC   STKEL(2),1(R1)                                                   
         BNE   STIN6                                                            
         MVC   GLARGS(1),3(R1)                                                  
         B     STIN8                                                            
         SPACE 1                                                                
STIN6    LA    R1,4(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   STIN4                                                            
         ZAP   0(8,R2),=P'0'       NO AVAILABLE ROUTINE                         
         B     STIN10                                                           
         SPACE 1                                                                
STIN8    BAS   RE,STKGENIN         GO AND FILL ONE ENTRY                        
         SPACE 1                                                                
STIN10   LA    R2,8(R2)                                                         
         SPACE 1                                                                
STIN12   LA    R3,L'STKENT(R3)                                                  
         B     STACKIN2                                                         
         DROP  R3                                                               
         SPACE 1                                                                
STKGENIN NTR1                                                                   
*                                                                               
         CLI   ELCODE,TAPDELQ      PAYING                                       
         BNE   *+8                                                              
         L     R1,=A(INPDTAB)                                                   
*                                                                               
         CLI   ELCODE,TABDELQ      BILLING                                      
         BNE   *+8                                                              
         L     R1,=A(INBDTAB)                                                   
*                                                                               
         CLI   ELCODE,TABDELQ2     CHLOE BILLING                                
         BNE   *+8                                                              
         L     R1,=A(INCHTAB)                                                   
*                                                                               
         CLI   ELCODE,TABDELQ3     BILLING IN EUROS                             
         BNE   *+8                                                              
         L     R1,=A(INEUBTAB)                                                  
*                                                                               
         CLI   ELCODE,TAEUELQ      PAYING IN EUROS                              
         BNE   *+8                                                              
         L     R1,=A(INEUPTAB)                                                  
*                                                                               
         CLI   ELCODE,TACXELQ      CHECK EXTRA                                  
         BNE   *+8                                                              
         L     R1,=A(INCXTAB)                                                   
*                                                                               
*        CLI   ELCODE,TATUELQ      TAX UNIT DETAILS                             
*        BNE   *+8                                                              
*        L     R1,=A(INTUTAB)                                                   
*        LA    R3,4                                                             
*        B     GENIN                                                            
*                                                                               
         LA    R3,8                MAKE ENTRY LENGTH 8                          
         B     GENIN                                                            
         SPACE 1                                                                
MYGLARGS DS    CL16                                                             
         SPACE 1                                                                
STKMODTB DS    0F                                                               
         DC    C'I'                INDIV.    PAY TO PAYI                        
         DC    AL1(TAPDELQ),AL1(41),AL1(14)                                     
         DC    C'I'                          PAY+ TO PAYI+                      
         DC    AL1(TAPDELQ),AL1(42),AL1(43)                                     
         DC    C'C'                CORPORATE PAY TO PAYC                        
         DC    AL1(TAPDELQ),AL1(41),AL1(15)                                     
         DC    C'C'                          PAY+ TO PAYC+                      
         DC    AL1(TAPDELQ),AL1(42),AL1(44)                                     
         DC    C'I'                INDIV.    HAND TO HND                        
         DC    AL1(TABDELQ),AL1(09),AL1(5)                                      
         DC    C'I'                          HAND+ TO HNDI+                     
         DC    AL1(TABDELQ),AL1(10),AL1(11)                                     
         DC    C'C'                CORPORATE HAND TO HNDC                       
         DC    AL1(TABDELQ),AL1(09),AL1(6)                                      
         DC    C'C'                          HAND+ TO HNDC                      
         DC    AL1(TABDELQ),AL1(10),AL1(6)                                      
         DC    C'I'                INDIV.    HAND TO HND                        
         DC    AL1(TABDELQ2),AL1(09),AL1(5)                                     
         DC    C'I'                          HAND+ TO HNDI+                     
         DC    AL1(TABDELQ2),AL1(10),AL1(11)                                    
         DC    C'C'                CORPORATE HAND TO HNDC                       
         DC    AL1(TABDELQ2),AL1(09),AL1(6)                                     
         DC    C'C'                          HAND+ TO HNDC                      
         DC    AL1(TABDELQ2),AL1(10),AL1(6)                                     
         DC    C'I'                INDIV.    PAY TO PAYI                        
         DC    AL1(TAEUELQ),AL1(41),AL1(14)                                     
         DC    C'I'                          PAY+ TO PAYI+                      
         DC    AL1(TAEUELQ),AL1(42),AL1(43)                                     
         DC    C'C'                CORPORATE PAY TO PAYC                        
         DC    AL1(TAEUELQ),AL1(41),AL1(15)                                     
         DC    C'C'                          PAY+ TO PAYC+                      
         DC    AL1(TAEUELQ),AL1(42),AL1(44)                                     
         DC    C'I'                INDIV.    HAND TO HND                        
         DC    AL1(TABDELQ3),AL1(09),AL1(5)                                     
         DC    C'I'                          HAND+ TO HNDI+                     
         DC    AL1(TABDELQ3),AL1(10),AL1(11)                                    
         DC    C'C'                CORPORATE HAND TO HNDC                       
         DC    AL1(TABDELQ3),AL1(09),AL1(6)                                     
         DC    C'C'                          HAND+ TO HNDC                      
         DC    AL1(TABDELQ3),AL1(10),AL1(6)                                     
         DC    X'FF'                                                            
         EJECT                                                                  
*              STACK OUTPUT ROUTINE                                             
         SPACE 3                                                                
STACKOUT LA    R4,STACKDEF                                                      
         USING STKENTD,R4                                                       
         ZAP   TOTPACK,=P'0'                                                    
         ZAP   SUBPACK,=P'0'                                                    
         ZAP   LSTPACK,=P'0'                                                    
         ZAP   THSPACK,=P'0'                                                    
         SPACE 1                                                                
STKOUT2  CLI   STKCON,X'FF'                                                     
         BE    XIT2                                                             
         TM    STKCON,STKCDET                                                   
         BO    STKOUT4                                                          
         TM    STKCON,STKCTOT                                                   
         BO    STKOUT6                                                          
         B     STKOUT8                                                          
         SPACE 1                                                                
STKOUT4  TM    GLINDS,GLTOTLIN     DETAILS ONLY                                 
         BNO   STKOUT8                                                          
         B     STKOUTX2                                                         
         SPACE 1                                                                
STKOUT6  TM    GLINDS,GLTOTLIN     TOTALS ONLY                                  
         BNO   STKOUTX2                                                         
         SPACE 1                                                                
STKOUT8  CLI   STKEL,0             IF NOT A CONTROL                             
         BE    STKOUT10                                                         
         MVC   DUB,0(R2)                                                        
         AP    TOTPACK,DUB            ADJUST COUNTERS                           
         AP    SUBPACK,DUB                                                      
         MVC   LSTPACK,THSPACK                                                  
         MVC   THSPACK,DUB                                                      
         BAS   RE,CONED               AND EDIT                                  
         B     STKOUTX                                                          
         SPACE 1                                                                
STKOUT10 CLI   STKROUT,STKSP       SELECT ONE OF THE CONTROLS                   
         BE    STKOUT12                                                         
         CLI   STKROUT,STKSBTOT                                                 
         BE    STKOUT14                                                         
         CLI   STKROUT,STKTOT                                                   
         BE    STKOUT16                                                         
         CLI   STKROUT,STKDIFF                                                  
         BE    STKOUT18                                                         
         DC    H'0'                                                             
         SPACE 1                                                                
STKOUT12 MVI   0(R3),0             SPACE                                        
         B     STKOUTX                                                          
         SPACE 1                                                                
STKOUT14 MVC   DUB,SUBPACK         SUBTOTAL                                     
         BAS   RE,CONED                                                         
         ZAP   SUBPACK,=P'0'                                                    
         B     STKOUTX                                                          
         SPACE 1                                                                
STKOUT16 MVC   DUB,TOTPACK         TOTAL                                        
         BAS   RE,CONED                                                         
         ZAP   TOTPACK,=P'0'                                                    
         B     STKOUTX                                                          
         SPACE 1                                                                
STKOUT18 MVC   DUB,LSTPACK         DIFFERENCE                                   
         SP    DUB,THSPACK                                                      
         BAS   RE,CONED                                                         
         SPACE 1                                                                
STKOUTX  LA    R3,198(R3)                                                       
         SPACE 1                                                                
STKOUTX2 CLI   STKEL,0             IF A CONTROL                                 
         BE    *+8                    DON'T BUMP TO NEXT FIELD                  
         LA    R2,8(R2)                                                         
         LA    R4,L'STKENT(R4)                                                  
         B     STKOUT2                                                          
         SPACE 1                                                                
TOTPACK  DC    PL8'6'                                                           
SUBPACK  DC    PL8'6'                                                           
LSTPACK  DC    PL8'6'                                                           
THSPACK  DC    PL8'6'                                                           
         EJECT                                                                  
*              STACK DATA ROUTINES                                              
         SPACE 3                                                                
STDTAIN  MVI   3(R2),1             PHONEY NUMERIC COL.                          
         B     XIT2                SO WE GET OUTPUT CONTROL                     
         SPACE 1                                                                
STDTAOUT DS    0H                                                               
         LA    R4,STACKDEF                                                      
         USING STKENTD,R4                                                       
         SPACE 1                                                                
SDAOUT2  CLI   STKCON,X'FF'                                                     
         BE    XIT2                                                             
         TM    STKCON,STKCDET                                                   
         BO    SDAOUT4                                                          
         TM    STKCON,STKCTOT                                                   
         BO    SDAOUT6                                                          
         B     SDAOUT8                                                          
         SPACE 1                                                                
SDAOUT4  TM    GLINDS,GLTOTLIN     DETAILS ONLY                                 
         BNO   SDAOUT8                                                          
         B     SDAOUTX                                                          
         SPACE 1                                                                
SDAOUT6  TM    GLINDS,GLTOTLIN     TOTALS ONLY                                  
         BNO   SDAOUTX                                                          
         SPACE 1                                                                
SDAOUT8  L     R1,=A(STACKTAB)     LOOK UP TABLE                                
         SPACE 1                                                                
SDAOUT10 CLI   0(R1),X'FF'                                                      
         BE    SDAOUT12                                                         
         CLC   STKEL(2),9(R1)      FOR MATCH ON EL/ROUTE                        
         BE    SDAOUT12                                                         
         LA    R1,12(R1)                                                        
         B     SDAOUT10                                                         
         SPACE 1                                                                
SDAOUT12 MVC   0(8,R3),0(R1)                                                    
         LA    R3,198(R3)                                                       
         SPACE 1                                                                
SDAOUTX  LA    R4,L'STKENT(R4)                                                  
         B     SDAOUT2                                                          
         EJECT                                                                  
*              ROUTINE TO CONTROL EDITOR                                        
         SPACE 3                                                                
*                                  DUB CONTAINS AMOUNT TO EDIT                  
*                                  R3=A(OUTPUT)                                 
         SPACE 1                                                                
CONED    NTR1                                                                   
         L     R2,GLADTENT                                                      
         USING DROD,R2                                                          
         LA    R1,DUB                                                           
         ST    R1,EBAIN                                                         
         MVI   EBTIN,C'P'                                                       
         MVI   EBLIN,8                                                          
         SPACE 1                                                                
         ST    R3,EBAOUT           R3=A(OUTPUT)                                 
         MVC   EBLOUT,DROLEN       PASS OTHER OUTPUT VALUES                     
         MVC   EBDECS,DRODEC                                                    
         MVC   EBFILL,DROFILL                                                   
         MVC   EBFLOAT,DROFLOAT                                                 
         MVC   EBROUND,DRODIV                                                   
         MVC   EBOPT,DROEDIT                                                    
         MVC   EBTRIM,DROFORM                                                   
         MVC   EBALIGN,DROALIGN                                                 
         MVI   EBPWIDTH,198                                                     
         MVC   EBSCOUT,DROSCALE                                                 
         MVC   EBTRAIL,DROTRAIL                                                 
         GOTO1 GLAEDITR,DMCB,EBLOCK                                             
         B     XIT2                                                             
         DROP  R2                                                               
         SPACE 1                                                                
       ++INCLUDE DDEBLOCK                                                       
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - AGENCY                               
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R3=LENGTH-1                                  
*                                  R4=A(ELEMENT)                                
         SPACE 1                                                                
INALLINV MVC   WORK(6),0(R1)       INVOICE NUMBER                               
         OC    WORK(6),WORK                                                     
         BZ    XIT2                                                             
         GOTO1 TINVCON,DMCB,WORK,(R2),DATCON                                    
         B     XIT2                                                             
         SPACE 1                                                                
INANSTAT DS    0H                  AGENT STATUS                                 
         TM    0(R1),TAANSNCK                                                   
         BNO   *+14                                                             
         MVC   0(13,R2),=C'IGNORE ON SAG'                                       
         LA    R2,14(R2)                                                        
         TM    0(R1),TAANSOVR                                                   
         BNO   *+14                                                             
         MVC   0(12,R2),=C'ALLOW O/RIDE'                                        
         LA    R2,13(R2)                                                        
         B     XIT2                                                             
                                                                                
INANSSN  DS    0H                  AGENT SSN                                    
         L     RF,AGINDATA                                                      
         MVC   0(9,R2),0(RF)                                                    
         OC    0(9,R2),0(R2)                                                    
         BZ    XIT2                                                             
         TM    TGSYSTAT,TASYSPID                                                
         BZ    XIT2                                                             
         MVC   TGSSN,0(R2)                                                      
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   0(9,R2),SPACES2                                                  
         MVC   0(L'TGPID,R2),TGPID                                              
         B     XIT2                                                             
                                                                                
         USING TAATD,R4                                                         
INATUNIT CLC   =C'CN ',TAATUNIT    UNIT HAS TO BE CN                            
         BNE   XIT2                                                             
*        MVC   0(L'TAATUNIT,R2),TAATUNIT                                        
         MVC   0(L'TAATUNIT,R2),TIUNIT                                          
         B     XIT2                                                             
                                                                                
INATPUNT CLC   =C'CN ',TAATUNIT    UNIT CANNOT BE CN                            
         BE    XIT2                                                             
*        MVC   0(L'TAATUNIT,R2),TAATUNIT                                        
         MVC   0(L'TAATUNIT,R2),TIUNIT                                          
         B     XIT2                                                             
                                                                                
INATCAN  CLC   =C'CN ',TAATUNIT    UNIT HAS TO BE CN                            
         BE    CVD2                                                             
         SR    R1,R1                                                            
         B     CVD2                                                             
                                                                                
INATPRV  CLC   =C'CN ',TAATUNIT    UNIT CANNOT BE CN                            
         BNE   CVD2                                                             
         SR    R1,R1                                                            
         B     CVD2                                                             
                                                                                
INAYAGG  MVC   0(6,R2),0(R1)       AGENCY GROUP                                 
         CLC   0(6,R2),=C'OTHERS'  IF GROUP IS OTHERS                           
         BE    INAYAGG2                                                         
         CLI   0(R2),C' '             OR MISSING                                
         BH    XIT2                                                             
         SPACE 1                                                                
INAYAGG2 MVC   0(6,R2),TIAGY          USE AGENCY CODE INSTEAD                   
         B     XIT2                                                             
         EJECT                                                                  
INAYSTAT DS    0H                  AGENCY STATUS                                
         MVC   WORK(1),0(R1)       TAAYSTAT,TAAYSTA2,TAAYSTA3,&TAAYSTA4         
         MVC   WORK+1(3),TAAYSTA2-TAAYSTAT(R1)                                  
*                                                                               
         LA    RF,AYSTTAB                                                       
INAYST5  CLI   0(RF),X'FF'                                                      
         BE    XIT2                                                             
         MVC   FULL,0(RF)                                                       
         NC    FULL,WORK           IF BIT IS ON IN STATUS BYTE                  
         BZ    INAYST7                                                          
         MVC   0(10,R2),4(RF)      DISPLAY CODE                                 
*                                                                               
         LA    R2,9(R2)            PT TO LAST NON-SPACE                         
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         LA    R2,2(R2)            LEAVE SPACE BETWEEN CODES                    
INAYST7  LA    RF,L'AYSTTAB(RF)                                                 
         B     INAYST5                                                          
         SPACE 2                                                                
AYSTTAB  DS    0CL14               TABLE OF STATUS CODES AND THEIR BITS         
         DC    AL1(TAAYSCOD,0,0,0),CL10'PUR'                                    
         DC    AL1(TAAYSAPO,0,0,0),CL10'PO'                                     
         DC    AL1(TAAYS13W,0,0,0),CL10'13W'                                    
         DC    AL1(TAAYSCTX,0,0,0),CL10'CATAX'                                  
         DC    AL1(TAAYSATX,0,0,0),CL10'ACATAX'                                 
         DC    AL1(TAAYSNFT,0,0,0),CL10'NOFTRK'                                 
         DC    AL1(TAAYSEST,0,0,0),CL10'EST'                                    
         DC    AL1(TAAYSPAC,0,0,0),CL10'PRAC'                                   
         DC    AL1(0,TAAYSNDF,0,0),CL10'NODEF'                                  
         DC    AL1(0,TAAYSTRK,0,0),CL10'TRK'                                    
         DC    AL1(0,TAAYSRET,0,0),CL10'RETRO'                                  
         DC    AL1(0,TAAYSHND,0,0),CL10'HAND'                                   
         DC    AL1(0,TAAYSMAL,0,0),CL10'MAIL'                                   
         DC    AL1(0,TAAYSPCH,0,0),CL10'PCH'                                    
         DC    AL1(0,TAAYSOVN,0,0),CL10'OV'                                     
         DC    AL1(0,TAAYSCHI,0,0),CL10'CHI'                                    
         DC    AL1(0,0,TAAYSLCK,0),CL10'LOCK'                                   
         DC    AL1(0,0,TAAYSPRI,0),CL10'PRMAIL'                                 
         DC    AL1(0,0,TAAYSRES,0),CL10'SOAPRES'                                
         DC    AL1(0,0,TAAYS30M,0),CL10'30M'                                    
         DC    AL1(0,0,TAAYSNSD,0),CL10'NSA'                                    
         DC    AL1(0,0,TAAYSNCS,0),CL10'NCA'                                    
         DC    AL1(0,0,TAAYSPNX,0),CL10'PRNX'                                   
         DC    AL1(0,0,TAAYSADV,0),CL10'ADV'                                    
         DC    AL1(0,0,0,TAAYSNGT),CL10'NOGTRK'                                 
         DC    X'FF'                                                            
         EJECT                                                                  
INAYSTA3 DS    0H                  AGENCY STATUS 3                              
         TM    0(R1),TAAYSADV                                                   
         BNO   *+14                                                             
         MVC   0(9,R2),=C'AUTO ADVC'                                            
         LA    R2,10(R2)                                                        
         B     XIT2                                                             
         SPACE 1                                                                
INAYSTA6 DS    0H                  AGENCY STATUS 6                              
         TM    0(R1),TAAYVIBE                                                   
         BNO   *+14                                                             
         MVC   0(9,R2),=C'VITA BETA'                                            
         LA    R2,10(R2)                                                        
         B     XIT2                                                             
         SPACE 1                                                                
INAYSTA7 DS    0H                  AGENCY STATUS 7                              
         TM    0(R1),TAAYSPPL                                                   
         BNO   *+14                                                             
         MVC   0(2,R2),=C'P+'                                                   
         LA    R2,3(R2)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INAYHLD  DS    0H                  AGENCY HOLD FREQ                             
         MVC   0(6,R2),=C'WEEKLY'                                               
         TM    0(R1),X'80'                                                      
         BZ    *+10                                                             
         MVC   0(7,R2),=C'MONTHLY'                                              
         LA    R2,8(R2)                                                         
         ZIC   R3,0(R1)                                                         
         SLL   R3,25                                                            
         SRL   R3,25                                                            
         CVD   R3,DUB                                                           
         UNPK  0(1,R2),DUB+7(1)                                                 
         OI    0(R2),X'F0'                                                      
         MVC   2(7,R2),=C'WKS NTC'                                              
         LA    R2,10(R2)                                                        
         SPACE 1                                                                
         TM    1(R1),TAAYHS2D      AGENCY HOLD STATUS                           
         BNO   *+14                                                             
         MVC   0(10,R2),=C'NO 2ND NTC'                                          
         LA    R2,11(R2)                                                        
         TM    1(R1),TAAYHSCO                                                   
         BNO   *+14                                                             
         MVC   0(9,R2),=C'SHOW COMM'                                            
         LA    R2,10(R2)                                                        
         B     XIT2                                                             
         SPACE 1                                                                
INAYDAYS DS    0H                  BILLING DAYS DUE                             
         MVI   0(R2),0                                                          
         CLI   0(R1),X'FF'                                                      
         BE    XIT2                                                             
         MVC   0(1,R2),0(R1)                                                    
         CLI   0(R2),0                                                          
         BNE   XIT2                                                             
         MVI   0(R2),10            DEFAULT                                      
         B     XIT2                                                             
         SPACE 1                                                                
INAYLBOX DS    0H                  LOCKBOX CODE                                 
         CLI   0(R1),C' '          EXIT IF WE DON'T HAVE A CODE                 
         BNH   XIT2                                                             
         LA    R4,LBOXTAB                                                       
INAYLB10 CLI   0(R4),X'FF'         TEST END OF TABLE                            
         BE    INAYLB30                                                         
         CLC   0(1,R4),0(R1)                                                    
         BE    INAYLB20                                                         
         LA    R4,L'LBOXTAB(R4)    BUMP TO NEXT ENTRY                           
         B     INAYLB10                                                         
*                                                                               
INAYLB20 MVC   2(L'LBOXTAB-1,R2),1(R4)  SET NAME OF OFFICE                      
INAYLB30 MVC   0(1,R2),0(R1)       SET LOCKBOX CODE                             
         B     XIT2                                                             
*                                                                               
LBOXTAB  DS    0CL10               TABLE OF LOCKBOX CODES AND OFFICES           
         DC    C'C',CL9'CHICAGO'                                                
         DC    C'N',CL9'NEW YORK'                                               
         DC    C'D',CL9'NEW YORK'                                               
         DC    C'P',CL9'PRINT'                                                  
         DC    C'L',CL9'LOS ANG.'                                               
         DC    C'O',CL9'OTHER'                                                  
         DC    C'T',CL9'TPCHICAGO'                                              
         DC    X'FF'                                                            
         SPACE 1                                                                
INBRSTAT DS    0H                  BILLING RATE STATUS                          
         TM    0(R1),TABRS75K                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'NC +75K'                                              
         LA    R2,8(R2)                                                         
         TM    0(R1),TABRSNWK                                                   
         BNO   *+14                                                             
         MVC   0(8,R2),=C'NEED NWK'                                             
         LA    R2,9(R2)                                                         
         TM    0(R1),TABRSSRC                                                   
         BNO   *+14                                                             
         MVC   0(3,R2),=C'SRC'                                                  
         LA    R2,4(R2)                                                         
         TM    0(R1),TABRSQTR                                                   
         BNO   *+14                                                             
         MVC   0(4,R2),=C'ESTP'                                                 
         LA    R2,5(R2)                                                         
         TM    0(R1),TABRSNIN                                                   
         BNO   *+14                                                             
         MVC   0(6,R2),=C'NO INT'                                               
         LA    R2,7(R2)                                                         
         TM    0(R1),TABRSINT+TABRSCIN                                          
         BZ    *+14                                                             
         MVC   0(7,R2),=C'ON PROD'                                              
         LA    R2,8(R2)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INBRRATE L     R3,AGINDATA         BILLING RATES                                
         LA    R4,5                EDIT 5 FIELDS (TOTAL 29 CHARACTERS)          
         SPACE 1                                                                
INBRRAT2 EDIT  (2,0(R3)),(5,0(R2)),2,ZERO=BLANK                                 
         LA    R2,6(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R4,INBRRAT2                                                      
         B     XIT2                                                             
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - CLIENT                               
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R3=LENGTH-1                                  
*                                  R4=A(CLIENT RECORD)                          
         SPACE 1                                                                
INCLSTAT DS    0H                  CLIENT STATUS                                
         XC    WORK,WORK                                                        
         MVC   0(70,R2),SPACES2                                                 
         LR    R3,R4               ADDRESS OF CLIENT RECORD                     
         MVI   ELCODE,TACIELQ                                                   
         USING TACID,R4                                                         
         BAS   RE,GETEL2                                                        
         BNE   *+10                                                             
         MVC   WORK(1),TACISTAT    CLIENT INFORMATION STATUS                    
         LR    R4,R3               R4 ---> CLIENT RECORD                        
         MVI   ELCODE,TABRELQ                                                   
         USING TABRD,R4                                                         
         BAS   RE,GETEL2                                                        
         BNE   *+10                                                             
         MVC   WORK+1(1),TABRSTAT  BILLING RULES STATUS                         
*                                                                               
         LA    RF,CLSTTAB                                                       
INCLST5  CLI   0(RF),X'FF'                                                      
         BE    XIT2                                                             
         MVC   HALF,0(RF)                                                       
         NC    HALF,WORK           IF BIT IS ON IN STATUS BYTE                  
         BZ    INCLST7                                                          
         MVC   0(10,R2),2(RF)      DISPLAY CODE                                 
*                                                                               
         LA    R2,9(R2)            PT TO LAST NON-SPACE                         
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         LA    R2,2(R2)            LEAVE SPACE BETWEEN CODES                    
INCLST7  LA    RF,L'CLSTTAB(RF)                                                 
         B     INCLST5                                                          
         SPACE 2                                                                
CLSTTAB  DS    0CL12               TABLE OF STATUS CODES AND THEIR BITS         
         DC    AL1(TACISLCK,0),CL10'LOCKED'                                     
         DC    AL1(TACISCOD,0),CL10'PUR'                                        
         DC    AL1(TACIDCAE,0),CL10'NCLE'                                       
         DC    AL1(0,TABRSQTR),CL10'ESTPDQTR'                                   
         DC    AL1(0,TABRSNWK),CL10'NWK'                                        
         DC    AL1(0,TABRSSRC),CL10'SRC'                                        
         DC    AL1(0,TABRSNIN),CL10'NOINT'                                      
         DC    AL1(0,TABRSINT),CL10'INT'                                        
         DC    X'FF'                                                            
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - CAST                                 
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R3=LENGTH-1                                  
*                                  R4=A(ELEMENT)                                
         SPACE 1                                                                
INCACNT  MVC   0(4,R2),=F'1'       CAST COUNT                                   
         B     XIT2                                                             
         SPACE 1                                                                
INCASTAT DS    0H                  CATEGORY STATUS                              
         TM    0(R1),TACASTLF                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'ON LIFT'                                              
         LA    R2,8(R2)                                                         
         TM    0(R1),TACASTLO                                                   
         BNO   *+14                                                             
         MVC   0(9,R2),=C'LIFT ONLY'                                            
         LA    R2,10(R2)                                                        
         TM    0(R1),TACASTAG                                                   
         BNO   *+14                                                             
         MVC   0(5,R2),=C'AGENT'                                                
         LA    R2,6(R2)                                                         
         TM    0(R1),TACASXAC                                                   
         BNO   *+14                                                             
         MVC   0(8,R2),=C'NOFCGUAR'                                             
         LA    R2,9(R2)                                                         
         TM    0(R1),TACASTNF                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'NOTRACK'                                              
         LA    R2,8(R2)                                                         
         TM    0(R1),TACASTAO                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'AG OVER'                                              
         LA    R2,8(R2)                                                         
         TM    1(R1),TACASTCR                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'PAY CAN'                                              
         LA    R2,8(R2)                                                         
         TM    1(R1),TACASTDP                                                   
         BNO   *+14                                                             
         MVC   0(12,R2),=C'DONT PAY CAN'                                        
         LA    R2,13(R2)                                                        
         TM    1(R1),TACASCLB                                                   
         BNO   *+14                                                             
         MVC   0(9,R2),=C'CELEBRITY'                                            
         LA    R2,10(R2)                                                        
         TM    1(R1),TACASFGR                                                   
         BNO   *+14                                                             
         MVC   0(5,R2),=C'NOFGR'                                                
         LA    R2,6(R2)                                                         
         TM    1(R1),TACASINR                                                   
         BNO   *+14                                                             
         MVC   0(5,R2),=C'NOINR'                                                
         LA    R2,6(R2)                                                         
*                                                                               
         TM    1(R1),TACASINA                                                   
         BNO   *+14                                                             
         MVC   0(5,R2),=C'NOINA'                                                
         LA    R2,6(R2)                                                         
*                                                                               
         TM    1(R1),TACASEUR                                                   
         BNO   *+14                                                             
         MVC   0(5,R2),=C'EUROS'                                                
         LA    R2,5(R2)                                                         
*                                                                               
         B     XIT2                                                             
*                                                                               
INCAAGCD DS    0H                  TRANSLATE AGENT CODE                         
*                                                                               
         LR    R5,R1               SAVE R1                                      
*                                                                               
         GOTOR TRNSAGT,DMCB,(X'40',(R5)),0(R2)  TRANSLATE CODE                  
*                                                                               
         LR    R1,R5               RESTORE R1                                   
*                                                                               
         B     XIT2                                                             
*                                                                               
*        CAST RE-RECORD DATE                                                    
*                                                                               
INCARERC DS    0H                  MAKE SURE RE-RECORD DATE PRESENT             
*                                                                               
         USING TACAD,R4            ESTABLISH CAST DETAIL ELEMENT                
*                                                                               
         LLC   RF,TACALEN          GET ELEMENT LENGTH                           
*                                                                               
         CHI   RF,TACARERC-TACAEL  IF ELEMENT LACKS RE-RECORD DATE              
         BNL   *+14                                                             
         XC    0(3,R2),0(2)           CLEAR FIELD                               
         B     INCARERX                                                         
*                                  ELSE                                         
         MVC   0(L'TACARERC,R2),TACARERC   RETURN RERECORD DATE                 
*                                                                               
INCARERX DS    0H                                                               
         B     XIT2                                                             
*                                                                               
*        CAST RELEASE INFO                                                      
*                                                                               
         USING TARLD,R4                                                         
INCAREL  DS    0H                                                               
         L     R4,TIAREC                                                        
         MVI   ELCODE,TARLELQ      FIND RELEASE ELEMEMNT                        
         BRAS  RE,GETEL                                                         
         JE    INCAREL3                                                         
         XC    0(3,R2),0(R2)                                                    
         J     XIT2                                                             
INCAREL3 CLI   GLARGS,22           RELEASE LETTER                               
         JNE   INCAREL5                                                         
         MVC   0(1,R2),TARLSTAT                                                 
         J     XIT2                                                             
                                                                                
INCAREL5 CLI   GLARGS,23           RELEASE EFF DATE                             
         JNE   XIT2                                                             
         MVC   0(3,R2),TARLEFDT                                                 
         J     XIT2                                                             
*                                                                               
INCDSTAT DS    0H                  CHECK DETAIL STATUS                          
         TM    0(R1),TACDSLIN                                                   
         BNO   *+14                                                             
         MVC   0(4,R2),=C'LIEN'                                                 
         LA    R2,5(R2)                                                         
         TM    0(R1),TACDSSTA                                                   
         BNO   *+14                                                             
         MVC   0(5,R2),=C'STALE'                                                
         LA    R2,6(R2)                                                         
         TM    0(R1),TACDSTRS                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'TRUSTEE'                                              
         LA    R2,8(R2)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
*                                                                               
INCDSTA2 DS    0H                  CHECK DETAIL STATUS 2                        
         TM    0(R1),TACDSMSC                                                   
         BNO   *+14                                                             
         MVC   0(3,R2),=C'MSC'                                                  
         LA    R2,4(R2)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
         USING TACDD,R4                                                         
INCDNUM  MVC   0(8,R2),TACDCHK                                                  
         CLI   MYILEN,10           CHECK NUMBER                                 
         BNE   XIT2                                                             
         MVC   0(2,R2),=C'00'      SPECIAL 10 CHARACTER VERSION                 
         MVC   2(8,R2),TACDCHK                                                  
         CLI   2(R2),C'0'                                                       
         BH    XIT2                                                             
         MVI   2(R2),C'0'                                                       
         B     XIT2                                                             
         SPACE 1                                                                
INCDDAYS DS    0H                  N'DAYS TO CASH CHECK                         
         USING TACDD,R4                                                         
         XC    0(8,R2),0(R2)       PRECLEAR BOTH FIELDS                         
         OC    TACDDTE,TACDDTE     NEED CHECK DATE                              
         BZ    XIT2                                                             
         OC    TACDCSH,TACDCSH          AND CASH DATE                           
         BZ    XIT2                                                             
         GOTO1 DATCON,DMCB,(1,TACDDTE),(0,WORK)                                 
         GOTO1 DATCON,DMCB,(1,TACDCSH),(0,WORK+6)                               
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         LH    R1,DMCB+8           N'DAYS                                       
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                                                             
         ST    R1,0(R2)                                                         
         MVC   4(4,R2),=F'1'                                                    
         B     XIT2                                                             
         SPACE 1                                                                
INCDEARN DS    0H                  CHECK DETAILS EARNINGS                       
         USING TACDD,R4                                                         
         L     R1,TACDEARN                                                      
                                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         USING TAPDD,R4                                                         
         BAS   RE,GETEL2                                                        
         BNE   CVD2                                                             
         CLI   TAPDLEN,TAPDLNQ     NEW ELEMENT LENGTH                           
         BL    CVD2                                                             
         ICM   R0,15,TAPDTXNW      ADD IN TAXABLE REIMB                         
         AR    R1,R0                                                            
         B     CVD2                                                             
         SPACE 1                                                                
         USING TACDD,R4                                                         
INCDNCNT XC    0(4,R2),0(R2)       CHECK NUMBER COUNT                           
         CLC   TACDCHK,=CL8' '     CHECK NUMBER MUST EXIST                      
         BNH   XIT2                                                             
         MVC   0(4,R2),=F'1'       TO COUNT IT                                  
         B     XIT2                                                             
         SPACE 1                                                                
         USING TACDD,R4                                                         
INCDCNT  MVC   0(4,R2),=F'1'       EVERY CHECK, TRANS + REFUNDS TOO             
         B     XIT2                                                             
         SPACE 1                                                                
NUMDAYS  DS    0H                  N'DAYS BETWEEN PREVIOUS 2                    
         L     R4,TIAREC                                                        
         CLI   0(R4),TLCKCDQ       IF A CHECK AROUND                            
         BNE   NUMDAYS1                                                         
         L     R4,ATHISEL          ONLY PROCESS ONCE                            
         LTR   R4,R4                                                            
         BZ    NUMDAYS1                                                         
         CLI   0(R4),TAPDELQ                                                    
         BE    *+8                                                              
         CLI   0(R4),TAEUELQ                                                    
         BNE   XIT2                                                             
         SPACE 1                                                                
NUMDAYS1 LR    R3,R2               BACK UP 2 DATES                              
         SH    R3,=H'6'                                                         
         XC    0(8,R2),0(R2)       PRECLEAR BOTH FIELDS                         
         OC    0(3,R3),0(R3)       NEED BOTH DATES                              
         BZ    XIT2                                                             
         OC    3(3,R3),3(R3)                                                    
         BZ    XIT2                                                             
         GOTO1 DATCON,DMCB,(1,0(R3)),(0,WORK)                                   
         GOTO1 DATCON,DMCB,(1,3(R3)),(0,WORK+6)                                 
         MVI   NDSIGN,C'+'                                                      
         CLC   WORK+6(6),WORK                                                   
         BH    NUMDAYS2                                                         
         MVI   NDSIGN,C'-'                                                      
         GOTO1 DATCON,DMCB,(1,3(R3)),(0,WORK)                                   
         GOTO1 DATCON,DMCB,(1,0(R3)),(0,WORK+6)                                 
         SPACE 1                                                                
NUMDAYS2 GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         LH    R1,DMCB+8           N'DAYS                                       
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                                                             
         CLI   NDSIGN,C'-'                                                      
         BNE   *+6                                                              
         LCR   R1,R1                                                            
         ST    R1,0(R2)                                                         
         MVC   4(4,R2),=F'1'                                                    
         B     XIT2                                                             
         SPACE 1                                                                
NDSIGN   DC    C'+'                                                             
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - COMMERCIAL                           
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R3=LENGTH-1                                  
*                                  R4=A(ELEMENT)                                
*                                  RE=A(ADDROUT ENTRY)                          
         SPACE 1                                                                
INEXMVC  DS    0H                  EXECUTED MOVES                               
         MVC   MVCAREA,SPACES2                                                  
         ZIC   RF,1(R4)                                                         
         ZIC   R0,2(RE)                                                         
         SR    RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MVCAREA(0),0(R1)                                                 
         CLI   0(R4),TANUELQ       IF THIS IS AN ESTIMATE #                     
         BNE   INEXMVC2                                                         
                                                                                
         CLI   2(R4),TANUTRST                                                   
         BE    INEXMVC4                                                         
                                                                                
         CLI   2(R4),TANUTEST                                                   
         BNE   INEXMVC2                                                         
         CLI   GLARGS+3,C'E'       IF ESTIMATE FIELD ONLY                       
         BNE   *+12                                                             
         MVI   GLARGS+2,0          IGNORE 2ND ARGUMENT                          
         B     INEXMVC2                                                         
         CLI   1(R4),9             ELSE, IF < 6 CHARACTERS LONG                 
         BH    INEXMVC2                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL2                                                        
         BNE   INEXMVC2                                                         
         USING TAPDD,R4                                                         
         CLI   TAPDCLI+3,C' '                                                   
         BH    INEXMVC2                                                         
         CLI   TAPDPRD,C'A'                                                     
         BL    INEXMVC2                                                         
         MVC   MVCAREA+6(6),MVCAREA                                             
         MVC   MVCAREA(3),TAPDCLI  PREFIX WITH CLI/PRD                          
         MVC   MVCAREA+3(3),TAPDPRD                                             
         SPACE 1                                                                
INEXMVC2 ZIC   R1,GLARGS+2         OPTIONAL DISPLACEMENT                        
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                                                             
         LA    R1,MVCAREA(R1)                                                   
         EX    R3,*+8                                                           
         B     XIT2                                                             
         MVC   0(0,R2),0(R1)                                                    
                                                                                
INEXMVC4 TM    TGSYSTAT,TASYSPID                                                
         BZ    INEXMVC2                                                         
         ZIC   R1,GLARGS+2         CONVERT SS NUMBER                            
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                                                             
         LA    R1,MVCAREA(R1)                                                   
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TGSSN(0),0(R1)                                                   
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   0(L'TGPID,R2),TGPID                                              
         B     XIT2                                                             
                                                                                
MVCAREA  DS    CL180                                                            
         SPACE 1                                                                
INCOMED  MVC   0(1,R2),0(R1)                                                    
         GOTO1 MEDVAL,DMCB,(R2)    EXPAND NAME                                  
         MVC   0(5,R2),TGMENAME                                                 
         B     XIT2                                                             
         SPACE 1                                                                
INCOSTAT DS    0H                  COMMERCIAL STATUS                            
         TM    0(R1),TACOSTLO                                                   
         BNO   *+14                                                             
         MVC   0(6,R2),=C'LOCKED'                                               
         LA    R2,7(R2)                                                         
         TM    0(R1),TACOSTNO                                                   
         BNO   *+14                                                             
         MVC   0(5,R2),=C'NO HF'                                                
         LA    R2,6(R2)                                                         
         TM    0(R1),TACOSTRL                                                   
         BNO   *+14                                                             
         MVC   0(8,R2),=C'RELEASED'                                             
         LA    R2,9(R2)                                                         
         TM    0(R1),TACOSCAN                                                   
         BNO   *+14                                                             
         MVC   0(4,R2),=C'CAN$'                                                 
         LA    R2,5(R2)                                                         
         TM    0(R1),TACOSCRT                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'CANRATE'                                              
         LA    R2,8(R2)                                                         
         TM    0(R1),TACOSWDT                                                   
         BNO   *+14                                                             
         MVC   0(9,R2),=C'WORK DATE'                                            
         LA    R2,10(R2)                                                        
         TM    0(R1),TACOSRES                                                   
         BNO   *+14                                                             
         MVC   0(9,R2),=C'RESIDUALS'                                            
         LA    R2,10(R2)                                                        
         LA    RF,(TACOSTAT-TACOSTA2)                                           
         SR    R1,RF                                                            
         TM    0(R1),TACOSANO                                                   
         BNO   *+14                                                             
         MVC   0(3,R2),=C'ANO'                                                  
         LA    R2,4(R2)                                                         
         TM    0(R1),TACOSLFT                                                   
         BNO   *+14                                                             
         MVC   0(4,R2),=C'LIFT'                                                 
         LA    R2,5(R2)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INCOUVST DS    0H                  UNVERIFIED STATUS                            
         TM    0(R1),TACOUVCO                                                   
         BNO   *+14                                                             
         MVC   0(5,R2),=C'COMM.'                                                
         LA    R2,6(R2)                                                         
         TM    0(R1),TACOUVMU                                                   
         BNO   *+14                                                             
         MVC   0(8,R2),=C'MUSICIAN'                                             
         LA    R2,9(R2)                                                         
         TM    0(R1),TACOUVNM                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'NON-MUS'                                              
         LA    R2,8(R2)                                                         
         TM    0(R1),TACOUVER                                                   
         BNO   *+14                                                             
         MVC   0(8,R2),=C'VER ONCE'                                             
         LA    R2,9(R2)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INOCDTE  MVC   0(3,R2),0(R1)       CHANGE DATE                                  
         XC    0(3,R2),=3X'FF'     (UNCOMPLEMENT)                               
         B     XIT2                                                             
         EJECT                                                                  
INCOSES  DS    0H                  SESSION TYPE                                 
         CLI   0(R1),C' '          EXIT IF NO SESSION TYPE                      
         BNH   XIT2                                                             
         LA    R4,STYPTAB                                                       
INCOS10  CLI   0(R4),X'FF'         TEST END OF TABLE                            
         BE    INCOS30                                                          
         CLC   0(1,R4),0(R1)                                                    
         BE    INCOS20                                                          
         LA    R4,L'STYPTAB(R4)    BUMP TO NEXT ENTRY                           
         B     INCOS10                                                          
         SPACE 1                                                                
INCOS20  MVC   0(L'STYPTAB-1,R2),1(R4)  SET NAME OF SESSION TYPE                
         B     XIT2                                                             
INCOS30  MVC   0(1,R2),0(R1)       IF NO MATCH, JUST PRINT LETTER               
         B     XIT2                                                             
*                                                                               
STYPTAB  DS    0CL17        TABLE OF SESSTION TYPES AND DESCRIPTIONS            
         DC    C'D',CL16'DEMO'                                                  
         DC    C'F',CL16'FINAL'                                                 
         DC    C'M',CL16'MASTER RECORDING'                                      
         DC    C'T',CL16'TEST MARKET'                                           
         DC    X'FF'                                                            
         SPACE 1                                                                
         EJECT                                                                  
INCOATYP XC    0(4,R2),0(R2)                 ACTRA TYPE                         
         MVC   0(1,R2),0(R1)                                                    
         GOTO1 CCTYPVAL,DMCB,(X'80',(R2))                                       
         BNE   *+10                                                             
         MVC   0(4,R2),TGCCTCDE                                                 
         B     XIT2                                                             
         SPACE 1                                                                
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - OVERSCALE AMOUNTS ELEMENT            
         SPACE 1                                                                
INOAAMT  MVC   0(70,R2),SPACES2                                                 
         USING TAOAD,R4                                                         
         LA    R3,TAOASBEL                                                      
         ZIC   R1,TAOANUM                                                       
         DROP  R4                                                               
         LR    R4,R1                                                            
*                                                                               
INOAAMT5 CLC   0(3,R3),SPACES2     IF USE CODE IS SPACES                        
         BNE   *+14                                                             
         MVC   0(3,R2),=C'ALL'     DISPLAY 'ALL'                                
         B     *+10                                                             
         MVC   0(3,R2),0(R3)       ELSE, DISPLAY USE CODE                       
         MVI   3(R2),C'='          DISPLAY '=AMOUNT'                            
         ICM   RF,15,3(R3)                                                      
         GOTO1 DR2DEC,DMCB,(RF),4(R2),10                                        
         LA    R2,5(R2)            BUMP TO NEXT POSITION                        
         A     R2,0(R1)                                                         
         LA    R3,L'TAOASBEL(R3)                                                
         BCT   R4,INOAAMT5                                                      
         B     XIT2                                                             
         SPACE 2                                                                
*                                                                               
DR2DEC   NTR1                                                                   
         LM    R2,R4,0(R1)         R2 = NUMBER, R3 = A(DESTINATION)             
         BCTR  R4,0                R4 = LENGTH OF DESTINATION - 1               
*                                                                               
         LTR   R2,R2               IF NUMBER = 0                                
         BNZ   *+12                                                             
         MVI   0(R3),C'0'          THEN DISPLAY '0' AND RETURN                  
         B     DR2DECX                                                          
*                                  ELSE EDIT PERCENTAGE INTO BLOCK              
         EDIT  (R2),(10,BLOCK),2,ALIGN=LEFT                                     
         LA    RF,BLOCK            RF = A(LAST CHAR IN BLOCK)                   
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
*                                                                               
DR2DEC10 CLI   0(RF),C'.'          IF CHAR IS '.' THEN DONE                     
         BE    DR2DEC50                                                         
         CLI   0(RF),C'0'          ELSE IF CHAR IS <> '0' THEN DONE             
         BNE   DR2DEC60                                                         
         BCT   RF,DR2DEC10         ELSE BACK UP POINTER AND LOOP BACK           
*                                                                               
DR2DEC50 BCTR  RF,0                BACK UP ONE MORE FOR '.'                     
*                                                                               
DR2DEC60 LA    RE,BLOCK            RF = LENGTH OF NUMBER - 1                    
         SR    RF,RE                                                            
         CR    RF,R4               IF L(NUMBER) - 1 > L(DEST) - 1               
         BH    DR2DEC90            THEN DISPLAY '*'S                            
*                                                                               
         EX    RF,*+8              ELSE MOVE NUMBER TO DESTINATION              
         B     *+10                                                             
         MVC   0(0,R3),BLOCK                                                    
         LA    RF,1(RF)            RETURN LENGTH IN PARM1                       
         ST    RF,0(R1)                                                         
         B     DR2DECX                                                          
*                                                                               
DR2DEC90 MVI   0(R3),C'*'          DISPLAY '*'S                                 
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),0(R3)                                                    
         LA    RF,2(R4)            RETURN LENGTH IN PARM1                       
         ST    RF,0(R1)                                                         
*                                                                               
DR2DECX  B     XIT2                                                             
         EJECT                                                                  
         USING TACPD,R4                                                         
INCPMUS4 DS    0H                                                               
         BAS   RE,NEXTEL2          FOURTH MUSIC CODE                            
         BNE   XIT2                                                             
INCPMUS3 DS    0H                                                               
         BAS   RE,NEXTEL2          THIRD MUSIC CODE                             
         BNE   XIT2                                                             
INCPMUS2 DS    0H                                                               
         BAS   RE,NEXTEL2                                                       
         BNE   XIT2                                                             
         MVC   0(L'TACPMUS,R2),TACPMUS  SECOND MUSIC CODE                       
         B     XIT2                                                             
         SPACE 1                                                                
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - DUE COMPANY                          
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R3=LENGTH-1                                  
*                                  R4=A(ELEMENT)                                
         SPACE 1                                                                
INDUTYPE DS    0H                                                               
         MVC   0(9,R2),=C'AGY FAULT'                                            
         CLI   0(R1),TADUTYAY                                                   
         BE    XIT2                                                             
         MVC   0(11,R2),=C'DUE COMPANY'                                         
         B     XIT2                                                             
         SPACE 1                                                                
INDUSTAT DS    0H                  DUECOMP STATUS                               
         TM    0(R1),TADUSAGY                                                   
         BNO   *+14                                                             
         MVC   0(6,R2),=C'AGENCY'                                               
         LA    R2,7(R2)                                                         
         TM    0(R1),TADUSCLI                                                   
         BNO   *+14                                                             
         MVC   0(6,R2),=C'CLIENT'                                               
         LA    R2,7(R2)                                                         
         TM    0(R1),TADUSAUT                                                   
         BNO   *+14                                                             
         MVC   0(4,R2),=C'AUTO'                                                 
         LA    R2,5(R2)                                                         
         TM    0(R1),TADUSCAN                                                   
         BNO   *+14                                                             
         MVC   0(4,R2),=C'$CAN'                                                 
         LA    R2,5(R2)                                                         
         TM    0(R1),TADUSEUR                                                   
         BNO   *+14                                                             
         MVC   0(5,R2),=C'EUROS'                                                
         LA    R2,5(R2)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INGCAPPL DS    0H                  APPLIED = TOTAL - BALANCE                    
INCRAMT  DS    0H                  AMOUNT = APPLIED - BALANCE                   
INDUBAL  LM    RE,RF,0(R1)         BALANCE = DUE-COLLECTED                      
         SR    RE,RF               (USED FOR LIENS AS WELL)                     
         CVD   RE,0(R2)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INDUFLAG DS    0H                  DUECOMP STATUS                               
         MVI   0(R2),C'N'          NON-TAXABLE                                  
         TM    0(R1),TADUSNTR                                                   
         BO    XIT2                                                             
         MVI   0(R2),C'R'          TAXABLE REIMB                                
         TM    TADUSTA2-TADUSTAT(R1),TADUSTXR                                   
         BO    XIT2                                                             
         MVI   0(R2),C'T'          TAXABLE WAGES                                
         B     XIT2                                                             
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - COMMERCIAL                           
         SPACE 1                                                                
         USING TACSD,R4                                                         
INCSSTA  CLI   TACSLEN,TACSLNQ                                                  
         BL    XIT2                                                             
         MVC   0(2,R2),TACSSTAT                                                 
         OC    0(2,R2),SPACES2                                                  
         B     XIT2                                                             
         DROP  R4                                                               
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - ADVICE ELEMENTS                      
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R3=LENGTH-1                                  
*                                  R4=A(ELEMENT)                                
         SPACE 1                                                                
INDVSTAT DS    0H                                                               
         TM    0(R1),TADVSVER                                                   
         BNO   *+8                                                              
         MVI   0(R2),C'V'                                                       
         TM    0(R1),TADVSSNT                                                   
         BNO   *+8                                                              
         MVI   1(R2),C'S'                                                       
         TM    0(R1),TADVSRCV                                                   
         BNO   *+8                                                              
         MVI   2(R2),C'R'                                                       
         B     XIT2                                                             
         SPACE 1                                                                
INDVTYPE DS    0H                  ADVICE TYPE                                  
         CLI   0(R1),TADVTYPR      REUSE=PAYMENT                                
         BNE   *+10                                                             
         MVC   0(7,R2),=C'PAYMENT'                                              
         CLI   0(R1),TADVTYPS      SESSION=COMPLETION                           
         BNE   *+10                                                             
         MVC   0(8,R2),=C'COMPLETE'                                             
         B     XIT2                                                             
         SPACE 1                                                                
         USING TAVUD,R4                                                         
INVUUSE  MVC   0(3,R2),SPACES2                                                  
         CLI   TAVUUSE,0                                                        
         BE    XIT2                                                             
         CLI   TAVUUSE,X'FD'                                                    
         BH    XIT2                                                             
         GOTO1 USEVAL,DMCB,(X'80',TAVUUSE),WORK                                 
         MVC   0(3,R2),TGUSCDE                                                  
         B     XIT2                                                             
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - ESTIMATE PROFILE                     
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R3=LENGTH-1                                  
*                                  R4=A(ELEMENT)                                
         SPACE 1                                                                
         USING TAEPD,R3                                                         
INEPAC   LR    R3,R4                                                            
         SR    R1,R1                                                            
         CLI   TIMODE,PROCREC      ONLY PROCESS AT LOW LEVEL                    
         BNE   CVD2                                                             
         CLI   TAEPCOMM,TAEPCNO    (NO COMMISSION)                              
         BE    CVD2                                                             
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         USING TAPDD,R4                                                         
         BAS   RE,GETEL2                                                        
         BNE   CVD2                                                             
         L     R1,TAPDPAYI                                                      
         A     R1,TAPDPAYC                                                      
         A     R1,TAPDREXP                                                      
         CLI   TAEPCOMM,TAEPCPAY   OPTION PAY+                                  
         BE    INEPAC2                                                          
         A     R1,TAPDPNH                                                       
         CLI   TAEPCOMM,TAEPCYES   OPTION PAY+ PNH                              
         BE    INEPAC2                                                          
         L     R4,TIAREC                                                        
         MVI   ELCODE,TABDELQ                                                   
         USING TABDD,R4                                                         
         BAS   RE,GETEL2                                                        
         BNE   INEPAC2                                                          
         A     R1,TABDHND          OPTION PAY+ PNH + T&H                        
         A     R1,TABDHNDC                                                      
         A     R1,TABDTAX                                                       
         SPACE 1                                                                
INEPAC2  LH    R0,TAEPRATE         AGY. COMM. RATE (2 DECS)                     
         MR    R0,R0               COMPUTE A/C                                  
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         B     CVD2                                                             
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - INTERFACE DETAILS                    
         SPACE 3                                                                
INIFWCS  MVC   0(40,R2),SPACES2                                                 
         USING TAIFD,R4                                                         
         ZIC   R0,TAIFNWCS                                                      
         LA    R3,TAIFWCS                                                       
         DROP  R4                                                               
         SPACE 1                                                                
INIFWCS2 MVC   0(2,R2),0(R3)                                                    
         LA    R2,3(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R0,INIFWCS2                                                      
         B     XIT2                                                             
         SPACE 1                                                                
INPACK   L     RF,0(R1)            BINARY TO PL8                                
         CVD   RF,0(R2)                                                         
         B     XIT2                                                             
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - GUARANTEE DETAILS                    
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R3=LENGTH-1                                  
*                                  R4=A(ELEMENT)                                
         SPACE 1                                                                
INGUSTAT DS    0H                  GUARANTEE STATUS                             
         TM    0(R1),TAGUSDES                                                   
         BNO   *+14                                                             
         MVC   0(10,R2),=C'DESCENDING'                                          
         LA    R2,11(R2)                                                        
         TM    0(R1),TAGUSPNH                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'PAY P&&H'                                             
         LA    R2,8(R2)                                                         
         TM    0(R1),TAGUSOVR                                                   
         BNO   *+14                                                             
         MVC   0(11,R2),=C'PAY OVERAGE'                                         
         LA    R2,12(R2)                                                        
         TM    0(R1),TAGUSPAY                                                   
         BNO   *+14                                                             
         MVC   0(9,R2),=C'PAY ADDED'                                            
         LA    R2,10(R2)                                                        
         TM    0(R1),TAGUSINS                                                   
         BNO   *+14                                                             
         MVC   0(11,R2),=C'INSTALLMENT'                                         
         LA    R2,12(R2)                                                        
         TM    0(R1),TAGUSNO                                                    
         BNO   *+14                                                             
         MVC   0(8,R2),=C'DISABLED'                                             
         LA    R2,9(R2)                                                         
         TM    0(R1),TAGUSREF                                                   
         BNO   *+14                                                             
         MVC   0(11,R2),=C'BAL REBUILT'                                         
         LA    R2,12(R2)                                                        
         B     XIT2                                                             
         SPACE 1                                                                
INGUCRED LM    RE,RF,0(R1)         CREDITS = AMOUNT-BALANCE                     
         SR    RE,RF                                                            
         BP    *+6                                                              
         LCR   RE,RE                                                            
         CVD   RE,0(R2)                                                         
         B     XIT2                                                             
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - INVOICE                              
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R3=LENGTH-1                                  
*                                  R4=A(ELEMENT)                                
         SPACE 1                                                                
ININSTAT DS    0H                  INVOICE STATUS                               
         TM    0(R1),TAINSPAY                                                   
         BNO   *+14                                                             
         MVC   0(4,R2),=C'PAID'                                                 
         LA    R2,5(R2)                                                         
         TM    0(R1),TAINSAPR                                                   
         BNO   *+14                                                             
         MVC   0(8,R2),=C'APPROVED'                                             
         LA    R2,9(R2)                                                         
         TM    0(R1),TAINSBIL                                                   
         BNO   *+14                                                             
         MVC   0(6,R2),=C'BILLED'                                               
         LA    R2,7(R2)                                                         
         TM    0(R1),TAINSCHK                                                   
         BNO   *+14                                                             
         MVC   0(6,R2),=C'CHECKS'                                               
         LA    R2,7(R2)                                                         
         TM    0(R1),TAINSERR                                                   
         BNO   *+14                                                             
         MVC   0(8,R2),=C'IN ERROR'                                             
         LA    R2,9(R2)                                                         
         TM    0(R1),TAINSCIN                                                   
         BNO   *+14                                                             
         MVC   0(8,R2),=C'CANCELED'                                             
         LA    R2,9(R2)                                                         
         TM    0(R1),TAINSHLD                                                   
         BNO   *+14                                                             
         MVC   0(4,R2),=C'HOLD'                                                 
         LA    R2,5(R2)                                                         
         TM    0(R1),TAINSCAN                                                   
         BNO   *+14                                                             
         MVC   0(9,R2),=C'CANCELLED'                                            
         LA    R2,10(R2)                                                        
         TM    1(R1),TAINSURG      NOW ON 2ND STATUS BYTE                       
         BNO   *+14                                                             
         MVC   0(6,R2),=C'URGENT'                                               
         LA    R2,7(R2)                                                         
         TM    1(R1),TAINSHLR                                                   
         BNO   *+14                                                             
         MVC   0(8,R2),=C'RELEASED'                                             
         LA    R2,9(R2)                                                         
         B     XIT2                                                             
         EJECT                                                                  
         USING TAIND,R4                                                         
ININSTA3 DS    0H                  INVOICE STATUS                               
         TM    0(R1),TAINSRSK      RETRO SKIP FROM TRG                          
         BZ    XIT2                                                             
         MVC   0(4,R2),=C'SKIP'                                                 
         B     XIT2                                                             
                                                                                
         USING TAIND,R4                                                         
ININITIM DS    0H                  INVOICE ASSIGNMENT TIME                      
         MVC   0(3,R2),TAINITIM                                                 
         MVC   3(3,R2),TAINIDTE                                                 
         B     XIT2                                                             
ININPTIM DS    0H                  INVOICE PAID TIME                            
         MVC   0(3,R2),TAINPTIM                                                 
         MVC   3(3,R2),TAINPDTE                                                 
         B     XIT2                                                             
ININQTIM DS    0H                  INVOICE APPROVED TIME                        
         MVC   0(3,R2),TAINQTIM                                                 
         MVC   3(3,R2),TAINQDTE                                                 
         B     XIT2                                                             
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - MUSIC RECORDS                        
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R3=LENGTH-1                                  
*                                  R4=A(ELEMENT)                                
         SPACE 1                                                                
         USING TAMUD,R4                                                         
INMUNAME EX    R3,*+8              VARIOUS NAMES                                
         B     *+10                                                             
         MVC   0(0,R2),SPACES2                                                  
         ZIC   RF,TAMULEN          EL LENGTH                                    
         SH    RF,=H'5'            LESS OVERHEAD                                
         SR    RE,RE                                                            
         D     RE,=F'36'           N'36 CHARACTER LINES IN RF                   
*                                  L'LAST LINE IN RE                            
         LTR   RF,RF                                                            
         BZ    INMUNM4                                                          
         LA    RF,1                **** ONLY HANDLING ONE LINE                  
*                                  **** DRIVER HAD PROBS WITH LONG KEYS         
*                                  **** NOW MAX OF 36 CHARS                     
         SPACE 1                                                                
INMUNM2  MVC   0(36,R2),0(R1)      HANDLE FULL (36 BYTE) LINES                  
         LA    R2,36(R2)                                                        
         LA    R1,36(R1)                                                        
         BCT   RF,INMUNM2                                                       
         SPACE 1                                                                
INMUNM4  LTR   RE,RE               HANDLE LAST LINE                             
         BZ    XIT2                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     XIT2                                                             
         MVC   0(0,R2),0(R1)                                                    
         SPACE 1                                                                
INMULIC  MVC   0(1,R2),0(R1)       LICENSER - CODE                              
         GOTO1 LICVAL,DMCB,(X'80',0(R2))                                        
         MVC   0(9,R2),TGLCNAME    TO NAME                                      
         B     XIT2                                                             
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - PAY DETAILS                          
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R3=LENGTH-1                                  
*                                  R4=A(ELEMENT)                                
         SPACE 1                                                                
INPDUSNM LR    R5,R1               USE NAME                                     
         GOTO1 USEVAL,DMCB,(R5),3(R5)                                           
         MVC   0(16,R2),TGUSNAME                                                
         B     XIT2                                                             
         SPACE 1                                                                
         USING TAPDD,R4                                                         
INPDAPNH ZAP   0(8,R2),=P'0'       PRE-CLEAR                                    
         L     R1,TAPDPNH          APPLIED P&H   P&H                            
         M     R0,TAPDAPPL                     X APPLIED AMOUNT                 
         OC    TAPDGRS,TAPDGRS                                                  
         BZ    XIT2                                                             
         D     R0,TAPDGRS                      / GROSS                          
         B     CVD2                                                             
         SPACE 1                                                                
CVD1     L     R1,0(R1)                                                         
CVD2     CVD   R1,0(R2)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
ZERO1    MVI   0(R2),0             NO INFORMATION (1 BYTE)                      
         B     XIT2                                                             
ZERO2    XC    0(2,R2),0(R2)       NO INFORMATION (2 BYTES)                     
         B     XIT2                                                             
ZERO4    XC    0(4,R2),0(R2)       NO INFORMATION (4 BYTES)                     
         B     XIT2                                                             
         SPACE 1                                                                
         USING TABDD,R4                                                         
INBDHAND L     R1,TABDHND          HND + HNDC                                   
         A     R1,TABDHNDC                                                      
         B     CVD2                                                             
         SPACE 1                                                                
         USING TABDD,R4                                                         
INBDHNTX L     R1,TABDHND          HND + HNDC + TAX + FICR                      
         A     R1,TABDHNDC                                                      
         A     R1,TABDTAX                                                       
         A     R1,TABDFICR                                                      
         B     CVD2                                                             
         SPACE 1                                                                
         USING TABDD,R4                                                         
INBDHITX L     R1,TABDHND          HND + TAX + FICR                             
         A     R1,TABDTAX                                                       
         A     R1,TABDFICR                                                      
         B     CVD2                                                             
         SPACE 1                                                                
         USING TABDD,R4                                                         
INBDACSG L     R1,TABDACOM         HND + TAX + FICR                             
         A     R1,TABDSIGN                                                      
         B     CVD2                                                             
         SPACE 1                                                                
         USING TABDD,R4                                                         
INBDHST  L     R1,TABDGST          GST + PST                                    
         A     R1,TABDPST                                                       
         B     CVD2                                                             
         SPACE 1                                                                
         USING TABDD,R4                                                         
INBDCCVT SR    R1,R1               CCVT                                         
         ICM   R1,7,TABDCCVT                                                    
         BNZ   INBDCVT5                                                         
         ZAP   0(L'TABDCCVT,R2),=P'0'                                           
         B     XIT2                                                             
INBDCVT5 MVC   0(L'TABDCCVT,R2),TABDCCVT                                        
         B     XIT2                                                             
         SPACE 1                                                                
         USING TACXD,R4                                                         
INCXHST  L     R1,TACXGST          GST + PST                                    
         A     R1,TACXPST                                                       
         B     CVD2                                                             
         SPACE 1                                                                
INCXHSTY L     R1,TACXGSTY         YTD GST + YTD PST                            
         A     R1,TACXPSTY                                                      
         B     CVD2                                                             
         SPACE 1                                                                
         USING TAPDD,R4                                                         
INPDGRS  BAS   RE,UNITTATU         SEE IF THERE'S A UNIT FILTER & TATU          
         BE    INPDGRS5                                                         
         L     R1,TAPDGRS          GROSS + TXNW                                 
         BAS   RE,INPDTXNW                                                      
         B     CVD2                                                             
INPDGRS5 GOTO1 =A(TUAMTS),DMCB,(X'C0',0)                                        
         B     CVD2                                                             
*                                                                               
         USING TAPDD,R4                                                         
INPDPAY  BAS   RE,UNITTATU         SEE IF THERE'S A UNIT FILTER & TATU          
         BE    INPDPY5             THERE IS, SO DIFFERENT PROCESS               
         L     R1,TAPDPAYI         PAYI + PAYC + TXNW                           
         A     R1,TAPDPAYC                                                      
         BAS   RE,INPDTXNW                                                      
         B     CVD2                                                             
INPDPY5  GOTO1 =A(TUAMTS),DMCB,(X'C0',0)                                        
         B     CVD2                                                             
*                                                                               
         USING TAPDD,R4                                                         
INPDPAYP BAS   RE,UNITTATU         SEE IF THERE'S A UNIT FILTER & TATU          
         BE    INPDPYP5            THERE IS, SO DIFFERENT PROCESS               
         L     R1,TAPDPAYI         PAYI + PAYC + REXP                           
         A     R1,TAPDPAYC                                                      
         BAS   RE,INPDTXNW                                                      
         BAS   RE,INPDNTRM         REXP / NTNW                                  
         B     CVD2                                                             
INPDPYP5 GOTO1 =A(TUAMTS),DMCB,(X'E0',0)                                        
         B     CVD2                                                             
                                                                                
         USING TAPDD,R4                                                         
INPDPYI  BAS   RE,UNITTATU         SEE IF THERE'S A UNIT FILTER & TATU          
         BE    INPDPYI5            THERE IS, SO DIFFERENT PROCESS               
         L     R1,TAPDPAYI         PAYI + TXNW                                  
         BAS   RE,INPDTXNW                                                      
         B     CVD2                                                             
INPDPYI5 MVI   BYTE,X'C0'                                                       
         OC    TAPDPAYI,TAPDPAYI                                                
         BNZ   INPDPYI8                                                         
         MVI   BYTE,X'40'                                                       
INPDPYI8 GOTO1 =A(TUAMTS),DMCB,(BYTE,0)                                         
         B     CVD2                                                             
                                                                                
         USING TAPDD,R4                                                         
INPDPYIR BAS   RE,UNITTATU         SEE IF THERE'S A UNIT FILTER & TATU          
         BE    INPDPIR5            THERE IS, SO DIFFERENT PROCESS               
         L     R1,TAPDPAYI         PAYI + REXP                                  
         A     R1,TAPDREXP                                                      
         BAS   RE,INPDTXNW                                                      
         B     CVD2                                                             
INPDPIR5 MVI   BYTE,X'E0'                                                       
         OC    TAPDPAYI,TAPDPAYI                                                
         BNZ   INPDPIR8                                                         
         MVI   BYTE,X'60'                                                       
INPDPIR8 GOTO1 =A(TUAMTS),DMCB,(BYTE,0)                                         
         B     CVD2                                                             
                                                                                
         USING TAPDD,R4                                                         
INPDPYIT BAS   RE,UNITTATU         SEE IF THERE'S A UNIT FILTER & TATU          
         BE    INPDPIT5            THERE IS, SO DIFFERENT PROCESS               
         L     R1,TAPDPAYI         PAYI                                         
         B     CVD2                                                             
INPDPIT5 OC    TAPDPAYI,TAPDPAYI                                                
         BNZ   INPDPIR8                                                         
         XR    R1,R1                                                            
         B     CVD2                                                             
INPDPIT8 GOTO1 =A(TUAMTS),DMCB,(X'80',0)                                        
         B     CVD2                                                             
                                                                                
         USING TAPDD,R4                                                         
INPDPYC  BAS   RE,UNITTATU         SEE IF THERE'S A UNIT FILTER & TATU          
         BE    INPDPC5             THERE IS, SO DIFFERENT PROCESS               
         L     R1,TAPDPAYC         PAYC                                         
         B     CVD2                                                             
INPDPC5  OC    TAPDPAYC,TAPDPAYC                                                
         BNZ   INPDPC8                                                          
         XR    R1,R1                                                            
         B     CVD2                                                             
INPDPC8  GOTO1 =A(TUAMTS),DMCB,(X'80',0)                                        
         B     CVD2                                                             
                                                                                
         USING TAPDD,R4                                                         
INPDPYCR BAS   RE,UNITTATU         SEE IF THERE'S A UNIT FILTER & TATU          
         BE    INPDPCR5            THERE IS, SO DIFFERENT PROCESS               
         L     R1,TAPDPAYC         PAYC + NTNW                                  
         BAS   RE,INPDNTNW                                                      
         B     CVD2                                                             
INPDPCR5 MVI   BYTE,X'A0'                                                       
         OC    TAPDPAYC,TAPDPAYC                                                
         BNZ   INPDPIR8                                                         
         MVI   BYTE,X'20'                                                       
INPDPCR8 GOTO1 =A(TUAMTS),DMCB,(BYTE,0)                                         
         B     CVD2                                                             
                                                                                
         USING TAPDD,R4                                                         
INPDREXP BAS   RE,UNITTATU         SEE IF THERE'S A UNIT FILTER & TATU          
         BE    INPDRXP5            THERE IS, SO DIFFERENT PROCESS               
         L     R1,TAPDREXP         REXP - TXNW                                  
         BAS   RE,INPDSTNW                                                      
         B     CVD2                                                             
INPDRXP5 GOTO1 =A(TUAMTS),DMCB,(X'20',0)                                        
         B     CVD2                                                             
         SPACE 1                                                                
         USING TAPDD,R4                                                         
INPDTXN  XR    R1,R1                                                            
         CLI   TAPDLEN,TAPDLNQ                                                  
         BL    CVD2                                                             
         ICM   R1,15,TAPDTXNW                                                   
         B     CVD2                                                             
                                                                                
         USING TAPDD,R4                                                         
INPDNTN  XR    R1,R1                                                            
         CLI   TAPDLEN,TAPDLNQ                                                  
         BL    CVD2                                                             
         ICM   R1,15,TAPDNTNW                                                   
         B     CVD2                                                             
                                                                                
         USING TAPDD,R4                                                         
INPDREX  L     R1,TAPDREXP                                                      
         B     CVD2                                                             
         SPACE 1                                                                
         USING TAPDD,R4                                                         
INPDUSES LH    R1,TAPDUSES         NUMBER OF USES                               
         STCM  R1,7,0(R2)          SAVE AS 3 BYTES                              
         B     XIT2                                                             
         SPACE 1                                                                
         USING TAPDD,R4                                                         
INPDUNIT LH    R1,TAPDUNIT         NUMBER OF UNITS                              
         STCM  R1,7,0(R2)          SAVE AS 3 BYTES                              
         B     XIT2                                                             
         SPACE 1                                                                
INPDTAGS XC    0(2,R2),0(R2)       TAGS FROM TAG USE                            
         MVC   1(1,R2),0(R1)                                                    
         B     XIT2                                                             
         SPACE 1                                                                
INPDTXNW CLI   TAPDLEN,TAPDLNQ     ADD IN TAXABLE NON-WAGE                      
         BLR   RE                                                               
         ICM   RF,15,TAPDTXNW                                                   
         AR    R1,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
INPDNTRM ICM   RF,15,TAPDREXP      ADD IN REIMB                                 
         CLI   TAPDLEN,TAPDLNQ                                                  
         BL    *+8                                                              
         ICM   RF,15,TAPDNTNW      OR NON-TAXABLE REIMB IF WE HAVE IT           
         AR    R1,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
INPDNTNW ICM   RF,15,TAPDREXP                                                   
         AR    R1,RF                                                            
         CLI   TAPDLEN,TAPDLNQ     ADD IN NON-TAXABLE NON-WAGE                  
         BLR   RE                                                               
         ICM   RF,15,TAPDTXNW                                                   
         SR    R1,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
INPDSTNW CLI   TAPDLEN,TAPDLNQ     SUB OUT TAXABLE NON-WAGE                     
         BLR   RE                                                               
         ICM   RF,15,TAPDTXNW                                                   
         SR    R1,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
INPISTAT DS    0H                  CLIENT STATUS                                
         MVC   0(36,R2),SPACES2                                                 
         XC    WORK,WORK                                                        
         USING TAPID,R4                                                         
         MVC   WORK(1),TAPISTAT    CLIENT INFORMATION STATUS                    
*                                                                               
         LA    RF,PISTTAB                                                       
INPIST5  CLI   0(RF),X'FF'                                                      
         BE    XIT2                                                             
         MVC   HALF,0(RF)                                                       
         NC    HALF,WORK           IF BIT IS ON IN STATUS BYTE                  
         BZ    INPIST7                                                          
         MVC   0(10,R2),1(RF)      DISPLAY CODE                                 
*                                                                               
         LA    R2,9(R2)            PT TO LAST NON-SPACE                         
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         LA    R2,2(R2)            LEAVE SPACE BETWEEN CODES                    
INPIST7  LA    RF,L'PISTTAB(RF)                                                 
         B     INPIST5                                                          
         SPACE 2                                                                
PISTTAB  DS    0CL11               TABLE OF STATUS CODES AND THEIR BITS         
         DC    AL1(TAPISLCK),CL10'LOCKED'                                       
         DC    AL1(TAPIJPCN),CL10'NOCSF'                                        
         DC    AL1(TAPIJPCY),CL10'CSF'                                          
         DC    X'FF'                                                            
*                                                                               
INPITYPE DS    0H                  PRODUCT TYPE                                 
         CLI   TAPILEN,TAPILNQ2                                                 
         BL    XIT2                                                             
         MVC   0(6,R2),TAPIPTYP                                                 
         B     XIT2                                                             
         EJECT                                                                  
         SPACE 1                                                                
         USING TATUD,R4                                                         
INTUEARN DS    0H                  TU EARNINGS                                  
******** CLI   TATULEN,TATULNQ                                                  
******** BL    XIT2                                                             
         ICM   R1,15,TATUWAGE      WAGES +                                      
         ICM   R0,15,TATUTNWA      TAXABLE NON-WAGES                            
         AR    R1,R0                                                            
         B     CVD2                                                             
         EJECT                                                                  
         SPACE 1                                                                
         USING TAW4D,R4                                                         
INW4SEX  MVC   0(1,R2),TAW4SEX     W4 SEX                                       
         CLI   0(R2),C' '                                                       
         BH    XIT2                                                             
         MVI   0(R2),C'M'          (DEFAULT IS MALE)                            
         B     XIT2                                                             
         SPACE 1                                                                
         USING TAW4D,R4                                                         
INW4RCPS MVC   0(2,R2),=C'  '      DEFAULT SPACES                               
         CLC   TAW4RECP,SPACES2                                                 
         BNH   XIT2                                                             
         MVC   0(2,R2),TAW4RECP    NOT SPACES, USE IT                           
         B     XIT2                                                             
         SPACE 1                                                                
         USING TAW4D,R4                                                         
INW4MID  MVC   0(16,R2),SPACES2    DEFAULT BLANK                                
         CLI   TAW4LEN,TAW4LN2Q                                                 
         BL    XIT2                                                             
         CLC   TAW4MIDN,SPACES2    ANY MIDDLE NAME                              
         BNH   XIT2                                                             
         MVC   0(16,R2),TAW4MIDN                                                
         OC    0(16,R2),SPACES2                                                 
         B     XIT2                                                             
         SPACE 1                                                                
         USING TAW4D,R4                                                         
INW4RACE MVC   0(2,R2),TAW4RACE    W4 RACE                                      
         OC    0(2,R2),SPACES2                                                  
         CLC   0(2,R2),SPACES2                                                  
         BNE   XIT2                                                             
         MVI   0(R2),C'C'          (DEFAULT IS CAUCASIAN)                       
         B     XIT2                                                             
         SPACE 1                                                                
         USING TAW4D,R4                                                         
INW4FULL MVC   0(35,R2),SPACES2    DEFAULT BLANK                                
         MVC   0(16,R2),TAW4NAM1                                                
         LA    RF,17(R2)                                                        
         CLI   TAW4LEN,TAW4LN2Q                                                 
         BL    INW4F20                                                          
         CLI   TAW4TYPE,TAW4TYCO                                                
         BE    INW4F20                                                          
         CLI   TAW4TYPE,TAW4TYTR                                                
         BE    INW4F20                                                          
         MVC   0(1,RF),TAW4MIDN    MIDDLE INIT                                  
         LA    RF,2(RF)                                                         
INW4F20  MVC   0(16,RF),TAW4NAM2                                                
         OC    0(35,R2),SPACES2                                                 
         GOTO1 SQUASHER,DMCB,(R2),35                                            
         B     XIT2                                                             
         SPACE 1                                                                
INW4FTX  CLI   TAW4TYPE,TAW4TYFO   TAKE TAX ON FOREIGNERS                       
         BE    *+8                                                              
         CLI   TAW4TYPE,TAW4TYCA     AND ON FOREIGNERS                          
         BNE   XIT2                                                             
         MVI   0(R2),C'Y'                                                       
         TM    TAW4STA3,TAW4SNTX                                                
         BZ    XIT2                                                             
         MVI   0(R2),C'N'                                                       
         B     XIT2                                                             
         SPACE 1                                                                
INW4NHA  MVI   0(R2),C' '          NEW HIRE ACT STATUS                          
         TM    TAW4STA3,TAW4SNHA   ELIGIBLE?                                    
         BZ    *+8                                                              
         MVI   0(R2),C'Y'                                                       
         TM    TAW4STA3,TAW4SNHP   PENDING                                      
         BZ    *+8                                                              
         MVI   0(R2),C'P'                                                       
         B     XIT2                                                             
         SPACE 1                                                                
         USING TALND,R4                                                         
INLNSTAT DS    0H                  PAYMENT DETAIL STATUS                        
*                                                                               
         TM    0(R1),TALNSCAN                                                   
         BNO   *+14                                                             
         MVC   0(4,R2),=C'CAN$'                                                 
         LA    R2,5(R2)                                                         
*                                                                               
         TM    0(R1),TALNSEUR                                                   
         BNO   *+14                                                             
         MVC   0(5,R2),=C'EUROS'                                                
         LA    R2,5(R2)                                                         
*                                                                               
         B     XIT2                                                             
         SPACE 1                                                                
         USING TAPED,R4                                                         
INPECITY CLI   TAPELEN,TAPELNQ                                                  
         BL    XIT2                                                             
         MVC   0(L'TAPECITY,R2),TAPECITY                                        
         OC    0(L'TAPECITY,R2),SPACES2                                         
         B     XIT2                                                             
         SPACE 1                                                                
INPEST   CLI   TAPELEN,TAPELNQ                                                  
         BL    XIT2                                                             
         MVC   0(L'TAPEST,R2),TAPEST                                            
         OC    0(L'TAPEST,R2),SPACES2                                           
         B     XIT2                                                             
         SPACE 1                                                                
INPEZIP  CLI   TAPELEN,TAPELNQ                                                  
         BL    XIT2                                                             
         MVC   0(L'TAPEZIP,R2),TAPEZIP                                          
         OC    0(L'TAPEZIP,R2),SPACES2                                          
         B     XIT2                                                             
         SPACE 1                                                                
INPECTRY CLI   TAPELEN,TAPELNQ                                                  
         BL    XIT2                                                             
         MVC   0(L'TAPECTRY,R2),TAPECTRY                                        
         OC    0(L'TAPECTRY,R2),SPACES2                                         
         B     XIT2                                                             
         DROP  R4                                                               
         SPACE 1                                                                
INPOYTD  L     RE,0(R1)            POOLED YTD                                   
         A     RE,8(R1)                                                         
         CVD   RE,0(R2)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INGTUSED LR    R5,R4                                                            
         USING TAGTD,R5                                                         
         L     R4,=A(DUMELEM)      DUMMY UP A PD ELEM                           
         USING TAPDD,R4                                                         
         MVC   TAPDUSE(4),TAGTUSE                                               
         MVC   TAPDSTUS(7),TAGTSTUS AND DROP THROUGH ROUTINE BELOW              
         SPACE 1                                                                
INPDUSED DS    0H                  PAYMENT USE DETAILS                          
         USING TAPDD,R4                                                         
         OC    TAPDSTUS,TAPDSTUS   *** FUDGE                                    
         BNZ   INPDU10                                                          
         OC    TAPDUNIT(3),TAPDUNIT                                             
         BNZ   INPDU20                                                          
         OC    TAPDINS,TAPDINS     CHECK FOR INSERTS                            
         BZ    INPDU2                                                           
         EDIT  TAPDINS,(5,0(R2)),ALIGN=LEFT                                     
         B     INPDU6                                                           
INPDU2   OC    TAPDTAGS,TAPDTAGS   CHECK FOR TAGS                               
         BZ    INPDU4                                                           
         EDIT  TAPDTAGS,(3,0(R2)),ALIGN=LEFT                                    
         B     INPDU6                                                           
INPDU4   OC    TAPDDEMS,TAPDDEMS   CHECK FOR DEMOS                              
         BZ    INPDU6                                                           
         EDIT  TAPDDEMS,(3,0(R2)),ALIGN=LEFT                                    
INPDU6   DS    0H                                                               
***      GOTO1 USEVAL,DMCB,TAPDUSE,0                                            
***      TM    TGUSTYST,MAJORS                                                  
***      BO    INPDU20                                                          
***      TM    TGUSTYST,USES                                                    
***      BO    INPDU10                                                          
         B     XIT2                                                             
         SPACE 1                                                                
INPDU10  CLC   TAPDUSES,=H'1'                                                   
         BNE   INPDU12                                                          
         MVC   0(3,R2),=C'USE'                                                  
         EDIT  (2,TAPDSTUS),(4,4(R2)),ALIGN=LEFT                                
         B     XIT2                                                             
         SPACE 1                                                                
INPDU12  MVC   0(4,R2),=C'USES'                                                 
         LA    R2,5(R2)                                                         
         LH    R3,TAPDSTUS                                                      
         EDIT  (R3),(4,0(R2)),ALIGN=LEFT                                        
         AR    R2,R0                                                            
         MVI   0(R2),C'-'                                                       
         AH    R3,TAPDUSES                                                      
         BCTR  R3,0                                                             
         EDIT  (R3),(4,1(R2)),ALIGN=LEFT                                        
         B     XIT2                                                             
         SPACE 1                                                                
INPDU20  GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE                                     
         TM    TGUSTYST,MAJORS                                                  
         BNO   INPDU30                                                          
         GOTO1 MAJVAL,DMCB,(X'80',TAPDMAJ)                                      
         MVC   0(L'TGMACHAR,R2),TGMACHAR                                        
         LA    R2,L'TGMACHAR-1(R2)                                              
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
INPDU30  TM    TGUSTYST,UNITS                                                   
         BNO   XIT2                                                             
         EDIT  (2,TAPDUNIT),(4,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                   
         AR    R2,R0                                                            
         MVC   1(5,R2),=C'UNITS'                                                
         B     XIT2                                                             
         SPACE 1                                                                
INPDCURR DS    0H                  CURRENCY                                     
         MVC   0(4,R2),=C'US$ '                                                 
*                                                                               
         TM    0(R1),TAPDSCAN                                                   
         BNO   *+14                                                             
         MVC   0(4,R2),=C'CAN$'                                                 
         B     INPDCURX                                                         
*                                                                               
         USING TAPDD,R4            ESTABLISH PAYMENT DETAILS ELM                
*                                                                               
         TM    TAPDPST2,TAPDPEUR                                                
         BNO   *+10                                                             
         MVC   0(4,R2),=C'EURO'                                                 
*                                                                               
INPDCURX DS    0H                                                               
         B     XIT2                                                             
         SPACE 1                                                                
INPDSTAT DS    0H                  PAYMENT DETAIL STATUS                        
         TM    0(R1),TAPDSMAN                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'MAN O/R'                                              
         LA    R2,8(R2)                                                         
         TM    0(R1),TAPDSCAN                                                   
         BNO   *+14                                                             
         MVC   0(4,R2),=C'CAN$'                                                 
         LA    R2,5(R2)                                                         
         TM    0(R1),TAPDSLFT                                                   
         BNO   *+14                                                             
         MVC   0(4,R2),=C'LIFT'                                                 
         LA    R2,5(R2)                                                         
         TM    0(R1),TAPDSDDS                                                   
         BNO   *+14                                                             
         MVC   0(3,R2),=C'DDS'                                                  
         LA    R2,4(R2)                                                         
         TM    1(R1),TAPDSPRM                                                   
         BNO   *+14                                                             
         MVC   0(3,R2),=C'PRI'                                                  
         LA    R2,4(R2)                                                         
         TM    1(R1),TAPDSSUB                                                   
         BNO   *+14                                                             
         MVC   0(3,R2),=C'SUB'                                                  
         LA    R2,4(R2)                                                         
*                                                                               
         TM    1(R1),TAPDPEUR                                                   
         BNO   *+14                                                             
         MVC   0(5,R2),=C'EUROS'                                                
         LA    R2,5(R2)                                                         
*                                                                               
         B     XIT2                                                             
         SPACE 1                                                                
INPDPSTS DS    0H                  PAYMENT DETAIL PAYMENT STATUS                
         TM    0(R1),TAPDPSEL                                                   
         BNO   *+14                                                             
         MVC   0(8,R2),=C'CAST-SEL'                                             
         LA    R2,9(R2)                                                         
         TM    0(R1),TAPDPCRD                                                   
         BNO   *+14                                                             
         MVC   0(6,R2),=C'CREDIT'                                               
         LA    R2,7(R2)                                                         
         TM    0(R1),TAPDPBNP                                                   
         BNO   *+14                                                             
         MVC   0(3,R2),=C'BNP'                                                  
         LA    R2,4(R2)                                                         
         TM    0(R1),TAPDPDTL                                                   
         BNO   *+14                                                             
         MVC   0(6,R2),=C'DETAIL'                                               
         LA    R2,7(R2)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INPDOPTS DS    0H                  PAYMENT DETAIL OPTIONS                       
         TM    0(R1),TAPDOAPS                                                   
         BNO   *+14                                                             
         MVC   0(5,R2),=C'APPLY'                                                
         LA    R2,6(R2)                                                         
         TM    0(R1),TAPDONAC                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'NO-CRED'                                              
         LA    R2,8(R2)                                                         
         TM    0(R1),TAPDONGC                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'NO-GUAR'                                              
         LA    R2,8(R2)                                                         
         TM    0(R1),TAPDOCAN                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'CAN-TAX'                                              
         LA    R2,8(R2)                                                         
         TM    0(R1),TAPDOPHR                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'P&&H O/R'                                             
         LA    R2,8(R2)                                                         
         TM    0(R1),TAPDOTAX                                                   
         BNO   *+14                                                             
         MVC   0(7,R2),=C'TAX O/R'                                              
         LA    R2,8(R2)                                                         
         TM    0(R1),TAPDOHND                                                   
         BNO   *+14                                                             
         MVC   0(8,R2),=C'HAND O/R'                                             
         LA    R2,9(R2)                                                         
         TM    1(R1),TAPDOURG                                                   
         BNO   *+14                                                             
         MVC   0(6,R2),=C'URGENT'                                               
         LA    R2,7(R2)                                                         
         TM    1(R1),TAPDODCL                                                   
         BNO   *+14                                                             
         MVC   0(11,R2),=C'DUECOMP-CLI'                                         
         LA    R2,12(R2)                                                        
         TM    1(R1),TAPDODAY                                                   
         BNO   *+14                                                             
         MVC   0(11,R2),=C'DUECOMP-AGY'                                         
         LA    R2,12(R2)                                                        
         TM    1(R1),TAPDODAL                                                   
         BNO   *+14                                                             
         MVC   0(11,R2),=C'DUECOMP-ALL'                                         
         LA    R2,12(R2)                                                        
         TM    29(R1),TAPDONOI                                                  
         BNO   *+14                                                             
         MVC   0(5,R2),=C'NOINT'                                                
         LA    R2,12(R2)                                                        
         B     XIT2                                                             
*&&DO                                                                           
INPDVAR  DS    0H                  PAYMENT DETAIL STATUS 3                      
         TM    0(R1),TAPDSNR1                                                   
         BNO   XIT2                                                             
         MVC   0(4,R2),=C'1ST '                                                 
         LA    R2,8(R2)                                                         
         B     XIT2                                                             
*&&                                                                             
         EJECT                                                                  
*              SEE IF THERE IS A UNIT FILTER AND TATU ELEMENTS                  
UNITTATU NTR1                                                                   
         CLC   TIFUNIT,SPACES2     IS THERE A UNIT FILTER?                      
         JNH   NOGOOD2             NO, LEAVE                                    
                                                                                
         L     R2,AIO              SAVE AIO                                     
         MVI   ELCODE,TATUELQ      GET TAX UNIT                                 
         MVC   AIO,TIAREC                                                       
         LA    RF,TIFUNIT                                                       
         GOTO1 GETL,DMCB,(3,(RF))                                               
         ST    R2,AIO              RESTORE AIO                                  
                                                                                
         XIT1                      CONDITION CODE RETURNED                      
         EJECT                                                                  
*              YTD CHECK SUPPORT                                                
         SPACE 3                                                                
*              ARGUMENT 2  WHERE   F=FEDERAL ONLY                               
*                                  N=STATE & CANADA                             
*                                  S=STATE ONLY                                 
*                                  Y=CITY                                       
*                                  C=CANADA                                     
*                                  O=OTHERS                                     
         SPACE 1                                                                
         USING TACYD,R4                                                         
INCY     L     R4,ATHISEL                                                       
         LTR   R4,R4                                                            
         BZ    XIT2                                                             
         CLI   0(R4),TACYELQ                                                    
         BNE   XIT2                                                             
         MVC   THISUNIT,TACYUNIT   DO THE 'WHERE' FILTERING FIRST               
         BRAS  RE,TESTWHER                                                      
         BNE   XIT2                                                             
         OC    TISKUNIT,TISKUNIT                                                
         BZ    INCY100                                                          
         CLC   THISUNIT,TISKUNIT                                                
         BNE   XIT2                                                             
                                                                                
INCY100  CLI   GLARGS+1,C'F'                                                    
         BE    INCYFED                                                          
         CLI   GLARGS+1,C'N'                                                    
         BE    INCYNFED                                                         
         CLI   GLARGS+1,C'S'                                                    
         BE    INCYSTA                                                          
*        CLI   GLARGS+1,C'L'                                                    
*        BE    INCYSTA                                                          
         CLI   GLARGS+1,C'C'                                                    
         BE    INCYCAN                                                          
         CLI   GLARGS+1,C'Y'                                                    
         BE    INCYCITY                                                         
         CLI   GLARGS+1,C'O'                                                    
         BE    INCYOTH                                                          
         B     INCYSTAT                                                         
         SPACE 1                                                                
INCYFED  CLC   TACYUNIT(2),=C'FD'  FEDERAL ONLY                                 
         BNE   XIT2                                                             
         B     INCYSTAT                                                         
         SPACE 1                                                                
INCYNFED CLC   TACYUNIT(2),=C'FD'  STATES + CANADA - SO IGNORE FED              
         BE    XIT2                                                             
         CLI   TACYUNIT+2,C' '                  AND CITIES                      
         BH    XIT2                                                             
         B     INCYSTAT                                                         
         SPACE 1                                                                
INCYSTA  CLC   TACYUNIT(2),=C'FD'  ALL STATES - SO IGNORE FED                   
         BE    XIT2                                                             
         CLC   TACYUNIT(2),=C'CN'               AND CANADA                      
         BE    XIT2                                                             
         GOTO1 TAXVAL,DMCB,(2,TACYUNIT)                                         
         BNE   XIT2                                                             
         CLI   TACYUNIT+2,C' '                  AND CITIES                      
         BH    XIT2                                                             
         CLI   GLARGS+2,C'L'       LOOKING FOR FLI                              
         BNE   INCYSTAT                                                         
         CLI   TACYLEN,TACYLN2Q    HAS TO BE NEW LENGTH                         
         BL    XIT2                                                             
         B     INCYSTAT                                                         
         SPACE 1                                                                
INCYCAN  CLC   TACYUNIT(2),=C'CN'  CANADA ONLY                                  
         BE    INCYSTAT                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INCYOTH  CLC   TACYUNIT(2),=C'OT'  OTHERS ONLY                                  
         BE    INCYSTAT                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INCYCITY CLI   TACYUNIT+2,C' '     CITIES ONLY                                  
         BH    INCYSTAT                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INCYSTAT L     R1,=A(INCYTAB)      OK - PASSED WHERE TESTS                      
         MVI   ELCODE,TACYELQ                                                   
         B     GENDATA             SO, OFF TO GET THE DATA                      
         SPACE 1                                                                
INCYEARN L     R1,TACYEARN                                                      
         CLI   TACYLEN,TACYLN3Q                                                 
         BL    CVD2                                                             
         A     R1,TACYTXRE                                                      
         B     CVD2                                                             
         EJECT                                                                  
*              QTD CHECK SUPPORT                                                
         SPACE 3                                                                
*              ARGUMENT 2  WHERE   S=FEDERAL ONLY                               
*                                                                               
         USING TACQD,R4                                                         
INCQ     L     R4,ATHISEL                                                       
         LTR   R4,R4                                                            
         BZ    XIT2                                                             
         CLI   0(R4),TACQELQ                                                    
         BNE   XIT2                                                             
         OC    TISKUNIT,TISKUNIT                                                
         BZ    INCQ100                                                          
         CLC   TACQUNIT,TISKUNIT                                                
         BNE   XIT2                                                             
                                                                                
INCQ100  CLI   GLARGS+1,C'S'                                                    
         BE    INCQSTA                                                          
         B     INCQSTAT                                                         
         SPACE 1                                                                
INCQSTA  CLC   TACQUNIT(2),=C'FD'  ALL STATES - SO IGNORE FED                   
         BE    XIT2                                                             
         CLC   TACQUNIT(2),=C'CN'               AND CANADA                      
         BE    XIT2                                                             
         GOTO1 TAXVAL,DMCB,(3,TACQUNIT)                                         
         BNE   XIT2                                                             
         CLI   TACQUNIT+2,C' '                  AND CITIES                      
         BH    XIT2                                                             
         B     INCQSTAT                                                         
         SPACE 1                                                                
INCQSTAT L     R1,=A(INCQTAB)      OK - PASSED WHERE TESTS                      
         MVI   ELCODE,TACQELQ                                                   
         B     GENDATA             SO, OFF TO GET THE DATA                      
         EJECT                                                                  
*              PAYROLL SUPPORT                                                  
         SPACE 3                                                                
*              INPUT               SUBARGS YEAR/MONTH (PWOS)                    
*              ARGUMENT 1  TYPE    1=ALL EARNINGS (S+R)                         
*                                  S=SESSION EARNINGS (LIVE)                    
*                                  R=REUSE EARNINGS                             
*                                  N=NON-TAXABLE EARNINGS                       
*                                  2=NET                                        
*                                  T=TAXES                                      
*                                  O=OTHER WITHHOLDING                          
*                                  F=FICA                                       
*                                  D=EMPLOYEE DISABLILITY                       
*                                  U=EMPLOYEE UNEMPLOYMENT                      
*                                  G=CANADIAN GST                               
*                                  I=I&R                                        
*                                  L=LIENS                                      
*                                  V=OVERPAYMENT (OLD SYSTEM ONLY)              
*                                  X=REIMBURSED EXPENSES                        
*              ARGUMENT 2  WHERE   F=FEDERAL ONLY                               
*                                  N=STATE & CANADA                             
*                                  S=STATE ONLY                                 
*                                  T=STATE ONLY - NO CORPS                      
*                                  R=STATE ONLY FOR CORPS                       
*                                  U=STATE ONLY FOR CORPS + FOR                 
*                                  Y=CITY                                       
*                                  C=CANADA                                     
*                                  O=OTHERS                                     
*                                  V=STATE ONLY FOR FOREIGNERS ONLY             
*              ARGUMENT 3          F=GET DATA FROM FED ELEMENT                  
*              ARGUMENT 4          R=RESIDENT ONLY ** DEFUNCT ***               
*                                  W=WORKING ONLY  ** DEFUNCT ***               
*                                  T=TAXABLE (FOR EARNINGS)                     
         SPACE 1                                                                
INPH     L     R4,ATHISEL          WE WILL BE PASSED A(ELEMENT)                 
         LTR   R4,R4               IF NOT, FORGET IT                            
         JZ    XIT                                                              
         CLI   0(R4),TALNELQ       ROUTINES FOR LIENS                           
         BE    INC4                                                             
         CLI   0(R4),TAODELQ       OTHER DEDUCTIONS                             
         BE    INC5                                                             
         CLI   0(R4),TACWELQ       AND WITHHOLDING ELEMENTS                     
         BE    INCW                                                             
         CLI   0(R4),TAW2ELQ       OR W2 ELEMENTS                               
         BE    INW2                                                             
         CLI   0(R4),TAPDELQ       ODDMENTS FROM PAY DETAILS                    
         BE    INPHPD                                                           
         CLI   0(R4),TACDELQ       AND CHECK ELEMENTS                           
         BE    INPHCD                                                           
         CLI   0(R4),TATUELQ       AND TAX UNIT ELEMENTS                        
         BE    INPHTU                                                           
         CLI   0(R4),TAATELQ       AND ADDITIONAL TAX ELEMENTS                  
         BE    INPHAT                                                           
         J     XIT                                                              
         EJECT                                                                  
*              PAYROLL OUT OF PAY DETAILS AND CHECK ELEMENTS                    
         SPACE 3                                                                
*              INPUT               SEE INPH (ABOVE) FOR ARGUMENTS               
         SPACE 1                                                                
INPHPD   DS    0H                                                               
         USING TAPDD,R4                                                         
         MVC   SAVEREXP,TAPDREXP                                                
         CLI   TAPDLEN,TAPDLNQ                                                  
         BL    INPHPD5                                                          
         L     R1,SAVEREXP                                                      
         ICM   RF,15,TAPDTXNW                                                   
         SR    R1,RF                                                            
INPHPD5  MVI   LIVESW,C'R'         SET LIVE SWITCH TO REUSE                     
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE  LOOK UP USE TYPE                   
         TM    TGUSSTAT,SESSION                                                 
         BZ    *+8                                                              
         MVI   LIVESW,C'S'                                                      
         J     XIT                                                              
         SPACE 1                                                                
INPHCD   DS    0H                                                               
         LR    R5,R4                                                            
                                                                                
         USING TACDD,R4                                                         
         MVC   SAVENET,TACDNET                                                  
         MVC   SAVECDTE,TACDDTE                                                 
         MVC   SAVENTAX,TACDNTAX                                                
                                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TATUELQ      IF TATU EXISTS, USE THOSE                    
         BRAS  RE,GETEL                                                         
         JE    XIT                                                              
                                                                                
         LR    R4,R5                                                            
         MVC   SAVEEARN,TACDEARN                                                
                                                                                
         USING TAPDD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         CLI   TAPDLEN,TAPDLNQ                                                  
         JL    XIT                                                              
         ICM   R1,15,TAPDTXNW      ADD IN TAXABLE NON-WAGES IF EXISTS           
         A     R1,SAVEEARN                                                      
         ST    R1,SAVEEARN                                                      
                                                                                
         J     XIT                                                              
         EJECT                                                                  
         USING TATUD,R4                                                         
INPHTU   MVC   THISUNIT,TATUUNIT   DO THE 'WHERE' FILTERING FIRST               
         CLI   GLARGS+1,C'S'       IF ARG 2 = S, CHANGE IT TO T TO              
         JNE   *+8                                                              
         MVI   GLARGS+1,C'T'       DISTINGUISH IT FROM ELFLTCW CALL             
         BRAS  RE,TESTWHER                                                      
         JNE   NOGOOD2                                                          
         BRAS  RE,TESTRNWT         TEST RESIDENCY, WORK, TAXABLE                
         JNE   NOGOOD2                                                          
         OC    TISKUNIT,TISKUNIT                                                
         JZ    INPHTU1                                                          
         CLC   THISUNIT,TISKUNIT                                                
         JNE   NOGOOD2                                                          
                                                                                
INPHTU1  CLI   GLARGS,C'1'                                                      
         JE    INPHTU2                                                          
         CLI   GLARGS,C'S'                                                      
         JE    INPHTU2                                                          
         CLI   GLARGS,C'R'                                                      
         JE    INPHTU2                                                          
         CLI   GLARGS,C'N'         NON-TAXABLE EARNINGS                         
         JE    INPHTU4                                                          
         CLI   GLARGS,C'X'         NON-TAXABLE EARNINGS                         
         JE    INPHTU4                                                          
         J     NOGOOD2                                                          
                                                                                
INPHTU2  ICM   R1,15,TATUWAGE      WAGES + TAXABLE NON-WAGES                    
         ICM   R0,15,TATUTNWA                                                   
         CLI   GLARGS+1,C'V'       FOREIGNER                                    
         JNE   *+8                                                              
         ICM   R0,15,TATUSTRE                                                   
         AR    R1,R0                                                            
         ST    R1,SAVEEARN                                                      
         J     XIT                                                              
                                                                                
INPHTU4  ICM   R1,15,TATUNNWA      NON-TAXABLE NON-WAGES                        
         ST    R1,SAVEEARN                                                      
         J     XIT                                                              
                                                                                
         USING TAATD,R4                                                         
INPHAT   CLC   GLARGS(2),=C'TC'    CTAX KEYWORD ONLY                            
         JNE   NOGOOD2                                                          
         MVC   THISUNIT,TAATUNIT   DO THE 'WHERE' FILTERING FIRST               
         BRAS  RE,TESTWHER                                                      
         JNE   NOGOOD2                                                          
         OC    TISKUNIT,TISKUNIT                                                
         JZ    INPHAT1                                                          
         CLC   THISUNIT,TISKUNIT                                                
         JNE   INPHAT1                                                          
                                                                                
INPHAT1  CLI   GLARGS+3,C'X'       NON-RES FOR I & F ONLY?                      
         JNE   INPHAT3             NO, CONTINUE                                 
         CLI   TIW4TY,C'I'                                                      
         JE    INPHAT3                                                          
         CLI   TIW4TY,C'F'                                                      
         JNE   NOGOOD2                                                          
INPHAT3  LA    R1,TAATTAX                                                       
         J     INPHOUT                                                          
                                                                                
         SPACE 1                                                                
SAVEEARN DS    F                                                                
SAVENET  DS    F                                                                
SAVENTAX DS    F                                                                
SAVEREXP DS    F                                                                
SAVECDTE DS    XL3                                                              
LIVESW   DS    CL1                                                              
PHUNIT   DS    CL3                                                              
         EJECT                                                                  
*              PAYROLL OUT OF LIENS AND OTHERS                                  
         SPACE 3                                                                
*              INPUT               SEE INPH (ABOVE) FOR ARGUMENTS               
         SPACE 1                                                                
INC4     DS    0H                                                               
         USING TALWD,R4                                                         
         CLI   GLARGS,C'L'                                                      
         BNE   XIT2                                                             
         LA    R1,TALWREC          LIEN RECOVERED                               
         B     INPHOUT                                                          
         SPACE 1                                                                
INC5     DS    0H                                                               
         LR    R5,R4                                                            
                                                                                
         USING TAODD,R4                                                         
         CLI   GLARGS,C'O'                                                      
         BNE   XIT2                                                             
         CLI   GLARGS+1,0          OPTIONAL FILTER ON OTHER TYPE                
         BE    INC52                                                            
         CLC   TAODTYPE,GLARGS+1                                                
         BNE   XIT2                                                             
         SPACE 1                                                                
INC52    LA    R1,TAODAMT          'OTHER' DEDUCTION AMOUNT                     
         CLI   GLARGS+2,C'1'                                                    
         BNE   *+8                                                              
         LA    R1,SAVEEARN          OR RELATED EARNINGS                         
         CLI   GLARGS+2,C'N'                                                    
         BNE   *+8                                                              
         LA    R1,SAVENTAX          OR RELATED NON TAXABLE                      
         B     INPHOUT                                                          
         EJECT                                                                  
*              DUE COMPANY WITHHOLDING                                          
         SPACE 3                                                                
INDW     DS    0H                                                               
         L     R4,ATHISEL                                                       
         LTR   R4,R4                                                            
         BZ    XIT2                                                             
         USING TADWD,R4                                                         
         CLI   TADWEL,TADWELQ                                                   
         BNE   XIT2                                                             
         CLI   GLARGS,1            DUE COMPANY CODE                             
         BNE   INDW2                                                            
         MVC   0(L'TADWDUC,R2),TADWDUC                                          
         CLI   TADWDUC,X'FA'       IS THIS A YEAR 2000 DATE?                    
         BL    XIT2                NO, MUST BE OK TO PRINT                      
         MVC   WORK(4),TADWDUC     YES, CONVERT THE DATE TO DISPLAY             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK)                                
         MVC   0(4,R2),WORK                                                     
         MVC   4(2,R2),TADWDUC+4                                                
         B     XIT2                                                             
*                                                                               
INDW2    CLI   GLARGS,2            AMOUNT RECOVERED                             
         BNE   *+12                                                             
         LA    R1,TADWREC                                                       
         B     INPACK                                                           
         CLI   GLARGS,3            BALANCE                                      
         BNE   XIT2                                                             
         LA    R1,TADWBAL                                                       
         B     INPACK                                                           
         EJECT                                                                  
*              LIEN WITHHOLDING                                                 
         SPACE 3                                                                
INLW     DS    0H                                                               
         L     R4,ATHISEL                                                       
         LTR   R4,R4                                                            
         BZ    XIT2                                                             
         USING TALWD,R4                                                         
         CLI   TALWEL,TALWELQ                                                   
         BNE   XIT2                                                             
         CLI   GLARGS,1            LIEN CODE                                    
         BNE   *+14                                                             
         MVC   0(L'TALWLIN,R2),TALWLIN                                          
         B     XIT2                                                             
         CLI   GLARGS,2            AMOUNT RECOVERED                             
         BNE   *+12                                                             
         LA    R1,TALWREC                                                       
         B     INPACK                                                           
         CLI   GLARGS,3            BALANCE                                      
         BNE   XIT2                                                             
         LA    R1,TALWBAL                                                       
         B     INPACK                                                           
         EJECT                                                                  
*              PAYROLL OUT OF WITHHOLDING ELEMENTS                              
         SPACE 3                                                                
*              INPUT               SEE INPH (ABOVE) FOR ARGUMENTS               
         SPACE 1                                                                
         USING TACWD,R4                                                         
INCW     MVC   THISUNIT,TACWUNIT   DO THE 'WHERE' FILTERING FIRST               
         CLI   GLARGS+1,C'S'       IF ARG 2 = S, CHANGE IT TO T TO              
         BNE   *+8                                                              
         MVI   GLARGS+1,C'T'       DISTINGUISH IT FROM ELFLTCW CALL             
         BRAS  RE,TESTWHER                                                      
         BNE   NOGOOD2                                                          
         OC    TISKUNIT,TISKUNIT                                                
         BZ    INCW400                                                          
         CLC   THISUNIT,TISKUNIT   MATCH ON UNIT                                
         BNE   NOGOOD2                                                          
INCW400  CLI   GLARGS+3,C'R'       NEXT THE STATUS                              
         BE    INCWRES                                                          
         CLI   GLARGS+3,C'X'       NON-RES FOR INDIV & FORGN ONLY               
         BE    INCWNRIF                                                         
         CLI   GLARGS+3,C'N'       REGULAR NON-RES                              
         BE    INCWNRES                                                         
         CLI   GLARGS+3,C'W'                                                    
         BE    INCWWORK                                                         
         CLI   GLARGS+3,C'T'                                                    
         BE    INCWTXBL                                                         
         B     INCWEARN                                                         
         SPACE 1                                                                
INCWRES  TM    TACWSTAT,TACWSRES   C'R' RESIDENT ONLY                           
         BO    INCWEARN                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INCWNRIF CLI   TIW4TY,TAW4TYIN     INDIVIDUALS                                  
         JE    INCWNRES                                                         
         CLI   TIW4TY,TAW4TYFO     AND FOREIGNERS                               
         JNE   NOGOOD2                                                          
INCWNRES TM    TACWSTAT,TACWSRES   C'N' NON-RESIDENT ONLY                       
         BNO   INCWEARN                                                         
         CLC   TIEMP,=C'P+ '       P+ AND CITY                                  
         BNE   XIT2                IT MIGHT HAVE NRES TAX                       
         CLI   TACWUNIT+2,C' '                                                  
         BH    INCWEARN                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INCWWORK TM    TACWSTAT,TACWSWRK   C'W' WORK STATE ONLY                         
         BO    INCWEARN                                                         
         B     XIT2                                                             
         SPACE 1                                                                
INCWEARN CLI   GLARGS,C'1'         EARNINGS MUST BE 'TAXABLE' TOO               
         BE    INCWTXBL                                                         
         CLI   GLARGS,C'R'                                                      
         BE    INCWTXBL                                                         
         CLI   GLARGS,C'S'                                                      
         BNE   INCWOK                                                           
         SPACE 1                                                                
INCWTXBL TM    TACWSTAT,TACWSTAX   C'T' TAXABLE IF BIT IS ON                    
         BO    INCWOK                                                           
         CLC   SAVECDTE,=X'901030' IF BEFORE OCT30                              
         BNL   XIT2                                                             
         OC    TACWTAX,TACWTAX     TAXABLE IF TAXES WITHHELD,                   
         BNZ   INCWOK                                                           
         TM    TACWSTAT,TACWSWRK      OR IF WORK STATE                          
         BO    INCWOK                                                           
         B     XIT2                                                             
         SPACE 1                                                                
INCWOK   CLI   GLARGS+2,C'F'       ARG3 F=GET DATA FROM FEDERAL                 
         BNE   INCWOUT                                                          
         CLI   TACWLEN,TACWLN3Q       UNLESS STATE/CITY SAVED IT                
         BL    INCWOK5                                                          
         LA    R1,TACWPFTX                                                      
         CLI   GLARGS,C'T'         FED TAX                                      
         BE    INPHOUT                                                          
         LA    R1,TACWPFFI                                                      
         CLI   GLARGS,C'F'         FICA                                         
         BE    INPHOUT                                                          
******                                                                          
INCWOK5  L     R4,TIAREC                                                        
         MVI   ELCODE,TACWELQ                                                   
         USING TACWD,R4                                                         
         BAS   RE,GETEL2                                                        
         B     *+8                                                              
         SPACE 1                                                                
INCWGTFD BAS   RE,NEXTEL2                                                       
         BNE   XIT2                                                             
         CLC   TACWUNIT(2),=C'FD'  HAS TO BE FEDERAL UNIT                       
         BNE   INCWGTFD                                                         
         MVI   ELCODE,0                                                         
         SPACE 1                                                                
INCWOUT  LA    R1,TACWTAX          PASSED 'WHERE' TESTS                         
*                                                                               
         CLC   TIEMP,=C'P+ '       FOR P+ ONLY                                  
         BNE   INCWOUT5                                                         
         CLI   GLARGS,C'T'         TAXABLE                                      
         BNE   INCWOUT5                                                         
         CLI   GLARGS+1,C'Y'       CITY                                         
         BNE   INCWOUT5                                                         
*                                                                               
         CLI   GLARGS+3,C'R'       RESIDENT                                     
         BNE   INCWOUT4                                                         
         L     R1,TACWRTX              RES CITY TAX (CITY)                      
         B     INPHOUT2                                                         
INCWOUT4 CLI   GLARGS+3,C'N'       NON-RESIDENT                                 
         BNE   INPHOUT             NO, USE TACWTAX                              
         L     R1,TACWTAX              TAXES                                    
         S     R1,TACWRTX            - RES CITY TAX (CITY)                      
         B     INPHOUT2                                                         
*                                                                               
INCWOUT5 CLI   GLARGS,C'T'         NOW PICK OFF REQUIRED AMOUNT                 
         BE    INPHOUT                                                          
         LA    R1,TACWFICA                                                      
         CLI   GLARGS,C'F'         FICA                                         
         BNE   INCWOUT6                                                         
         CLI   GLARGS+1,C'N'                                                    
         BNE   INPHOUT                                                          
         CLI   TACWLEN,TACWLN3Q                                                 
         BL    *+8                                                              
         LA    R1,TACWPFFI                                                      
         B     INPHOUT                                                          
INCWOUT6 CLI   GLARGS,C'D'                                                      
         BNE   INCWOUT7                                                         
         CLI   TACWUNIT+2,C' '                                                  
         BNE   XIT2                                                             
         CLC   TACWUNIT,=C'FD '                                                 
         BE    XIT2                                                             
         B     INPHOUT                                                          
INCWOUT7 CLI   GLARGS,C'I'                                                      
         BE    INPHOUT                                                          
         LA    R1,TACWSUI                                                       
         CLI   GLARGS,C'U'                                                      
         BE    INPHOUT                                                          
         CLI   GLARGS,C'G'                                                      
         BE    INPHOUT                                                          
         CLI   TACWLEN,TACWLN2Q                                                 
         BL    INCWOUT8                                                         
         LA    R1,TACWSFLI                                                      
         CLI   GLARGS,C'L'                                                      
         BE    INPHOUT                                                          
                                                                                
INCWOUT8 LA    R1,SAVENET          AND NET                                      
         CLI   GLARGS,C'2'                                                      
         BE    INPHOUT                                                          
*                                  CHECK IF TATU EXISTS                         
         BRAS  RE,CWTUSET          IF SO, USE TATU'S BY TACWUNIT                
*                                                                               
         LA    R1,SAVEEARN         EARNINGS WE SAVED BEFORE                     
         CLI   GLARGS,C'1'            1=ALL                                     
         BE    INPHOUT                                                          
         CLC   LIVESW,GLARGS          R/S=REUSE/SESSION                         
         BE    INPHOUT                                                          
         LA    R1,SAVENTAX         NON TAX WE SAVED BEFORE                      
         CLI   GLARGS,C'N'                                                      
         BE    INPHOUT                                                          
*        LA    R1,SAVEREXP         AND REIMBURSED EXPENSES                      
*        CLI   GLARGS,C'X'                                                      
*        BE    INPHOUT                                                          
         B     XIT2                                                             
         SPACE 1                                                                
INPHOUT  L     R1,0(R1)                                                         
INPHOUT2 CVD   R1,0(R2)            RETURN PL8                                   
         B     XIT2                                                             
         EJECT                                                                  
*              RETURNED CHECK SUPPORT                                           
         SPACE 1                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R2=A(OUTPUT AREA)                            
*                                  R3=LENGTH-1                                  
         USING TARND,R4            R4=A(ELEMENT)                                
         SPACE 1                                                                
INRNSTAT DS    0H                                                               
         TM    0(R1),TARNFILE      RETURNED CHECK DISPOSITION INDICATOR         
         BZ    *+14                                                             
         MVC   0(5,R2),=C'FILED'                                                
         B     XIT2                                                             
         TM    0(R1),TARNMAIL                                                   
         BZ    *+14                                                             
         MVC   0(6,R2),=C'MAILED'                                               
         B     XIT2                                                             
         TM    0(R1),TARNFILE+TARNMAIL                                          
         BNZ   *+10                                                             
         MVC   0(8,R2),=C'RETURNED'                                             
         B     XIT2                                                             
         SPACE 1                                                                
INRNUID  DS    0H                                                               
         MVC   0(L'TGUSERID,R2),SPACES2                                         
         OC    TARNUID,TARNUID     IF USER ID SPECIFIED                         
         BZ    XIT2                                                             
         L     R5,AIO              SAVE AIO                                     
         MVC   AIO,AIO2                                                         
         XC    WORK,WORK                                                        
         MVC   WORK+8(L'TARNUID),TARNUID                                        
         GOTO1 USERVAL,DMCB,(X'80',WORK)                                        
         MVC   0(L'TGUSERID,R2),TGUSERID                                        
         ST    R5,AIO              RESTORE AIO                                  
         B     XIT2                                                             
         EJECT                                                                  
         USING TARPD,R4                                                         
INRPBASE XR    R1,R1                                                            
         ICM   R1,15,TARPBASE                                                   
         B     CVD2                                                             
                                                                                
INRPDIFF XR    R1,R1                                                            
         ICM   R1,15,TARPDIFF                                                   
         B     CVD2                                                             
                                                                                
*              ADDITIONAL INPUT ROUTINES - ECAST                                
         SPACE 1                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R2=A(OUTPUT AREA)                            
*                                  R3=LENGTH-1                                  
*                                  R4=A(ELEMENT)                                
         SPACE 1                                                                
INSOSTAT DS    0H                  SOAP EPISODE WRITER ROLE                     
         TM    0(R1),TASOSH                                                     
         BNO   *+14                                                             
         MVC   0(4,R2),=C'HEAD'                                                 
         LA    R2,5(R2)                                                         
         TM    0(R1),TASOSS                                                     
         BNO   *+14                                                             
         MVC   0(6,R2),=C'SCRIPT'                                               
         LA    R2,7(R2)                                                         
         TM    0(R1),TASOSB                                                     
         BNO   *+14                                                             
         MVC   0(9,R2),=C'BREAKDOWN'                                            
         LA    R2,10(R2)                                                        
         B     XIT2                                                             
         SPACE 1                                                                
INSOEPI  DS    0H                  SOAP EPISODE NUMBER                          
         MVC   0(L'TIEPI,R2),TIEPI                                              
         B     XIT2                                                             
         SPACE 1                                                                
INSOPNH  DS    0H                  SOAP PENSION & HEALTH                        
         XC    0(4,R2),0(R2)                                                    
         MVC   4-L'TASOPNH(L'TASOPNH,R2),0(R1)                                  
         B     XIT2                                                             
         SPACE 1                                                                
INSOPAY  DS    0H                  SOAP PAYMENT AMOUNT                          
         XC    0(4,R2),0(R2)                                                    
         MVC   4-L'TASOPAY(L'TASOPAY,R2),0(R1)                                  
         B     XIT2                                                             
         SPACE 1                                                                
INSOAPPL DS    0H                  SOAP APPLIED AMOUNT                          
         XC    0(4,R2),0(R2)                                                    
         MVC   4-L'TASOAPPL(L'TASOAPPL,R2),0(R1)                                
         B     XIT2                                                             
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - SESSION DETAILS ELS.                 
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R2=A(OUTPUT AREA)                            
*                                  R3=LENGTH-1                                  
         USING TASDD,R4            R4=A(ELEMENT)                                
         SPACE 1                                                                
INSDTDAY XC    0(4,R2),0(R2)       TV DAYS                                      
         MVC   3(1,R2),0(R1)                                                    
         B     XIT2                                                             
         SPACE 1                                                                
INSDTDT  XC    0(4,R2),0(R2)       TV DOUBLE TIME                               
         MVC   3(1,R2),0(R1)                                                    
         B     XIT2                                                             
         SPACE 1                                                                
INSDTOT  XC    0(4,R2),0(R2)       TV OVER TIME                                 
         MVC   3(1,R2),0(R1)                                                    
         B     XIT2                                                             
         SPACE 1                                                                
INSDTTAG XC    0(2,R2),0(R2)       TV TAGS                                      
         MVC   1(1,R2),0(R1)                                                    
         B     XIT2                                                             
         SPACE 1                                                                
INSDHRMN XC    0(4,R2),0(R2)       RADIO AND MUSIC - HOURS/MINUTES              
         MVC   2(2,R2),0(R1)                                                    
         B     XIT2                                                             
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - SUBSIDIARY INVOICE ELS.              
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R2=A(OUTPUT AREA)                            
*                                  R3=LENGTH-1                                  
         USING TASID,R4            R4=A(ELEMENT)                                
         SPACE 1                                                                
INSIINV  DS    0H                  INVOICE NUMBERS                              
         OC    TASIINV,TASIINV                                                  
         BZ    INSIINV5                                                         
         GOTO1 TINVCON,DMCB,TASIINV,(R2),DATCON                                 
         LA    R2,7(R2)                                                         
INSIINV5 BAS   RE,NEXTEL2                                                       
         BE    INSIINV                                                          
         B     XIT2                                                             
         SPACE 1                                                                
INSIPCT  DS    0H                  PERCENTAGES                                  
         EDIT  (3,TASIPCT3),(8,(R2)),4,TRAIL=C'%'                               
         LA    R2,9(R2)                                                         
         BAS   RE,NEXTEL2                                                       
         BE    INSIPCT                                                          
         B     XIT2                                                             
         SPACE 1                                                                
INSIEST  DS    0H                  ESTIMATE NUMBERS                             
         ZIC   R1,TASILEN                                                       
         SH    R1,=AL2(TASILNQ+1)                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TASIEST                                                  
         LA    R2,17(R2)                                                        
         BAS   RE,NEXTEL2                                                       
         BE    INSIEST                                                          
         B     XIT2                                                             
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - NETWORK CLA ELEMENTS                 
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R2=A(OUTPUT AREA)                            
*                                  R3=LENGTH-1                                  
         USING TANPD,R4            R4=A(ELEMENT)                                
         SPACE 1                                                                
INNPCLA  MVC   0(198,R2),SPACES2   PROCESS USE INFO 1-10 ONLY                   
         MVC   198(52,R2),SPACES2                                               
         BAS   RE,INNPUSES         PROCESS THE USES                             
         B     XIT2                                                             
*                                                                               
INNPCLA2 MVC   0(198,R2),SPACES2                                                
         MVC   198(52,R2),SPACES2                                               
         LA    R0,10               PROCESS USE INFO 11-20 ONLY                  
INNPCL2A BAS   RE,NEXTEL2                                                       
         BNE   XIT2                                                             
         BAS   RE,INNPFLT                                                       
         BNE   INNPCL2A                                                         
         BCT   R0,INNPCL2A                                                      
*                                                                               
         BAS   RE,INNPUSES         PROCESS THE USES                             
         B     XIT2                                                             
*                                                                               
INNPFLT  OC    TIFLTNET,TIFLTNET                                                
         BZ    INNPFLTX                                                         
         CLC   TANPNWK,TIFLTNET    NETWORK MUST MATCH                           
INNPFLTX BR    RE                                                               
*                                                                               
INNPCNT  XR    R1,R1                                                            
INNPCNT5 BAS   RE,INNPFLT                                                       
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         BAS   RE,NEXTEL2                                                       
         BE    INNPCNT5                                                         
         STCM  R1,7,0(R2)          SAVE COUNT OF TANPD ELEMENTS                 
         B     XIT2                                                             
         SPACE 2                                                                
*              ROUTINE TO PROCESS 10 USES                                       
*                                                                               
INNPUSES NTR1                                                                   
         LA    R0,10               PROCESS 10 USES                              
INNPUSE5 BAS   RE,INNPFLT          CHECK NETWORK FILTER                         
         BNE   INNPUSE9                                                         
         CLC   TANPDATE,=C'TBA'    IF DATE UNKNOWN                              
         BNE   *+14                                                             
         MVC   0(L'TANPDATE,R2),TANPDATE MOVE IT IN                             
         B     INNPUSE8                                                         
         GOTO1 DATCON,DMCB,(1,TANPDATE),(4,0(R2))                               
INNPUSE8 LA    R2,5(R2)                                                         
*                                                                               
         MVC   0(L'TANPPNME,R2),TANPPNME  PROGRAM NAME                          
         LA    R2,L'TANPPNME+1(R2)                                              
         MVC   0(1,R2),TANPLFT     LIFT                                         
         LA    R2,2(R2)                                                         
         MVC   0(1,R2),TANPNWK     NETWORK                                      
         LA    R2,1(R2)                                                         
         MVI   0(R2),C' '          SET LAST BLANK FOR CHOPPER                   
         LA    R2,1(R2)                                                         
*                                                                               
INNPUSE9 BAS   RE,NEXTEL2                                                       
         BNE   XIT2                                                             
         BAS   RE,INNPFLT                                                       
         BNE   INNPUSE9                                                         
         BCT   R0,INNPUSE5                                                      
         B     XIT2                                                             
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - MUSIC CONTRACT DETAILS               
*              NOTE - BECAUSE WE NEED TO GET 2 DIFFERENT ELEMENTS,              
*                     WE DO NOT GO THROUGH THE GENIN ROUTINE                    
         SPACE 3                                                                
*        INPUT                     R1=A(DATA)                                   
*                                  R2=A(OUTPUT AREA)                            
*                                  R3=LENGTH-1                                  
         SPACE 1                                                                
INMCCON  MVC   0(252,R2),SPACES2   INIT OUTPUT AREA                             
         LR    R3,R4              STORE ADDRESS OF RECORD                       
         MVI   ELCODE,TAMCELQ      FIND MUSIC TRACK ELEMENT                     
         BAS   RE,GETEL2                                                        
         BE    INMCC10                                                          
*                                                                               
         LR    R4,R3              RESTORE ADDRESS OF RECORD                     
         MVI   ELCODE,TATRELQ      FIND CONTRACT/TRACK ELEMENT                  
         BAS   RE,GETEL2                                                        
         BE    INMCC20                                                          
         B     XIT2                                                             
*                                                                               
         USING TAMCD,R4            R4=A(ELEMENT)                                
INMCC10  MVC   0(L'TAMCCON,R2),TAMCCON  CONTRACT NUMBER                         
         LA    R2,L'TAMCCON+1(R2)                                               
*                                                                               
         MVC   0(1,R2),TAMCTRK     TRACK                                        
         OC    0(1,R2),SPACES2                                                  
         LA    R2,2(R2)                                                         
*                                                                               
         MVC   0(1,R2),TAMCLFT     LIFT                                         
         OC    0(1,R2),SPACES2                                                  
         LA    R2,2(R2)                                                         
*                                                                               
         EDIT  TAMCLLEN,(3,0(R2)),ALIGN=LEFT   LENGTH                           
         LA    R2,4(R2)                                                         
*                                                                               
         MVC   0(1,R2),TAMCTYP     MUSIC TYPE                                   
         OC    0(1,R2),SPACES2                                                  
         LA    R2,2(R2)                                                         
*                                                                               
         MVC   0(12,R2),TAMCBAS    AFM BASIC                                    
         OC    0(12,R2),SPACES2                                                 
         LA    R2,13(R2)                                                        
*                                                                               
         BAS   RE,NEXTEL2                                                       
         BNE   XIT2                                                             
*                                                                               
         B     INMCC10                                                          
*                                                                               
*        CONTRACT/TRACK ELEMENT                                                 
*                                                                               
         USING TATRD,R4            ESTABLISH CONTRACT/TRACK ELEMENT             
INMCC20  XC    KEY,KEY                                                          
         LA    R5,KEY              ESTABLISH COMMERCIAL PASSIVES                
         USING TLCOPD,R5                                                        
*                                                                               
         MVI   TLCOPCD,TLCOCCDQ    BUILD INTERNALCOMM # PASSIVE                 
         MVC   TLCOCCOM,TATRCOM    INTERNAL COMMERCIAL CODE                     
*                                                                               
         GOTO1 HIGH                READ FOR MATCH                               
*                                                                               
         CLC   TLCOPKEY,KEYSAVE    MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R0,AIO              SAVE IOAREA POINTER                          
         MVC   AIO,AIO2            READ CONTRACT INTO IOA3                      
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         ST    R0,AIO              RESTORE IOAREA POINTER                       
*                                                                               
         LR    R5,R4               SAVE TATRD POINTER                           
         L     R4,AIO2             POINT TO FOUND CONTRACT RECORD               
*                                                                               
         DROP  R4                                                               
*                                                                               
         USING TATRD,R5            ESTABLISH CONTRACT/TRACK ELEMENT             
*                                                                               
         MVI   ELCODE,TACOELQ      FIND COMMERCIAL DETAILS ELEMENT              
         BAS   RE,GETEL2                                                        
         BNE   INMCC29             SKIP IF NONE                                 
*                                                                               
         USING TACOD,R4            ESTABLISH ELEMENT                            
*                                                                               
         MVC   0(L'TAMCCON,R2),TACOCID  RETURN CONTRACT NUMBER                  
         LA    R2,L'TAMCCON+1(R2)                                               
*                                                                               
         L     R4,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         MVI   ELCODE,TAMCELQ      FIND MUSIC CONTRACT ELEMENT                  
         BAS   RE,GETEL2                                                        
         USING TAMCD,R4            ESTABLISH ELEMENT                            
*                                                                               
INMCC25  DS    0H                                                               
*                                                                               
         BNE   INMCC29                                                          
*                                                                               
         CLC   TATRTRK,TAMCTRK     MATCH ON TRACK NUMBER                        
         BE    INMCC27                                                          
*                                                                               
         BAS   RE,NEXTEL2          NEXT ELEMENT                                 
         B     INMCC25                                                          
*                                                                               
INMCC27  DS    0H                  FOUND MUSIC TRACK ELEMENT                    
*                                                                               
*        BUILD OUTPUT                                                           
*                                                                               
         MVC   0(1,R2),TAMCTRK     TRACK                                        
         OC    0(1,R2),SPACES2                                                  
         LA    R2,2(R2)                                                         
*                                                                               
         MVC   0(1,R2),TAMCLFT     LIFT                                         
         OC    0(1,R2),SPACES2                                                  
         LA    R2,2(R2)                                                         
*                                                                               
         EDIT  TAMCLLEN,(3,0(R2)),ALIGN=LEFT   LENGTH                           
         LA    R2,4(R2)                                                         
*                                                                               
         MVC   0(1,R2),TAMCTYP     MUSIC TYPE                                   
         OC    0(1,R2),SPACES2                                                  
         LA    R2,2(R2)                                                         
*                                                                               
         MVC   0(12,R2),TAMCBAS    AFM BASIC                                    
         OC    0(12,R2),SPACES2                                                 
         LA    R2,13(R2)                                                        
*                                                                               
INMCC29  DS    0H                                                               
         MVC   0(36,R2),SPACES2    CLEAR NEXT RETURN AREA                       
*                                                                               
         MVC   KEY,TIKEY           RESTORE FILE READ SEQ                        
         GOTO1 HIGH                                                             
*                                                                               
         B     XIT2                                                             
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINES - DEAL                                 
*                                                                               
*        INPUT                     R1=A(DATA)                                   
*                                  R2=A(OUTPUT AREA)                            
*                                  R3=LENGTH-1                                  
*                                  R4=A(ELEMENT)                                
         SPACE 1                                                                
INDLTERM DS    0H                  DEAL TERM                                    
         CLI   0(R1),TADLTUNL      IF UNLIMITED                                 
         BNE   INDLT5                                                           
         MVC   2(2,R2),=C'UN'      DISPLAY UN                                   
         B     XIT2                                                             
INDLT5   CLI   0(R1),TADLT1X       IF ONE TIME                                  
         BNE   INDLT10                                                          
         MVC   2(2,R2),=C'1X'      DISPLAY 1X                                   
         B     XIT2                                                             
INDLT10  EDIT  (1,0(R1)),(4,0(R2)),ZERO=BLANK  ELSE EDIT THE NUMBER             
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO FILTER DATES IN ELEMENTS                              
         SPACE 3                                                                
*              INPUTS              THISDATE IS SET                              
*              OUTPUT              SET CONDITION CODE NE IF FAIL                
         SPACE 1                                                                
TESTDATE NTR1                                                                   
         CLI   TIQPSTR,0           ANY START FILTERING SET?                     
         BE    TESTDEND                                                         
         CLC   THISDATE,TIQPSTR    CHECK DATE NOT BEFORE START                  
         BL    NOGOOD2                                                          
         SPACE 1                                                                
TESTDEND CLI   TIQPEND,0           ANY END FILTERING SET?                       
         BE    ITSFINE2                                                         
         CLC   THISDATE,TIQPEND    CHECK DATE NOT AFTER END                     
         BH    NOGOOD2                                                          
         B     ITSFINE2                                                         
         SPACE 1                                                                
THISDATE DS    CL3                                                              
         EJECT                                                                  
THISUNIT DS    CL3                                                              
         EJECT                                                                  
INUPDET  DS    0H                  UPGRADE DETAIL                               
         USING TAUPD,R4                                                         
         TM    TGUSTYST,MAJORS                                                  
         BNO   INUPU30                                                          
         GOTO1 MAJVAL,DMCB,(X'80',TAUPIMAJ)                                     
         MVC   0(L'TGMACHAR,R2),TGMACHAR                                        
         LA    R2,L'TGMACHAR-1(R2)                                              
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
INUPU30  TM    TGUSTYST,UNITS                                                   
         BNO   XIT2                                                             
         EDIT  (2,TAUPIUNT),(4,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                   
         AR    R2,R0                                                            
         MVC   1(5,R2),=C'UNITS'                                                
         B     XIT2                                                             
*              PAYROLL OUT OF WITHHOLDING ELEMENTS                              
         SPACE 3                                                                
*              INPUT               SEE INPH (ABOVE) FOR ARGUMENTS               
         SPACE 1                                                                
         USING TAW2D,R4                                                         
INW2     MVC   THISUNIT,TAW2UNIT   DO THE 'WHERE' FILTERING FIRST               
         BRAS  RE,TESTWHER                                                      
         BNE   XIT2                                                             
         OC    TISKUNIT,TISKUNIT                                                
         BZ    INW2100                                                          
         CLC   THISUNIT,TISKUNIT   MATCH ON UNIT                                
         BNE   XIT2                                                             
INW2100  LA    R1,TAW2TAX          PASSED 'WHERE' TESTS                         
         CLI   GLARGS,C'T'         NOW PICK OFF REQUIRED AMOUNT                 
         BE    INPHOUT                                                          
         LA    R1,TAW2FICA         FICA                                         
         CLI   GLARGS,C'F'                                                      
         BE    INW2FED                                                          
         LA    R1,TAW2SDI                                                       
         CLI   GLARGS,C'D'         STATE DISABILITY                             
         BE    INW2STAT                                                         
         CLI   GLARGS,C'I'                                                      
         BE    INW2STAT                                                         
         LA    R1,TAW2SUI          STATE UI                                     
         CLI   GLARGS,C'U'                                                      
         BE    INW2STAT                                                         
         LA    R1,TAW2EARN                                                      
         CLI   GLARGS,C'1'                                                      
         BE    INPHOUT                                                          
         LA    R1,TAW2REXP         NON-TAX REIMB                                
         CLI   GLARGS,C'X'                                                      
         BE    INW2FED                                                          
         LA    R1,TAW2SFLI         FLI (NJ ONLY)                                
         CLI   GLARGS,C'L'                                                      
         BE    INW2STAT                                                         
         CLI   TAW2LEN,TAW2LNQ2                                                 
         BL    XIT2                                                             
         LA    R1,TAW2TXRE         TAXABLE REIMB                                
         CLI   GLARGS,C'R'                                                      
         BE    INPHOUT                                                          
         B     XIT2                                                             
*                                                                               
INW2FED  CLC   =C'FD ',THISUNIT                                                 
         BNE   XIT2                                                             
         B     INPHOUT                                                          
*                                                                               
INW2STAT CLC   =C'FD ',THISUNIT                                                 
         BE    XIT2                                                             
         CLI   THISUNIT+2,C' '                                                  
         BH    XIT2                                                             
         B     INPHOUT                                                          
         EJECT                                                                  
*              DATE ROUTINES                                                    
         SPACE 3                                                                
TODAYDT  MVC   0(3,R2),TGTODAY1                                                 
         B     XIT2                                                             
         SPACE 1                                                                
DATEIN   BRAS  RE,SETDATE                                                       
         MVC   0(3,R2),WORK                                                     
         B     XIT2                                                             
         SPACE 1                                                                
WEEKIN   BRAS  RE,SETDATE          WEEK (START)                                 
         OC    WORK(3),WORK                                                     
         BZ    XIT2                                                             
         MVC   0(3,R2),WORK                                                     
         GOTO1 DATCON,DMCB,(1,0(R2)),(0,WORK)                                   
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         CLI   DMCB,1              IS DATE A MONDAY                             
         BE    XIT2                                                             
         ZIC   R1,DMCB             CALCULATE MONDAY'S DATE                      
         BCTR  R1,0                                                             
         LCR   R1,R1                                                            
         ST    R1,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,(R2))                                  
         B     XIT2                                                             
         SPACE 1                                                                
MONIN    BRAS  RE,SETDATE          MONTH                                        
         MVC   0(2,R2),WORK                                                     
         CLI   GLARGS+1,2          ARG2:2=MONTH ONLY                            
         BL    XIT2                                                             
         MVC   0(1,R2),WORK+1                                                   
         B     XIT2                                                             
         SPACE 1                                                                
QUARTIN  BRAS  RE,SETDATE                                                       
         MVC   0(1,R2),WORK                                                     
         ZIC   R1,WORK+1           GET QUARTER FROM MONTH                       
         CH    R1,=H'10'           CONVERT PWOS 10-12                           
         BL    *+8                                                              
         SH    R1,=H'6'            X'10' BECOMES X'0A'                          
         BCTR  R1,0                                                             
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         STC   R1,1(R2)                                                         
         B     XIT2                                                             
         SPACE 1                                                                
         SPACE 1                                                                
YEARIN   BRAS  RE,SETDATE          YEAR                                         
         MVC   0(1,R2),WORK        SAVE INTERNAL YEAR                           
         B     XIT2                                                             
         EJECT                                                                  
*              INPUT - MISCELLANEOUS                                            
         SPACE 3                                                                
COMPUTIN ZAP   0(8,R2),=P'0'                                                    
         B     XIT2                                                             
         SPACE 1                                                                
COUNTIN  CLI   TIMODE,PROCREC      COUNT N'ACCOUNTS                             
         BNE   XIT2                                                             
         MVI   3(R2),1             (4 BYTE BINARY)                              
         B     XIT2                                                             
         SPACE 1                                                                
COUNTOUT CLI   GLARGS,C'X'         SUPPRESS ON TOTAL LINES                      
         BNE   *+12                                                             
         TM    GLINDS,GLTOTLIN     GET OUT IF THIS IS TOTALS                    
         BO    XIT2                                                             
         EDIT  (4,0(R2)),(7,(R3))                                               
         B     XIT2                                                             
         SPACE 1                                                                
GAPIN    MVC   0(4,R2),SPACES2                                                  
         B     XIT2                                                             
         EJECT                                                                  
*              PERIOD IN HEADINGS                                               
         SPACE 3                                                                
*              ARGUMENTS           1   DATE TYPE                                
*                                  2-4 START YMD (PWOS)                         
*                                  5-7 END YMD (PWOS)                           
         SPACE 1                                                                
PERPOP   MVC   WORK,SPACES2         PERIOD TO HEADLINES                         
         GOTO1 DATCON,DMCB,(1,GLARGS+1),(7,WORK)                                
         LA    R2,WORK+3                                                        
         CLC   0(2,R2),=C'00'                                                   
         BE    *+8                                                              
         LA    R2,2(R2)                                                         
         MVC   0(3,R2),SPACES2                                                  
         SPACE 1                                                                
*                                   NOW HAVE MMM OR MMMDD                       
         OC    GLARGS+4(3),GLARGS+4 ANY END DATE?                               
         BZ    PERPOP2                                                          
         MVI   0(R2),C'-'                                                       
         GOTO1 DATCON,DMCB,(1,GLARGS+4),(7,1(R2))                               
         LA    R2,4(R2)                                                         
         CLC   0(2,R2),=C'00'                                                   
         BE    *+8                                                              
         LA    R2,2(R2)                                                         
         MVC   0(3,R2),SPACES2      NOW HAVE MMM-MMM OR MMMDD-MMMDD             
         SPACE 1                                                                
PERPOP2  L     R4,GLADTENT                                                      
         USING DRHDD,R4                                                         
         ZIC   R2,DRHDWDTH                                                      
         GOTO1 CHOPPER,DMCB,(11,WORK),((R2),(R3)),(198,2)                       
         GOTO1 CENTER,DMCB,(R3),(R2)                                            
         B     XIT2                                                             
         EJECT                                                                  
ITSFINE2 SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NOGOOD2  LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT2     XIT1                                                                   
         SPACE 1                                                                
         GETEL2 (R4),DATADISP,ELCODE                                            
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
SPACES2  DC     CL256' '                                                        
AGINDATA DS    A                                                                
ELTYPE   DC    X'00'                                                            
INMAIN   DC    C'N'                                                             
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE ADDRESS LOOK-UP TABLE                                    
         SPACE 3                                                                
ROUTLIST DS    0F                                                               
         DC    C'INNAME  ',A(INNAME)      GENERAL                               
         DC    C'OUTNAME ',A(OUTNAME)                                           
         DC    C'INADD   ',A(INADD)                                             
         DC    C'INSHORT ',A(INSHORT)                                           
         DC    C'OUTSHORT',A(OUTSHORT)                                          
         DC    C'INFREE  ',A(INFREE)                                            
         DC    C'OUTFREE ',A(OUTFREE)                                           
         DC    C'INNUM   ',A(INNUM)                                             
         DC    C'INNU    ',A(INNU)                                              
         DC    C'OUTNU   ',A(OUTNU)                                             
         DC    C'MIDHDIN ',A(MIDHDIN)                                           
         DC    C'TOTRQIN ',A(TOTRQIN)                                           
         DC    C'TOTRQOUT',A(TOTRQOUT)                                          
         DC    C'NOPRTIN ',A(NOPRTIN)                                           
         DC    C'NOPRTOUT',A(NOPRTOUT)                                          
         DC    C'AVPAYOUT',A(AVPAYOUT)                                          
         DC    C'INCWST  ',A(INCWST)                                            
         SPACE 1                                                                
         DC    C'OFIN    ',A(OFIN)        CODE AND/OR NAME                      
         DC    C'OFOUT   ',A(OFOUT)                                             
         DC    C'AGIN    ',A(AGIN)                                              
         DC    C'AGOUT   ',A(AGOUT)                                             
         DC    C'AYIN    ',A(AYIN)                                              
         DC    C'AYOUT   ',A(AYOUT)                                             
         DC    C'CGIN    ',A(CGIN)                                              
         DC    C'CGOUT   ',A(CGOUT)                                             
         DC    C'CLIN    ',A(CLIN)                                              
         DC    C'CLOUT   ',A(CLOUT)                                             
         DC    C'INCLTPC ',A(INCLTPC)                                           
         DC    C'OUTCLTPC',A(OUTCLTPC)                                          
         DC    C'CURROUT ',A(CURROUT)                                           
         DC    C'PGIN    ',A(PGIN)                                              
         DC    C'PGOUT   ',A(PGOUT)                                             
         DC    C'PRIN    ',A(PRIN)                                              
         DC    C'PROUT   ',A(PROUT)                                             
         DC    C'CIDOUT  ',A(CIDOUT)                                            
         DC    C'COIN    ',A(COIN)                                              
         DC    C'COOUT   ',A(COOUT)                                             
         DC    C'DLIN    ',A(DLIN)                                              
         DC    C'EMIN    ',A(EMIN)                                              
         DC    C'EMOUT   ',A(EMOUT)                                             
         DC    C'W4IN    ',A(W4IN)                                              
         DC    C'W4OUT   ',A(W4OUT)                                             
         DC    C'STIN    ',A(STIN)                                              
         DC    C'STOUT   ',A(STOUT)                                             
         DC    C'ANIN    ',A(ANIN)                                              
         DC    C'ANOUT   ',A(ANOUT)                                             
         DC    C'MUIN    ',A(MUIN)                                              
         DC    C'MUOUT   ',A(MUOUT)                                             
         DC    C'ACDEOUT ',A(ACDEOUT)                                           
         DC    C'INVIN   ',A(INVIN)                                             
         DC    C'INVOUT  ',A(OUTINV)                                            
         DC    C'EPIN    ',A(EPIN)                                              
         DC    C'EPOUT   ',A(EPOUT)                                             
         DC    C'ESIN    ',A(ESIN)                                              
         SPACE 1                                                                
         DC    C'FILTIN  ',A(FILTIN)      OTHER SYSIO CODES                     
         DC    C'FILTOUT ',A(FILTOUT)                                           
         DC    C'CATIN   ',A(CATIN)                                             
         DC    C'CATOUT  ',A(CATOUT)                                            
         DC    C'W4TIN   ',A(W4TIN)                                             
         DC    C'W4TOUT  ',A(W4TOUT)                                            
         DC    C'ONOFIN  ',A(ONOFIN)                                            
         DC    C'ONOFOUT ',A(ONOFOUT)                                           
         DC    C'CORPIN  ',A(CORPIN)                                            
         DC    C'CORPOUT ',A(CORPOUT)                                           
         DC    C'GUAIN   ',A(GUAIN)                                             
         DC    C'GUAOUT  ',A(GUAOUT)                                            
         DC    C'TPCIN   ',A(TPCIN)                                             
         DC    C'TPCOUT  ',A(TPCOUT)                                            
         DC    C'MGRIN   ',A(MGRIN)                                             
         DC    C'MGROUT  ',A(MGROUT)                                            
         DC    C'LACTIN  ',A(LACTIN)                                            
         DC    C'LACTOUT ',A(LACTOUT)                                           
         DC    C'LEADIN  ',A(LEADIN)                                            
         DC    C'PTYPEIN ',A(PTYPEIN)                                           
         DC    C'UNIN    ',A(UNIN)                                              
         DC    C'UNOUT   ',A(UNOUT)                                             
         DC    C'UNITIN  ',A(UNITIN)                                            
         DC    C'UNITOUT ',A(UNITOUT)                                           
         DC    C'USEOUT  ',A(USEOUT)                                            
         DC    C'LOCLIN  ',A(LOCLIN)                                            
         DC    C'LOCLOUT ',A(LOCLOUT)                                           
         DC    C'MEDIN   ',A(MEDIN)                                             
         DC    C'MEDOUT  ',A(MEDOUT)                                            
         DC    C'BNKOUT  ',A(BNKOUT)                                            
         DC    C'ADJINVIN',A(ADJINVIN)                                          
         DC    C'AGYINV  ',A(AGYINV)                                            
         DC    C'HLDUSE  ',A(HLDUSE)                                            
         SPACE 1                                                                
         DC    C'DATEIN  ',A(DATEIN)      DATE RELATED                          
         DC    C'DATOUT  ',A(DATOUT)                                            
         DC    C'DATOUTRV',A(DATOUTRV)                                          
         DC    C'TODAYDT ',A(TODAYDT)                                           
         DC    C'DUEOUT  ',A(DUEOUT)                                            
         DC    C'AIROUT  ',A(AIROUT)                                            
         DC    C'WRKOUT  ',A(WRKOUT)                                            
         DC    C'MONIN   ',A(MONIN)                                             
         DC    C'MONOUT  ',A(MONOUT)                                            
         DC    C'NUMDAYS ',A(NUMDAYS)                                           
         DC    C'QUARTIN ',A(QUARTIN)                                           
         DC    C'QUARTOUT',A(QUARTOUT)                                          
         DC    C'USECOUT ',A(USECOUT)                                           
         DC    C'WEEKIN  ',A(WEEKIN)                                            
         DC    C'YEARIN  ',A(YEARIN)                                            
         DC    C'YEAROUT ',A(YEAROUT)                                           
         DC    C'PERPOP  ',A(PERPOP)                                            
         SPACE 1                                                                
         DC    C'COMPUTIN',A(COMPUTIN)    MISCELLANEOUS                         
         DC    C'COUNTIN ',A(COUNTIN)                                           
         DC    C'COUNTOUT',A(COUNTOUT)                                          
         DC    C'DVIN    ',A(DVIN)                                              
         DC    C'GAPIN   ',A(GAPIN)                                             
         DC    C'GTIN    ',A(GTIN)                                              
         DC    C'OUTCD   ',A(OUTCD)                                             
         DC    C'LOWEST  ',A(OUTLOW)                                            
         DC    C'STACKIN ',A(STACKIN)                                           
         DC    C'STACKOUT',A(STACKOUT)                                          
         DC    C'STDTAIN ',A(STDTAIN)                                           
         DC    C'STDTAOUT',A(STDTAOUT)                                          
         DC    C'DSKADDIN',A(DSKADDIN)                                          
         DC    C'CRPSOUT ',A(CRPSOUT)                                           
         DC    C'TIMOUT  ',A(TIMOUT)                                            
         DC    C'VRIN    ',A(VRIN)                                              
         DC    C'VROUT   ',A(VROUT)                                             
         DC    C'OUTMUC  ',A(OUTMUC)                                            
         DC    C'TEXT    ',A(TEXT)                                              
         DC    C'AGNTIN  ',A(AGNTIN)                                            
         SPACE 1                                                                
*                                        ELEMENT RELATED                        
         DC    C'INKEY   ',A(INKEY)                                             
         DC    C'INAK    ',A(INAK)                                              
         DC    C'INAN    ',A(INAN)                                              
         DC    C'INAT    ',A(INAT)                                              
         DC    C'INAY    ',A(INAY)                                              
         DC    C'INBD    ',A(INBD)                                              
         DC    C'INBR    ',A(INBR)                                              
         DC    C'INCA    ',A(INCA)                                              
         DC    C'INCAROL ',A(INCAROL)                                           
         DC    C'INCC    ',A(INCC)                                              
         DC    C'INMC    ',A(INMC)                                              
         DC    C'INCD    ',A(INCD)                                              
         DC    C'INCH    ',A(INCH)                                              
         DC    C'INCL    ',A(INCL)                                              
         DC    C'INCM    ',A(INCM)                                              
         DC    C'INCO    ',A(INCO)                                              
         DC    C'INCR    ',A(INCR)                                              
         DC    C'INCP    ',A(INCP)                                              
         DC    C'INCS    ',A(INCS)                                              
         DC    C'INCX    ',A(INCX)                                              
         DC    C'INCY    ',A(INCY)                                              
         DC    C'INCQ    ',A(INCQ)                                              
         DC    C'INDD    ',A(INDD)                                              
         DC    C'INDL    ',A(INDL)                                              
         DC    C'INDU    ',A(INDU)                                              
         DC    C'INDV    ',A(INDV)                                              
         DC    C'INDW    ',A(INDW)                                              
         DC    C'INEI    ',A(INEI)                                              
         DC    C'INEMAGY ',A(INEMAGY)                                           
         DC    C'INEMCLI ',A(INEMCLI)                                           
         DC    C'INEMPRD ',A(INEMPRD)                                           
         DC    C'INEMATT ',A(INEMATT)                                           
         DC    C'INEMSTF ',A(INEMSTF)                                           
         DC    C'INEP    ',A(INEP)                                              
         DC    C'INEUB   ',A(INEUB)                                             
         DC    C'INEUP   ',A(INEUP)                                             
         DC    C'INFN    ',A(INFN)                                              
         DC    C'INGC    ',A(INGC)                                              
         DC    C'INGH    ',A(INGH)                                              
         DC    C'INGT    ',A(INGT)                                              
         DC    C'INGU    ',A(INGU)                                              
         DC    C'INIF    ',A(INIF)                                              
         DC    C'ININ    ',A(ININ)                                              
         DC    C'INIS    ',A(INIS)                                              
         DC    C'INKP    ',A(INKP)                                              
         DC    C'INLF    ',A(INLF)                                              
         DC    C'INLN    ',A(INLN)                                              
         DC    C'INLW    ',A(INLW)                                              
         DC    C'INMA    ',A(INMA)                                              
         DC    C'INMU    ',A(INMU)                                              
         DC    C'INMUH   ',A(INMUH)                                             
         DC    C'INMUT   ',A(INMUT)                                             
         DC    C'INMUTL  ',A(INMUTL)                                            
         DC    C'INMUC   ',A(INMUC)                                             
         DC    C'INNP    ',A(INNP)                                              
         DC    C'INNX    ',A(INNX)                                              
         DC    C'INOA    ',A(INOA)                                              
         DC    C'INOC    ',A(INOC)                                              
         DC    C'INOP    ',A(INOP)                                              
         DC    C'INOW    ',A(INOW)                                              
         DC    C'INPD    ',A(INPD)                                              
         DC    C'INPE    ',A(INPE)                                              
         DC    C'INPH    ',A(INPH)                                              
         DC    C'INPI    ',A(INPI)                                              
         DC    C'INPO    ',A(INPO)                                              
         DC    C'INRN    ',A(INRN)                                              
         DC    C'INRP    ',A(INRP)                                              
         DC    C'INR1    ',A(INR1)                                              
         DC    C'INSD    ',A(INSD)                                              
         DC    C'INSI    ',A(INSI)                                              
         DC    C'INSO    ',A(INSO)                                              
         DC    C'INST    ',A(INST)                                              
         DC    C'INTU    ',A(INTU)                                              
         DC    C'INT4    ',A(INT4)                                              
         DC    C'INUL    ',A(INUL)                                              
         DC    C'INUP    ',A(INUP)                                              
         DC    C'INVR    ',A(INVR)                                              
         DC    C'INVU    ',A(INVU)                                              
         DC    C'INWH    ',A(INWH)                                              
         DC    C'INW2    ',A(INW2)                                              
         DC    C'INW4    ',A(INW4)                                              
         DC    C'INWX    ',A(INWX)                                              
         DC    C'INXT    ',A(INXT)                                              
         DC    C'INYE    ',A(INYE)                                              
         DC    X'FF'                                                            
         EJECT                                                                  
*              DATA FROM KEYS                                                   
         SPACE 1                                                                
INKEYTAB DC    AL1(TLCASORT-TLCAKEY),AL1(0)   1=CAST SORT DATA                  
         DC    AL1(TLDUDUC-TLDUKEY),AL1(0)    2=DUE COMPANY CODE                
         DC    AL1(TLLNLIN-TLLNKEY),AL1(0)    3=LIEN CODE                       
         DC    AL1(TLCKSORT-TLCKKEY),AL1(0)   4=CHECK CAST SEQ NO               
         SPACE 1                                                                
*              DATA FROM AKA NAME ELEMENT                                       
         SPACE 1                                                                
INAKTAB  DC    AL1(TAAKNAM1-TAAKEL),AL1(0)    1=AKA FIRST NAME                  
         DC    AL1(TAAKNAM2-TAAKEL),AL1(0)    2=AKA LAST NAME                   
         SPACE 1                                                                
*              DATA FROM AGENT ELEMENT                                          
         SPACE 1                                                                
INANTAB  DC    AL1(TAANTEL-TAANEL),AL1(0)     1=TELEPHONE #                     
         DC    AL1(TAANSTAT-TAANEL),AL1(1)    2=AGENT STATUS                    
         DC    AL1(TAANTNR-TAANEL),AL1(0)     3=OLD T&R CODE                    
         DC    AL1(TAANSSN-TAANEL),AL1(2)     4=SSN                             
         SPACE 1                                                                
*              DATA FROM CANADIAN TAX WITHHOLDING ELEMENT                       
         SPACE 1                                                                
INATTAB  DC    AL1(TAATUNIT-TAATEL),AL1(1)    1=UNIT                            
         DC    AL1(TAATUNIT-TAATEL),AL1(2)    2=PROV UNIT                       
         DC    AL1(TAATTAX-TAATEL),AL1(3)     3=CANADIAN TAX                    
         DC    AL1(TAATTAX-TAATEL),AL1(4)     4=PROVINCE TAX                    
         DC    AL1(TAATPP-TAATEL),AL1(4)      5=PENSION PLAN                    
         DC    AL1(TAATEI-TAATEL),AL1(4)      6=EMP INSURANCE PREM              
         DC    AL1(TAATPIP-TAATEL),AL1(4)     7=PARENTIAL INSURANCE             
         SPACE 1                                                                
*              DATA FROM AGENCY ELEMENT                                         
         SPACE 1                                                                
INAYTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAAYAGG-TAAYEL),AL1(4)     1=AGENCY GROUP                    
         DC    AL1(TAAYTPOF-TAAYEL),AL1(0)    2=TP OFFICE                       
         DC    AL1(TAAYTEL-TAAYEL),AL1(0)     3=TELEPHONE#                      
         DC    AL1(TAAYNBIL-TAAYEL),AL1(0)    4=NUMBER BILLS                    
         DC    AL1(TAAYSTAT-TAAYEL),AL1(1)    5=STATUS                          
         DC    AL1(TAAYHLD-TAAYEL),AL1(2)     6=HOLDING FEE NOT                 
         DC    AL1(TAAYTPC-TAAYEL),AL1(0)     7=DEFAULT TPC                     
         DC    AL1(TAAYRINV-TAAYEL),AL1(0)    8=RESET INVOICE                   
         DC    AL1(TAAYIAGY-TAAYEL),AL1(0)    9=INVOICE AGENCY                  
         DC    AL1(TAAYNINV-TAAYEL),AL1(3)    10=NEXT INVOICE                   
         DC    AL1(TAAYDAYS-TAAYEL),AL1(5)    11=BILLING DAYS DUE               
         DC    AL1(TAAYLBOX-TAAYEL),AL1(6)    12=LOCK BOX CODE                  
         DC    AL1(TAAYSTA6-TAAYEL),AL1(7)    13=STATUS 6                       
         DC    AL1(TAAYSTA7-TAAYEL),AL1(8)    14=STATUS 7                       
         DC    AL1(TAAYSTA3-TAAYEL),AL1(9)    13=STATUS 3                       
         SPACE 1                                                                
*              DATA FROM BILL DETAILS ELEMENT                                   
         SPACE 1                                                                
INBDTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TABDTYPE-TABDEL),AL1(0)    1=BILLING TYPE                    
         DC    AL1(TABDCCVT-TABDEL),AL1(7)    2=CANADIAN CONVERSION RTE         
         DC    AL1(TABDTOT-TABDEL),AL1(3)     3=BILLING TOTAL                   
         DC    AL1(TABDTAX-TABDEL),AL1(3)     4=PAYROLL TAXES                   
         DC    AL1(TABDHND-TABDEL),AL1(3)     5=HANDLING                        
         DC    AL1(TABDHNDC-TABDEL),AL1(3)    6=HANDLING CORP                   
         DC    AL1(TABDCSF-TABDEL),AL1(3)     7=CONTRACT SERVICE FEE            
         DC    AL1(TABDFICR-TABDEL),AL1(3)    8=FICA CREDIT                     
         DC    AL1(TABDHND-TABDEL),AL1(1)     9=ALL HANDLING                    
         DC    AL1(TABDHND-TABDEL),AL1(2)     10=ALL HANDLING+TAXES             
         DC    AL1(TABDHND-TABDEL),AL1(4)     11=HANDLING+TAXES                 
         DC    AL1(TABDGST-TABDEL),AL1(3)     12=CANADIAN GST                   
         DC    AL1(TABDACOM-TABDEL),AL1(5)    13=AGENCY COMMISSION              
         DC    AL1(TABDSIGN-TABDEL),AL1(5)    14=SIGNATORY FEE                  
         DC    AL1(TABDPST-TABDEL),AL1(3)     15=CANADIAN PST                   
         DC    AL1(TABDPST-TABDEL),AL1(6)     16=CANADIAN HST                   
         SPACE 1                                                                
*              DATA FROM BILL DETAILS ELEMENT FOR CHLOE                         
         SPACE 1                                                                
INCHTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TABDTYPE-TABDEL),AL1(0)    1=BILLING TYPE                    
         DC    AL1(TABDCCVT-TABDEL),AL1(7)    2=CANADIAN CONVERSION RTE         
         DC    AL1(TABDTOT-TABDEL),AL1(3)     3=BILLING TOTAL                   
         DC    AL1(TABDTAX-TABDEL),AL1(3)     4=PAYROLL TAXES                   
         DC    AL1(TABDHND-TABDEL),AL1(3)     5=HANDLING                        
         DC    AL1(TABDHNDC-TABDEL),AL1(3)    6=HANDLING CORP                   
         DC    AL1(TABDCSF-TABDEL),AL1(3)     7=CONTRACT SERVICE FEE            
         DC    AL1(TABDFICR-TABDEL),AL1(3)    8=FICA CREDIT                     
         DC    AL1(TABDHND-TABDEL),AL1(1)     9=ALL HANDLING                    
         DC    AL1(TABDHND-TABDEL),AL1(2)     10=ALL HANDLING+TAXES             
         DC    AL1(TABDHND-TABDEL),AL1(4)     11=HANDLING+TAXES                 
         DC    AL1(TABDGST-TABDEL),AL1(3)     12=CANADIAN GST                   
         DC    AL1(TABDACOM-TABDEL),AL1(5)    13=AGENCY COMMISSION              
         DC    AL1(TABDSIGN-TABDEL),AL1(5)    14=SIGNATORY FEE                  
         DC    AL1(TABDPST-TABDEL),AL1(3)     15=CANADIAN PST                   
         DC    AL1(TABDPST-TABDEL),AL1(6)     16=CANADIAN HST                   
         SPACE 1                                                                
*              DATA FROM BILLING RULES ELEMENT                                  
         SPACE 1                                                                
INBRTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TABRTYPE-TABREL),AL1(0)    1=BILLING TYPE                    
         DC    AL1(TABRSTAT-TABREL),AL1(1)    2=STATUS                          
         DC    AL1(TABROEOR-TABREL),AL1(0)    3=OVERRIDE EMPLOYER               
         DC    AL1(TABRRECV-TABREL),AL1(0)    4=RECEIVABLE CODE                 
         DC    AL1(TABRRATE-TABREL),AL1(2)    5=CURRENT RATES                   
         DC    AL1(TABRFUTA-TABREL),AL1(0)    6=FUTA RATE                       
         DC    AL1(TABRSUTA-TABREL),AL1(0)    7=SUTA RATE                       
         DC    AL1(TABRFICA-TABREL),AL1(0)    8=FICA RATE                       
         DC    AL1(TABROVER-TABREL),AL1(0)    9=OVER FICA MAX                   
         DC    AL1(TABRIFIC-TABREL),AL1(0)    10=IFIC                           
**EQU**  DC    AL1(TABRCORP-TABREL),AL1(0)    10=CORP RATE                      
         DC    AL1(TABRHAND-TABREL),AL1(0)    11=HANDLING TAX                   
         DC    AL1(TABRCAN-TABREL),AL1(0)     12=CANADIAN TAX                   
*NOTUSED*DC    AL1(TABROMED-TABREL),AL1(0)    13=OVER MEDICARE                  
         DC    AL1(TABRWCRP-TABREL),AL1(0)    13=WC ON CORPS                    
         DC    AL1(TABRHRLS-TABREL),AL1(0)    14=HANDLING RULES                 
         SPACE 1                                                                
*              DATA FROM CAST DETAIL ELEMENTS                                   
         SPACE 1                                                                
INCATAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TACAONOF-TACAEL),AL1(2)    1=CAST COUNT                      
         DC    AL1(TACAONOF-TACAEL),AL1(0)    2=ON/OFF CAMERA                   
         DC    AL1(TACAUN-TACAEL),AL1(0)      3=UNION                           
         DC    AL1(TACALOCL-TACAEL),AL1(0)    4=LOCAL                           
         DC    AL1(TACAYEAR-TACAEL),AL1(0)    5=UNION YEAR                      
         DC    AL1(TACAFRST-TACAEL),AL1(0)    6=FIRST SERVICES DATE             
         DC    AL1(TACALAST-TACAEL),AL1(0)    7=LAST SERVICES DATE              
         DC    AL1(TACAEXP-TACAEL),AL1(0)     8=EXPIRY DATE                     
         DC    AL1(TACASDTE-TACAEL),AL1(0)    9=SHOOT DATE                      
         DC    AL1(TACANCDE-TACAEL),AL1(3)    10=AGENT CODE                     
         DC    AL1(TACACORP-TACAEL),AL1(0)    11=CORP. CODE                     
         DC    AL1(TACAUNIT-TACAEL),AL1(0)    12=TAX UNIT                       
         DC    AL1(TACAOV2-TACAEL),AL1(0)     13=OVERSCALE 2                    
         DC    AL1(TACADBL-TACAEL),AL1(0)     14=N'DOUBLES                      
         DC    AL1(TACASTAT-TACAEL),AL1(1)    15=STATUS                         
         DC    AL1(0),AL1(0)                  16=SPARE                          
         DC    AL1(TACAFCYC-TACAEL),AL1(0)    17=FIRST FIXED CYCLE DATE         
         DC    AL1(TACAPAYE-TACAEL),AL1(0)    18=PAYEE CODE A=AGENT ETC         
         DC    AL1(TACAEBAS-TACAEL),AL1(0)    19=EXPIRY BASIS                   
         DC    AL1(TACAGUA-TACAEL),AL1(0)     20=GUARANTEE CODE                 
         DC    AL1(TACARERC-TACAEL),AL1(4)    21=RE-RECORD DATE                 
         DC    AL1(0),AL1(5)                  22=RELEASE LETTER                 
         DC    AL1(0),AL1(5)                  23=RELEASE EFFECTIVE DATE         
         SPACE 1                                                                
*              DATA FROM CONTRACTS ELEMENT                                      
         SPACE 1                                                                
INCCTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TACCCON-TACCEL),AL1(1)     1=CONTRACTS                       
         SPACE 1                                                                
*              DATA FROM MUSIC CONTRACT DETAILS ELEMENT                         
         SPACE 1                                                                
INMCTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAMCCON-TAMCEL),AL1(0)     1=CONTRACT                        
         SPACE 1                                                                
*              DATA FROM CHECK DETAILS ELEMENT                                  
         SPACE 1                                                                
INCDTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TACDDTE-TACDEL),AL1(0)     1=CHECK DATE                      
         DC    AL1(TACDRUN-TACDEL),AL1(0)     2=RUN DATE                        
         DC    AL1(TACDCSH-TACDEL),AL1(0)     3=DATE CASHED                     
         DC    AL1(TACDBNK-TACDEL),AL1(0)     4=BANK CODE                       
         DC    AL1(TACDSTAT-TACDEL),AL1(1)    5=STATUS                          
         DC    AL1(TACDSTA2-TACDEL),AL1(5)    6=STATUS 2                        
         DC    AL1(TACDCHK-TACDEL),AL1(3)     7=CHECK NUMBER                    
         DC    AL1(TACDFREQ-TACDEL),AL1(0)    8=FREQUENCY CODE                  
         DC    AL1(TACDEARN-TACDEL),AL1(6)    9=GROSS EARNINGS                  
         DC    AL1(TACDNTAX-TACDEL),AL1(4)    10=NON-TAXABLE                    
         DC    AL1(TACDNET-TACDEL),AL1(4)     11=NET                            
         DC    AL1(TACDCHK-TACDEL),AL1(7)     12=CHECK NUM COUNT                
         DC    AL1(TACDYREX-TACDEL),AL1(4)    13=YTD REIMBURSE EXP.             
         DC    AL1(TACDSEQ-TACDEL),AL1(0)     14=SEQUENCE                       
         DC    AL1(TACDCSH-TACDEL),AL1(2)     15=N'DAYS TO CASH                 
         DC    AL1(TACDCHK-TACDEL),AL1(8)     16=CHECK COUNT                    
         SPACE 1                                                                
*              DATA FROM CLIENT ELEMENT                                         
         SPACE 1                                                                
INCLTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TACISTAT-TACIEL),AL1(1)    1=STATUS                          
         SPACE 1                                                                
*              DATA FROM COMMENTS ELEMENT                                       
         SPACE 1                                                                
INCMTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TACMCOMM-TACMEL),AL1(1)    1=COMMENT                         
         SPACE 1                                                                
*              DATA FROM COMMERCIAL DETAIL ELEMENTS                             
         SPACE 1                                                                
INCOTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(0),AL1(0)                  1=SPARE                           
         DC    AL1(TACOCID-TACOEL),AL1(0)     2=COMMERCIAL ID (CID)             
         DC    AL1(TACOFCYC-TACOEL),AL1(0)    3=FIRST FIXED CYC                 
         DC    AL1(TACOAIR-TACOEL),AL1(0)     4=FIRST AIR DATE                  
         DC    AL1(TACOEXP-TACOEL),AL1(0)     5=EXPIRY DATE                     
         DC    AL1(TACOVDTE-TACOEL),AL1(0)    6=CAST VER. DATE                  
         DC    AL1(TACOVST-TACOEL),AL1(0)     7=CAST VER. STAFF ID              
         DC    AL1(TACODUB-TACOEL),AL1(0)     8=DUB DATE                        
         DC    AL1(TACOUVST-TACOEL),AL1(5)    9=UNVERIFY STATUS                 
         DC    AL1(TACOMED-TACOEL),AL1(2)     10=MEDIA CODE                     
         DC    AL1(TACOADST-TACOEL),AL1(0)    11=ADDENDUM STATE                 
         DC    AL1(TACOCONT-TACOEL),AL1(0)    12=CONTRACT TYPE                  
         DC    AL1(TACOSEC-TACOEL),AL1(0)     13=SECONDS LENGTH                 
         DC    AL1(TACOCTYP-TACOEL),AL1(7)    14=ACTRA TYPE                     
         DC    AL1(TACOTYPE-TACOEL),AL1(0)    15=COMM. TYPE                     
         DC    AL1(TACOPDTE-TACOEL),AL1(0)    16=LAST PAY DATE                  
         DC    AL1(TACOTID-TACOEL),AL1(0)     17=TRACK ID                       
         DC    AL1(TACOTLN-TACOEL),AL1(0)     18=TRACK LENGTH                   
         DC    AL1(TACOAFM-TACOEL),AL1(0)     19=AFM RATE                       
         DC    AL1(TACOATT-TACOEL),AL1(0)     20=BILL TO CODE                   
         DC    AL1(TACOSTAT-TACOEL),AL1(4)    21=STATUS                         
         DC    AL1(TACOCLG-TACOEL),AL1(0)     22=CLIENT GROUP                   
         DC    AL1(TACOSES-TACOEL),AL1(6)     23=SESSION TYPE                   
         SPACE 1                                                                
*              DATA FROM APPLIED CREDIT ELEMENTS                                
         SPACE 1                                                                
INCRTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TACRSTRT-TACREL),AL1(0)    1=CYCLE START                     
         DC    AL1(TACREND-TACREL),AL1(0)     2=CYCLE END                       
         DC    AL1(TACRSCAL-TACREL),AL1(1)    3=SCALE AMOUNT                    
         DC    AL1(TACRAPPL-TACREL),AL1(1)    4=AMOUNT TO BE APPLIED            
         DC    AL1(TACRBAL-TACREL),AL1(1)     5=BALANCE                         
         DC    AL1(TACRLPG-TACREL),AL1(0)     6=LAST PAGE PRINTED               
         DC    AL1(TACRSTAT-TACREL),AL1(0)    7=STATUS                          
         DC    AL1(TACRUSE-TACREL),AL1(0)     8=USE CODE                        
         DC    AL1(TACRINV-TACREL),AL1(3)     9=INVOICE NUMBER                  
         DC    AL1(TACRAPPL-TACREL),AL1(2)    10=APPLIED (APPL-BAL)             
         SPACE 1                                                                
*              DATA FROM COMMERCIAL PUBLISHED MUSIC ELEMENTS                    
         SPACE 1                                                                
INCPTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TACPMUS-TACPEL),AL1(0)     1=MUSIC CODE                      
         DC    AL1(TACPMUS-TACPEL),AL1(1)     2=MUSIC CODE (2ND ELE)            
         DC    AL1(TACPMUS-TACPEL),AL1(2)     3=MUSIC CODE (3RD ELE)            
         DC    AL1(TACPMUS-TACPEL),AL1(3)     4=MUSIC CODE (4TH ELE)            
         SPACE 1                                                                
*              DATA FROM COMMERCIAL EXTRA DETAIL ELEMENTS                       
         SPACE 1                                                                
INCSTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TACSDATE-TACSEL),AL1(0)    1=DATE                            
         DC    AL1(TACSSTUD-TACSEL),AL1(0)    2=STUDIO                          
         DC    AL1(TACSCITY-TACSEL),AL1(0)    3=CITY                            
         DC    AL1(TACSSTAT-TACSEL),AL1(1)    4=STATE                           
         SPACE 1                                                                
*              DATA FROM CHECK EXTRA DETAILS ELEMENT                            
         SPACE 1                                                                
INCXTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TACXGST-TACXEL),AL1(0)     1=GST                             
         DC    AL1(TACXGSTY-TACXEL),AL1(0)    2=GST YTD                         
         DC    AL1(TACXPST-TACXEL),AL1(0)     3=PST                             
         DC    AL1(TACXPST-TACXEL),AL1(1)     4=HST                             
         DC    AL1(TACXMDDY-TACXEL),AL1(0)    5=MISC DEDUCTION YTD              
         DC    AL1(TACXPSTY-TACXEL),AL1(0)    6=PST YTD                         
         DC    AL1(TACXPSTY-TACXEL),AL1(2)    7=HST                             
         SPACE 1                                                                
*              DATA FROM DUE DATE ELEMENT                                       
         SPACE 1                                                                
INDDTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TADDOVRD-TADDEL),AL1(0)    1=ORIGINAL DUE DATE               
         SPACE 1                                                                
*              DATA FROM DEAL ELEMENTS (PRINT)                                  
         SPACE 1                                                                
INDLTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TADLAREA-TADLEL),AL1(0)    1=AREA                            
         DC    AL1(TADLUSE-TADLEL),AL1(0)     2=USE                             
         DC    AL1(TADLAMT-TADLEL),AL1(0)     3=AMOUNT                          
         DC    AL1(TADLRATE-TADLEL),AL1(0)    4=AGENT COMMISSION RATE           
         DC    AL1(TADLTERM-TADLEL),AL1(1)    5=TERM (N'MONTHS)                 
         DC    AL1(TADLPUB-TADLEL),AL1(0)     6=PUBLICATION DATE                
         DC    AL1(TADLEXP-TADLEL),AL1(0)     7=EXPIRATION DATE                 
         SPACE 1                                                                
*              DATA FROM YTD WITHHOLDING ELEMENTS                               
         SPACE 1                                                                
INCYTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TACYUNIT-TACYEL),AL1(0)    1=UNIT                            
         DC    AL1(0),AL1(0)                  2=SPARE                           
         DC    AL1(TACYTAX-TACYEL),AL1(1)     3=TAX                             
         DC    AL1(TACYFICA-TACYEL),AL1(1)    4=FICA OR SDI                     
         DC    AL1(TACYSUI-TACYEL),AL1(1)     5=SUI OR GST                      
         DC    AL1(TACYEARN-TACYEL),AL1(2)    6=EARNINGS                        
         DC    AL1(TACYSFLI-TACYEL),AL1(1)    7=FLI                             
         SPACE 1                                                                
*              DATA FROM QTD WITHHOLDING ELEMENTS                               
         SPACE 1                                                                
INCQTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TACQUNIT-TACQEL),AL1(0)    1=UNIT                            
         DC    AL1(TACQSDI-TACQEL),AL1(1)     2=SDI                             
         DC    AL1(TACQEARN-TACQEL),AL1(1)    3=EARNINGS                        
         SPACE 1                                                                
*              DATA FROM DUE COMPANY ELEMENT                                    
         SPACE 1                                                                
INDUTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TADUAGY-TADUEL),AL1(0)     1=ERROR AGENCY                    
         DC    AL1(TADUINV-TADUEL),AL1(1)     2=ERROR INVOICE #                 
         DC    AL1(TADUCINV-TADUEL),AL1(1)    3=CREDIT INVOICE #                
         DC    AL1(TADUFCLI-TADUEL),AL1(0)    4=FILTER CLIENT                   
         DC    AL1(TADUTYPE-TADUEL),AL1(2)    5=TYPE                            
         DC    AL1(TADUSTAT-TADUEL),AL1(5)    6=STATUS                          
         DC    AL1(TADUDUE-TADUEL),AL1(3)     7=AMOUNT DUE                      
         DC    AL1(TADUCOL-TADUEL),AL1(3)     8=AMOUNT COLLECTED                
         DC    AL1(TADUDUE-TADUEL),AL1(4)     9=BALANCE                         
         DC    AL1(TADUPCT-TADUEL),AL1(0)     10=DEDUCTION PERCENTAGE           
         DC    AL1(TADUUNI-TADUEL),AL1(0)     11=UNION                          
         DC    AL1(TADUEMP-TADUEL),AL1(0)     12=EMPLOYER                       
         DC    AL1(TADUSTAT-TADUEL),AL1(6)    13=FLAG                           
         SPACE 1                                                                
*              DATA FROM ADVICE ELEMENT                                         
         SPACE 1                                                                
INDVTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TADVSTAT-TADVEL),AL1(1)    1=STATUS                          
         DC    AL1(TADVVST-TADVEL),AL1(0)     2=VERIFIED BY                     
         DC    AL1(TADVVDTE-TADVEL),AL1(0)    3=VERIFIED DATE                   
         DC    AL1(TADVSST-TADVEL),AL1(0)     4=SENT BY                         
         DC    AL1(TADVSDTE-TADVEL),AL1(0)    5=SENT DATE                       
         DC    AL1(TADVRST-TADVEL),AL1(0)     6=RECEIVED BY                     
         DC    AL1(TADVRDTE-TADVEL),AL1(0)    7=RECEIVED DATE                   
         DC    AL1(TADVTYPE-TADVEL),AL1(2)    8=ADVICE TYPE                     
         DC    AL1(TADVPDTE-TADVEL),AL1(0)    9=PAID DATE                       
         SPACE 1                                                                
*              DATA FROM EPISODE INFORMATION ELEMENT                            
         SPACE 1                                                                
INEITAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAEIWDT-TAEIEL),AL1(0)     1=WORK DATE                       
         DC    AL1(TAEIADT-TAEIEL),AL1(0)     2=AIR DATE                        
         SPACE 1                                                                
*              DATA FROM ESTIMATE PROFILE ELEMENT                               
         SPACE 1                                                                
INEPTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAEPFILT-TAEPEL),AL1(0)    1=REP GEN OPTION                  
         DC    AL1(TAEPCOMM-TAEPEL),AL1(0)    2=COMM BASIS                      
         DC    AL1(TAEPMTH-TAEPEL),AL1(0)     3=BINARY START MONTH              
         DC    AL1(TAEPEXP-TAEPEL),AL1(0)     4=ESTIMATE PAST EXPIRY            
         DC    AL1(TAEPMUS-TAEPEL),AL1(0)     5=SEPARATE MUSIC                  
         DC    AL1(TAEPEST-TAEPEL),AL1(0)     6=FILTER ACTUALS                  
         DC    AL1(TAEPFREE-TAEPEL),AL1(0)    7=FILTER MUS PAY FREE             
         DC    AL1(0),AL1(0)                  8=SPARE                           
         DC    AL1(TAEPFMT-TAEPEL),AL1(0)     9=FORMAT OPTION                   
         DC    AL1(TAEPAUTO-TAEPEL),AL1(0)    10=GENERATE AUTO PAY              
         DC    AL1(TAEPACTL-TAEPEL),AL1(0)    11=INCLUDE ACTUALS                
         DC    AL1(TAEPHORZ-TAEPEL),AL1(0)    12=HORIZ TOTALS                   
         DC    AL1(TAEPSACC-TAEPEL),AL1(0)    13=ACCUM SUMMARY OPT.             
         DC    AL1(TAEPCAST-TAEPEL),AL1(0)    14=INCLUDE CAST LISTS             
         DC    AL1(TAEPPBSS-TAEPEL),AL1(0)    15=PRINT SESSIONS                 
         DC    AL1(TAEPCOML-TAEPEL),AL1(0)    16=PRINT COMM INCL SUMM           
         DC    AL1(0),AL1(0)                  17=SPARE                          
         DC    AL1(TAEPLEFT-TAEPEL),AL1(0)    18=LEFT ALIGN                     
         DC    AL1(0),AL1(0)                  19=SPARE                          
         DC    AL1(TAEPRATE-TAEPEL),AL1(0)    20=COMMISSION RATE                
         DC    AL1(TAEPRATE-TAEPEL),AL1(1)    21=AGENCY COMMISION               
         SPACE 1                                                                
*              DATA FROM BILL DETAILS ELEMENT FOR EURO                          
         SPACE 1                                                                
INEUBTAB DS    0H                             ARGUMENTS                         
         DC    AL1(TABDTYPE-TABDEL),AL1(0)    1=BILLING TYPE                    
         DC    AL1(TABDCCVT-TABDEL),AL1(7)    2=CANADIAN CONVERSION RTE         
         DC    AL1(TABDTOT-TABDEL),AL1(3)     3=BILLING TOTAL                   
         DC    AL1(TABDTAX-TABDEL),AL1(3)     4=PAYROLL TAXES                   
         DC    AL1(TABDHND-TABDEL),AL1(3)     5=HANDLING                        
         DC    AL1(TABDHNDC-TABDEL),AL1(3)    6=HANDLING CORP                   
         DC    AL1(TABDCSF-TABDEL),AL1(3)     7=CONTRACT SERVICE FEE            
         DC    AL1(TABDFICR-TABDEL),AL1(3)    8=FICA CREDIT                     
         DC    AL1(TABDHND-TABDEL),AL1(1)     9=ALL HANDLING                    
         DC    AL1(TABDHND-TABDEL),AL1(2)     10=ALL HANDLING+TAXES             
         DC    AL1(TABDHND-TABDEL),AL1(4)     11=HANDLING+TAXES                 
         DC    AL1(TABDGST-TABDEL),AL1(3)     12=CANADIAN GST                   
         DC    AL1(TABDACOM-TABDEL),AL1(5)    13=AGENCY COMMISSION              
         DC    AL1(TABDSIGN-TABDEL),AL1(5)    14=SIGNATORY FEE                  
         DC    AL1(TABDPST-TABDEL),AL1(3)     15=CANADIAN PST                   
         DC    AL1(TABDPST-TABDEL),AL1(6)     16=CANADIAN HST                   
         SPACE 1                                                                
*              DATA FROM PAYMENT DETAIL ELEMENT                                 
         SPACE 1                                                                
INEUPTAB DS    0H                             ARGUMENTS                         
         DC    AL1(TAPDCOM-TAPDEL),AL1(0)     1=INTERNAL COMMERCIAL #           
         DC    AL1(TAPDINV-TAPDEL),AL1(1)     2=INVOICE NUMBER                  
         DC    AL1(TAPDUSE-TAPDEL),AL1(0)     3=USE CODE                        
         DC    AL1(TAPDTYPE-TAPDEL),AL1(0)    4=USE TYPE                        
         DC    AL1(TAPDW4TY-TAPDEL),AL1(0)    5=W4 TYPE                         
         DC    AL1(TAPDCYCS-TAPDEL),AL1(0)    6=CYCLE START                     
         DC    AL1(TAPDCYCE-TAPDEL),AL1(0)    7=CYCLE END                       
         DC    AL1(TAPDACDE-TAPDEL),AL1(0)    8=APPLY CODE                      
         DC    AL1(TAPDICDE-TAPDEL),AL1(0)    9=INCLUDE CODE                    
         DC    AL1(TAPDESPD-TAPDEL),AL1(0)    10=ESTIMATE PERIOD                
         DC    AL1(TAPDGRS-TAPDEL),AL1(17)    11=GROSS                          
         DC    AL1(TAPDAPPL-TAPDEL),AL1(11)   12=APPLIED CREDITS                
         DC    AL1(TAPDGUAR-TAPDEL),AL1(11)   13=GUARANTEE CREDITS              
         DC    AL1(TAPDPAYI-TAPDEL),AL1(18)   14=PAYMENT INDIV                  
         DC    AL1(TAPDPAYC-TAPDEL),AL1(11)   15=PAYMENT CORP                   
         DC    AL1(TAPDREXP-TAPDEL),AL1(19)   16=REIMBURSED EXPENSES            
         DC    AL1(TAPDSPNH-TAPDEL),AL1(11)   17=SUBJECT TO P&H                 
         DC    AL1(TAPDMDED-TAPDEL),AL1(11)   18=MISC. DEDUCTION                
         DC    AL1(TAPDPNH-TAPDEL),AL1(11)    19=P&H AMOUNT                     
         DC    AL1(TAPDHNW-TAPDEL),AL1(11)    20=H&W AMOUNT                     
         DC    AL1(TAPDINR-TAPDEL),AL1(11)    21=I&R AMOUNT                     
         DC    AL1(TAPDPNH-TAPDEL),AL1(7)     22=APPLIED P&H                    
         DC    AL1(TAPDSTUS-TAPDEL),AL1(2)    23=USE DETAILS                    
         DC    AL1(TAPDSTUS-TAPDEL),AL1(0)    24=START USE NUMBER               
         DC    AL1(TAPDUSES-TAPDEL),AL1(14)   25=NUMBER OF USES                 
         DC    AL1(TAPDUNIT-TAPDEL),AL1(16)   26=N'UNITS                        
         DC    AL1(TAPDMAJ-TAPDEL),AL1(0)     27=MAJOR CITIES                   
         DC    AL1(TAPDSTAT-TAPDEL),AL1(3)    28=STATUS                         
         DC    AL1(TAPDSTAT-TAPDEL),AL1(10)   29=CURRENCY                       
         DC    AL1(TAPDPSTS-TAPDEL),AL1(4)    30=PAYMENT STATUS                 
         DC    AL1(0),AL1(0)                  31=SPARE                          
         DC    AL1(TAPDOPTS-TAPDEL),AL1(5)    32=PAYMENT OPTIONS                
         DC    AL1(0),AL1(0)                  33=SPARE                          
         DC    AL1(0),AL1(0)                  34=SPARE                          
         DC    AL1(TAPDEOR-TAPDEL),AL1(0)     35=EOR                            
         DC    AL1(TAPDCLI-TAPDEL),AL1(0)     36=CLIENT                         
         DC    AL1(TAPDPRD-TAPDEL),AL1(0)     37=PRODUCT                        
         DC    AL1(TAPDOV1-TAPDEL),AL1(0)     38=OVERSCALE RATE 1               
         DC    AL1(0),AL1(0)                  39=SPARE                          
         DC    AL1(TAPDUSE-TAPDEL),AL1(6)     40=USE NAME                       
         DC    AL1(TAPDPAYI-TAPDEL),AL1(8)    41=PAYI + PAYC                    
         DC    AL1(TAPDPAYI-TAPDEL),AL1(9)    42=PAYI + PAYC + REXP             
         DC    AL1(TAPDPAYI-TAPDEL),AL1(12)   43=PAYI + REXP                    
         DC    AL1(TAPDPAYI-TAPDEL),AL1(13)   44=PAYC + REXP                    
         DC    AL1(TAPDDUES-TAPDEL),AL1(11)   45=UNION DUES                     
         DC    AL1(TAPDTAGS-TAPDEL),AL1(15)   46=TAGS FROM TAG USE              
         DC    AL1(TAPDPAYI-TAPDEL),AL1(11)   47=PAYI                           
         DC    AL1(TAPDNTNW-TAPDEL),AL1(19)   48=REXP                           
         SPACE 1                                                                
*              DATA FROM FREE FORM NAME ELEMENT                                 
         SPACE 1                                                                
INFNTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAFNNAME-TAFNEL),AL1(1)    1=NAME                            
         SPACE 1                                                                
*              DATA FROM GUARANTEE CYCLES                                       
         SPACE 1                                                                
INGCTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAGCSTRT-TAGCEL),AL1(0)    1=CYCLE START                     
         DC    AL1(TAGCEND-TAGCEL),AL1(0)     2=CYCLE END                       
         DC    AL1(TAGCAMT-TAGCEL),AL1(1)     3=TOTAL GUARANTEE AMOUNT          
         DC    AL1(TAGCAMT-TAGCEL),AL1(2)     4=AMOUNT APPLIED                  
         DC    AL1(TAGCBAL-TAGCEL),AL1(1)     5=BALANCE                         
         SPACE 1                                                                
*              DATA FROM GUARANTEE HANDLING                                     
         SPACE 1                                                                
INGHTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAGHHDL-TAGHEL),AL1(0)     1=GUARANTEE HANDLING RATE         
         DC    AL1(TAGHLIM-TAGHEL),AL1(0)     2=GUARANTEE LIMIT                 
*                                                                               
*              DATA FROM GUARANTEE TRACKING                                     
         SPACE 1                                                                
INGTTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAGTCID-TAGTEL),AL1(0)     1=CID                             
         DC    AL1(TAGTCAT-TAGTEL),AL1(0)     2=CATEGORY                        
         DC    AL1(TAGTCAM-TAGTEL),AL1(0)     3=CAMERA                          
         DC    AL1(TAGTUNI-TAGTEL),AL1(0)     4=UNION                           
         DC    AL1(TAGTYEAR-TAGTEL),AL1(0)    5=UNION YEAR                      
         DC    AL1(TAGTOV-TAGTEL),AL1(0)      6=OVERSCALE RATE                  
         DC    AL1(TAGTOV2-TAGTEL),AL1(0)     7=OVERSCALE RATE 2                
         DC    AL1(TAGTPG-TAGTEL),AL1(0)      8=PAGE NUMBER                     
         DC    AL1(TAGTINV-TAGTEL),AL1(4)     9=INVOICE NUMBER                  
         DC    AL1(TAGTUSE-TAGTEL),AL1(0)     10=USE                            
         DC    AL1(TAGTTYPE-TAGTEL),AL1(0)    11=USE TYPE                       
         DC    AL1(TAGTSTRT-TAGTEL),AL1(0)    12=CYCLE START                    
         DC    AL1(TAGTEND-TAGTEL),AL1(0)     13=CYCLE END                      
         DC    AL1(TAGTPAY-TAGTEL),AL1(3)     14=PAY AMOUNT                     
         DC    AL1(TAGTPNH-TAGTEL),AL1(3)     15=P&H                            
         DC    AL1(TAGTCRD-TAGTEL),AL1(3)     16=GUARANTEE CREDITS              
         DC    AL1(TAGTBAL-TAGTEL),AL1(3)     17=GUARANTEE BALANCE              
         DC    AL1(TAGTSTUS-TAGTEL),AL1(1)    18=USE DETAILS                    
         DC    AL1(TAGTUSE-TAGTEL),AL1(2)     19=USE NAME                       
         DC    AL1(TAGTAGY-TAGTEL),AL1(0)     20=AGENCY                         
         SPACE 1                                                                
*              DATA FROM GUARANTEE DETAILS                                      
         SPACE 1                                                                
INGUTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAGUAGY-TAGUEL),AL1(0)     1=AGENCY                          
         DC    AL1(TAGUCLI-TAGUEL),AL1(0)     2=CLIENT                          
         DC    AL1(TAGUSTRT-TAGUEL),AL1(0)    3=START                           
         DC    AL1(TAGUEND-TAGUEL),AL1(0)     4=END                             
         DC    AL1(TAGUCRP-TAGUEL),AL1(0)     5=CORPORATION CODE                
         DC    AL1(TAGUSTAT-TAGUEL),AL1(1)    6=STATUS                          
         DC    AL1(TAGUAMT-TAGUEL),AL1(3)     7=TOTAL GUAR AMOUNT               
         DC    AL1(TAGUBAL-TAGUEL),AL1(3)     8=BALANCE                         
         DC    AL1(TAGUAMT-TAGUEL),AL1(2)     9=CREDITS                         
         DC    AL1(TAGULPG-TAGUEL),AL1(0)     10=LAST PAGE NUMBER               
         DC    AL1(TAGUPAY-TAGUEL),AL1(0)     11=PAYMENT INSTALMENT             
         DC    AL1(TAGUINV-TAGUEL),AL1(4)     12=INVOICE THAT ADDED GRT         
         SPACE 1                                                                
*              DATA FROM INTERFACE ELEMENT                                      
         SPACE 1                                                                
INIFTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAIFAGY-TAIFEL),AL1(0)     1=AGENCY USER ID                  
         DC    AL1(TAIFVEND-TAIFEL),AL1(0)    2=VENDOR ACCOUNT                  
         DC    AL1(TAIFTV-TAIFEL),AL1(0)      3=TV DEFAULT JOB #                
         DC    AL1(TAIFRAD-TAIFEL),AL1(0)     4=RADIO DEFAULT JOB #             
         DC    AL1(TAIFWCS-TAIFEL),AL1(1)     5=WORK CODES                      
         SPACE 1                                                                
*              DATA FROM INVOICE STATUS ELEMENT                                 
         SPACE 1                                                                
ININTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAINIST-TAINEL),AL1(0)     1=ASSIGNED BY                     
         DC    AL1(TAINIDTE-TAINEL),AL1(0)    2=DATE ASSIGNED                   
         DC    AL1(TAINPST-TAINEL),AL1(0)     3=PAID BY                         
         DC    AL1(TAINPDTE-TAINEL),AL1(0)    4=DATE PAID                       
         DC    AL1(TAINQST-TAINEL),AL1(0)     5=APPROVED BY                     
         DC    AL1(TAINQDTE-TAINEL),AL1(0)    6=DATE APPROVED                   
         DC    AL1(TAINBDTE-TAINEL),AL1(0)    7=BILL DATE                       
         DC    AL1(TAINCDTE-TAINEL),AL1(0)    8=CHECK DATE                      
         DC    AL1(TAINSTAT-TAINEL),AL1(1)    9=STATUS                          
         DC    AL1(TAINITIM-TAINEL),AL1(2)    10=ASSIGNED TIME                  
         DC    AL1(TAINTERR-TAINEL),AL1(0)    11=ERROR                          
         DC    AL1(TAINHDTE-TAINEL),AL1(0)    12=DATE COD INV PRINTED           
         DC    AL1(TAINPTIM-TAINEL),AL1(3)    13=PAID TIME                      
         DC    AL1(TAINQTIM-TAINEL),AL1(4)    14=APPROVED TIME                  
         DC    AL1(TAINSTA3-TAINEL),AL1(5)    15=RETRO SKIP STATUS              
         SPACE 1                                                                
*              DATA FROM INTERFACE SUBSIDIARY ELEMENT                           
         SPACE 1                                                                
INISTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAISCDE-TAISEL),AL1(0)     1=CODE                            
         SPACE 1                                                                
*              DATA FROM STOP CHECK ELEMENT                                     
         SPACE 1                                                                
INKPTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAKPRECD-TAKPEL),AL1(0)    1=REISSUE DATE                    
         DC    AL1(TAKPSRDT-TAKPEL),AL1(0)    2=REQUEST DATE                    
         DC    AL1(TAKPSPDT-TAKPEL),AL1(0)    3=PLACED  DATE                    
         SPACE 1                                                                
*              DATA FROM LIFT ELEMENT                                           
         SPACE 1                                                                
INLFTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TALFLID-TALFEL),AL1(0)     1=LIFT ID                         
         DC    AL1(TALFSEC-TALFEL),AL1(0)     2=LIFT SECONDS                    
         SPACE 1                                                                
*              DATA FROM LIEN ELEMENT                                           
         SPACE 1                                                                
INLNTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TALNTYPE-TALNEL),AL1(0)    1=TYPE                            
         DC    AL1(TALNRNK-TALNEL),AL1(0)     2=RANK                            
         DC    AL1(TALNDUE-TALNEL),AL1(3)     3=AMOUNT DUE                      
         DC    AL1(TALNCOL-TALNEL),AL1(3)     4=AMOUNT COLLECTED                
         DC    AL1(TALNXMPT-TALNEL),AL1(0)    5=EXEMPT AMOUNT                   
         DC    AL1(TALNAMT-TALNEL),AL1(0)     6=DEDUCTION AMOUNT                
         DC    AL1(TALNPCT-TALNEL),AL1(0)     7=DEDUCTION %                     
         DC    AL1(TALNEXP-TALNEL),AL1(0)     8=EXPIRY DATE                     
         DC    AL1(TALNUNIT-TALNEL),AL1(0)    9=TAX UNIT                        
         DC    AL1(TALNPAYE-TALNEL),AL1(0)    10=PAYEE                          
         DC    AL1(TALNDUE-TALNEL),AL1(1)     11=BALANCE                        
         DC    AL1(TALNSTAT-TALNEL),AL1(4)    12=STATUS                         
                                                                                
*              DATA FROM BILL DETAILS ELEMENT                                   
         SPACE 1                                                                
INMATAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAMAASBS-TAMAEL),AL1(1)    1=AOS BASE WAGE                   
         DC    AL1(TAMAASAM-TAMAEL),AL1(1)    2=AOS AMOUNT                      
         DC    AL1(TAMAASPR-TAMAEL),AL1(1)    3=AOS PST RATE                    
         DC    AL1(TAMAASPA-TAMAEL),AL1(1)    4=AOS PST AMOUNT                  
         SPACE 1                                                                
*              DATA FROM MUSIC ELEMENT                                          
         SPACE 1                                                                
INMUTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAMUNAME-TAMUEL),AL1(1)    1=NAME                            
         DC    AL1(TAMULIC-TAMUEL),AL1(2)     2=LICENSER                        
         SPACE 1                                                                
*              DATA FROM AUTHOR/COMPOSER/PUBLISHER ELEMENT                      
         SPACE 1                                                                
INMUTTAB DS    0H                             ARGUMENTS                         
         DC    AL1(TAMUTYPE-TAMUEL),AL1(1)    1=TYPE                            
         DC    AL1(TAMUTNUM-TAMUEL),AL1(2)    2=NUMBER                          
         SPACE 1                                                                
*              DATA FROM MUSIC HISTORY ELEMENT                                  
         SPACE 1                                                                
INMUHTAB DS    0H                             ARGUMENTS                         
         DC    AL1(0),AL1(1)                  1=AGY/CODE                        
         SPACE 1                                                                
*              DATA FROM NETWORK CLA ELEMENT                                    
         SPACE 1                                                                
INNPTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TANPPNME-TANPEL),AL1(1)    1=1-10 NAME/DATE/LFT/NWK          
         DC    AL1(TANPPNME-TANPEL),AL1(2)    2=11-20 "/"/"/"                   
         DC    AL1(0),AL1(3)                  3=COUNT TANPD ELEMENTS            
         DC    AL1(TANPNWK-TANPEL),AL1(0)     4=NETWORK CODE                    
         SPACE 1                                                                
*              DATA FROM NUMBER ELEMENT                                         
         SPACE 1                                                                
INNUTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TANUMBER-TANUEL),AL1(1)    1=NUMBER                          
         SPACE 1                                                                
*              DATA FROM HOLD OR ADVICE RECORD                                  
         SPACE 1                                                                
INNXTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TANXAGY-TANXEL),AL1(0)     1=TALENT AGENCY                   
         DC    AL1(TANXUID-TANXEL),AL1(0)     2=WORKER FILE ID                  
         DC    AL1(TANXNCID-TANXEL),AL1(0)    3=NETWORK COMMERCIAL              
         DC    AL1(TANXSEC-TANXEL),AL1(0)     4=LENGTH IN SECONDS               
         DC    AL1(TANXMDTE-TANXEL),AL1(0)    5=DATE MATCHED                    
         DC    AL1(TANXUDTE-TANXEL),AL1(0)    6=DATE USED                       
         DC    AL1(TANXADTE-TANXEL),AL1(0)    7=DATE ADDED                      
         DC    AL1(TANXCOM-TANXEL),AL1(0)     8=INTERNAL COMML #                
         DC    AL1(TANXTYPE-TANXEL),AL1(0)    9=COMMERCIAL TYPE                 
         DC    AL1(TANXCCDE-TANXEL),AL1(0)    10=CHANGED CODE                   
         DC    AL1(TANXSTAT-TANXEL),AL1(0)    11=STATUS                         
         SPACE 1                                                                
*              DATA FROM OVERSCALE AMOUNTS ELEMENT                              
         SPACE 1                                                                
INOATAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAOAAMT-TAOAEL),AL1(1)     1=USE AND AMOUNT                  
         SPACE 1                                                                
*              DATA FROM OLD COMMERCIAL ELEMENT                                 
         SPACE 1                                                                
INOCTAB  DC    AL1(TAOCAGY-TAOCEL),AL1(0)     1=OLD AGENCY                      
         DC    AL1(TAOCCID-TAOCEL),AL1(0)     2=OLD COMMERCIAL                  
         DC    AL1(TAOCDTE-TAOCEL),AL1(1)     3=COPY DATE                       
         SPACE 1                                                                
*              DATA FROM OVERSCALE % ELEMENT                                    
         SPACE 1                                                                
INOPTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAOPPCT-TAOPEL),AL1(0)     1=OVERSCALE PERCENT 1             
         SPACE 1                                                                
*              DATA FROM OVERSCALE 2ND % ELEMENT                                
         SPACE 1                                                                
INO2TAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAO2PCT-TAO2EL),AL1(0)     1=OVERSCALE 2ND % 1               
         SPACE 1                                                                
*              DATA FROM OTHER WITHHOLDING ELEMENT                              
         SPACE 1                                                                
INOWTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAOWFLAT-TAOWEL),AL1(0)    1=FLAT RATE (2 DECS)              
         SPACE 1                                                                
*              DATA FROM PAYMENT DETAIL ELEMENT                                 
         SPACE 1                                                                
INPDTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAPDCOM-TAPDEL),AL1(0)     1=INTERNAL COMMERCIAL #           
         DC    AL1(TAPDINV-TAPDEL),AL1(1)     2=INVOICE NUMBER                  
         DC    AL1(TAPDUSE-TAPDEL),AL1(0)     3=USE CODE                        
         DC    AL1(TAPDTYPE-TAPDEL),AL1(0)    4=USE TYPE                        
         DC    AL1(TAPDW4TY-TAPDEL),AL1(0)    5=W4 TYPE                         
         DC    AL1(TAPDCYCS-TAPDEL),AL1(0)    6=CYCLE START                     
         DC    AL1(TAPDCYCE-TAPDEL),AL1(0)    7=CYCLE END                       
         DC    AL1(TAPDACDE-TAPDEL),AL1(0)    8=APPLY CODE                      
         DC    AL1(TAPDICDE-TAPDEL),AL1(0)    9=INCLUDE CODE                    
         DC    AL1(TAPDESPD-TAPDEL),AL1(0)    10=ESTIMATE PERIOD                
         DC    AL1(TAPDGRS-TAPDEL),AL1(17)    11=GROSS                          
         DC    AL1(TAPDAPPL-TAPDEL),AL1(11)   12=APPLIED CREDITS                
         DC    AL1(TAPDGUAR-TAPDEL),AL1(11)   13=GUARANTEE CREDITS              
         DC    AL1(TAPDPAYI-TAPDEL),AL1(18)   14=PAYMENT INDIV                  
         DC    AL1(TAPDPAYC-TAPDEL),AL1(21)   15=PAYMENT CORP                   
         DC    AL1(TAPDREXP-TAPDEL),AL1(19)   16=REIMBURSED EXPENSES            
         DC    AL1(TAPDSPNH-TAPDEL),AL1(11)   17=SUBJECT TO P&H                 
         DC    AL1(TAPDMDED-TAPDEL),AL1(11)   18=MISC. DEDUCTION                
         DC    AL1(TAPDPNH-TAPDEL),AL1(11)    19=P&H AMOUNT                     
         DC    AL1(TAPDHNW-TAPDEL),AL1(11)    20=H&W AMOUNT                     
         DC    AL1(TAPDINR-TAPDEL),AL1(11)    21=I&R AMOUNT                     
         DC    AL1(TAPDPNH-TAPDEL),AL1(7)     22=APPLIED P&H                    
         DC    AL1(TAPDSTUS-TAPDEL),AL1(2)    23=USE DETAILS                    
         DC    AL1(TAPDSTUS-TAPDEL),AL1(0)    24=START USE NUMBER               
         DC    AL1(TAPDUSES-TAPDEL),AL1(14)   25=NUMBER OF USES                 
         DC    AL1(TAPDUNIT-TAPDEL),AL1(16)   26=N'UNITS                        
         DC    AL1(TAPDMAJ-TAPDEL),AL1(0)     27=MAJOR CITIES                   
         DC    AL1(TAPDSTAT-TAPDEL),AL1(3)    28=STATUS                         
         DC    AL1(TAPDSTAT-TAPDEL),AL1(10)   29=CURRENCY                       
         DC    AL1(TAPDPSTS-TAPDEL),AL1(4)    30=PAYMENT STATUS                 
         DC    AL1(TAPDREXP-TAPDEL),AL1(24)   31=REIMB EXPENSES ONLY            
         DC    AL1(TAPDOPTS-TAPDEL),AL1(5)    32=PAYMENT OPTIONS                
         DC    AL1(TAPDTXNW-TAPDEL),AL1(22)   33=TAXABLE NON-WAGES              
         DC    AL1(TAPDNTNW-TAPDEL),AL1(23)   34=NON-TAXABLE NON-WAGES          
         DC    AL1(TAPDEOR-TAPDEL),AL1(0)     35=EOR                            
         DC    AL1(TAPDCLI-TAPDEL),AL1(0)     36=CLIENT                         
         DC    AL1(TAPDPRD-TAPDEL),AL1(0)     37=PRODUCT                        
         DC    AL1(TAPDOV1-TAPDEL),AL1(0)     38=OVERSCALE RATE 1               
         DC    AL1(TAPDPAYI-TAPDEL),AL1(20)   39=PAYI (TATU)                    
         DC    AL1(TAPDUSE-TAPDEL),AL1(6)     40=USE NAME                       
         DC    AL1(TAPDPAYI-TAPDEL),AL1(8)    41=PAYI + PAYC                    
         DC    AL1(TAPDPAYI-TAPDEL),AL1(9)    42=PAYI + PAYC + REXP             
         DC    AL1(TAPDPAYI-TAPDEL),AL1(12)   43=PAYI + REXP                    
         DC    AL1(TAPDPAYI-TAPDEL),AL1(13)   44=PAYC + REXP                    
         DC    AL1(TAPDDUES-TAPDEL),AL1(11)   45=UNION DUES                     
         DC    AL1(TAPDTAGS-TAPDEL),AL1(15)   46=TAGS FROM TAG USE              
         DC    AL1(TAPDPAYI-TAPDEL),AL1(11)   47=PAYI                           
         DC    AL1(TAPDNTNW-TAPDEL),AL1(19)   48=REXP                           
         DC    AL1(TAPDSTA3-TAPDEL),AL1(25)   49=PDVAR                          
         DC    AL1(TAPDGCON-TAPDEL),AL1(11)   50=PDGCON                         
         SPACE 1                                                                
*              DATA FROM PAYEE ELEMENT                                          
         SPACE 1                                                                
INPETAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAPELEN-TAPEEL),AL1(0)     1=LENGTH (FOR ACTIVITY)           
         DC    AL1(TAPENAME-TAPEEL),AL1(0)    2=NAME                            
         DC    AL1(TAPEADD1-TAPEEL),AL1(0)    3=ADDRESS                         
         DC    AL1(TAPEEXP-TAPEEL),AL1(0)     4=EXPIRY                          
         DC    AL1(TAPEACT-TAPEEL),AL1(0)     5=ACTIVE                          
         DC    AL1(TAPECITY-TAPEEL),AL1(1)    6=CITY                            
         DC    AL1(TAPEST-TAPEEL),AL1(2)      7=STATE                           
         DC    AL1(TAPEZIP-TAPEEL),AL1(3)     8=ZIP                             
         DC    AL1(TAPECTRY-TAPEEL),AL1(4)    9=COUNTRY                         
         SPACE 1                                                                
*              DATA FROM PRODUCT INFORMATION ELEMENT                            
         SPACE 1                                                                
INPITAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAPIAC-TAPIEL),AL1(0)      1=COMMISSION PCT                  
         DC    AL1(TAPISTAT-TAPIEL),AL1(1)    2=STATUS                          
         DC    AL1(TAPIPTYP-TAPIEL),AL1(2)    3=TYPE                            
         SPACE 1                                                                
*              DATA FROM POOLED EARNINGS ELEMENT                                
         SPACE 1                                                                
INPOTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAPOPYTD-TAPOEL),AL1(1)    1=YTD EARNINGS                    
         DC    AL1(TAPOPYTS-TAPOEL),AL1(1)    2=YTD SUI                         
         SPACE 1                                                                
*              DATA FROM RETURNED CHECK ELEMENT                                 
         SPACE 1                                                                
INRNTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TARNRDTE-TARNEL),AL1(0)    1=RETURNED DATE                   
         DC    AL1(TARNUID-TARNEL),AL1(1)     2=USER ID                         
         DC    AL1(TARNSTAF-TARNEL),AL1(0)    3=STAFF ID OF BILLER              
         DC    AL1(TARNDDTE-TARNEL),AL1(0)    4=DISPOSITION DATE                
         DC    AL1(TARNSTAT-TARNEL),AL1(2)    5=DISPOSITION INDICATOR           
         SPACE 1                                                                
*              DATA FROM RETRO PNH AMOUNT ELEMENT                               
         SPACE 1                                                                
INRPTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TARPBASE-TARPEL),AL1(1)    1=USER ID                         
         DC    AL1(TARPDIFF-TARPEL),AL1(2)    2=DISPOSITION INDICATOR           
         SPACE 1                                                                
*              DATA FROM RL1 ELEMENTS                                           
         SPACE 1                                                                
INR1TAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAR1EARN-TAR1EL),AL1(1)    1=EMPLOYMENT INCOME               
         DC    AL1(TAR1QPPE-TAR1EL),AL1(1)    2=QPP PENS EARNINGS               
         DC    AL1(TAR1QEIE-TAR1EL),AL1(1)    3=QEI EARNINGS                    
         DC    AL1(TAR1PIPW-TAR1EL),AL1(1)    4=QPIP WAGES                      
         DC    AL1(TAR1TAX-TAR1EL),AL1(1)     5=QC TAX DEDUCTED                 
         DC    AL1(TAR1QPPC-TAR1EL),AL1(1)    6=QPP CONTRIBUTION                
         DC    AL1(TAR1QEIP-TAR1EL),AL1(1)    7=QEI PREMIUM                     
         DC    AL1(TAR1PIPP-TAR1EL),AL1(1)    8=QPIP PREMIUMS                   
         SPACE 1                                                                
*              DATA FROM SESSION DETAIL ELEMENT - TV SESSIONS                   
         SPACE 1                                                                
INSDTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TASDFEE-TASDEL),AL1(0)     1=SESSION FEE                     
         DC    AL1(TASDHR-TASDEL),AL1(0)      2=HOURLY RATE                     
         DC    AL1(TASDSP-TASDEL),AL1(0)      3=SPOTS                           
         DC    AL1(TASDSPA-TASDEL),AL1(0)     4=SPOTS AMT                       
         DC    AL1(TASDDAY-TASDEL),AL1(7)     5=DAYS                            
         DC    AL1(TASDDAA-TASDEL),AL1(0)     6=DAYS AMT                        
         DC    AL1(TASDOT-TASDEL),AL1(8)      7=OVERTIME                        
         DC    AL1(TASDOTA-TASDEL),AL1(0)     8=OVERTIME AMT                    
         DC    AL1(TASDDT-TASDEL),AL1(9)      9=DOUBLE TIME                     
         DC    AL1(TASDDTA-TASDEL),AL1(0)     10=DOUBLE TIME AMT                
         DC    AL1(TASDTRV-TASDEL),AL1(0)     11=TRAVEL TIME                    
         DC    AL1(TASDTRA-TASDEL),AL1(0)     12=TRAVEL TIME AMT                
         DC    AL1(TASDPDW-TASDEL),AL1(0)     13=PRIOR DAY WR                   
         DC    AL1(TASDPDA-TASDEL),AL1(0)     14=PRIOR DAY AMT                  
         DC    AL1(TASDTAG-TASDEL),AL1(10)    15=TAGS                           
         DC    AL1(TASDTAA-TASDEL),AL1(0)     16=TAG AMT                        
         SPACE 1                                                                
*              DATA FROM SESSION DETAIL ELEMENT - MUSIC SESSIONS                
         SPACE 1                                                                
INSDMTAB DS    0H                             ARGUMENTS                         
         DC    AL1(TASDFEE-TASDEL),AL1(0)     1=SESSION FEE                     
         DC    AL1(TASDHR-TASDEL),AL1(0)      2=HOURLY RATE                     
         DC    AL1(TASDMSP-TASDEL),AL1(0)     3=SPOTS                           
         DC    AL1(TASDMSPA-TASDEL),AL1(0)    4=SPOTS AMT                       
         DC    AL1(TASDMHM-TASDEL),AL1(6)     5=HOURS/MINUTES                   
         DC    AL1(0),AL1(2)                  6=ZERO FOR 4                      
         DC    AL1(0),AL1(1)                  7=ZERO FOR 1                      
         DC    AL1(0),AL1(2)                  8=ZERO FOR 4                      
         DC    AL1(0),AL1(1)                  9=ZERO FOR 1                      
         DC    AL1(0),AL1(2)                  10=ZERO FOR 4                     
         DC    AL1(0),AL1(3)                  11=ZERO FOR 2                     
         DC    AL1(0),AL1(2)                  12=ZERO FOR 4                     
         DC    AL1(0),AL1(3)                  13=ZERO FOR 2                     
         DC    AL1(0),AL1(2)                  14=ZERO FOR 4                     
         DC    AL1(0),AL1(1)                  15=ZERO FOR 1                     
         DC    AL1(0),AL1(2)                  16=ZERO FOR 4                     
         SPACE 1                                                                
*              DATA FROM SESSION DETAIL ELEMENT - RADIO SESSIONS                
         SPACE 1                                                                
INSDRTAB DS    0H                             ARGUMENTS                         
         DC    AL1(TASDFEE-TASDEL),AL1(0)     1=SESSION FEE                     
         DC    AL1(TASDHR-TASDEL),AL1(0)      2=HOURLY RATE                     
         DC    AL1(TASDRSP-TASDEL),AL1(0)     3=SPOTS                           
         DC    AL1(TASDRSPA-TASDEL),AL1(0)    4=SPOTS AMT                       
         DC    AL1(TASDRHM-TASDEL),AL1(6)     5=HOURS/MINUTES                   
         DC    AL1(0),AL1(2)                  6=ZERO FOR 4                      
         DC    AL1(TASDRTG-TASDEL),AL1(10)    7=TAGS                            
         DC    AL1(0),AL1(2)                  8=ZERO FOR 4                      
         DC    AL1(0),AL1(1)                  9=ZERO FOR 1                      
         DC    AL1(0),AL1(2)                  10=ZERO FOR 4                     
         DC    AL1(0),AL1(3)                  11=ZERO FOR 2                     
         DC    AL1(0),AL1(2)                  12=ZERO FOR 4                     
         DC    AL1(0),AL1(3)                  13=ZERO FOR 2                     
         DC    AL1(0),AL1(2)                  14=ZERO FOR 4                     
         DC    AL1(0),AL1(1)                  15=ZERO FOR 1                     
         DC    AL1(0),AL1(2)                  16=ZERO FOR 4                     
         SPACE 1                                                                
*              DATA FROM SUBSIDIARY INVOICE ELEMENT                             
         SPACE 1                                                                
INSITAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TASIINV-TASIEL),AL1(1)     1=INVOICE NUMBERS                 
         DC    AL1(TASIPCT3-TASIEL),AL1(2)    2=PERCENTAGES                     
         DC    AL1(TASIEST-TASIEL),AL1(3)     3=ESTIMATE NUMBERS                
         SPACE 1                                                                
*              DATA FROM SOAP EPISODE ELEMENT                                   
         SPACE 1                                                                
INSOTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TASOSTAT-TASOEL),AL1(1)    1=STATUS                          
         DC    AL1(TASOEPI-TASOEL),AL1(2)     2=EPISODE                         
         DC    AL1(TASOPNH-TASOEL),AL1(3)     3=P&H                             
         DC    AL1(TASOPAY-TASOEL),AL1(4)     4=PAYMENT                         
         DC    AL1(TASOAPPL-TASOEL),AL1(5)    5=APPLIED AMOUNT                  
         SPACE 1                                                                
*              DATA FROM STAFF ELEMENT                                          
         SPACE 1                                                                
INSTTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TASTFST-TASTEL),AL1(0)     1=FIRST NAME                      
         DC    AL1(TASTLST-TASTEL),AL1(0)     2=LAST NAME                       
         DC    AL1(TASTPWD-TASTEL),AL1(0)     3=PASSWORD                        
         DC    AL1(TASTTYPE-TASTEL),AL1(0)    4=TYPE                            
         DC    AL1(TASTTEL-TASTEL),AL1(0)     5=TELEPHONE NUMBER                
         DC    AL1(0),AL1(0)                  6=SPARE                           
         DC    AL1(TASTMGR-TASTEL),AL1(0)     7=MANAGER                         
         SPACE 1                                                                
*              DATA FROM TAX UNIT ELEMENTS                                      
         SPACE 1                                                                
INTUTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TATUUNIT-TATUEL),AL1(0)    1=UNIT                            
         DC    AL1(TATUWAGE-TATUEL),AL1(0)    2=WAGES                           
         DC    AL1(TATUTNWA-TATUEL),AL1(0)    3=TAXABLE NON-WAGES               
         DC    AL1(TATUNNWA-TATUEL),AL1(0)    4=NON-TAXABLE NON-WAGES           
         DC    AL1(TATUWAGE-TATUEL),AL1(1)    5=WAGES + TAX NON-WAGES           
*              DATA FROM T4 ELEMENTS                                            
         SPACE 1                                                                
INT4TAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAT4EARN-TAT4EL),AL1(1)    1=EMPLOYMENT INCOME               
         DC    AL1(TAT4PPER-TAT4EL),AL1(1)    2=CPP/QPP PENS EARN               
         DC    AL1(TAT4EINE-TAT4EL),AL1(1)    3=EI INSUR EARNINGS               
         DC    AL1(TAT4QPIE-TAT4EL),AL1(1)    4=QPIP INS EARNINGS               
         DC    AL1(TAT4TAX-TAT4EL),AL1(1)     5=INCOME TAX DEDUCTED             
         DC    AL1(TAT4CPPC-TAT4EL),AL1(1)    6=CPP CONTRIBUTION                
         DC    AL1(TAT4EPRM-TAT4EL),AL1(1)    7=EI PREMIUM                      
         DC    AL1(TAT4QPPR-TAT4EL),AL1(1)    8=QPIP PREMIUMS                   
*              DATA FROM UNION/LOCAL LIST                                       
         SPACE 1                                                                
INULTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAUPDATA-TAUPEL),AL1(0)    1=UPGRADE DETAIL                  
         SPACE 1                                                                
*              DATA FROM UPGRADE                                                
         SPACE 1                                                                
INUPTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAULULS-TAULEL),AL1(1)     1=FIRST UNION                     
         SPACE 1                                                                
*              DATA FROM VERSION ELEMENT                                        
         SPACE 1                                                                
INVRTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAVRVERS-TAVREL),AL1(0)    1=VERSION CODE                    
         DC    AL1(TAVRCID-TAVREL),AL1(0)     2=VERSION ID                      
         DC    AL1(TAVRSEC-TAVREL),AL1(0)     3=VERSION SECONDS                 
         SPACE 1                                                                
*              DATA FROM ADVICE USE DETAILS                                     
         SPACE 1                                                                
INVUTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAVUMED-TAVUEL),AL1(0)     1=MEDIA                           
         DC    AL1(TAVUUSE-TAVUEL),AL1(1)     2=USE                             
         SPACE 1                                                                
*              DATA FROM WH ELEMENT                                             
         SPACE 1                                                                
INWHTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAWHEMP-TAWHEL),AL1(0)     1=EMPLOYER                        
         DC    AL1(TAWHUNIT-TAWHEL),AL1(0)    2=TAX UNIT                        
         DC    AL1(TAWHSTAT-TAWHEL),AL1(0)    3=STATUS                          
         DC    AL1(TAWHEXS-TAWHEL),AL1(0)     4=N'EXEMPTIONS                    
         DC    AL1(TAWHFLAT-TAWHEL),AL1(0)    5=FLAT TAX RATE                   
         SPACE 1                                                                
*              DATA FROM W4 ELEMENT                                             
         SPACE 1                                                                
INW4TAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAW4NAM1-TAW4EL),AL1(0)    1=FIRST NAME                      
         DC    AL1(TAW4NAM2-TAW4EL),AL1(0)    2=LAST NAME                       
         DC    AL1(TAW4SEX-TAW4EL),AL1(2)     3=SEX CODE                        
         DC    AL1(TAW4MIDN-TAW4EL),AL1(3)    4=MIDDLE NAME                     
         DC    AL1(TAW4RACE-TAW4EL),AL1(1)    5=RACE CODE                       
         DC    AL1(TAW4FREQ-TAW4EL),AL1(0)    6=FREQUENCY                       
         DC    AL1(TAW4STAT-TAW4EL),AL1(0)    7=STATUS                          
         DC    AL1(TAW4INDT-TAW4EL),AL1(0)    8=INDEM. DATE                     
         DC    AL1(TAW4TYPE-TAW4EL),AL1(0)    9=W4 TYPE                         
         DC    AL1(TAW4LOCL-TAW4EL),AL1(0)    10=AFM LOCAL                      
         DC    AL1(TAW4MIDN-TAW4EL),AL1(4)    11=FIRST MID LAST NAME            
         DC    AL1(TAW4STA3-TAW4EL),AL1(5)    12=TAKE TAX ON FGR                
         DC    AL1(TAW4STA3-TAW4EL),AL1(6)    13=NEW HIRE ACT                   
         DC    AL1(TAW4NHAD-TAW4EL),AL1(0)    14=NEW HIRE ACT DATE              
         DC    AL1(TAW4RECP-TAW4EL),AL1(7)    15=RECIPROCAL STATE               
         SPACE 1                                                                
*              DATA FROM W4 EXTRA DETAILS ELEMENT                               
         SPACE 1                                                                
INWXTAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAWXDOB-TAWXEL),AL1(0)     1=DATE OF BIRTH                   
         DC    AL1(TAWXPCT-TAWXEL),AL1(0)     2=PERCENTAGE WITHHELD             
         DC    AL1(TAWXTSSN-TAWXEL),AL1(1)    3=TRUSTEE SSN                     
         DC    AL1(TAWXHTP-TAWXEL),AL1(0)     4=TP AMT COLLECTED & HELD         
         DC    AL1(TAWXHPPL-TAWXEL),AL1(0)    5=P+ AMT COLLECTED & HELD         
         DC    AL1(TAWXHPP-TAWXEL),AL1(0)     6=PP AMT COLLECTED & HELD         
         DC    AL1(TAWXMERN-TAWXEL),AL1(0)    7=MINOR LIFETIME EARNINGS         
         SPACE 1                                                                
*              DATA FROM YTD EARNINGS ELEMENTS                                  
         SPACE 1                                                                
INYETAB  DS    0H                             ARGUMENTS                         
         DC    AL1(TAYEEARN-TAYEEL),AL1(1)    1=YTD EARNINGS                    
         DC    AL1(TAYEFUI-TAYEEL),AL1(1)     2=YTD FUI                         
         DC    AL1(TAYESUI-TAYEEL),AL1(1)     3=YTD SUI                         
         DC    AL1(TAYEFICA-TAYEEL),AL1(1)    4=YTD FICA                        
         DC    AL1(TAYEMED-TAYEEL),AL1(1)     5=YTD MEDICARE                    
         DC    AL1(TAYENTAX-TAYEEL),AL1(1)    6=YTD NON TAXABLE                 
         DC    AL1(TAYETERN-TAYEEL),AL1(1)    7=TAXABLE EARNINGS                
         DC    AL1(TAYETFUI-TAYEEL),AL1(1)    8=TAXABLE FUI                     
         DC    AL1(TAYETSUI-TAYEEL),AL1(1)    9=TAXABLE SUI                     
         DC    AL1(TAYETFIC-TAYEEL),AL1(1)    10=TAXABLE FICA                   
         DC    AL1(TAYETMED-TAYEEL),AL1(1)    11=TAXABLE MEDICARE               
         DC    AL1(TAYETOMD-TAYEEL),AL1(1)    12=TAXABLE OVER MEDICARE          
         SPACE 1                                                                
         EJECT                                                                  
*              ADDITIONAL INPUT ROUTINE TABLE                                   
         SPACE 3                                                                
ADDROUT  DS    0D                                                               
         DC    AL1(TAANELQ),AL1(1),AL1(0),AL1(0),AL4(INANSTAT)                  
         DC    AL1(TAANELQ),AL1(2),AL1(0),AL1(0),AL4(INANSSN)                   
         DC    AL1(TAATELQ),AL1(1),AL1(0),AL1(0),AL4(INATUNIT)                  
         DC    AL1(TAATELQ),AL1(2),AL1(0),AL1(0),AL4(INATPUNT)                  
         DC    AL1(TAATELQ),AL1(3),AL1(0),AL1(0),AL4(INATCAN)                   
         DC    AL1(TAATELQ),AL1(4),AL1(0),AL1(0),AL4(INATPRV)                   
         DC    AL1(TAAYELQ),AL1(1),AL1(0),AL1(0),AL4(INAYSTAT)                  
         DC    AL1(TAAYELQ),AL1(2),AL1(0),AL1(0),AL4(INAYHLD)                   
         DC    AL1(TAAYELQ),AL1(3),AL1(0),AL1(0),AL4(INALLINV)                  
         DC    AL1(TAAYELQ),AL1(4),AL1(0),AL1(0),AL4(INAYAGG)                   
         DC    AL1(TAAYELQ),AL1(5),AL1(0),AL1(0),AL4(INAYDAYS)                  
         DC    AL1(TAAYELQ),AL1(6),AL1(0),AL1(0),AL4(INAYLBOX)                  
         DC    AL1(TAAYELQ),AL1(7),AL1(0),AL1(0),AL4(INAYSTA6)                  
         DC    AL1(TAAYELQ),AL1(8),AL1(0),AL1(0),AL4(INAYSTA7)                  
         DC    AL1(TAAYELQ),AL1(9),AL1(0),AL1(0),AL4(INAYSTA3)                  
         DC    AL1(TABDELQ),AL1(1),AL1(0),AL1(0),AL4(INBDHAND)                  
         DC    AL1(TABDELQ),AL1(2),AL1(0),AL1(0),AL4(INBDHNTX)                  
         DC    AL1(TABDELQ),AL1(3),AL1(0),AL1(0),AL4(CVD1)                      
         DC    AL1(TABDELQ),AL1(4),AL1(0),AL1(0),AL4(INBDHITX)                  
         DC    AL1(TABDELQ),AL1(5),AL1(0),AL1(0),AL4(INBDACSG)                  
         DC    AL1(TABDELQ),AL1(6),AL1(0),AL1(0),AL4(INBDHST)                   
         DC    AL1(TABDELQ),AL1(7),AL1(0),AL1(0),AL4(INBDCCVT)                  
         DC    AL1(TABDELQ2),AL1(1),AL1(0),AL1(0),AL4(INBDHAND)                 
         DC    AL1(TABDELQ2),AL1(2),AL1(0),AL1(0),AL4(INBDHNTX)                 
         DC    AL1(TABDELQ2),AL1(3),AL1(0),AL1(0),AL4(CVD1)                     
         DC    AL1(TABDELQ2),AL1(4),AL1(0),AL1(0),AL4(INBDHITX)                 
         DC    AL1(TABDELQ2),AL1(5),AL1(0),AL1(0),AL4(INBDACSG)                 
         DC    AL1(TABDELQ2),AL1(6),AL1(0),AL1(0),AL4(INBDHST)                  
         DC    AL1(TABDELQ2),AL1(7),AL1(0),AL1(0),AL4(INBDCCVT)                 
         DC    AL1(TABDELQ3),AL1(1),AL1(0),AL1(0),AL4(INBDHAND)                 
         DC    AL1(TABDELQ3),AL1(2),AL1(0),AL1(0),AL4(INBDHNTX)                 
         DC    AL1(TABDELQ3),AL1(3),AL1(0),AL1(0),AL4(CVD1)                     
         DC    AL1(TABDELQ3),AL1(4),AL1(0),AL1(0),AL4(INBDHITX)                 
         DC    AL1(TABDELQ3),AL1(5),AL1(0),AL1(0),AL4(INBDACSG)                 
         DC    AL1(TABDELQ3),AL1(6),AL1(0),AL1(0),AL4(INBDHST)                  
         DC    AL1(TABDELQ3),AL1(7),AL1(0),AL1(0),AL4(INBDCCVT)                 
         DC    AL1(TABRELQ),AL1(1),AL1(0),AL1(0),AL4(INBRSTAT)                  
         DC    AL1(TABRELQ),AL1(2),AL1(0),AL1(0),AL4(INBRRATE)                  
         DC    AL1(TACAELQ),AL1(1),AL1(0),AL1(0),AL4(INCASTAT)                  
         DC    AL1(TACAELQ),AL1(2),AL1(0),AL1(0),AL4(INCACNT)                   
         DC    AL1(TACAELQ),AL1(3),AL1(0),AL1(0),AL4(INCAAGCD)                  
         DC    AL1(TACAELQ),AL1(4),AL1(0),AL1(0),AL4(INCARERC)                  
         DC    AL1(TACAELQ),AL1(5),AL1(0),AL1(0),AL4(INCAREL)                   
         DC    AL1(TACCELQ),AL1(1),AL1(5),AL1(0),AL4(INEXMVC)                   
         DC    AL1(TACDELQ),AL1(1),AL1(0),AL1(0),AL4(INCDSTAT)                  
         DC    AL1(TACDELQ),AL1(2),AL1(0),AL1(0),AL4(INCDDAYS)                  
         DC    AL1(TACDELQ),AL1(3),AL1(0),AL1(0),AL4(INCDNUM)                   
         DC    AL1(TACDELQ),AL1(4),AL1(0),AL1(0),AL4(CVD1)                      
         DC    AL1(TACDELQ),AL1(5),AL1(0),AL1(0),AL4(INCDSTA2)                  
         DC    AL1(TACDELQ),AL1(6),AL1(0),AL1(0),AL4(INCDEARN)                  
         DC    AL1(TACDELQ),AL1(7),AL1(0),AL1(0),AL4(INCDNCNT)                  
         DC    AL1(TACDELQ),AL1(8),AL1(0),AL1(0),AL4(INCDCNT)                   
         DC    AL1(TACIELQ),AL1(1),AL1(0),AL1(0),AL4(INCLSTAT)                  
         DC    AL1(TACMELQ),AL1(1),AL1(4),AL1(0),AL4(INEXMVC)                   
         DC    AL1(TACOELQ),AL1(2),AL1(0),AL1(0),AL4(INCOMED)                   
         DC    AL1(TACOELQ),AL1(4),AL1(0),AL1(0),AL4(INCOSTAT)                  
         DC    AL1(TACOELQ),AL1(5),AL1(0),AL1(0),AL4(INCOUVST)                  
         DC    AL1(TACOELQ),AL1(6),AL1(0),AL1(0),AL4(INCOSES)                   
         DC    AL1(TACOELQ),AL1(7),AL1(0),AL1(0),AL4(INCOATYP)                  
         DC    AL1(TACPELQ),AL1(1),AL1(0),AL1(0),AL4(INCPMUS2)                  
         DC    AL1(TACPELQ),AL1(2),AL1(0),AL1(0),AL4(INCPMUS3)                  
         DC    AL1(TACPELQ),AL1(3),AL1(0),AL1(0),AL4(INCPMUS4)                  
         DC    AL1(TACQELQ),AL1(1),AL1(0),AL1(0),AL4(CVD1)                      
         DC    AL1(TACRELQ),AL1(1),AL1(0),AL1(0),AL4(CVD1)                      
         DC    AL1(TACRELQ),AL1(2),AL1(0),AL1(0),AL4(INCRAMT)                   
         DC    AL1(TACSELQ),AL1(1),AL1(0),AL1(0),AL4(INCSSTA)                   
         DC    AL1(TACXELQ),AL1(1),AL1(0),AL1(0),AL4(INCXHST)                   
         DC    AL1(TACXELQ),AL1(2),AL1(0),AL1(0),AL4(INCXHSTY)                  
         DC    AL1(TACYELQ),AL1(1),AL1(0),AL1(0),AL4(CVD1)                      
         DC    AL1(TACYELQ),AL1(2),AL1(0),AL1(0),AL4(INCYEARN)                  
         DC    AL1(TADLELQ),AL1(1),AL1(0),AL1(0),AL4(INDLTERM)                  
         DC    AL1(TADUELQ),AL1(1),AL1(0),AL1(0),AL4(INALLINV)                  
         DC    AL1(TADUELQ),AL1(2),AL1(0),AL1(0),AL4(INDUTYPE)                  
         DC    AL1(TADUELQ),AL1(3),AL1(0),AL1(0),AL4(INPACK)                    
         DC    AL1(TADUELQ),AL1(4),AL1(0),AL1(0),AL4(INDUBAL)                   
         DC    AL1(TADUELQ),AL1(5),AL1(0),AL1(0),AL4(INDUSTAT)                  
         DC    AL1(TADUELQ),AL1(6),AL1(0),AL1(0),AL4(INDUFLAG)                  
         DC    AL1(TADVELQ),AL1(1),AL1(0),AL1(0),AL4(INDVSTAT)                  
         DC    AL1(TADVELQ),AL1(2),AL1(0),AL1(0),AL4(INDVTYPE)                  
         DC    AL1(TAEPELQ),AL1(1),AL1(0),AL1(0),AL4(INEPAC)                    
         DC    AL1(TAEUELQ),AL1(1),AL1(0),AL1(0),AL4(INALLINV)                  
         DC    AL1(TAEUELQ),AL1(2),AL1(0),AL1(0),AL4(INPDUSED)                  
         DC    AL1(TAEUELQ),AL1(3),AL1(0),AL1(0),AL4(INPDSTAT)                  
         DC    AL1(TAEUELQ),AL1(4),AL1(0),AL1(0),AL4(INPDPSTS)                  
         DC    AL1(TAEUELQ),AL1(5),AL1(0),AL1(0),AL4(INPDOPTS)                  
         DC    AL1(TAEUELQ),AL1(6),AL1(0),AL1(0),AL4(INPDUSNM)                  
         DC    AL1(TAEUELQ),AL1(7),AL1(0),AL1(0),AL4(INPDAPNH)                  
         DC    AL1(TAEUELQ),AL1(8),AL1(0),AL1(0),AL4(INPDPAY)                   
         DC    AL1(TAEUELQ),AL1(9),AL1(0),AL1(0),AL4(INPDPAYP)                  
         DC    AL1(TAEUELQ),AL1(10),AL1(0),AL1(0),AL4(INPDCURR)                 
         DC    AL1(TAEUELQ),AL1(11),AL1(0),AL1(0),AL4(CVD1)                     
         DC    AL1(TAEUELQ),AL1(12),AL1(0),AL1(0),AL4(INPDPYIR)                 
         DC    AL1(TAEUELQ),AL1(13),AL1(0),AL1(0),AL4(INPDPYCR)                 
         DC    AL1(TAEUELQ),AL1(14),AL1(0),AL1(0),AL4(INPDUSES)                 
         DC    AL1(TAEUELQ),AL1(15),AL1(0),AL1(0),AL4(INPDTAGS)                 
         DC    AL1(TAEUELQ),AL1(16),AL1(0),AL1(0),AL4(INPDUNIT)                 
         DC    AL1(TAEUELQ),AL1(17),AL1(0),AL1(0),AL4(INPDGRS)                  
         DC    AL1(TAEUELQ),AL1(18),AL1(0),AL1(0),AL4(INPDPYI)                  
         DC    AL1(TAEUELQ),AL1(19),AL1(0),AL1(0),AL4(INPDREXP)                 
         DC    AL1(TAEUELQ),AL1(20),AL1(0),AL1(0),AL4(INPDPYIT)                 
         DC    AL1(TAEUELQ),AL1(21),AL1(0),AL1(0),AL4(INPDPYC)                  
         DC    AL1(TAEUELQ),AL1(22),AL1(0),AL1(0),AL4(INPDTXN)                  
         DC    AL1(TAEUELQ),AL1(23),AL1(0),AL1(0),AL4(INPDNTN)                  
         DC    AL1(TAEUELQ),AL1(24),AL1(0),AL1(0),AL4(INPDREX)                  
*        DC    AL1(TAEUELQ),AL1(25),AL1(0),AL1(0),AL4(INPDVAR)                  
         DC    AL1(TAFNELQ),AL1(1),AL1(4),AL1(0),AL4(INEXMVC)                   
         DC    AL1(TAGCELQ),AL1(1),AL1(0),AL1(0),AL4(CVD1)                      
         DC    AL1(TAGCELQ),AL1(2),AL1(0),AL1(0),AL4(INGCAPPL)                  
         DC    AL1(TAGTELQ),AL1(1),AL1(0),AL1(0),AL4(INGTUSED)                  
         DC    AL1(TAGTELQ),AL1(2),AL1(0),AL1(0),AL4(INPDUSNM)                  
         DC    AL1(TAGTELQ),AL1(3),AL1(0),AL1(0),AL4(INPACK)                    
         DC    AL1(TAGTELQ),AL1(4),AL1(0),AL1(0),AL4(INALLINV)                  
         DC    AL1(TAGUELQ),AL1(1),AL1(0),AL1(0),AL4(INGUSTAT)                  
         DC    AL1(TAGUELQ),AL1(2),AL1(0),AL1(0),AL4(INGUCRED)                  
         DC    AL1(TAGUELQ),AL1(3),AL1(0),AL1(0),AL4(INPACK)                    
         DC    AL1(TAGUELQ),AL1(4),AL1(0),AL1(0),AL4(INALLINV)                  
         DC    AL1(TAIFELQ),AL1(1),AL1(0),AL1(0),AL4(INIFWCS)                   
         DC    AL1(TAINELQ),AL1(1),AL1(0),AL1(0),AL4(ININSTAT)                  
         DC    AL1(TAINELQ),AL1(2),AL1(0),AL1(0),AL4(ININITIM)                  
         DC    AL1(TAINELQ),AL1(3),AL1(0),AL1(0),AL4(ININPTIM)                  
         DC    AL1(TAINELQ),AL1(4),AL1(0),AL1(0),AL4(ININQTIM)                  
         DC    AL1(TAINELQ),AL1(5),AL1(0),AL1(0),AL4(ININSTA3)                  
         DC    AL1(TALNELQ),AL1(1),AL1(0),AL1(0),AL4(INDUBAL)                   
         DC    AL1(TALNELQ),AL1(3),AL1(0),AL1(0),AL4(INPACK)                    
         DC    AL1(TALNELQ),AL1(4),AL1(0),AL1(0),AL4(INLNSTAT)                  
         DC    AL1(TAMAELQ),AL1(1),AL1(0),AL1(0),AL4(CVD1)                      
         DC    AL1(TAMUELQ),AL1(1),AL1(0),AL1(0),AL4(INMUNAME)                  
         DC    AL1(TAMUELQ),AL1(2),AL1(0),AL1(0),AL4(INMULIC)                   
         DC    AL1(TANPELQ),AL1(1),AL1(0),AL1(0),AL4(INNPCLA)                   
         DC    AL1(TANPELQ),AL1(2),AL1(0),AL1(0),AL4(INNPCLA2)                  
         DC    AL1(TANPELQ),AL1(3),AL1(0),AL1(0),AL4(INNPCNT)                   
         DC    AL1(TANUELQ),AL1(1),AL1(4),AL1(0),AL4(INEXMVC)                   
         DC    AL1(TAOAELQ),AL1(1),AL1(0),AL1(0),AL4(INOAAMT)                   
         DC    AL1(TAOCELQ),AL1(1),AL1(0),AL1(0),AL4(INOCDTE)                   
         DC    AL1(TAPDELQ),AL1(1),AL1(0),AL1(0),AL4(INALLINV)                  
         DC    AL1(TAPDELQ),AL1(2),AL1(0),AL1(0),AL4(INPDUSED)                  
         DC    AL1(TAPDELQ),AL1(3),AL1(0),AL1(0),AL4(INPDSTAT)                  
         DC    AL1(TAPDELQ),AL1(4),AL1(0),AL1(0),AL4(INPDPSTS)                  
         DC    AL1(TAPDELQ),AL1(5),AL1(0),AL1(0),AL4(INPDOPTS)                  
         DC    AL1(TAPDELQ),AL1(6),AL1(0),AL1(0),AL4(INPDUSNM)                  
         DC    AL1(TAPDELQ),AL1(7),AL1(0),AL1(0),AL4(INPDAPNH)                  
         DC    AL1(TAPDELQ),AL1(8),AL1(0),AL1(0),AL4(INPDPAY)                   
         DC    AL1(TAPDELQ),AL1(9),AL1(0),AL1(0),AL4(INPDPAYP)                  
         DC    AL1(TAPDELQ),AL1(10),AL1(0),AL1(0),AL4(INPDCURR)                 
         DC    AL1(TAPDELQ),AL1(11),AL1(0),AL1(0),AL4(CVD1)                     
         DC    AL1(TAPDELQ),AL1(12),AL1(0),AL1(0),AL4(INPDPYIR)                 
         DC    AL1(TAPDELQ),AL1(13),AL1(0),AL1(0),AL4(INPDPYCR)                 
         DC    AL1(TAPDELQ),AL1(14),AL1(0),AL1(0),AL4(INPDUSES)                 
         DC    AL1(TAPDELQ),AL1(15),AL1(0),AL1(0),AL4(INPDTAGS)                 
         DC    AL1(TAPDELQ),AL1(16),AL1(0),AL1(0),AL4(INPDUNIT)                 
         DC    AL1(TAPDELQ),AL1(17),AL1(0),AL1(0),AL4(INPDGRS)                  
         DC    AL1(TAPDELQ),AL1(18),AL1(0),AL1(0),AL4(INPDPYI)                  
         DC    AL1(TAPDELQ),AL1(19),AL1(0),AL1(0),AL4(INPDREXP)                 
         DC    AL1(TAPDELQ),AL1(20),AL1(0),AL1(0),AL4(INPDPYIT)                 
         DC    AL1(TAPDELQ),AL1(21),AL1(0),AL1(0),AL4(INPDPYC)                  
         DC    AL1(TAPDELQ),AL1(22),AL1(0),AL1(0),AL4(INPDTXN)                  
         DC    AL1(TAPDELQ),AL1(23),AL1(0),AL1(0),AL4(INPDNTN)                  
         DC    AL1(TAPDELQ),AL1(24),AL1(0),AL1(0),AL4(INPDREX)                  
*        DC    AL1(TAPDELQ),AL1(25),AL1(0),AL1(0),AL4(INPDVAR)                  
         DC    AL1(TAPEELQ),AL1(1),AL1(0),AL1(0),AL4(INPECITY)                  
         DC    AL1(TAPEELQ),AL1(2),AL1(0),AL1(0),AL4(INPEST)                    
         DC    AL1(TAPEELQ),AL1(3),AL1(0),AL1(0),AL4(INPEZIP)                   
         DC    AL1(TAPEELQ),AL1(4),AL1(0),AL1(0),AL4(INPECTRY)                  
         DC    AL1(TAPIELQ),AL1(1),AL1(0),AL1(0),AL4(INPISTAT)                  
         DC    AL1(TAPIELQ),AL1(2),AL1(0),AL1(0),AL4(INPITYPE)                  
         DC    AL1(TAPOELQ),AL1(1),AL1(0),AL1(0),AL4(INPOYTD)                   
         DC    AL1(TARNELQ),AL1(1),AL1(0),AL1(0),AL4(INRNUID)                   
         DC    AL1(TARNELQ),AL1(2),AL1(0),AL1(0),AL4(INRNSTAT)                  
         DC    AL1(TARPELQ),AL1(1),AL1(0),AL1(0),AL4(INRPBASE)                  
         DC    AL1(TARPELQ),AL1(2),AL1(0),AL1(0),AL4(INRPDIFF)                  
         DC    AL1(TAR1ELQ),AL1(1),AL1(0),AL1(0),AL4(CVD1)                      
         DC    AL1(TASDELQ),AL1(1),AL1(0),AL1(0),AL4(ZERO1)                     
         DC    AL1(TASDELQ),AL1(2),AL1(0),AL1(0),AL4(ZERO4)                     
         DC    AL1(TASDELQ),AL1(3),AL1(0),AL1(0),AL4(ZERO2)                     
         DC    AL1(TASDELQ),AL1(6),AL1(0),AL1(0),AL4(INSDHRMN)                  
         DC    AL1(TASDELQ),AL1(7),AL1(0),AL1(0),AL4(INSDTDAY)                  
         DC    AL1(TASDELQ),AL1(8),AL1(0),AL1(0),AL4(INSDTOT)                   
         DC    AL1(TASDELQ),AL1(9),AL1(0),AL1(0),AL4(INSDTDT)                   
         DC    AL1(TASDELQ),AL1(10),AL1(0),AL1(0),AL4(INSDTTAG)                 
         DC    AL1(TASIELQ),AL1(1),AL1(0),AL1(0),AL4(INSIINV)                   
         DC    AL1(TASIELQ),AL1(2),AL1(0),AL1(0),AL4(INSIPCT)                   
         DC    AL1(TASIELQ),AL1(3),AL1(0),AL1(0),AL4(INSIEST)                   
         DC    AL1(TASOELQ),AL1(1),AL1(0),AL1(0),AL4(INSOSTAT)                  
         DC    AL1(TASOELQ),AL1(2),AL1(0),AL1(0),AL4(INSOEPI)                   
         DC    AL1(TASOELQ),AL1(3),AL1(0),AL1(0),AL4(INSOPNH)                   
         DC    AL1(TASOELQ),AL1(4),AL1(0),AL1(0),AL4(INSOPAY)                   
         DC    AL1(TASOELQ),AL1(5),AL1(0),AL1(0),AL4(INSOAPPL)                  
         DC    AL1(TATUELQ),AL1(1),AL1(0),AL1(0),AL4(INTUEARN)                  
         DC    AL1(TAT4ELQ),AL1(1),AL1(0),AL1(0),AL4(CVD1)                      
         DC    AL1(TAUPELQ),AL1(1),AL1(0),AL1(0),AL4(INUPDET)                   
         DC    AL1(TAVUELQ),AL1(1),AL1(0),AL1(0),AL4(INVUUSE)                   
         DC    AL1(TAWXELQ),AL1(1),AL1(0),AL1(0),AL4(INANSSN)                   
         DC    AL1(TAW4ELQ),AL1(1),AL1(0),AL1(0),AL4(INW4RACE)                  
         DC    AL1(TAW4ELQ),AL1(2),AL1(0),AL1(0),AL4(INW4SEX)                   
         DC    AL1(TAW4ELQ),AL1(3),AL1(0),AL1(0),AL4(INW4MID)                   
         DC    AL1(TAW4ELQ),AL1(4),AL1(0),AL1(0),AL4(INW4FULL)                  
         DC    AL1(TAW4ELQ),AL1(5),AL1(0),AL1(0),AL4(INW4FTX)                   
         DC    AL1(TAW4ELQ),AL1(6),AL1(0),AL1(0),AL4(INW4NHA)                   
         DC    AL1(TAW4ELQ),AL1(7),AL1(0),AL1(0),AL4(INW4RCPS)                  
         DC    AL1(TAYEELQ),AL1(1),AL1(0),AL1(0),AL4(CVD1)                      
         DC    X'FF'                                                            
         SPACE 1                                                                
DUMELEM  DC    250X'00'                                                         
         EJECT                                                                  
*              STACK DEFINITION TABLE                                           
         SPACE 3                                                                
STACKTAB DS    0F                                                               
         DC    X'00'                                                            
         DC    C'       ',AL1(0),AL1(0),AL1(1),AL1(0)                           
         DC    C'*SUB*   ',AL1(0),AL1(0),AL1(2),AL1(0)                          
         DC    C'*TOTAL* ',AL1(0),AL1(0),AL1(3),AL1(0)                          
         DC    C'(DIFF)  ',AL1(0),AL1(0),AL1(4),AL1(0)                          
         DC    C'BILL TOT',AL1(0),AL1(TABDELQ),AL1(3),AL1(0)                    
         DC    C'TAXES   ',AL1(0),AL1(TABDELQ),AL1(4),AL1(0)                    
         DC    C'HANDLING',AL1(0),AL1(TABDELQ),AL1(5),AL1(0)                    
         DC    C'CORP HND',AL1(0),AL1(TABDELQ),AL1(6),AL1(0)                    
         DC    C'CSF     ',AL1(0),AL1(TABDELQ),AL1(7),AL1(0)                    
         DC    C'FICA CR.',AL1(0),AL1(TABDELQ),AL1(8),AL1(0)                    
         DC    C'HANDLING',AL1(0),AL1(TABDELQ),AL1(9),AL1(0)                    
         DC    C'HAND+TAX',AL1(0),AL1(TABDELQ),AL1(10),AL1(0)                   
         SPACE 1                                                                
         DC    C'BILL TOT',AL1(0),AL1(TABDELQ2),AL1(3),AL1(0)                   
         DC    C'TAXES   ',AL1(0),AL1(TABDELQ2),AL1(4),AL1(0)                   
         DC    C'HANDLING',AL1(0),AL1(TABDELQ2),AL1(5),AL1(0)                   
         DC    C'CORP HND',AL1(0),AL1(TABDELQ2),AL1(6),AL1(0)                   
         DC    C'CSF     ',AL1(0),AL1(TABDELQ2),AL1(7),AL1(0)                   
         DC    C'FICA CR.',AL1(0),AL1(TABDELQ2),AL1(8),AL1(0)                   
         DC    C'HANDLING',AL1(0),AL1(TABDELQ2),AL1(9),AL1(0)                   
         DC    C'HAND+TAX',AL1(0),AL1(TABDELQ2),AL1(10),AL1(0)                  
         SPACE 1                                                                
         DC    C'BILL TOT',AL1(0),AL1(TABDELQ3),AL1(3),AL1(0)                   
         DC    C'TAXES   ',AL1(0),AL1(TABDELQ3),AL1(4),AL1(0)                   
         DC    C'HANDLING',AL1(0),AL1(TABDELQ3),AL1(5),AL1(0)                   
         DC    C'CORP HND',AL1(0),AL1(TABDELQ3),AL1(6),AL1(0)                   
         DC    C'CSF     ',AL1(0),AL1(TABDELQ3),AL1(7),AL1(0)                   
         DC    C'FICA CR.',AL1(0),AL1(TABDELQ3),AL1(8),AL1(0)                   
         DC    C'HANDLING',AL1(0),AL1(TABDELQ3),AL1(9),AL1(0)                   
         DC    C'HAND+TAX',AL1(0),AL1(TABDELQ3),AL1(10),AL1(0)                  
         SPACE 1                                                                
         DC    C'GROSS   ',AL1(0),AL1(TAPDELQ),AL1(11),AL1(0)                   
         DC    C'APPLIED ',AL1(0),AL1(TAPDELQ),AL1(12),AL1(0)                   
         DC    C'GUAR CR.',AL1(0),AL1(TAPDELQ),AL1(13),AL1(0)                   
         DC    C'PAYMENTS',AL1(0),AL1(TAPDELQ),AL1(41),AL1(0)                   
         DC    C'IND. PAY',AL1(0),AL1(TAPDELQ),AL1(14),AL1(0)                   
         DC    C'CORP PAY',AL1(0),AL1(TAPDELQ),AL1(15),AL1(0)                   
         DC    C'REIMB EX',AL1(0),AL1(TAPDELQ),AL1(16),AL1(0)                   
         DC    C'SUB P&&H ',AL1(0),AL1(TAPDELQ),AL1(17),AL1(0)                  
         DC    C'MISC DED',AL1(0),AL1(TAPDELQ),AL1(18),AL1(0)                   
         DC    C'P&&H     ',AL1(0),AL1(TAPDELQ),AL1(19),AL1(0)                  
         DC    C'H&&W     ',AL1(0),AL1(TAPDELQ),AL1(20),AL1(0)                  
         DC    C'I&&R     ',AL1(0),AL1(TAPDELQ),AL1(21),AL1(0)                  
         DC    C'APP P&&H ',AL1(0),AL1(TAPDELQ),AL1(22),AL1(0)                  
         DC    C'PAYMENT ',AL1(0),AL1(TAPDELQ),AL1(42),AL1(0)                   
         SPACE 1                                                                
         DC    C'GROSS   ',AL1(0),AL1(TAEUELQ),AL1(11),AL1(0)                   
         DC    C'APPLIED ',AL1(0),AL1(TAEUELQ),AL1(12),AL1(0)                   
         DC    C'GUAR CR.',AL1(0),AL1(TAEUELQ),AL1(13),AL1(0)                   
         DC    C'PAYMENTS',AL1(0),AL1(TAEUELQ),AL1(41),AL1(0)                   
         DC    C'IND. PAY',AL1(0),AL1(TAEUELQ),AL1(14),AL1(0)                   
         DC    C'CORP PAY',AL1(0),AL1(TAEUELQ),AL1(15),AL1(0)                   
         DC    C'REIMB EX',AL1(0),AL1(TAEUELQ),AL1(16),AL1(0)                   
         DC    C'SUB P&&H ',AL1(0),AL1(TAEUELQ),AL1(17),AL1(0)                  
         DC    C'MISC DED',AL1(0),AL1(TAEUELQ),AL1(18),AL1(0)                   
         DC    C'P&&H     ',AL1(0),AL1(TAEUELQ),AL1(19),AL1(0)                  
         DC    C'H&&W     ',AL1(0),AL1(TAEUELQ),AL1(20),AL1(0)                  
         DC    C'I&&R     ',AL1(0),AL1(TAEUELQ),AL1(21),AL1(0)                  
         DC    C'APP P&&H ',AL1(0),AL1(TAEUELQ),AL1(22),AL1(0)                  
         DC    C'PAYMENT ',AL1(0),AL1(TAEUELQ),AL1(42),AL1(0)                   
         DC    X'FF'                                                            
         EJECT                                                                  
*              RECORD SAVE BUFFER AREAS                                         
         SPACE 3                                                                
NEEDTAB  DS    0F                                                               
         DC    AL1(TLOFCDQ),AL3(OFBUFF)                                         
         DC    AL1(TLAYCDQ),AL3(AYBUFF)                                         
         DC    AL1(TLAGCDQ),AL3(AGBUFF)                                         
         DC    AL1(TLCLCDQ),AL3(CLBUFF)                                         
         DC    AL1(TLCGCDQ),AL3(CGBUFF)                                         
         DC    AL1(TLPRCDQ),AL3(PRBUFF)                                         
         DC    AL1(TLPGCDQ),AL3(PGBUFF)                                         
         DC    AL1(TLCOCCDQ),AL3(COBUFF)                                        
         DC    AL1(TLW4CDQ),AL3(W4BUFF)                                         
         DC    AL1(TLANCDQ),AL3(ANBUFF)                                         
         DC    AL1(TLEMCDQ),AL3(EMBUFF)                                         
         DC    AL1(TLDUCDQ),AL3(DUBUFF)                                         
         DC    AL1(TLCTCDQ),AL3(CTBUFF)                                         
         DC    AL1(TLSTCDQ),AL3(STBUFF)                                         
         DC    AL1(TLEPCDQ),AL3(EPBUFF)                                         
         DC    AL1(TLCACCDQ),AL3(CABUFF)                                        
         DC    X'FF',AL3(OTBUFF)                                                
         EJECT                                                                  
*======================================================================         
*              ROUTINES TO FILTER COLUMNS                                       
*                                                                               
*              GLARGS              7-9  UP TO 3 FILTER SLOT NUMBERS             
*                                  EACH SLOT NUMBER REFERS TO A 16              
*                                  BYTE ENTRY, ADDRESSED BY ACOLFILT            
*                                                                               
*                                  EACH ENTRY IS....                            
*                                     1. DISPLACEMENT INTO TICODES              
*                                     2. LENGTH OF CODE                         
*                                     3. SPARE                                  
*                                     4. BITS FOR NEGATIVE & LISTS              
*                                        EX: X'BF'=NEG, X'7F'=LIST              
*                                  5-16. FILTER EXPRESSION                      
*                                                                               
*                                  10    DATE TYPE                              
*                                  11-13 START DATE                             
*                                  14-16 END DATE                               
*======================================================================         
         SPACE 1                                                                
COLFILT  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,GLARGS+6                                                      
         LA    R0,3                                                             
         CLI   0(R2),0                                                          
         BE    CFILT8                                                           
         L     R4,=A(MYSYSIOD)     CREATE A PHONY SYSIO BLOCK                   
         MOVE  ((R4),1000),TASYSIOD       SAVE THE REAL ONE                     
         XC    TIFILTS,TIFILTS            CREAM THE CURRENT FILTERS             
*                                         (RECORD ALREADY FILTERED)             
         SPACE 1                                                                
CFILTNXT CLI   0(R2),0                                                          
         BE    CFILT2                                                           
         ZIC   R3,0(R2)            (SLOT NUMBER)                                
         BCTR  R3,0                                                             
         SLL   R3,4                (16 BYTES PER SLOT ENTRY)                    
         A     R3,ACOLFILT         DISPLACE INTO COLUMN FILTER BUFFER           
         BRAS  RE,FILTSET          AND SET THIS FILTER                          
         LA    R2,1(R2)            ON TO THE NEXT ARGUMENT!                     
         BCT   R0,CFILTNXT                                                      
         SPACE 1                                                                
CFILT2   MVI   TIREAD,TIFONLY      TELL SYSIO I AM ONLY FILTERING               
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         BE    CFILT4                   (SYSIO OK'D)                            
         MOVE  (TASYSIOD,800),0(R4)     RESTORE REAL SYSIO BLOCK                
         B     NOGOOD2                                                          
         SPACE 1                                                                
CFILT4   MOVE  (TASYSIOD,800),0(R4)     RESTORE REAL SYSIO BLOCK                
         EJECT                                                                  
*              DATE FILTERS                                                     
         SPACE 3                                                                
*                                        ARGS 10 IS DATE TYPE                   
CFILT8   OC    GLARGS+10(6),GLARGS+10    ARGS 11-16 ARE DATES                   
         BZ    ITSFINE2                                                         
         IC    R0,GLARGS           (BORROW GLARGS FOR SETDATE)                  
         MVC   GLARGS(1),GLARGS+9  (PASSING DATE TYPE TO SETDATE)               
         BRAS  RE,SETDATE          (WORK NOW HAS RELEVANT YMD PWOS)             
         STC   R0,GLARGS                                                        
         SPACE 1                                                                
         LA    R1,1                ARE WE FILTERING ON YM                       
         CLI   GLARGS+12,0                                                      
         BE    *+8                                                              
         LA    R1,2                                 OR YMD                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),GLARGS+10                                                
         BL    NOGOOD2            MUST NOT BE BEFORE START                      
         BE    CFILT10                                                          
         OC    GLARGS+13(3),GLARGS+13  AFTER START OK IF END DATE               
         BZ    NOGOOD2                                   IN FILTER              
         SPACE 1                                                                
CFILT10  OC    GLARGS+13(3),GLARGS+13  NO END DATE?                             
         BZ    ITSFINE2                                                         
         LA    R1,1               IS END DATE YM                                
         CLI   GLARGS+15,0                                                      
         BE    *+8                                                              
         LA    R1,2                        OR YMD                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),GLARGS+13                                                
         BH    NOGOOD2             CAN'T BE AFTER END DATE                      
         B     ITSFINE2                                                         
         EJECT                                                                  
*======================================================================         
*              IS A CANADIAN PROV                                               
*                  CC = EQ - YES                                                
*                     = NE - NO                                                 
*======================================================================         
CANPROV  NTR1  BASE=*,LABEL=*                                                   
         USING CTRYTABD,R3                                                      
         L     R3,TGACTRYS                                                      
         ZIC   R0,CTRYLEN                                                       
         AR    R3,R0               SKIP TO CANADA                               
CPRVNO   LTR   RB,RB                                                            
         B     *+6                                                              
CPRVYES  CR    RB,RB                                                            
         J     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              ROUTINE TO FILTER UNITS                                          
*                                                                               
*              INPUTS              THISUNIT IS SET                              
*              ARGUMENT 2  WHERE   F=FEDERAL ONLY                               
*                                  N=STATE & CANADA                             
*                                  S=STATE ONLY                                 
*                                  T=STATE ONLY - NO CORPS                      
*                                  R=STATE ONLY FOR CORPS                       
*                                  U=STATE ONLY FOR CORPS + FOR                 
*                                  L=LOCAL OR                                   
*                                  Y=CITY                                       
*                                  C=CANADA                                     
*                                  O=OTHERS                                     
*                                  V=FOREIGNERS ONLY                            
*              OUTPUT              SET CONDITION CODE NE IF FAIL                
*======================================================================         
         SPACE 1                                                                
TESTWHER NTR1  BASE=*,LABEL=*                                                   
         CLI   GLARGS+1,C'F'       SELECT WHICH TEST TO DO                      
         JE    WHERFED                                                          
         CLI   GLARGS+1,C'N'                                                    
         JE    WHERNFED                                                         
         CLI   GLARGS+1,C'S'                                                    
         JE    WHERSTA                                                          
         CLI   GLARGS+1,C'R'                                                    
         JE    WHERSTA                                                          
         CLI   GLARGS+1,C'T'                                                    
         JE    WHERSTA                                                          
         CLI   GLARGS+1,C'C'                                                    
         JE    WHERCAN                                                          
         CLI   GLARGS+1,C'Y'                                                    
         JE    WHERCITY                                                         
         CLI   GLARGS+1,C'L'                                                    
         JE    WHERCITY                                                         
         CLI   GLARGS+1,C'O'                                                    
         JE    WHEROTH                                                          
         CLI   GLARGS+1,C'U'                                                    
         JE    WHERSTA                                                          
         CLI   GLARGS+1,C'V'                                                    
         JE    WHERSTA                                                          
         J     ITSFINE2                                                         
         SPACE 1                                                                
WHERFED  CLC   THISUNIT(2),=C'FD'  FEDERAL ONLY                                 
         JNE   NOGOOD2                                                          
         J     ITSFINE2                                                         
         SPACE 1                                                                
WHERNFED CLC   THISUNIT(2),=C'FD'  STATES + CANADA - SO IGNORE FED              
         JE    NOGOOD2                                                          
         CLI   THISUNIT+2,C' '                  AND CITIES                      
         JH    NOGOOD2                                                          
         J     ITSFINE2                                                         
         SPACE 1                                                                
WHERSTA  CLC   THISUNIT(2),=C'FD'  ALL STATES - SO IGNORE FED                   
         JE    NOGOOD2                                                          
         CLC   THISUNIT(2),=C'CN'               AND CANADA                      
         JE    NOGOOD2                                                          
         GOTO1 TAXVAL,DMCB,(2,THISUNIT)                                         
         JNE   NOGOOD2                                                          
*&&DO                                                                           
         CLC   TIFCTRY,=C'  '                   NO COUNTRY FILTERING            
         BNH   WHERSTA4                                                         
         CLC   TIFCTRY,=C'US'                   US ONLY                         
         BNE   WHERSTA3                                                         
         GOTO1 TAXVAL,DMCB,(3,TACYUNIT)                                         
         BE    WHERSTA4                                                         
         B     NOGOOD2                                                          
WHERSTA3 MVC   TGCTRY,TIFCTRY                   OTHER COUNTRIES                 
         GOTO1 TAXVAL,DMCB,(X'FF',TACYUNIT)                                     
         BNE   XIT2                                                             
*&&                                                                             
WHERSTA4 CLI   THISUNIT+2,C' '                  AND CITIES                      
         JH    NOGOOD2                                                          
         CLI   GLARGS+1,C'S'       IF ARG 2 = S, THEN DONE                      
         JE    ITSFINE2                                                         
         CLI   GLARGS+1,C'L'       IF ARG 2 = L, THEN DONE                      
         JE    ITSFINE2                                                         
         CLI   GLARGS+1,C'T'       IF ARG 2 = T                                 
         JNE   WHERSTA5                                                         
         CLI   TIW4TY,TAW4TYCO     ALSO IGNORE CORPS                            
         JE    NOGOOD2                                                          
         CLI   TIW4TY,TAW4TYCA     AND CANADIANS                                
         JE    NOGOOD2                                                          
         CLI   TIW4TY,TAW4TYTR     AND TRUSTEES                                 
         JE    NOGOOD2                                                          
         CLI   TIW4TY,TAW4TYFO     AND FOREIGNERS                               
         JE    NOGOOD2                                                          
         J     ITSFINE2                                                         
                                                                                
WHERSTA5 CLI   GLARGS+1,C'V'       IF ARG 2 = V, THEN FGNR + CAN ONLY           
         JNE   WHERSTA7                                                         
         CLI   TIW4TY,TAW4TYFO                                                  
         JE    ITSFINE2                                                         
         CLI   TIW4TY,TAW4TYCA       INCLUDE CANADIANS AS FOREIGNERS            
         JE    ITSFINE2                 (JAN 2015)                              
         J     NOGOOD2                                                          
                                                                                
WHERSTA7 CLI   GLARGS+1,C'R'       IF ARG 2 = R, THEN CORPS ONLY                
         JNE   WHERSTA9                                                         
         CLI   TIW4TY,TAW4TYCO                                                  
         JE    ITSFINE2                                                         
         J     NOGOOD2                                                          
                                                                                
WHERSTA9 CLI   TIW4TY,TAW4TYCO     ARG2 = U - MUST BE CORP                      
         JE    ITSFINE2                                                         
         CLI   TIW4TY,TAW4TYCA     OR CANADIAN                                  
         JE    ITSFINE2                                                         
         CLI   TIW4TY,TAW4TYTR     OR TRUSTEE                                   
         JE    ITSFINE2                                                         
         CLI   TIW4TY,TAW4TYFO     OR FOREIGNERS                                
         JE    ITSFINE2                                                         
         J     NOGOOD2                                                          
         SPACE 1                                                                
WHERCAN  CLC   THISUNIT(2),=C'CN'  CANADA                                       
         JE    ITSFINE2                                                         
         CLC   THISUNIT(2),=C'QC'  AND QUEBEC ONLY                              
         BE    ITSFINE2                                                         
         MVC   WORK(2),TGCTRY                                                   
         MVC   TGCTRY,=C'CA'       AND CANADIAN PROVINCES                       
         GOTO1 TAXVAL,DMCB,(X'FF',THISUNIT)                                     
         MVC   TGCTRY,WORK                                                      
         JE    ITSFINE2                                                         
         J     NOGOOD2                                                          
         SPACE 1                                                                
WHEROTH  CLC   THISUNIT(2),=C'OT'  OTHERS ONLY                                  
         JE    ITSFINE2                                                         
         J     NOGOOD2                                                          
         SPACE 1                                                                
WHERCITY CLI   THISUNIT+2,C' '     CITIES ONLY                                  
         JH    ITSFINE2                                                         
         J     NOGOOD2                                                          
         SPACE 1                                                                
*======================================================================         
*              ADD UP TATU AMOUNTS                                              
*                  INPUT X'80' = ADD TATUWAGES                                  
*                        X'40' = ADD TATUTNWA                                   
*                        X'20' = ADD TATUNNWA                                   
*                        X'10' = ADD TATUSTRE                                   
*======================================================================         
TUAMTS   NTR1  BASE=*,LABEL=*                                                   
         MVC   BYTE,0(R1)          SAVE OFF                                     
                                                                                
         MVI   ELCODE,TATUELQ      GET TAX UNIT                                 
         MVC   AIO,TIAREC                                                       
         LA    RF,TIFUNIT                                                       
         GOTO1 GETL,DMCB,(3,(RF))                                               
                                                                                
         L     R4,TGELEM                                                        
         USING TATUD,R4                                                         
         XR    R1,R1                                                            
                                                                                
         TM    BYTE,X'80'                                                       
         BZ    *+8                                                              
         ICM   R1,15,TATUWAGE      ADD UNIT'S WAGE                              
                                                                                
         TM    BYTE,X'40'                                                       
         BZ    *+10                                                             
         ICM   RE,15,TATUTNWA      TAXABLE REIMBURSEMENTS                       
         AR    R1,RE                                                            
                                                                                
         TM    BYTE,X'20'                                                       
         BZ    *+10                                                             
         ICM   RE,15,TATUNNWA      NON-TAXABLE REIMBURSEMENTS                   
         AR    R1,RE                                                            
                                                                                
         TM    BYTE,X'10'                                                       
         BZ    *+10                                                             
         ICM   RE,15,TATUSTRE      TAXABLE REIMBS (FOR NON-INDS)                
         AR    R1,RE                                                            
                                                                                
         XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*======================================================================         
TESTRNWT NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TACWELQ                                                   
         MVC   AIO,TIAREC                                                       
         LA    RF,THISUNIT                                                      
         GOTO1 GETL,DMCB,(3,(RF))                                               
         MVC   AIO,AIO1                                                         
         L     R4,TGELEM                                                        
         CLI   GLARGS+3,C'R'       NEXT THE STATUS                              
         JE    TRNWT100                                                         
         CLI   GLARGS+3,C'N'                                                    
         JE    TRNWT200                                                         
         CLI   GLARGS+3,C'X'       NON-RES FOR W4TYPE I AND F                   
         JE    TRNWT200                                                         
         CLI   GLARGS+3,C'W'                                                    
         JE    TRNWT300                                                         
         CLI   GLARGS+3,C'T'                                                    
         JE    TRNWT400                                                         
         J     ITSFINE2                                                         
                                                                                
         USING TACWD,R4                                                         
TRNWT100 TM    TACWSTAT,TACWSRES   C'R' RESIDENT ONLY                           
         JO    ITSFINE2                                                         
         J     NOGOOD2                                                          
TRNWT200 TM    TACWSTAT,TACWSRES   C'N' NON-RESIDENT ONLY                       
         JNO   ITSFINE2                                                         
         J     NOGOOD2                                                          
TRNWT300 TM    TACWSTAT,TACWSWRK   C'W' WORK STATE ONLY                         
         JO    ITSFINE2                                                         
         J     NOGOOD2                                                          
TRNWT400 TM    TACWSTAT,TACWSTAX   C'T' TAXABLE STATE ONLY                      
         JO    ITSFINE2                                                         
         J     NOGOOD2                                                          
         EJECT                                                                  
SETDATE  NTR1  BASE=*,LABEL=*                                                   
         CLI   GLARGS,1                                                         
         JNE   *+14                                                             
         MVC   WORK(3),TIBIDATE               1=BILLED DATE                     
         J     XIT2                                                             
         CLI   GLARGS,2                                                         
         JNE   *+14                                                             
         MVC   WORK(3),TIPADATE               2=PAY DATE                        
         J     XIT2                                                             
         CLI   GLARGS,3                                                         
         JNE   *+14                                                             
         MVC   WORK(3),TICKDATE               3=CHECK DATE                      
         J     XIT2                                                             
         CLI   GLARGS,4                                                         
         JNE   *+14                                                             
         MVC   WORK(3),TIDUDATE               4=DUE DATE                        
         J     XIT2                                                             
         CLI   GLARGS,5                                                         
         JNE   *+14                                                             
         MVC   WORK(3),TILADATE               5=LAST ACTIVITY DATE              
         J     XIT2                                                             
         CLI   GLARGS,6                                                         
         JNE   *+14                                                             
         MVC   WORK(3),TICSDATE               6=CYCLE START                     
         J     XIT2                                                             
         CLI   GLARGS,7                                                         
         JNE   *+14                                                             
         MVC   WORK(3),TICEDATE               7=CYCLE END                       
         J     XIT2                                                             
         MVC   WORK(3),TIDATE           0 OR >7=GENERAL DATE                    
         J     XIT2                                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET UP TSEARN BASED ON TACW AND TATU                  
         USING TATUD,R4                                                         
CWTUSET  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,TATUELQ                                                   
         L     R5,AIO              SAVE AIO                                     
         MVC   AIO,TIAREC                                                       
         L     R4,AIO                                                           
         GOTO1 GETL,DMCB,(3,THISUNIT)                                           
         ST    R5,AIO              RESTORE AIO                                  
         BNE   XIT2                                                             
         L     R4,TGELEM                                                        
                                                                                
         CLI   GLARGS,C'1'                                                      
         JE    CWTUSET2                                                         
         CLI   GLARGS,C'S'                                                      
         JE    CWTUSET2                                                         
         CLI   GLARGS,C'R'                                                      
         JE    CWTUSET2                                                         
         CLI   GLARGS,C'N'         NON-TAXABLE EARNINGS                         
         JE    CWTUSET4                                                         
         CLI   GLARGS,C'X'         NON-TAXABLE EARNINGS                         
         JE    CWTUSET4                                                         
         J     XIT                                                              
                                                                                
CWTUSET2 ICM   R1,15,TATUWAGE      WAGES + TAXABLE NON-WAGES                    
         ICM   R0,15,TATUTNWA                                                   
         CLI   GLARGS+1,C'V'       FOREIGNER                                    
         JNE   *+8                                                              
         ICM   R0,15,TATUSTRE                                                   
         AR    R1,R0                                                            
         ST    R1,SAVEEARN                                                      
         J     XIT2                                                             
                                                                                
CWTUSET4 ICM   R1,15,TATUNNWA      NON-TAXABLE NON-WAGES                        
         ST    R1,SAVEEARN                                                      
         J     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO SET UP SYSIOD FOR COLUMN FILTERS                      
         SPACE 3                                                                
*                                  R3=A(16 BYTE FILTER SLOT)                    
*                                     BYTE 1    DISPLACEMENT TIFILTS            
*                                     BYTE 2    L'FILTER                        
*                                     BYTE 3    C'-' FOR NEGATIVE               
*                                     BYTE 4    SPARE                           
*                                     BYTE 5-16 FILTER TEST VALUE               
         SPACE 1                                                                
FILTSET  NTR1  BASE=*,LABEL=*                                                   
         LA    RE,TIFINSTY         SPECIAL TESTS FOR INVOICE STATUS             
         CLI   1(R3),101                                                        
         JE    FILTSTAT                                                         
         LA    RE,TIFINS2Y                                                      
         CLI   1(R3),102                                                        
         JE    FILTSTAT                                                         
         LA    RE,TIFPDPY                                                       
         CLI   1(R3),103                                                        
         JE    FILTSTAT                                                         
         LA    RE,TIFPDSY                                                       
         CLI   1(R3),104                                                        
         JE    FILTSTAT                                                         
         CLI   1(R3),120                                                        
         JE    FILTCUST                                                         
         ZIC   RE,0(R3)            RELATIVE DISPLACEMENT IN FILTERS             
         LA    RE,TIFILTS(RE)      (NOW HAVE THE ADDRESS)                       
         ZIC   RF,1(R3)            RF=LENGTH                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RE),4(R3)       FILTER VALUE TO TIFILTS                      
*******  CLI   2(R3),C'-'          CHECK FOR MINUS FILTER                       
*******  BNE   XIT2                                                             
*******  NI    0(RE),X'BF'         40 BIT OFF IN FIRST BYTE                     
         NC    0(1,RE),3(R3)       MAY TURN OFF BITS FOR LISTS/NEGS             
         J     XIT2                                                             
         SPACE 1                                                                
FILTSTAT CLI   4(R3),C'N'         STATUS - NEGATIVE OPTION                      
         JNE   *+8                                                              
         LA    RE,1(RE)                                                         
         OC    0(1,RE),0(R3)                                                    
         J     XIT2                                                             
         SPACE 1                                                                
FILTCUST MVC   TIFCUST,0(R3)       CUSTOM FILTERS                               
         J     XIT2                                                             
                                                                                
         LTORG                                                                  
                                                                                
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*OFFICE*'                                                      
OFBUFF   DC    F'8'                                                             
         DC    F'400'                                                           
         DC    F'0'                                                             
         DC    3200X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*AGENCY*'                                                      
AYBUFF   DC    F'10'                                                            
         DC    F'1000'                                                          
         DC    F'0'                                                             
         DC    10000X'00'                                                       
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*AGROUP*'                                                      
AGBUFF   DC    F'10'                                                            
         DC    F'200'                                                           
         DC    F'0'                                                             
         DC    2000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*CLIENT*'                                                      
CLBUFF   DC    F'10'                                                            
         DC    F'1000'                                                          
         DC    F'0'                                                             
         DC    10000X'00'                                                       
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*CGROUP*'                                                      
CGBUFF   DC    F'10'                                                            
         DC    F'200'                                                           
         DC    F'0'                                                             
         DC    2000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*PRODCT*'                                                      
PRBUFF   DC    F'10'                                                            
         DC    F'1000'                                                          
         DC    F'0'                                                             
         DC    10000X'00'                                                       
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*PGROUP*'                                                      
PGBUFF   DC    F'10'                                                            
         DC    F'200'                                                           
         DC    F'0'                                                             
         DC    2000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**COMM**'                                                      
COBUFF   DC    F'10'                                                            
         DC    F'1000'                                                          
         DC    F'0'                                                             
         DC    10000X'00'                                                       
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'***W4***'                                                      
W4BUFF   DC    F'10'                                                            
         DC    F'1000'                                                          
         DC    F'0'                                                             
         DC    10000X'00'                                                       
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*AGENTS*'                                                      
ANBUFF   DC    F'10'                                                            
         DC    F'400'                                                           
         DC    F'0'                                                             
         DC    4000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**EMPS**'                                                      
EMBUFF   DC    F'4'                                                             
         DC    F'2000'                                                          
         DC    F'0'                                                             
         DC    8000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*DUCMPS*'                                                      
DUBUFF   DC    F'10'                                                            
         DC    F'400'                                                           
         DC    F'0'                                                             
         DC    4000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*CNTRLS*'                                                      
CTBUFF   DC    F'20'                                                            
         DC    F'200'                                                           
         DC    F'0'                                                             
         DC    4000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*STAFFS*'                                                      
STBUFF   DC    F'20'                                                            
         DC    F'400'                                                           
         DC    F'0'                                                             
         DC    8000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*OTHERS*'                                                      
OTBUFF   DC    F'4'                                                             
         DC    F'2000'                                                          
         DC    F'0'                                                             
         DC    8000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*EPISDE*'                                                      
EPBUFF   DC    F'20'                                                            
         DC    F'100'                                                           
         DC    F'0'                                                             
         DC    2000X'00'                                                        
         DS    0D                                                               
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**CAST**'                                                      
CABUFF   DC    F'10'                                                            
         DC    F'1000'                                                          
         DC    F'0'                                                             
         DC    10000X'00'                                                       
         SPACE 1                                                                
         DC    C'MYSYSIOD'                                                      
MYSYSIOD DC    1000X'00'                                                        
         SPACE 1                                                                
*              DSECT TO COVER STACK DEFINITION ENTRY                            
         SPACE 1                                                                
STKENTD  DSECT                                                                  
STKENT   DS    0CL3                                                             
STKCON   DS    XL1                                                              
STKCDET  EQU   X'80'               ONLY APPLIES TO DETAILS                      
STKCTOT  EQU   X'40'               ONLY APPLIES TO TOTALS                       
STKEL    DS    XL1                 ELEMENT CODE OR ZERO FOR CONTROLS            
STKROUT  DS    XL1                                                              
STKSP    EQU   1                   SPECIAL IF STKEL=0                           
STKSBTOT EQU   2                                                                
STKTOT   EQU   3                                                                
STKDIFF  EQU   4                                                                
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE TAREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'151TASYSDRIVE02/24/17'                                      
         END                                                                    
