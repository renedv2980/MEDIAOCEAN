*          DATA SET GELDMSGA   AT LEVEL 005 AS OF 12/11/90                      
*PHASE GELDMSGA                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE SCANNER                                                                
         TITLE 'GELDMSG -GENDIR/FIL LOAD/DUMP EXTERNAL TO EXTRACT MSGS'         
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
*               FORMAT OF PARAM CARD IS :-                                      
*                          ANY OMITTED PARAMS ARE ASSUMED 'ALL'                 
*                                                                               
*               >>>>>>>>   ALL OTHER RECORDS WILL BE DROPPED    <<<<<<          
*                                                                               
*                          SYS=AAA   - SYSTEM TO BE EXTRACTED FOR               
*                          PROG=XX   - PROGRAM TO BE EXTRACTED FOR              
*                          SCRN=XX   - SCREEN TO BE EXTRACTED FOR               
*                          TYPE=I    - MESSAGE TYPE TO EXTRACT                  
*                          LANG=AAA  - LANGUAGE TO EXTRACT                      
*                          START=NN  - LOW MSG NUMBER TO EXTRACT                
*                          HIGH=NN   - HIGH MSG NUMBER TO EXTRACT               
*                          MESSAGE=N - DON'T EXTRACT MESSAGE RECORDS            
*                          HELP=N    - DON'T EXTRACT HELP RECORDS               
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 (WORKX-WORKD),MPLDMSG                                            
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         AP    RECKEEP,=P'1'                                                    
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         AP    RECDEL,=P'1'                                                     
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
*                                                                               
         ICM   R2,15,APARAMC                                                    
         BNZ   *+6                                                              
         DC    H'0'                NO PARM=CARD                                 
         LA    R0,SCANWRK                                                       
         LA    R1,L'SCANWRK                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),SCANWRK                             
         CLI   4(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO PARM=CARD                                 
         ZIC   R5,4(R1)            R5=N'PARAMS                                  
         LA    R3,SCANWRK                                                       
*                                                                               
DMXI002  CLC   12(3,R3),=C'SYS'                                                 
         BE    DMXI010                                                          
         CLC   12(4,R3),=C'TYPE'                                                
         BE    DMXI020                                                          
         CLC   12(4,R3),=C'LANG'                                                
         BE    DMXI030                                                          
         CLC   12(4,R3),=C'START'                                               
         BE    DMXI040                                                          
         CLC   12(3,R3),=C'END'                                                 
         BE    DMXI050                                                          
         CLC   12(4,R3),=C'HELP'                                                
         BE    DMXI060                                                          
         CLC   12(4,R3),=C'PROG'                                                
         BE    DMXI070                                                          
         CLC   12(4,R3),=C'SCRN'                                                
         BE    DMXI080                                                          
         CLC   12(7,R3),=C'MESSAGE'                                             
         BE    DMXI090                                                          
         DC    H'0'                UNRECOGNISED PARAMETER                       
         EJECT                                                                  
* VALIDATE SYSTEM NAME - MAY BE NUMERIC                                         
*                                                                               
DMXI010  CLI   REQSYS,0                                                         
         BE    *+6                                                              
         DC    H'0'                MULTIPLE SYS= PARAMETERS                     
         ZIC   R1,1(R3)            L'INPUT                                      
         BCTR  R1,0                                                             
         LA    RE,SYSGEN           DUMMY SYSLIST ENTRY FOR GENERAL              
         USING SYSLSTD,RE                                                       
         EX    R1,*+12                                                          
         BE    DMXI016                                                          
         B     *+10                                                             
         CLC   SYSLNAME(0),22(R3)  CLC C'GENERAL',SCANWRK                       
*                                                                               
         LA    RE,SYSLST                                                        
         LA    RE,6(RE)            RE=A(SYSTEM LIST)                            
*                                                                               
DMXI012  CLI   0(RE),0             TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID SYSTEM                               
         TM    3(R3),X'80'         TEST NUMERIC                                 
         BZ    DMXI013                                                          
         CLC   11(1,R3),SYSLNUM                                                 
         BE    DMXI016                                                          
         B     DMXI014                                                          
*                                                                               
DMXI013  EX    R1,*+8              MATCH INPUT TO TABLE                         
         B     *+10                                                             
         CLC   SYSLNAME(0),22(R3)                                               
         BE    DMXI016                                                          
DMXI014  LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DMXI012                                                          
         SPACE 2                                                                
DMXI016  MVC   REQSYS,SYSLNUM      SAVE SYSTEM NUMBER                           
         CLI   REQSYS,0                                                         
         BNE   *+8                                                              
         MVI   REQSYS,X'FF'        REQUEST FOR GENERAL SYSTEM MSGS              
         B     DMXI100                                                          
         DROP  RE                                                               
         EJECT                                                                  
* VALIDATE TYPE= REQUEST                                                        
*                                                                               
DMXI020  CLI   REQTYPE,0                                                        
         BE    *+6                                                              
         DC    H'0'                MULTIPLE TYPE= PARAMS                        
         CLI   1(R3),1             ONLY 1 CHR INPUT                             
         BE    *+6                                                              
         DC    H'0'                INVALID TYPE= PARAM                          
         LA    RE,MTYPTAB                                                       
DMXI022  CLI   0(RE),0             EOT                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,RE),22(R3)                                                   
         BE    DMXI024                                                          
         LA    RE,1(RE)                                                         
         B     DMXI022                                                          
DMXI024  MVC   REQTYPE,0(RE)                                                    
         B     DMXI100                                                          
         EJECT                                                                  
* VALIDATE LANG= REQUEST                                                        
*                                                                               
DMXI030  CLI   REQLANG,0                                                        
         BE    *+6                                                              
         DC    H'0'                MULTIPLE LANG= PARAMS                        
         LA    R1,LANGTAB                                                       
         LH    RE,0(R1)            L'TABLE ENTRIES                              
         L     RF,2(R1)            A(LANGUAGE TABLE END)                        
         LA    R1,6(R1)            A(FIRST TABLE ENTRY)                         
         USING LANGTABD,R1         R1=A(LANGUAGE TABLE)                         
         ZIC   R2,1(R3)            R2=L'INPUT-1                                 
         BCTR  R2,0                                                             
DMXI032  CLM   R2,1,=AL1(L'LANGSHR) TEST IF LONGER THAN SH NAME                 
         BH    DMXI034                                                          
         EX    R2,*+8              MATCH INPUT TO SHORT NAME                    
         B     *+10                                                             
         CLC   LANGSHR(0),22(R3)   ENGLISH SHORT NAME                           
         BE    DMXI036                                                          
*                                                                               
DMXI034  EX    R2,*+8              MATCH INPUT TO FULL NAME                     
         B     *+10                                                             
         CLC   LANGFUL(0),22(R3)   ENGLISH FULL NAME                            
         BE    DMXI036                                                          
         BXLE  R1,RE,DMXI032       NO - BUMP TO NEXT TABLE ENTRY                
         DC    H'0'                INVALID LANGUAGE NAME                        
DMXI036  MVC   REQLANG,LANGCODE                                                 
         XI    REQLANG,X'FF'       SWAP TO KEY SEQUENCE CODE                    
         B     DMXI100                                                          
         DROP  R1                                                               
         EJECT                                                                  
* VALIDATE START= REQUEST                                                       
*                                                                               
DMXI040  OC    REQSTART,REQSTART   START MESSAGE NUMBER OPTION                  
         BZ    *+6                                                              
         DC    H'0'                MULTIPLE START= PARAMS                       
         TM    3(R3),X'80'                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OC    8(2,R3),8(R3)       HALF WORD MAX                                
         BZ    *+6                                                              
         DC    H'0'                MESSAGE NUMBER TOO LARGE                     
         MVC   REQSTART,10(R3)                                                  
         B     DMXI100                                                          
         EJECT                                                                  
* VALIDATE END= REQUEST                                                         
*                                                                               
DMXI050  OC    REQEND,REQEND       END MESSAGE NUMBER OPTION                    
         BZ    *+6                                                              
         DC    H'0'                MULTIPLE END= PARAMS                         
         TM    3(R3),X'80'                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OC    8(2,R3),8(R3)       HALF WORD MAX                                
         BZ    *+6                                                              
         DC    H'0'                MESSAGE NUMBER TOO LARGE                     
         MVC   REQEND,10(R3)                                                    
         B     DMXI100                                                          
* VALIDATE HELP=Y/N                                                             
*                                                                               
DMXI060  CLI   HELPRECS,0                                                       
         BE    *+6                                                              
         DC    H'0'                MULTIPLE HELP= PARAMS                        
         CLI   1(R3),1             ONLY 1 CHR INPUT                             
         BE    *+6                                                              
         DC    H'0'                INVALID HELP= PARAM                          
         CLI   22(R3),C'Y'                                                      
         BE    DMXI062                                                          
         CLI   22(R3),C'N'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID HELP=?                               
DMXI062  MVC   HELPRECS,22(R3)                                                  
         B     DMXI100                                                          
* VALIDATE PROG=XX                                                              
*                                                                               
DMXI070  CLI   REQPROG,0                                                        
         BE    *+6                                                              
         DC    H'0'                MULTIPLE PROG= PARAMS                        
         CLI   1(R3),2             ONLY 2 CHR INPUT                             
         BE    *+6                                                              
         DC    H'0'                INVALID PROG= PARAM                          
         GOTO1 =V(HEXIN),DMCB,22(R3),REQPROG,2                                  
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+6                                                              
         DC    H'0'                INVALID PROG= PARAM                          
         B     DMXI100                                                          
* VALIDATE SCRN=XX                                                              
*                                                                               
DMXI080  CLI   REQSCRN,0                                                        
         BE    *+6                                                              
         DC    H'0'                MULTIPLE SCRN= PARAMS                        
         CLI   1(R3),2             ONLY 2 CHR INPUT                             
         BE    *+6                                                              
         DC    H'0'                INVALID SCRN= PARAM                          
         GOTO1 =V(HEXIN),DMCB,22(R3),REQSCRN,2                                  
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+6                                                              
         DC    H'0'                INVALID SCRN= PARAM                          
         B     DMXI100                                                          
* VALIDATE MESSAGE=Y/N                                                          
*                                                                               
DMXI090  CLI   MESSRECS,0                                                       
         BE    *+6                                                              
         DC    H'0'                MULTIPLE MESSAGE= PARAMS                     
         CLI   1(R3),1             ONLY 1 CHR INPUT                             
         BE    *+6                                                              
         DC    H'0'                INVALID MESSAGE= PARAM                       
         CLI   22(R3),C'Y'                                                      
         BE    *+8                                                              
         CLI   22(R3),C'N'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID MESSAGE=?                            
         MVC   MESSRECS,22(R3)                                                  
         B     DMXI100                                                          
         EJECT                                                                  
DMXI100  LA    R3,32(R3)           A(NEXT SCANWRK AREA)                         
         BCT   R5,DMXI002                                                       
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R2,AREC                                                          
         TM    34(R2),X'80'        ALREADY DELETED                              
         BNZ   DMXPURGE                                                         
         CLI   HELPRECS,C'N'       DONT EXTRACT HELP RECORDS                    
         BE    DMXR005                                                          
         USING HV1KEYD,R2                                                       
         CLI   HV1KSYS,HV1KSYSQ    TEST FOR HELP RECS                           
         BNE   DMXR005                                                          
         CLI   HV1TYPE,HV1TYPEQ                                                 
         BNE   DMXR005                                                          
         CLI   REQLANG,0           SPECIFIC LANGUAGE ONLY                       
         BE    *+14                                                             
         CLC   HV1LANG,REQLANG                                                  
         BNE   DMXPURGE                                                         
         CLI   REQSYS,0            NO SPECIFIC SYSTEM                           
         BE    DMXKEEP                                                          
         CLC   HV1SYS,REQSYS       ONLY EXTRACT SPECIFIED SYSTEM                
         BNE   DMXPURGE            NOTE SYS0 DOES NOT APPLY TO HELP             
         CLI   REQPROG,0           NO SPECIFIC PROGRAM                          
         BE    DMXKEEP                                                          
         CLC   HV1PROG,REQPROG     ONLY EXTRACT SPECIFIED PROGRAM               
         BNE   DMXPURGE                                                         
         CLI   REQSCRN,0           NO SPECIFIC SCREEN                           
         BE    DMXKEEP                                                          
         CLC   HV1SCRN,REQSCRN     ONLY EXTRACT SPECIFIED SCREEN                
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
         USING GMSGD,R2                                                         
DMXR005  OC    GMKMAJ(2),GMKMAJ    NO MAJOR/MINOR SYS FOR MSGS                  
         BNZ   DMXPURGE                                                         
         CLI   GMKREC,GMKRECQ      CHECK IT'S A MESSAGE RECORD                  
         BNE   DMXPURGE                                                         
         CLI   MESSRECS,C'N'       TEST MESSAGE RECORDS REQUIRED                
         BE    DMXPURGE                                                         
         CLI   REQSYS,X'FF'        SPECIFIC SYS0 REQUEST (GENERAL)              
         BNE   DMXR010                                                          
         CLI   GMKSYS,0            SYS ZERO RECS O.K.                           
         BE    DMXR020                                                          
         B     DMXPURGE                                                         
*                                                                               
DMXR010  CLI   REQSYS,0            SYSTEM SPECIFIC REQUEST                      
         BE    DMXR020                                                          
         CLC   GMKSYS,REQSYS                                                    
         BNE   DMXPURGE                                                         
         SPACE 2                                                                
DMXR020  CLI   REQTYPE,0                                                        
         BE    DMXR030                                                          
         CLC   GMKTYP,REQTYPE                                                   
         BNE   DMXPURGE                                                         
         SPACE 2                                                                
DMXR030  CLI   REQLANG,0                                                        
         BE    DMXR040                                                          
         CLC   GMKLANG,REQLANG                                                  
         BNE   DMXPURGE                                                         
         SPACE 2                                                                
DMXR040  OC    REQSTART,REQSTART                                                
         BZ    DMXR050                                                          
         CLC   GMKMSG,REQSTART                                                  
         BL    DMXPURGE                                                         
         SPACE 2                                                                
DMXR050  OC    REQEND,REQEND                                                    
         BZ    DMXR060                                                          
         CLC   GMKMSG,REQEND                                                    
         BH    DMXPURGE                                                         
         SPACE 2                                                                
DMXR060  B     DMXKEEP                                                          
*                                                                               
*                                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(11),=C'ACTIVE RECS'                                         
         EDIT  (P4,RECKEEP),(8,P)                                               
         AP    RECKEEP,RECKEEP                                                  
         BNZ   *+10                                                             
         MVC   P,=8C'.'                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(12),=C'DELETED RECS'                                        
         EDIT  (P4,RECDEL),(8,P)                                                
         AP    RECDEL,RECDEL                                                    
         BNZ   *+10                                                             
         MVC   P,=8C'.'                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
RECDEL   DC    PL4'0'                                                           
RECKEEP  DC    PL4'0'                                                           
*                                                                               
REQSYS   DC    XL1'00'                                                          
REQPROG  DC    XL1'00'                                                          
REQSCRN  DC    XL1'00'                                                          
REQTYPE  DC    XL1'00'                                                          
REQLANG  DC    XL1'00'                                                          
REQSTART DC    XL2'00'                                                          
REQEND   DC    XL2'00'                                                          
HELPRECS DC    XL1'00'                                                          
MESSRECS DC    XL1'00'                                                          
         SPACE 2                                                                
*FASYSLST                                                                       
       ++INCLUDE FASYSLST                                                       
         SPACE 2                                                                
*FALANGTAB                                                                      
       ++INCLUDE FALANGTAB                                                      
         SPACE 2                                                                
MTYPTAB  DS    0CL1                                                             
         DC    AL1(GMKTERR)                                                     
         DC    AL1(GMKTINF)                                                     
         DC    AL1(GMKTSCR)                                                     
         DC    AL1(GMKTTXT)                                                     
         DC    AL1(GMKTWRN)                                                     
         DC    AL1(0)                                                           
         SPACE 1                                                                
SYSGEN   DC    X'0000',C'GENERALGEN',C'  ',X'0000' DUMMY SYSLIST ENTRY          
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
SCANWRK  DS    0CL(20*32)                                                       
SCANLIN  DS    20CL32                                                           
*                                                                               
WORK     DS    CL128                                                            
WORKX    DS    0C                                                               
         EJECT                                                                  
*FASYSLSTD                                                                      
       ++INCLUDE FASYSLSTD                                                      
*FALANG                                                                         
       ++INCLUDE FALANG                                                         
         EJECT                                                                  
*GEGENMSG                                                                       
       ++INCLUDE GEGENMSG                                                       
         EJECT                                                                  
*CTGENHV1                                                                       
       ++INCLUDE CTGENHV1                                                       
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005GELDMSGA  12/11/90'                                      
         END                                                                    
