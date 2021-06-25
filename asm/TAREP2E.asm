*          DATA SET TAREP2E    AT LEVEL 014 AS OF 08/13/14                      
*PHASE T7032EC,*                                                                
*INCLUDE TALIM                                                                  
         TITLE 'T7032E - W2 AUDIT AND W2C FORMS'                                
T7032E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T7032E,R7,R8,CLEAR=YES                             
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         GOTO1 INITIAL,DMCB,0                                                   
         BAS   RE,MYCLEAR                                                       
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         SPACE 1                                                                
         LA    R2,SPLYEARH         YEAR                                         
         GOTO1 ANY                                                              
         CLI   5(R2),4             MUST BE 4 CHARACTERS                         
         BNE   INVXIT                                                           
         MVC   YEAR,8(R2)                                                       
         MVC   WORK(4),=C'0000'                                                 
         MVZ   WORK(4),8(R2)                                                    
         CLC   WORK(4),=C'0000'                                                 
         BNE   INVXIT                                                           
         MVC   TIFYEAR(2),10(R2)                                                
         SPACE 1                                                                
         LA    R2,SPLEMPH          EMPLOYER                                     
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VREC2                                                            
         GOTO1 ANY                                                              
         MVC   TIFEMP,WORK                                                      
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2),0                                       
         SPACE 1                                                                
VREC2    LA    R2,SPLPERH          PERIOD                                       
         GOTO1 ANY                 (REQUIRED)                                   
         ST    R2,APERH                                                         
         GOTO1 VALPERD                                                          
         SPACE 1                                                                
         LA    R2,SPLUNTH          UNIT                                         
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VREC4                                                            
         GOTO1 ANY                                                              
         MVC   TIFUNIT,WORK                                                     
         GOTO1 TAXVAL,DMCB,(3,8(R2))                                            
         BNE   INVXIT                                                           
         SPACE 1                                                                
VREC4    LA    R2,SPLSSNH          SS#                                          
         CLI   5(R2),0             (OPTIONAL)                                   
         BE    VREC6                                                            
         GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLW4CDQ,(R2),0                                       
         MVC   TIFSSN,TGSSN                                                     
         SPACE 1                                                                
VREC6    LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         BAS   RE,VOPTS                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VOPTS    NTR1                                                                   
         ZAP   TRALIMIT,=P'0'                                                   
         ZAP   RECLIMIT,=P'99999999'                                            
         ZAP   REPLIMIT,=P'0'                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
OPT2     CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT4                                                             
         MVI   TRACOPT,C'Y'                                                     
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    OPTEND                                                           
         CVD   R1,DUB                                                           
         ZAP   TRALIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     DS    0H                                                               
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     BADXIT                                                           
         SPACE 1                                                                
INVXIT   MVC   CONHEAD(L'INVERR),INVERR                                         
         SPACE 1                                                                
BADXIT   GOTO1 ERRXIT                                                           
         SPACE 1                                                                
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
INVERR   DC    C'** ERROR ** INVALID DATA'                                      
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
TRALIMIT DC    PL6'0'                                                           
RECLIMIT DC    PL6'9999999'                                                     
REPLIMIT DC    PL6'0'                                                           
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
PREP     NTR1                                                                   
         GOTO1 DATCON,DMCB,(5,0),(1,PTODAY)                                     
         CLI   RECNUM,W2C                                                       
         BE    PREP2                                                            
         SPACE 1                                                                
         L     R5,ASPOOLD          INITIALIZED FOR AUDIT                        
         USING SPOOLD,R5                                                        
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVC   MYSORTER,SORTER                                                  
         DROP  R5                                                               
         MVC   MYTITLE,MYSPACES                                                 
         MVC   MYTITLE(8),=C'W2 AUDIT'                                          
         B     PREP4                                                            
         SPACE 1                                                                
PREP2    BAS   RE,LINEUP           INITIALIZE FOR W2C FORMS                     
         SPACE 1                                                                
PREP4    MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         MVI   TIREAD,TLW2CDQ      SET TO READ W2 RECORDS                       
         MVI   TIFCUR,C'U'         ONLY INTERESTED IN $US                       
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         BAS   RE,PRAUDIT          PRINT LAST AUDIT IF RELEVANT                 
         CLI   RECNUM,W2C                                                       
         BE    XIT                                                              
         BAS   RE,SORTTOTS         THEN SORT                                    
         BAS   RE,PRINTOTS         AND PRINT THE TOTALS                         
         B     XIT                                                              
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
         BAS   RE,PROCW2           PROCESS A W2 RECORD                          
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS A W2 RECORD                                              
         SPACE 3                                                                
PROCW2   NTR1                                                                   
         L     R4,TIAREC                                                        
         USING TLW2D,R4                                                         
         CLC   THISSSN,TLW2SSN                                                  
         BE    PROCW22                                                          
         OC    THISSSN,THISSSN     IF CHANGE OF W2                              
         BZ    *+8                 AND THIS IS NOT FIRST TIME                   
         BAS   RE,PRAUDIT          PRINT AN AUDIT                               
         XC    AFIRSTW2,AFIRSTW2                                                
         XC    ALASTW2,ALASTW2                                                  
         MVC   THISSSN,TLW2SSN                                                  
         MVC   THISEMP,TLW2EMP                                                  
         SPACE 1                                                                
PROCW22  MVC   WORK(3),TLW2CDTE    CHECK IF CHANGE DATE FITS                    
         XC    WORK(3),=X'FFFFFF'                                               
         CLC   WORK(3),TIQPSTR                                                  
         BL    PROCW24                                                          
         CLC   WORK(3),TIQPEND                                                  
         BH    XIT                                                              
         SPACE 1                                                                
*                                  IT DOES - SAVE IF WE DON'T HAVE              
*                                  LAST YET. WE NEED THE LATEST                 
         OC    ALASTW2,ALASTW2                                                  
         BNZ   XIT                                                              
         L     R2,=A(LASTW2)       (WE SEE THE LAST RECORD FIRST)               
         ST    R2,ALASTW2                                                       
         MOVE  ((R2),4000),(R4)                                                 
         B     XIT                                                              
         SPACE 1                                                                
*                                  RECORD IS BEFORE CHANGE RANGE                
*                                  WE JUST NEED THE LATEST                      
*                                  - A PICTURE OF W2 BEFORE THE PERIOD          
PROCW24  OC    AFIRSTW2,AFIRSTW2                                                
         BNZ   XIT                                                              
         L     R2,=A(FIRSTW2)                                                   
         ST    R2,AFIRSTW2                                                      
         MOVE  ((R2),4000),(R4)                                                 
         B     XIT                                                              
         EJECT                                                                  
*              PRINT AN AUDIT REPORT                                            
         SPACE 3                                                                
*                                  COMPARING 2 W2 RECORDS                       
*                                  FIRST MAY BE AT AFIRSTW2                     
*                                  SECOND MUST BE AT ALASTW2                    
         SPACE 1                                                                
PRAUDIT  NTR1                                                                   
         CLI   RECNUM,W2C                                                       
         BE    PRW2C                                                            
         OC    ALASTW2,ALASTW2     NEED A CURRENT W2 RECORD                     
         BZ    XIT                                                              
         AP    W2COUNT,=P'1'                                                    
         CP    W2COUNT,TRALIMIT                                                 
         BH    PRAUD2                                                           
         BAS   RE,TRACEINP                                                      
         SPACE 1                                                                
PRAUD2   L     R4,ALASTW2                                                       
         USING TLW2D,R4                                                         
         LA    R2,MYP                                                           
         USING MYPRINT,R2                                                       
         LA    R1,MYSSN            EDIT SS#                                     
         BAS   RE,GETSSN                                                        
         BAS   RE,NEEDW4                                                        
         MVC   MYNAME,NEEDNAME                                                  
         GOTO1 SQUASHER,DMCB,MYNAME,33                                          
         SPACE 1                                                                
         L     R6,ALASTW2          GET PRINTED DATE OUT OF LAST                 
         MVI   ELCODE,TAWSELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAWSD,R6                                                         
         OC    TAWSDPRT,TAWSDPRT                                                
         BZ    PRAUD4                                                           
         GOTO1 DATCON,DMCB,(1,TAWSDPRT),(8,MYPDATE)                             
         SPACE 1                                                                
PRAUD4   L     R6,ALASTW2          GET ACTIVITY OUT OF LAST                     
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL                                                         
         USING TAACD,R6                                                         
         BNE   XIT                                                              
         MVC   WORK(3),TLW2CDTE                                                 
         XC    WORK(3),=X'FFFFFF'                                               
         GOTO1 DATCON,DMCB,(1,WORK),(8,MYCHANGE)                                
         LA    R2,MYP2                                                          
         MVC   MYCHANGE,TAACSTAF                                                
         BAS   RE,POSTELS          GET UNIT ELEMENTS FROM BOTH                  
         BAS   RE,SORTELS          SORT THESE                                   
         BAS   RE,PRINTELS         AND PRINT DIFFERENCES                        
         B     XIT                                                              
         EJECT                                                                  
*              UNIT ELEMENT ROUTINES - POST AND SORT                            
         SPACE 3                                                                
POSTELS  NTR1                                                                   
         MVI   NPOOL,0                                                          
         L     RE,=A(ELPOOL)                                                    
         LH    RF,=Y(L'ELPOOL)                                                  
         XCEFL                                                                  
         L     R6,AFIRSTW2         PICK ELEMENTS FROM FIRST                     
         MVI   FLSW,C'F'                                                        
         LTR   R6,R6               (IF AROUND)                                  
         BZ    *+8                                                              
         BAS   RE,POSTELS2                                                      
         L     R6,ALASTW2          AND LAST RECORDS                             
         MVI   FLSW,C'L'                                                        
         BAS   RE,POSTELS2                                                      
         B     XIT                                                              
         SPACE 1                                                                
POSTELS2 NTR1                                                                   
         MVI   ELCODE,TAW2ELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
POSTELS4 BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TAW2D,R6                                                         
         CLC   TAW2UNIT,=C'CN '    IGNORE CANADA                                
         BE    POSTELS4                                                         
         CLC   =C'LAX',TAW2UNIT    IGNORE LAX (FAKE CA CITY)                    
         BE    POSTELS4                                                         
         ZIC   R1,NPOOL            UPDATE POOL COUNTER                          
         LR    R2,R1                                                            
         CH    R1,=Y(NPOOLENT)                                                  
         BNL   XIT                                                              
         LA    R1,1(R1)                                                         
         STC   R1,NPOOL                                                         
         MH    R2,=Y(LPOOLENT)                                                  
         A     R2,=A(ELPOOL)       DISPLACE INTO POOL                           
         USING POOLD,R2                                                         
         MVC   POOLUNIT,TAW2UNIT   AND SAVE DETAILS                             
         MVC   POOLFL,FLSW                                                      
         MVC   POOLEARN,TAW2EARN                                                
         MVC   POOLTAX,TAW2TAX                                                  
         MVC   POOLFICA,TAW2FICA   SUI                                          
         MVC   POOLREXP,TAW2REXP   SDI                                          
         BAS   RE,POSTTOTS                                                      
         B     POSTELS4                                                         
SORTELS  NTR1                                                                   
         L     R2,=A(ELPOOL)                                                    
         ZIC   R3,NPOOL                                                         
         LA    R4,LPOOLENT                                                      
         GOTO1 XSORT,DMCB,(0,(R2)),(R3),20,4,0                                  
         B     XIT                                                              
         EJECT                                                                  
*              POST UNIT INTO TOTALS POOL                                       
         SPACE 3                                                                
*              INPUT               R2=A(POOL LINE)                              
         SPACE 1                                                                
         USING POOLD,R2                                                         
POSTTOTS NTR1                                                                   
         L     R3,=A(TOTPOOL)      CHECK IN TOTAL POOL                          
         LA    R1,1                                                             
         SPACE 1                                                                
PTOT2    CLC   0(3,R3),POOLUNIT    LOOK FOR A MATCH ON UNIT                     
         BE    PTOT6                                                            
         CLI   0(R3),0             OR AN EMPTY SLOT                             
         BE    PTOT4                                                            
         LA    R3,LPOOLENT(R3)                                                  
         LA    R1,1(R1)                                                         
         B     PTOT2                                                            
         SPACE 1                                                                
PTOT4    STC   R1,NTPOOL           NOTE NUMBER IN TOTAL POOL                    
         SPACE 1                                                                
PTOT6    MVC   0(3,R3),POOLUNIT         NOTE THIS UNIT                          
         LA    RE,POOLEARN-POOLD(R3)    A(TOTAL ACCUMS)                         
         LA    RF,POOLEARN              A(THIS ACCUMS)                          
         LA    R0,4                     4 TO ADD                                
         SPACE 1                                                                
PTOT8    L     R1,0(RF)            THIS IS THE NEW VALUE                        
         CLI   POOLFL,C'F'         WHICH WE SUBTRACT IF FIRST                   
         BNE   *+6                                                              
         LCR   R1,R1                                                            
         A     R1,0(RE)            ADD IN CURRENT                               
         ST    R1,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,PTOT8            REPEAT FOR 4 ACCUMS                          
         B     XIT                                                              
         SPACE 1                                                                
SORTTOTS NTR1                                                                   
         L     R2,=A(TOTPOOL)                                                   
         ZIC   R3,NTPOOL                                                        
         LA    R4,LPOOLENT                                                      
         GOTO1 XSORT,DMCB,(0,(R2)),(R3),20,4,0                                  
         B     XIT                                                              
         EJECT                                                                  
*              PRINT FINAL TOTALS                                               
         SPACE 3                                                                
PRINTOTS NTR1                                                                   
         CLI   NTPOOL,0                                                         
         BE    XIT                                                              
         BAS   RE,MYCLEAR                                                       
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         DROP  R5                                                               
         LA    R4,MYP                                                           
         USING MYPRINT,R4                                                       
         MVC   MYPDATE,=C'*TOTALS*'                                             
         L     R2,=A(TOTPOOL)                                                   
         USING POOLD,R2                                                         
         ZIC   R3,NTPOOL                                                        
         SPACE 1                                                                
PRTOT2   BAS   RE,FPOOL                                                         
         BAS   RE,SPLAT                                                         
         LA    R2,LPOOLENT(R2)                                                  
         BCT   R3,PRTOT2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              UNIT ELEMENT ROUTINES - PRINT                                    
         SPACE 3                                                                
PRINTELS NTR1                                                                   
         L     R2,=A(ELPOOL)                                                    
         USING POOLD,R2                                                         
         ZIC   R3,NPOOL                                                         
         SPACE 1                                                                
PELS2    CLC   POOLUNIT,POOLUNIT+20          SAME UNIT?                         
         BNE   PELS6                                                            
         CLC   POOLEARN(16),POOLEARN+20      ANY CHANGES                        
         BE    PELS4                                                            
         SPACE 1                                                                
         LA    R4,MYP              FORMAT OLD ON LINE 1                         
         USING MYPRINT,R4                                                       
         BAS   RE,FPOOL                                                         
         MVC   MYOLDNEW,=C'OLD'                                                 
         LA    R4,MYP2             THEN NEW ON LINE 2                           
         AH    R2,=Y(LPOOLENT)                                                  
         BAS   RE,FPOOL                                                         
         MVC   MYOLDNEW,=C'NEW'                                                 
         SH    R2,=Y(LPOOLENT)                                                  
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         SPACE 1                                                                
PELS4    AH    R2,=Y(LPOOLENT)                                                  
         BCTR  R3,0                                                             
         AH    R2,=Y(LPOOLENT)                                                  
         BCT   R3,PELS2                                                         
         B     XIT                                                              
         SPACE 1                                                                
PELS6    LA    R4,MYP              ADD/DELETE ON LINE 1                         
         BAS   RE,FPOOL                                                         
         MVC   MYOLDNEW,=C'DEL'                                                 
         CLI   POOLFL,C'L'                                                      
         BNE   *+10                                                             
         MVC   MYOLDNEW,=C'ADD'                                                 
         CLC   MYP2,MYSPACES                                                    
         BNE   *+8                                                              
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         AH    R2,=Y(LPOOLENT)                                                  
         BCT   R3,PELS2                                                         
         B     XIT                                                              
         SPACE 1                                                                
FPOOL    NTR1                                                                   
         USING POOLD,R2                                                         
         GOTO1 TAXVAL,DMCB,(3,POOLUNIT)                                         
         MVC   MYUNIT,TGTANAME                                                  
         EDIT  (4,POOLEARN),(12,MYEARN),2,MINUS=YES,ZERO=BLANK                  
         EDIT  (4,POOLTAX),(12,MYTAX),2,MINUS=YES,ZERO=BLANK                    
         EDIT  (4,POOLFICA),(12,MYFICA),2,MINUS=YES,ZERO=BLANK                  
         EDIT  (4,POOLREXP),(12,MYREXP),2,MINUS=YES,ZERO=BLANK                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT W2 CHANGE FORMS                                 
         SPACE 3                                                                
*                                  COMPARING 2 W2 RECORDS                       
*                                  FIRST MAY BE AT AFIRSTW2                     
*                                  SECOND MUST BE AT ALASTW2                    
         SPACE 1                                                                
PRW2C    OC    ALASTW2,ALASTW2     NEED A CURRENT W2 RECORD                     
         BZ    XIT                                                              
         AP    W2COUNT,=P'1'                                                    
         CP    W2COUNT,TRALIMIT                                                 
         BH    PRW2C2                                                           
         BAS   RE,TRACEINP                                                      
         SPACE 1                                                                
PRW2C2   BAS   RE,NEEDEM           ENSURE EMPLOYER                              
         BAS   RE,NEEDW4           AND W4                                       
         MVC   W4NAME,NEEDNAME                                                  
         MVC   W4ADD,NEEDADD                                                    
         BAS   RE,POSTELS          BUILD A POOL FOR THIS W2                     
         SPACE 1                                                                
PRW2C4   BAS   RE,SORTELS                                                       
         L     R2,=A(ELPOOL)                                                    
         CLI   0(R2),X'FF'                                                      
         BE    XIT                                                              
         BAS   RE,GETFED           DIG OUT THE VALUES                           
         BAS   RE,GETFICA          GET FICA AND MEDCARE VALUES                  
         BAS   RE,GETSTATE                                                      
         BAS   RE,GETLOCAL                                                      
         BAS   RE,DOFORM           AND PRINT A FORM                             
         B     PRW2C4                                                           
         EJECT                                                                  
*              ROUTINE TO PRINT LINEUP PATTERNS                                 
         SPACE 3                                                                
LINEUP   NTR1                                                                   
         MVI   W4ADD,C'W'                                                       
         MVC   W4ADD+1(119),W4ADD                                               
         MVC   W4NAME,W4ADD                                                     
         MVI   EMPADD,C'E'                                                      
         MVC   EMPADD+1(119),EMPADD                                             
         MVC   EMPNAME,EMPADD                                                   
         MVC   STATE,=C'ST '                                                    
         MVC   LOCAL,=C'LOC'                                                    
         MVC   THISSSN,=C'123456789'                                            
         L     R2,=F'111111111'                                                 
         L     R3,=F'222222222'                                                 
         LA    R0,2                TWO LINEUP PATTERNS                          
         SPACE 1                                                                
LINEUP2  STM   R2,R3,FICA                                                       
         STM   R2,R3,FICAEARN                                                   
         STM   R2,R3,FEDEARN                                                    
         STM   R2,R3,MEDTAX                                                     
         STM   R2,R3,MEDWAGE                                                    
         STM   R2,R3,FEDTAX                                                     
         STM   R2,R3,STAEARN                                                    
         STM   R2,R3,STATAX                                                     
         STM   R2,R3,STASDI                                                     
         STM   R2,R3,STASUI                                                     
         STM   R2,R3,LOCEARN                                                    
         STM   R2,R3,LOCTAX                                                     
         BAS   RE,DOFORM                                                        
         A     R2,=F'111111111'                                                 
         A     R3,=F'111111111'                                                 
         BCT   R0,LINEUP2                                                       
         XC    THISSSN,THISSSN                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT CHANGE FORMS                                    
         SPACE 3                                                                
DOFORM   NTR1                                                                   
         CLC   FICA(4),FICA+4            LOOK FOR ANY ACTIVITY                  
         BNE   DOFORMOK                                                         
         CLC   FEDEARN(4),FEDEARN+4                                             
         BNE   DOFORMOK                                                         
         CLC   FEDREXP(4),FEDREXP+4                                             
         BNE   DOFORMOK                                                         
         CLC   MEDWAGE(4),MEDWAGE+4                                             
         BNE   DOFORMOK                                                         
         CLC   STAEARN(4),STAEARN+4                                             
         BNE   DOFORMOK                                                         
         CLC   LOCEARN(4),LOCEARN+4                                             
         BNE   DOFORMOK                                                         
         CLC   FEDTAX(4),FEDTAX+4                                               
         BNE   DOFORMOK                                                         
         CLC   MEDTAX(4),MEDTAX+4                                               
         BNE   DOFORMOK                                                         
         CLC   STATAX(4),STATAX+4                                               
         BNE   DOFORMOK                                                         
         CLC   STASDI(4),STASDI+4                                               
         BNE   DOFORMOK                                                         
         CLC   STASUI(4),STASUI+4                                               
         BNE   DOFORMOK                                                         
         CLC   LOCTAX(4),LOCTAX+4                                               
         BNE   DOFORMOK                                                         
         B     XIT                                                              
         SPACE 1                                                                
DOFORMOK MVC   MYP+6(2),TIFYEAR                                                 
         BAS   RE,SPLAT            LINE 1 - YEAR                                
         BAS   RE,SPLAT            LINE 2 - SKIP A LINE                         
         MVC   MYP+4(30),W4NAME                                                 
         MVC   MYP+36(30),EMPNAME                                               
         BAS   RE,SPLAT            LINE 3 - EMPLOYER/EMPLOYEE NAME              
         MVC   MYP+4(30),W4ADD                                                  
         MVC   MYP+36(30),EMPADD                                                
         BAS   RE,SPLAT            LINE 4 - ADDRESSES                           
         MVC   MYP+4(30),W4ADD+30                                               
         MVC   MYP+36(30),EMPADD+30                                             
         BAS   RE,SPLAT            LINE 5 - ADDRESSES                           
         MVC   MYP+4(30),W4ADD+60                                               
         MVC   MYP+36(30),EMPADD+60                                             
         BAS   RE,SPLAT            LINE 6 - ADDRESSES                           
         BAS   RE,SPLAT            LINE 7 - SKIP A LINE                         
*                                                                               
         USING TLW2D,R4                                                         
         LA    R1,MYP+4                                                         
         BAS   RE,GETSSN                                                        
         MVC   MYP+36(L'EMPFDID),EMPFDID                                        
         MVC   TIDUNIT,STATE                                                    
         BAS   RE,GETTID                                                        
         MVC   MYP+53(L'TIDID),TIDID                                            
         BAS   RE,SPLAT            LINE 8 -FEDERAL & STATE NUMBERS              
*                                                                               
         BAS   RE,SPLAT            LINE 9 - SKIP A LINE                         
         BAS   RE,SPLAT            LINE 10- SKIP A LINE                         
         BAS   RE,SPLAT            LINE 11- SKIP A LINE                         
         MVI   MYP+15,C'X'                                                      
         MVI   MYP+44,C'X'                                                      
         BAS   RE,SPLAT            LINE 12- CHECK BOXES                         
         BAS   RE,SPLAT            LINE 13- SKIP A LINE                         
*                                                                               
         LA    R2,FEDTAX                                                        
         BAS   RE,EDLINE           LINE 14- FEDERAL INCOME                      
*                                                                               
         OC    FEDEARN,FEDEARN                                                  
         BNZ   DOFORM2                                                          
         MVC   MYP+20(12),=C'(STATE ONLY)'                                      
         BAS   RE,SPLAT            LINE 15- WAGES, TIPS, OTHER COMP             
         B     DOFORM4                                                          
         SPACE 1                                                                
DOFORM2  LA    R2,FEDEARN         FEDERAL EARNINGS=FEDEARN+REIM. EXPS           
         LA    R1,FEDREXP                                                       
         L     RE,0(R2)                                                         
         A     RE,0(R1)                                                         
         ST    RE,0(R2)                                                         
         L     RE,4(R2)                                                         
         A     RE,4(R1)                                                         
         ST    RE,4(R2)                                                         
         BAS   RE,EDLINE           LINE 15- WAGES, TIPS, OTHER COMP             
*                                                                               
DOFORM4  LA    R2,FICA                                                          
         BAS   RE,EDLINE           LINE 16- SOCIAL SEC TAX WITHELD              
         LA    R2,FICAEARN                                                      
         BAS   RE,EDLINE           LINE 17- SOCIAL SEC WAGES                    
         BAS   RE,SPLAT            LINE 18 -SKIP LINE                           
*                                                                               
         LA    R2,MEDWAGE                                                       
         BAS   RE,EDLINE           LINE 19- MEDICARE WAGES & TIPS               
         LA    R2,MEDTAX                                                        
         BAS   RE,EDLINE           LINE 20- MEDICARE TAX WITHELD                
         OC    STASDI(8),STASDI    ANY SDI TO PRINT ?                           
         BNZ   DOFORM6                                                          
         BAS   RE,SPLAT            LINE 21                                      
         B     DOFORM8                                                          
         SPACE 1                                                                
DOFORM6  MVC   MYP+11(2),STATE                                                  
         MVC   MYP+14(3),=C'SDI'                                                
         LA    R2,STASDI                                                        
         BAS   RE,EDLINE           LINE 21                                      
*                                                                               
DOFORM8  OC    STASUI(8),STASUI    ANY SUI TO PRINT ?                           
         BNZ   *+12                                                             
         BAS   RE,SPLAT            LINE 22                                      
         B     DOFORM9                                                          
         SPACE 1                                                                
         MVC   MYP+11(2),STATE                                                  
         MVC   MYP+14(3),=C'SUI'                                                
         LA    R2,STASUI                                                        
         BAS   RE,EDLINE           LINE 22                                      
*                                                                               
DOFORM9  BAS   RE,SPLAT            LINE 23                                      
         BAS   RE,SPLAT            LINE 24                                      
*                                                                               
         LA    R2,STATAX                                                        
         BAS   RE,EDLINE           LINE 25- STATE TAX WITHELD                   
         MVC   MYP+15(3),STATE                                                  
         LA    R2,STAEARN                                                       
         BAS   RE,EDLINE           LINE 26- STATE WAGES                         
         LA    R2,LOCTAX                                                        
         BAS   RE,EDLINE           LINE 27- LOCAL TAX WITHELD                   
         MVC   MYP+15(3),LOCAL                                                  
         LA    R2,LOCEARN                                                       
         BAS   RE,EDLINE           LINE 28- LOCAL WAGES                         
         CLI   W2SW,1                                                           
         BE    SKIPFORM                                                         
         MVI   W2SW,1                                                           
         BAS   RE,SPLAT            LINE 29                                      
         BAS   RE,SPLAT            LINE 30                                      
         BAS   RE,SPLAT            LINE 31                                      
         BAS   RE,SPLAT            LINE 32                                      
         BAS   RE,SPLAT            LINE 33                                      
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         XC    LINE,LINE           STOP UNNEEDED SKIPPING                       
         B     XIT                                                              
         SPACE 1                                                                
SKIPFORM MVI   W2SW,0                                                           
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR W2 PRINTING                              
         SPACE 3                                                                
GETTID   NTR1                                                                   
         MVC   TIDID,MYSPACES      PASS TIDUNIT - RETURN TIDID                  
         L     R6,ATHISEX                                                       
         MVI   ELCODE,TATIELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
GETTID2  BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TATID,R6                                                         
         CLC   TATIUNIT,TIDUNIT                                                 
         BNE   GETTID2                                                          
         MVC   TIDID,TATIID                                                     
         B     XIT                                                              
         SPACE 1                                                                
TIDUNIT  DS    CL3                                                              
TIDID    DS    CL14                                                             
         SPACE 1                                                                
GETSSN   MVC   0(3,R1),THISSSN                                                  
         MVI   3(R1),C'-'                                                       
         MVC   4(2,R1),THISSSN+3                                                
         MVI   6(R1),C'-'                                                       
         MVC   7(4,R1),THISSSN+5                                                
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO GET FED DATA                                          
         SPACE 3                                                                
*                                  LOOKS IN ELPOOL FOR FED DATA                 
*                                  RETURNS FEDEARN FEDTAX FICA                  
         SPACE 1                                                                
GETFED   NTR1                                                                   
         XC    FEDEARN,FEDEARN                                                  
         XC    FEDTAX,FEDTAX                                                    
         XC    FICA,FICA                                                        
         L     R2,=A(ELPOOL)                                                    
         USING POOLD,R2                                                         
         ZIC   R3,NPOOL                                                         
         SPACE 1                                                                
GETFED2  CLC   POOLUNIT,=C'FD '                                                 
         BNE   GETFED6                                                          
         MVC   POOLUNIT,=X'FFFFFF' MARK THIS ONE USED                           
         CLI   POOLFL,C'L'                                                      
         BE    GETFED4                                                          
         MVC   FEDEARN(4),POOLEARN                                              
         MVC   FEDTAX(4),POOLTAX                                                
         MVC   FICA(4),POOLFICA                                                 
         MVC   FEDREXP(4),POOLREXP                                              
         B     GETFED6                                                          
         SPACE 1                                                                
GETFED4  MVC   FEDEARN+4(4),POOLEARN                                            
         MVC   FEDTAX+4(4),POOLTAX                                              
         MVC   FICA+4(4),POOLFICA                                               
         MVC   FEDREXP+4(4),POOLREXP                                            
         SPACE 1                                                                
GETFED6  LA    R2,LPOOLENT(R2)                                                  
         BCT   R3,GETFED2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET STATE DATA                                        
         SPACE 3                                                                
*                                  LOOKS IN ELPOOL FOR STATE DATA               
*                                  RETURNS STAEARN STATAX STATE STASDI          
*                                  AND STASUI                                   
         SPACE 1                                                                
GETSTATE NTR1                                                                   
         XC    STAEARN,STAEARN                                                  
         XC    STASDI,STASDI                                                    
         XC    STASUI,STASUI                                                    
         XC    STATAX,STATAX                                                    
         XC    STATE,STATE                                                      
         L     R2,=A(ELPOOL)                                                    
         USING POOLD,R2                                                         
         ZIC   R3,NPOOL                                                         
         SPACE 1                                                                
GETSTA1  CLC   POOLUNIT,=C'FD '    REJECT FED                                   
         BE    GETSTA6                                                          
         CLI   POOLUNIT+2,C' '     AND LOCAL UNITS                              
         BH    GETSTA6                                                          
         OC    STATE,STATE         IF FIRST TIME FOR STATE                      
         BNZ   GETSTA2                                                          
         MVC   STATE,POOLUNIT      SET STATE                                    
         BAS   RE,CHKLOC           COUNT # OF LOCALS FOR STATE                  
         B     GETSTA3                                                          
*                                                                               
GETSTA2  CLC   STATE,POOLUNIT      REJECT IF NOT STATE MATCH                    
         BNE   GETSTA6                                                          
*                                                                               
GETSTA3  CLI   NLOC,1              IF NONE OR ONE LOCAL                         
         BH    *+10                                                             
         MVC   POOLUNIT,=X'FFFFFF' MARK THIS ONE USED                           
         SPACE 1                                                                
         CLI   POOLFL,C'L'                                                      
         BE    GETSTA4                                                          
         MVC   STAEARN(4),POOLEARN                                              
         MVC   STATAX(4),POOLTAX                                                
         MVC   STASDI(4),POOLSDI                                                
         MVC   STASUI(4),POOLSUI                                                
         B     GETSTA6                                                          
         SPACE 1                                                                
GETSTA4  MVC   STAEARN+4(4),POOLEARN                                            
         MVC   STATAX+4(4),POOLTAX                                              
         MVC   STASDI+4(4),POOLSDI                                              
         MVC   STASUI+4(4),POOLSUI                                              
         SPACE 1                                                                
GETSTA6  LA    R2,LPOOLENT(R2)                                                  
         BCT   R3,GETSTA1                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO COUNTS NUMBER OF LOCAL CITIES FOR STATE               
         SPACE 3                                                                
*                                  RETUNS NLOC                                  
CHKLOC   NTR1                                                                   
         XC    SVLLOC,SVLLOC      SAVE LAST LOCAL CITY                          
         L     R2,=A(ELPOOL)                                                    
         USING POOLD,R2                                                         
         ZIC   R3,NPOOL                                                         
         SR    R4,R4              COUNTER FOR LOCAL CITIES                      
CHKLOC2  CLI   POOLUNIT+2,C'A'     REJECT NON LOCAL                             
         BL    CHKLOC6                                                          
         CLI   POOLUNIT+2,X'FF'    AND USED UNITS                               
         BE    CHKLOC6                                                          
         OC    SVLLOC,SVLLOC                                                    
         BZ    CHKLOC4                                                          
         CLC   SVLLOC,POOLUNIT                                                  
         BE    CHKLOC6            ALREADY PROCESSED THIS LOCAL CITY             
*                                                                               
CHKLOC4  GOTO1 TAXVAL,DMCB,(3,POOLUNIT)                                         
         CLC   STATE,TGTASTCY                                                   
         BNE   CHKLOC6                                                          
         MVC   SVLLOC,POOLUNIT    SAVED LOCAL ADDED TO COUNT                    
         LA    R4,1(R4)           THIS LOCAL BELONGS TO STATE                   
*                                                                               
CHKLOC6  LA    R2,LPOOLENT(R2)                                                  
         BCT   R3,CHKLOC2                                                       
         STC   R4,NLOC            RETURN NUMBER OF LOCAL CITIES                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET LOCAL DATA                                        
         SPACE 3                                                                
*                                  LOOKS IN ELPOOL FOR LOCAL DATA               
*                                  RETURNS LOCEARN LOCTAX LOCAL                 
         SPACE 1                                                                
GETLOCAL NTR1                                                                   
         XC    LOCEARN,LOCEARN                                                  
         XC    LOCTAX,LOCTAX                                                    
         XC    LOCAL,LOCAL                                                      
         L     R2,=A(ELPOOL)                                                    
         USING POOLD,R2                                                         
         ZIC   R3,NPOOL                                                         
         SPACE 1                                                                
GETLOC2  CLI   POOLUNIT+2,C'A'     REJECT NON LOCAL                             
         BL    GETLOC6                                                          
         CLI   POOLUNIT+2,X'FF'    AND USED UNITS                               
         BE    GETLOC6                                                          
         GOTO1 TAXVAL,DMCB,(3,POOLUNIT)                                         
         CLC   STATE,TGTASTCY      PRINT LOCAL FOR MATCHING STATE               
         BNE   GETLOC6                                                          
         OC    LOCAL,LOCAL         OK IF FIRST FOR LOCAL                        
         BZ    GETLOC3                                                          
         CLC   LOCAL,POOLUNIT      OR MATCH ON LOCAL                            
         BNE   GETLOC6                                                          
         SPACE 1                                                                
GETLOC3  MVC   LOCAL,POOLUNIT                                                   
         MVC   POOLUNIT,=X'FFFFFF' MARK THIS ONE USED                           
         CLI   POOLFL,C'L'                                                      
         BE    GETLOC4                                                          
         MVC   LOCEARN(4),POOLEARN                                              
         MVC   LOCTAX(4),POOLTAX                                                
         B     GETLOC6                                                          
         SPACE 1                                                                
GETLOC4  MVC   LOCEARN+4(4),POOLEARN                                            
         MVC   LOCTAX+4(4),POOLTAX                                              
         SPACE 1                                                                
GETLOC6  LA    R2,LPOOLENT(R2)                                                  
         BCT   R3,GETLOC2                                                       
*                                                                               
         SR    RE,RE                                                            
         IC    RE,NLOC                                                          
         BZ    XIT                                                              
         BCTR  RE,0               DECREMENT LOCAL CITY COUNT                    
         STC   RE,NLOC                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET FICA WAGES                                        
         SPACE 3                                                                
GETFICA  NTR1                                                                   
         LA    R0,LIMBLOCK                                                      
         LHI   R1,L'LIMBLOCK                                                    
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    FICAEARN,FICAEARN                                                
         LA    R2,LIMBLOCK                                                      
         USING TMD,R2                                                           
         ST    RC,TMRC                                                          
         MVC   WORK(2),TIFYEAR                                                  
         MVC   WORK+2(4),=C'1231'                                               
         GOTO1 DATCON,DMCB,(0,WORK),(0,TMEFDTE0)  SET INTERNAL FORMAT           
         MVC   TMEMP,TIEMP                                                      
         MVC   TMUNIT,=C'FD'                                                    
         OI    TMSTAT,TMSBSRT                                                   
         SPACE 1                                                                
         GOTO1 =V(TALIM),DMCB,(R2)                                              
         MVC   FICAEARN(4),FEDEARN      SET FICAEARN                            
         CLC   FICAEARN(4),TMBFICA                                              
         BL    *+10                                                             
         MVC   FICAEARN(4),TMBFICA                                              
*                                                                               
         MVC   FICAEARN+4(4),FEDEARN+4                                          
         CLC   FICAEARN+4(4),TMBFICA                                            
         BL    *+10                                                             
         MVC   FICAEARN+4(4),TMBFICA    SET FICAEARN+4                          
*                                                                               
*                                                                               
         MVC   MEDWAGE(4),FEDEARN                                               
         CLC   MEDWAGE(4),TMBMED                                                
         BL    *+10                                                             
         MVC   MEDWAGE(4),TMBMED        SET MEDWAGE                             
*                                                                               
         MVC   MEDWAGE+4(4),FEDEARN+4                                           
         CLC   MEDWAGE+4(4),TMBMED                                              
         BL    *+10                                                             
         MVC   MEDWAGE+4(4),TMBMED      SET MEDWAGE+4                           
*                                                                               
*                                                                               
         L     R1,MEDWAGE               COMPUTE MEDICARE TAX                    
         M     R0,TMRMED                                                        
         D     R0,=F'50000'                                                     
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         C     R1,FICA                  INSURE NOT > TOTAL                      
         BNH   *+8                                                              
         L     R1,FICA                                                          
         ST    R1,MEDTAX                SET MEDTAX                              
*                                                                               
         L     R0,FICA                  FICA = TOTAL - MEDICARE                 
         SR    R0,R1                                                            
         ST    R0,FICA                  SET FICA                                
*                                                                               
*                                                                               
         L     R1,MEDWAGE+4             COMPUTE MEDICARE TAX                    
         M     R0,TMRMED                                                        
         D     R0,=F'50000'                                                     
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         C     R1,FICA+4                INSURE NOT > TOTAL                      
         BNH   *+8                                                              
         L     R1,FICA+4                                                        
         ST    R1,MEDTAX+4              SET MEDTAX+4                            
*                                                                               
         L     R0,FICA+4                FICA = TOTAL - MEDICARE                 
         SR    R0,R1                                                            
         ST    R0,FICA+4                SET FICA+4                              
         B     XIT                                                              
         EJECT                                                                  
*              EDIT A LINE OF NUMBERS FOR W2 CHANGE                             
         SPACE 3                                                                
*              INPUT               R2=A(FIRST - LAST)                           
EDLINE   NTR1                                                                   
         OC    0(4,R2),0(R2)                                                    
         BZ    EDLINE2                                                          
         EDIT  (4,0(R2)),(14,MYP+20),2,MINUS=YES,COMMAS=YES                     
         SPACE 1                                                                
EDLINE2  OC    4(4,R2),4(R2)                                                    
         BZ    EDLINE4                                                          
         EDIT  (4,4(R2)),(14,MYP+36),2,MINUS=YES,COMMAS=YES                     
         SPACE 1                                                                
EDLINE4  L     R1,4(R2)            DIFF                                         
         S     R1,0(R2)                                                         
         EDIT  (R1),(14,MYP+52),2,MINUS=YES,COMMAS=YES                          
         SPACE 1                                                                
EDLINE6  BAS   RE,SPLAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO ENSURE SUB RECORDS AROUND                            
         SPACE 3                                                                
NEEDEM   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     FIRST READ EMTAX RECORD                      
         LA    R4,NEEDKEY                                                       
         USING TLEXD,R4                                                         
         MVI   TLEXCD,TLEXCDQ                                                   
         MVC   TLEXEMP,THISEMP                                                  
         CLC   TLEXEMP,=C'TP '     IF EMPLOYER TP                               
         BNE   NEEDEM05                                                         
         CLC   YEAR,=C'2004'       AND BEFORE 2004                              
         BNL   NEEDEM05                                                         
         MVC   TLEXEMP,=C'TP1'     USE EMPLOYER TP1                             
NEEDEM05 BAS   RE,NEEDREC                                                       
         MVC   ATHISEX,NEEDAREC    AND SAVE ADDRESS OF RECORD                   
*                                                                               
         XC    NEEDKEY,NEEDKEY     ENSURE EMPLOYER AROUND                       
         LA    R4,NEEDKEY                                                       
         USING TLEMD,R4                                                         
         MVI   TLEMCD,TLEMCDQ                                                   
         MVC   TLEMEMP,THISEMP                                                  
         CLC   TLEMEMP,=C'TP '     IF EMPLOYER TP                               
         BNE   NEEDEM07                                                         
         CLC   YEAR,=C'2004'       AND BEFORE 2004                              
         BNL   NEEDEM07                                                         
         MVC   TLEMEMP,=C'TP1'     USE EMPLOYER TP1                             
NEEDEM07 BAS   RE,NEEDREC                                                       
*                                                                               
         MVC   EMPNAME,NEEDNAME    SAVE EMPLOYER NAME                           
         CLC   TLEMEMP(2),=C'TP'                                                
         BNE   NEEDEM10                                                         
         CLC   YEAR,=C'2004'                                                    
         BL    NEEDEM10                                                         
         MVC   EMPNAME,=CL36'TALENT PARTNERS COMMERCIAL SERVICES'               
*                                                                               
NEEDEM10 MVC   EMPADD,NEEDADD      SAVE EMPLOYER ADDRESS                        
         MVC   AIO,NEEDAREC                                                     
         MVC   EMPFDID,MYSPACES    SAVE EMPLOYER FEDERAL ID                     
         MVI   ELCODE,TATIELQ                                                   
         MVI   WORK,TATITYUN                                                    
         MVC   WORK+1(3),=C'FD '                                                
         GOTO1 GETL,DMCB,(4,WORK)                                               
         BNE   XIT                                                              
         L     R6,TGELEM                                                        
         USING TATID,R6                                                         
         MVC   EMPFDID,TATIID                                                   
         B     XIT                                                              
         SPACE 1                                                                
NEEDW4   NTR1                                                                   
         XC    NEEDKEY,NEEDKEY     ENSURE W4 AROUND                             
         LA    R4,NEEDKEY                                                       
         USING TLW4D,R4                                                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,THISSSN                                                  
         BAS   RE,NEEDREC                                                       
         B     XIT                                                              
         EJECT                                                                  
*              GENERAL BUFFER HANDLER                                           
         SPACE 3                                                                
NEEDREC  NTR1                                                                   
         L     R2,ABUFFER                                                       
         LTR   R2,R2                                                            
         BNZ   NREC2                                                            
         L     R0,LBUFFER                                                       
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ABUFFER                                                       
         L     R2,ABUFFER                                                       
         MVC   0(4,R2),=F'150'     SET UP FOR 150 RECORDS                       
         MVC   4(4,R2),=F'4000'    4000 BYTES EACH                              
         XC    8(4,R2),8(R2)                                                    
         LA    RF,150                                                           
         M     RE,=F'4000'                                                      
         LA    RE,12(R2)                                                        
*                                  CLEAR BUFFER FIRST TIME                      
         XCEFL                                                                  
         B     NREC2                                                            
         SPACE 1                                                                
ABUFFER  DC    A(0)                                                             
LBUFFER  DC    F'600016'           (150*4000 + 16)                              
         SPACE 1                                                                
NREC2    DS    0H                  NOW R2 HAS A(BUFFER)                         
*                                  BYTES  1-4 N'ENTRIES                         
*                                  BYTES  5-8 L'ENTRY                           
*                                  BYTES 9-12 NUMBER OF LAST ENTRY              
         LA    R4,12(R2)           BYTES 13+  THE BUFFER!                       
         L     R0,0(R2)                                                         
         SPACE 1                                                                
NREC6    CLC   NEEDKEY,0(R4)       IS MY RECORD IN THE BUFFER?                  
         BE    NREC10                                                           
         A     R4,4(R2)                                                         
         BCT   R0,NREC6                                                         
         SPACE 1                                                                
         MVI   NEEDHIT,C'N'                                                     
         MVC   KEY,NEEDKEY         NO, NOW NEED THE RECORD                      
         GOTO1 HIGH                                                             
         CLC   NEEDKEY(32),KEY                                                  
         BNE   XIT                                                              
         SPACE 1                                                                
NREC8    L     R1,8(R2)            NO - PICK UP N'LAST ENTRY                    
         LA    R1,1(R1)                 ROUND ROBIN                             
         C     R1,0(R2)            HAVE WE GOT TO THE END OF BUFFER?            
         BNH   *+8                                                              
         LA    R1,1                YES, SO GO BACK TO THE BEGINNING             
         ST    R1,8(R2)                                                         
         BCTR  R1,0                                                             
         M     R0,4(R2)            DISPLACE INTO THE BUFFER                     
         LA    R4,12(R1,R2)                                                     
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         L     R2,4(R2)                                                         
         MOVE  ((R4),(R2)),(R3)    MOVE INTO OUR AREA                           
         OC    TIKEY,TIKEY         IS SYSIO READING RECORDS                     
         BZ    NREC10                                                           
         TM    TISTAT,TISTRDCK     UNLESS READING CHECK FILE                    
         BO    NREC10                                                           
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                REREAD TO ESTABLISH SEQUENCE                 
         SPACE 1                                                                
NREC10   ST    R4,NEEDAREC         PASS BACK A RECORD                           
         BAS   RE,GETNAME                                                       
         BAS   RE,GETADD                                                        
         MVI   NEEDHIT,C'Y'                                                     
         B     ITSFINE                                                          
         SPACE 1                                                                
YEAR     DS    CL4                                                              
NEEDAREC DS    A                                                                
NEEDKEY  DC    XL32'00'                                                         
NEEDNAME DS    CL36                                                             
NEEDADD  DS    CL120                                                            
NEEDHIT  DS    CL1                                                              
NEEDTYPE DS    CL1                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
GETNAME  NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDNAME                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVC   NEEDNAME,MYSPACES                                                
         CLI   0(R4),TLW4CDQ                                                    
         BE    GETW4NM                                                          
         MVI   ELCODE,TANAELQ                                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TANAD,R6                                                         
         ZIC   R1,TANALEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     ITSFINE                                                          
         MVC   NEEDNAME(0),TANANAME                                             
         SPACE 1                                                                
GETW4NM  MVI   ELCODE,TAW4ELQ                                                   
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL       RESTORE PRESENT ELCODE                       
         BNE   NOGOOD                                                           
         USING TAW4D,R6                                                         
         MVC   NEEDNAME(16),TAW4NAM1                                            
         MVC   NEEDNAME+17(16),TAW4NAM2                                         
         GOTO1 SQUASHER,DMCB,NEEDNAME,33                                        
         MVC   NEEDTYPE,TAW4TYPE                                                
         B     ITSFINE                                                          
         SPACE 1                                                                
GETADD   NTR1                                                                   
*                                  R4=A(RECORD) TO NEEDSHRT                     
         MVC   SAVEEL,ELCODE       SAVE PRESENT ELCODE                          
         MVC   NEEDADD,MYSPACES                                                 
         CLI   0(R4),TLW4CDQ       TEST HAVE W4 RECORD                          
         BNE   GETADD10                                                         
         LR    R6,R4                                                            
         MVI   ELCODE,TAA2ELQ      TEST FOR NEW STYLE ADDRESS                   
         BAS   RE,GETEL                                                         
         BNE   GETADD10                                                         
         USING TAA2D,R6                                                         
         LA    R2,NEEDADD          R2=WHERE TO SAVE IT IN NEEDADD               
         LA    R4,TAA2ADD1                                                      
         LHI   R0,3                ASSUME 3 ADDRESS LINES                       
         CLI   TAA2LEN,TAA2LNQ     FOR OLD STYLE ADDRESSES                      
         BL    GETADD2                                                          
         CLC   TAA2CTRY,=C'US'     AND NEW STYLE US ADDRESSES                   
         BE    GETADD2                                                          
         LHI   R0,2                ELSE USE 2                                   
GETADD2  CLC   0(30,R4),MYSPACES   TEST FOR ANY DATA LEFT                       
         BNH   GETADD5                                                          
         MVC   0(30,R2),0(R4)                                                   
         LA    R2,30(R2)           BUMP IN NEEDADD                              
         LA    R4,30(R4)           BUMP TO NEXT ADDRESS LINE                    
         BCT   R0,GETADD2                                                       
         SPACE                                                                  
GETADD5  XC    WORK(39),WORK                                                    
         MVC   WORK(25),TAA2CITY      CITY                                      
         CLC   TAA2ZIP,MYSPACES       IF HAVE ZIP                               
         BNH   GETADD8                                                          
         SPACE                                                                  
         GOTO1 SQUASHER,DMCB,WORK,25  SQUASH CITY                               
         MVC   WORK+21(1),MYSPACES    CHOP OFF CITY IF LONGER THAN 21           
         MVC   WORK+22(2),TAA2ST      STATE                                     
         MVC   WORK+24(1),MYSPACES                                              
         MVC   WORK+25(10),TAA2ZIP    ZIP                                       
         GOTO1 SQUASHER,DMCB,WORK,35  SQUASH AGAIN                              
GETADD8  MVC   0(30,R2),WORK                                                    
         SPACE                                                                  
         CLI   TAA2LEN,TAA2LNQ        FOR OLD STYLE ADDRESSES                   
         BL    GETADD9                                                          
         CLC   TAA2CTRY,=C'US'        AND NEW STYLE US ADDRESSES                
         BE    GETADD9                                                          
         GOTO1 VALCTRY,DMCB,(X'80',TAA2CTRY)                                    
         BNE   GETADD9                                                          
         USING CTRYTABD,R1                                                      
         L     R1,TGACTRY                                                       
         ZIC   RE,CTRYDSP                                                       
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         IC    RE,CTRYLEN                                                       
         SHI   RE,CTRYDESC-CTRYTABD+1                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   30(0,R2),CTRYDESC                                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    30(0,R2),MYSPACES                                                
         DROP  R1                                                               
GETADD9  MVC   ELCODE,SAVEEL                                                    
         B     XIT                                                              
         SPACE                                                                  
GETADD10 MVI   ELCODE,TAADELQ      ADDRESS                                      
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         MVC   ELCODE,SAVEEL                                                    
         BNE   XIT                                                              
         USING TAADD,R6                                                         
         ZIC   R1,TAADLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   NEEDADD(0),TAADADD                                               
         SPACE 1                                                                
ITSFINE  SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NOGOOD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
*              TRACING ROUTINES                                                 
         SPACE 3                                                                
TRACEINP NTR1                                                                   
         L     R6,AFIRSTW2                                                      
         MVC   RECTYPE,=CL16'FIRST W2'                                          
         LTR   R6,R6                                                            
         BZ    *+8                                                              
         BAS   RE,TRACEREC                                                      
         L     R6,ALASTW2                                                       
         MVC   RECTYPE,=CL16'LAST W2'                                           
         BAS   RE,TRACEREC                                                      
         B     XIT                                                              
         SPACE 1                                                                
TRACEREC NTR1                                                                   
         MVI   MYP,X'BF'                                                        
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP+60(16),RECTYPE                                               
         BAS   RE,SPLAT                                                         
         LA    R2,40                                                            
         BAS   RE,TRACEL                                                        
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
TRACERC2 BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         ZIC   R2,1(R6)                                                         
         BAS   RE,TRACEL                                                        
         B     TRACERC2                                                         
         SPACE 1                                                                
TRACEL   NTR1                                                                   
*                                  R2=LENGTH, R6=ADDRESS                        
         MVC   MYP,MYSPACES                                                     
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP(0),0(R6)                                                     
         OC    MYP,MYSPACES                                                     
         GOTO1 HEXOUT,DMCB,(R6),MYP3,132,=C'SEP'                                
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP2(0),MYP3                                                     
         MVC   MYP3,MYSPACES                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYP3(0),MYP4                                                     
         MVC   MYP4,MYSPACES                                                    
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 3                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   P,MYP                                                            
         MVC   P2,MYP2                                                          
         MVC   P3,MYP3                                                          
         MVC   P4,MYP4                                                          
         GOTO1 SPOOL,DMCB,(R5)                                                  
         DROP  R5                                                               
         BAS   RE,MYCLEAR                                                       
         B     XIT                                                              
         SPACE 1                                                                
MYCLEAR  NTR1                                                                   
         MVI   MYP,C' '                                                         
         MVC   MYP+1(131),MYP                                                   
         MVC   MYP2,MYP                                                         
         MVC   MYP3,MYP                                                         
         MVC   MYP4,MYP                                                         
         MVC   MYSPACES,MYP                                                     
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINES ETC                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         MVC   H1+52(24),MYTITLE                                                
         GOTO1 CENTER,DMCB,H1+52,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
         MVC   H3+52(6),=C'PERIOD'                                              
         L     R1,APERH                                                         
         MVC   H3+59(17),8(R1)                                                  
         MVC   H6,MYH6                                                          
         MVC   H7,MYH7                                                          
         SPACE 1                                                                
         L     R4,ABOX             SET UP BOXES                                 
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES      SET ROWS                                     
         MVI   BOXROWS+4,C'T'      TOP                                          
         MVI   BOXROWS+7,C'M'      MIDDLE                                       
         MVI   BOXROWS+58,C'B'     BOTTOM                                       
         SPACE 1                                                                
         MVC   BOXCOLS(132),MYBOXES     SET COLUMNS                             
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         DROP  R5                                                               
         DROP  R4                                                               
         XIT1                                                                   
         SPACE 1                                                                
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         DC    H'0'                                                             
         SPACE 1                                                                
APERH    DS    A                                                                
         SPACE 1                                                                
MYH6     DS    0C                                                               
         DC    CL1' '                                                           
         DC    C' '                                                             
H6SSN    DC    CL11'S/S NUMBER'                                                 
         DC    CL1' '                                                           
H6NAME   DC    CL33'PERFORMER NAME'                                             
         DC    CL1' '                                                           
H6CHANGE DC    CL8'CHANGED'                                                     
         DC    CL1' '                                                           
H6PDATE  DC    CL8'   W2'                                                       
         DC    CL1' '                                                           
H6UNIT   DC    CL8'  UNIT'                                                      
         DC    CL1' '                                                           
H6OLDNEW DC    CL3' '                                                           
         DC    CL1' '                                                           
H6EARN   DC    CL12'  EARNINGS'                                                 
         DC    CL1' '                                                           
H6TAX    DC    CL12'   TAXES'                                                   
         DC    CL1' '                                                           
H6SUI    DC    CL12'  FICA/SUI'                                                 
         DC    CL1' '                                                           
H6SDI    DC    CL12'REIM EXPENSE'                                               
         DC    CL1' '                                                           
         DC    CL8' '                                                           
         SPACE 1                                                                
MYH7     DS    0C                                                               
         DC    CL1' '                                                           
         DC    C' '                                                             
H7SSN    DC    CL11' '                                                          
         DC    CL1' '                                                           
H7NAME   DC    CL33' '                                                          
         DC    CL1' '                                                           
H7CHANGE DC    CL8' ON/BY'                                                      
         DC    CL1' '                                                           
H7PDATE  DC    CL8'PRINTED'                                                     
         DC    CL1' '                                                           
H7UNIT   DC    CL8'   '                                                         
         DC    CL1' '                                                           
H7OLDNEW DC    CL3'   '                                                         
         DC    CL1' '                                                           
H7EARN   DC    CL12'          '                                                 
         DC    CL1' '                                                           
H7TAX    DC    CL12'        '                                                   
         DC    CL1' '                                                           
H7SUI    DC    CL12'  /MEDICARE'                                                
         DC    CL1' '                                                           
H7SDI    DC    CL12'    /SDI'                                                   
         DC    CL1' '                                                           
         DC    CL4' '                                                           
         SPACE 1                                                                
MYBOXES  DS    0C                                                               
         DC    CL1' '                                                           
         DC    C'L'                                                             
BXSSN    DC    CL11' '                                                          
         DC    CL1'C'                                                           
BXNAME   DC    CL33' '                                                          
         DC    CL1'C'                                                           
BXCHANGE DC    CL8' '                                                           
         DC    CL1'C'                                                           
BXPDATE  DC    CL8' '                                                           
         DC    CL1'C'                                                           
BXUNIT   DC    CL8' '                                                           
         DC    CL1'C'                                                           
BXOLDNEW DC    CL3' '                                                           
         DC    CL1'C'                                                           
BXEARN   DC    CL12' '                                                          
         DC    CL1'C'                                                           
BXTAX    DC    CL12' '                                                          
         DC    CL1'C'                                                           
BXSUI    DC    CL12' '                                                          
         DC    CL1'C'                                                           
BXSDI    DC    CL12' '                                                          
         DC    CL1'R'                                                           
         DC    CL4' '                                                           
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
*                                  COUNTS                                       
W2C      EQU   44                  RECNUM EQUATES                               
W2A      EQU   40                                                               
         SPACE 1                                                                
TRACOUNT DC    PL6'0'                                                           
W2COUNT  DC    PL6'0'                                                           
         DS    0D                                                               
         SPACE 1                                                                
         LTORG                                                                  
         DS    0D                                                               
LIMBLOCK DS    CL(TMLNQ)                                                        
FIRSTW2  DS    4000C                                                            
LASTW2   DS    4000C                                                            
LPOOLENT EQU   20                                                               
NPOOLENT EQU   100                                                              
ELPOOL   DS    CL(LPOOLENT*NPOOLENT)                                            
TOTPOOL  DS    CL(LPOOLENT*NPOOLENT)                                            
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE                                                                  
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
MYREGS   DS    16F                                                              
MYTITLE  DS    CL32                                                             
MYP      DS    CL132                                                            
MYP2     DS    CL132                                                            
MYP3     DS    CL132                                                            
MYP4     DS    CL132                                                            
MYSPACES DS    CL132                                                            
MYSORTER DS    A                                                                
         SPACE 1                                                                
THISSSN  DS    CL9                                                              
AFIRSTW2 DS    A                                                                
ALASTW2  DS    A                                                                
THISEMP  DS    CL3                                                              
*                                  OPTIONS                                      
TRACOPT  DS    CL1                 Y=TRACE                                      
         DS    CL6                 SPARE                                        
         SPACE 1                                                                
RECTYPE  DS    CL16                                                             
PTODAY   DS    PL3                                                              
NPOOL    DS    XL1                                                              
NTPOOL   DS    XL1                                                              
FLSW     DS    CL1                                                              
W2SW     DS    CL1                                                              
NLOC     DS    XL1                                                              
         SPACE 1                                                                
ATHISEX  DS    A                                                                
EMPNAME  DS    CL36                                                             
EMPADD   DS    CL120                                                            
EMPFDID  DS    CL14                                                             
ATHISW4  DS    A                                                                
W4NAME   DS    CL36                                                             
W4ADD    DS    CL120                                                            
SAVEEL   DS    XL1                                                              
THISUNIT DS    CL3                                                              
STATE    DS    CL3                                                              
LOCAL    DS    CL3                                                              
SVLLOC   DS    CL3                                                              
FICAEARN DS    D                                                                
FICA     DS    D                                                                
MEDWAGE  DS    D                                                                
MEDTAX   DS    D                                                                
FEDEARN  DS    D                                                                
FEDTAX   DS    D                                                                
FEDREXP  DS    D                                                                
STAEARN  DS    D                                                                
STATAX   DS    D                                                                
STASDI   DS    D                                                                
STASUI   DS    D                                                                
LOCEARN  DS    D                                                                
LOCTAX   DS    D                                                                
MYEND    DS    0D                                                               
         EJECT                                                                  
*              DSECT TO COVER PRINT LINES                                       
         SPACE 3                                                                
MYPRINT  DSECT                                                                  
         DS    CL1                                                              
         DS    CL1                                                              
MYSSN    DS    CL11                                                             
         DS    CL1                                                              
MYNAME   DS    CL33                                                             
         DS    CL1                                                              
MYCHANGE DS    CL8                                                              
         DS    CL1                                                              
MYPDATE  DS    CL8                                                              
         DS    CL1                                                              
MYUNIT   DS    CL8                                                              
         DS    CL1                                                              
MYOLDNEW DS    CL3                                                              
         DS    CL1                                                              
MYEARN   DS    CL12                                                             
         DS    CL1                                                              
MYTAX    DS    CL12                                                             
         DS    CL1                                                              
MYFICA   DS    0CL12                                                            
MYSUI    DS    CL12                                                             
         DS    CL1                                                              
MYREXP   DS    0CL12                                                            
MYSDI    DS    CL12                                                             
         DS    CL1                                                              
         DS    CL4                                                              
         SPACE 3                                                                
*              DSECT TO COVER POOL ENTRY                                        
         SPACE 1                                                                
POOLD    DSECT                                                                  
POOLUNIT DS    CL3                                                              
POOLFL   DS    CL1                                                              
POOLEARN DS    F                                                                
POOLTAX  DS    F                                                                
POOLFICA DS    0F                                                               
POOLSUI  DS    F                                                                
POOLREXP DS    0F                                                               
POOLSDI  DS    F                                                                
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE TALIMD                                                         
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*TASYSCTRYD                                                                     
*DDTWADCONS                                                                     
*DDBIGBOX                                                                       
*FAFACTS                                                                        
*FATIOB                                                                         
*TAREPFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSCTRYD                                                     
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE TAREPEED                                                       
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014TAREP2E   08/13/14'                                      
         END                                                                    
