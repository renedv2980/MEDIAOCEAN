*          DATA SET TAREP2D    AT LEVEL 051 AS OF 11/20/14                      
*PHASE T7032DB,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'T7032D - W2 BUILD'                                              
T7032D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T7032D,R7                                          
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BAS   RE,OPENTAPE                                                      
         BAS   RE,PREP                                                          
         BAS   RE,CLOSTAPE                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         SPACE 1                                                                
         LA    R2,SPLYEARH         YEAR                                         
         GOTO1 ANY                 (REQUIRED)                                   
         MVC   THISYEAR,WORK                                                    
         CLI   5(R2),4             MUST BE 4 CHARACTERS                         
         BNE   FLDINV                                                           
         MVC   WORK(4),=C'0000'                                                 
         MVZ   WORK(4),8(R2)                                                    
         CLC   WORK(4),=C'0000'                                                 
         BNE   FLDINV                                                           
         MVC   WORK(4),THISYEAR                                                 
         MVC   WORK+4(4),=C'0101'                                               
         GOTO1 DATCON,DMCB,(9,WORK),(1,TIQPSTR)                                 
         MVC   WORK(4),THISYEAR                                                 
         MVC   WORK+4(4),=C'1231'                                               
         GOTO1 DATCON,DMCB,(9,WORK),(1,TIQPEND)                                 
         SPACE 1                                                                
         LA    R2,SPLCURH          CURRENCY                                     
         MVC   CEMPLIST,ALLLIST    PRESET TO RUN ALL CURRS/EMPS                 
         CLI   5(R2),0                                                          
         BE    VREC2                                                            
         GOTO1 ANY                                                              
         MVC   CEMPLIST(1),WORK                                                 
         SPACE 1                                                                
         LA    R2,SPLEMPH          EMPLOYER                                     
         GOTO1 ANY                                                              
         MVC   CEMPLIST+1(3),WORK                                               
         MVI   CEMPLIST+4,X'FF'                                                 
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2),0                                       
         SPACE 1                                                                
VREC2    LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         BAS   RE,VOPTS                                                         
                                                                                
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 2                                                                
VOPTS    NTR1                                                                   
         ZAP   TRALIMIT,=P'0'                                                   
         ZAP   RECLIMIT,=P'99999999'                                            
         ZAP   REPLIMIT,=P'0'                                                   
         MVI   INOPT,C'D'          DEFAULT INPUT FROM DISK                      
         MVI   CSCOPT,C'N'         DEFAULT NOT FOR CSC                          
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
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
OPT4     CLC   12(5,R4),=C'LIMIT'  LIMIT OPTION                                 
         BNE   OPT5                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    FLDINV                                                           
         CVD   R1,DUB                                                           
         ZAP   RECLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT5     CLC   12(6,R4),=C'REPORT' REPORT LIMIT                                 
         BNE   OPT6                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    FLDINV                                                           
         CVD   R1,DUB                                                           
         ZAP   REPLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(3,R4),=C'SSN'    TESTING OPTION 1 SS#                         
         BNE   OPT7                                                             
         MVC   TIFSSN,22(R4)       (CHECK NUMERIC)                              
         B     OPTEND                                                           
         SPACE 1                                                                
OPT7     CLC   12(3,R4),=C'CSC'    CSC OPTION (ADDREC)                          
         BNE   OPT8                                                             
         MVI   CSCOPT,C'Y'                                                      
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(4,R4),=C'TAPE'   OPTIONAL TAPE INPUT                          
         BNE   FLDINV                                                           
         MVI   INOPT,C'T'                                                       
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 2                                                                
PREP     NTR1                                                                   
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         MVC   MYSORTER,SORTER                                                  
         ZAP   CHKCOUNT,=P'0'                                                   
         MVI   ANYW2,C'N'                                                       
         SPACE 1                                                                
         CLI   INOPT,C'T'                                                       
         BNE   *+12                                                             
         BAS   RE,TREP                                                          
         B     XIT                                                              
         SPACE 1                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         LA    R2,CEMPLIST                                                      
         SPACE 1                                                                
PREP4    CLI   0(R2),X'FF'                                                      
         BE    XIT                                                              
         MVC   TIFCUR,0(R2)        SET CURRENCY                                 
         MVC   TIFEMP,1(R2)            AND EMPLOYER                             
         MVI   TIREAD,TLCKYCDQ     SET TO READ YTD CHECKS                       
         OI    TIQFLAGS,TIQFSKIP       SKIPPING                                 
         MVI   TIQDTYPE,TIQDCHK                                                 
                                                                                
         CLI   CSCOPT,C'Y'         IF CSC AND EMPLOYER=TP                       
         BNE   PREP5                                                            
         LA    R3,LTPLIST          PROCESS SPECIAL PERFORMERS TOO               
         CLC   TIFEMP,=C'TP '                                                   
         BE    PREP4C                                                           
         LA    R3,LPPLIST          PROCESS SPECIAL PERFORMERS TOO               
         CLC   TIFEMP,=C'PP '                                                   
         BE    PREP4C                                                           
         LA    R3,LPLLIST          PROCESS SPECIAL PERFORMERS TOO               
PREP4C   CP    RECLIMIT,=P'1000'                                                
         BNH   PREP5                                                            
         ZAP   RECLIMIT,=P'1000'                                                
                                                                                
PREP5    GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         BAS   RE,PUTTAPE                                                       
                                                                                
         CLI   CSCOPT,C'Y'         IF CSC AND EMPLOYER=TP                       
         BNE   PREP7                                                            
*        CLC   TIFEMP,=C'TP '                                                   
*        BNE   PREP7                                                            
         CLI   0(R3),X'FF'                                                      
         BE    PREP7                                                            
         MVC   TIFSSN,0(R3)        FILTER BY SSN NOW                            
         AHI   R3,9                AND BUMP TO NEXT ONE                         
         AP    RECLIMIT,=P'1'                                                   
         CLI   TRACOPT,C'Y'                                                     
         BNE   PREP5                                                            
         AP    TRALIMIT,=P'1'                                                   
         B     PREP5                                                            
                                                                                
PREP7    LA    R2,4(R2)                                                         
         B     PREP4                                                            
         EJECT                                                                  
*              SYSIO HOOK                                                       
         SPACE 2                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   XIT                                                              
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAPDD,R6                                                         
         CLI   TAPDW4TY,TAW4TYCO   REJECT CORP CHECKS                           
         BE    XIT                                                              
         CLI   TAPDW4TY,TAW4TYCA   AND CANADIAN CHECKS                          
         BE    XIT                                                              
         CLI   TAPDW4TY,TAW4TYTR   AND TRUSTEE CHECKS                           
         BE    XIT                                                              
         AP    CHKCOUNT,=P'1'                                                   
         CP    CHKCOUNT,TRALIMIT                                                
         BH    IOHOOK2                                                          
         BAS   RE,TRACEINP                                                      
         SPACE 1                                                                
IOHOOK2  BAS   RE,PROCHECK         PROCESS A CHECK RECORD                       
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS A CHECK RECORD                                           
         SPACE 2                                                                
PROCHECK NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TLW2D,R5                                                         
         CLC   TLW2EMP,TIEMP       CHECK IF FOR SAME EMP/CUR/SSN                
         BNE   PROC2                                                            
         CLC   TLW2CUR,TICUR                                                    
         BNE   PROC2                                                            
         CLC   TLW2SSN,TISSN                                                    
         BNE   PROC2                                                            
         B     PROC4                                                            
         SPACE 1                                                                
PROC2    OC    TLW2SSN,TLW2SSN     IF THIS IS NOT THE FIRST                     
         BZ    PROC3                                                            
         CP    TAPCOUNT,RECLIMIT                                                
         BH    PROC3                                                            
         BAS   RE,PUTTAPE             RELEASE PENDING TAPE RECORD               
         SPACE 1                                                                
PROC3    LR    RE,R5                                                            
         LA    RF,1000                                                          
         XCEF                                                                   
         MVI   TLW2CD,TLW2CDQ      BUILD NEW W2 KEY                             
         MVC   TLW2YEAR,THISYEAR                                                
         MVC   TLW2EMP,TIEMP                                                    
         MVC   TLW2CUR,TICUR                                                    
         MVC   TLW2SSN,TISSN                                                    
         MVC   TLW2CDTE,TGTODAY1                                                
         XC    TLW2CDTE,=X'FFFFFF' COMPLEMENT THIS                              
         SPACE 1                                                                
PROC4    L     R6,TIAREC           LOOK FOR CHECK DETAILS                       
         XC    THISYREX,THISYREX                                                
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PROC5                                                            
         USING TACDD,R6                                                         
         MVC   THISYREX,TACDYREX                                                
         SPACE 1                                                                
PROC5    L     R6,TIAREC           NOW LOOK FOR YTD ELEMENTS                    
         MVI   ELCODE,TACYELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
PROC6    BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TACYD,R6                                                         
         LA    R1,TIKEY                                                         
         USING TLCKPKEY,R1                                                      
         CLC   TLCKYTXU,TACYUNIT   LOOK FOR MATCH OF UNIT                       
         BNE   PROC6                                                            
         CLC   TIEMP,=C'TP '       IF EMPLOYER EXCLUDING DEFAULT STATES         
         BE    *+12                                                             
         TM    TLCKYSTS,TACWSTAX+TACWSDEF SKIP STATES TAXABLE BY DFLT           
         BO    XIT                                                              
         DROP  R1                                                               
         LA    R5,ELEMENT                                                       
         XC    ELEMENT(64),ELEMENT                                              
         USING TAW2D,R5                                                         
         MVI   TAW2EL,TAW2ELQ                                                   
         MVI   TAW2LEN,TAW2LNQ2                                                 
         MVC   TAW2UNIT,TACYUNIT                                                
         MVC   TAW2EARN,TACYEARN                                                
         MVC   TAW2TAX,TACYTAX                                                  
         MVC   TAW2FICA,TACYFICA                                                
*        MVC   TAW2REXP,THISYREX              FOR 2012                          
*                                                                               
         CLC   TIEMP,=C'P+ '                                                    
         BNE   PROC6A                                                           
         CLI   TACYLEN,TACYLN3Q                                                 
         BL    PROC6A                                                           
         MVC   TAW2TXRE,TACYTXRE                                                
*                                                                               
PROC6A   CLC   TAW2UNIT(2),=C'FD'                                               
         BE    PROC7                                                            
         MVC   TAW2SDI,TACYSDI                                                  
         MVC   TAW2SUI,TACYSUI                                                  
         MVC   TAW2SFLI,TACYSFLI   FAMILY LEAVE INSURANCE FOR STATE             
PROC7    OC    TAW2EARN(20),TAW2EARN                                            
         BZ    XIT                                                              
         BAS   RE,ADDEL                                                         
         MVI   ANYW2,C'Y'                                                       
         CLC   TAW2UNIT(2),=C'FD'  IF THIS IS FEDERAL                           
         BNE   XIT                                                              
         L     R6,TIAREC           LOOK FOR YTD EARNINGS                        
         MVI   ELCODE,TAYEELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
PROC8    BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TAYED,R6                                                         
         CLI   TAYETYPE,TAYETCHK   (NEED THE CHECK VERSION)                     
         BNE   PROC8                                                            
         LR    R5,R6                                                            
         BAS   RE,ADDEL                                                         
                                                                                
         L     R6,TIAREC           LOOK FOR YTD WITHHOLDINGS                    
         MVI   ELCODE,TACYELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
PROC10   BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TACYD,R6                                                         
         CLC   TACYUNIT(2),=C'FD'   LOOK FOR FEDERAL YTD                        
         BNE   PROC10                                                           
         LR    R5,R6                                                            
         BAS   RE,ADDEL                                                         
         B     XIT                                                              
         EJECT                                                                  
*              READ TAPE AND BUILD NEW W2 RECORD                                
         SPACE 3                                                                
TREP     NTR1                                                                   
         LA    R4,INIO                                                          
         USING IND,R4                                                           
         BAS   RE,GETTAPE                                                       
         XC    LASTSSN,LASTSSN                                                  
         SPACE 1                                                                
TREP2    GOTO1 MYSORTER,DMCB,=C'GET'                                            
         L     R2,4(R1)                                                         
         LTR   R2,R2                                                            
         BZ    TREPEOF                                                          
         BAS   RE,SORTMOVE                                                      
         OC    INSSN,INSSN         IGNORE RECORDS WITHOUT SSN                   
         BZ    TREP2                                                            
         UNPK  WORK(9),INSSN       CONVERT SSN FROM PACKED                      
         CLC   WORK(9),=C'255708108'                                            
         BNE   *+8                                                              
         BAS   RE,TRACEM                                                        
         CLC   WORK(9),=C'041285240'                                            
         BNE   *+8                                                              
         BAS   RE,TRACEM                                                        
         AP    CHKCOUNT,=P'1'                                                   
         CP    CHKCOUNT,RECLIMIT                                                
         BH    XIT                                                              
         CP    CHKCOUNT,TRALIMIT                                                
         BH    TREP3                                                            
         BAS   RE,TRACEM                                                        
         SPACE 1                                                                
TREP3    CLC   LASTSSN,INSSN                                                    
         BE    TREP4                                                            
         OC    LASTSSN,LASTSSN                                                  
         BZ    *+8                                                              
         BAS   RE,PUTTAPE                                                       
         MVC   LASTSSN,INSSN                                                    
         LA    R5,TAPEIO                                                        
         USING TLW2D,R5                                                         
         LR    RE,R5                                                            
         LA    RF,1000                                                          
         XCEF                                                                   
         MVI   TLW2CD,TLW2CDQ      BUILD NEW W2 KEY                             
         MVC   TLW2YEAR,THISYEAR                                                
         CLC   INEMP,=C'00'                                                     
         BNE   *+10                                                             
         MVC   TLW2EMP,=C'TP '                                                  
         CLC   INEMP,=C'56'                                                     
         BNE   *+10                                                             
         MVC   TLW2EMP,=C'P+ '                                                  
         CLC   INEMP,=C'62'                                                     
         BNE   *+10                                                             
         MVC   TLW2EMP,=C'DM '                                                  
         MVI   TLW2CUR,C'U'        ONLY GETTING US, I HOPE                      
         UNPK  TLW2SSN,INSSN       CONVERT SSN FROM PACKED                      
         MVC   TLW2CDTE,TGTODAY1                                                
         XC    TLW2CDTE,=X'FFFFFF' COMPLEMENT THIS                              
         SPACE 1                                                                
TREP4    LA    R5,ELEMENT          ELEMENT FOR EACH UNIT ACTIVE                 
         USING TAW2D,R5                                                         
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     FEDERAL                                      
         MVI   CASHACT,C'N'                                                     
         MVI   TAW2EL,TAW2ELQ                                                   
         MVI   TAW2LEN,TAW2LNQ2                                                 
         MVC   TAW2UNIT,=C'FD '                                                 
         GOTO1 CONVCASH,DMCB,INFGROS,TAW2EARN                                   
         GOTO1 CONVCASH,DMCB,INFTAX,TAW2TAX                                     
         GOTO1 CONVCASH,DMCB,INFICA,TAW2FICA                                    
*        GOTO1 CONVCASH,DMCB,INREXP,TAW2REXP     FOR 2012                       
         CLI   CASHACT,C'Y'        ADD IF ACTIVE                                
         BNE   *+8                                                              
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         XC    ELEMENT(64),ELEMENT STATE                                        
         MVI   CASHACT,C'N'                                                     
         MVI   TAW2EL,TAW2ELQ                                                   
         MVI   TAW2LEN,TAW2LNQ2                                                 
         MVC   TAW2UNIT,INSTATE                                                 
         MVI   TAW2UNIT+2,C' '                                                  
         GOTO1 CONVCASH,DMCB,INSGROS,TAW2EARN                                   
         GOTO1 CONVCASH,DMCB,INSTAX,TAW2TAX                                     
         GOTO1 CONVCASH,DMCB,INSUI,TAW2SUI                                      
         GOTO1 CONVCASH,DMCB,INSDI,TAW2SDI                                      
         CLI   CASHACT,C'Y'        ADD IF ACTIVE                                
         BNE   *+8                                                              
         BAS   RE,ADDEL                                                         
         SPACE 1                                                                
         XC    ELEMENT(64),ELEMENT LOCAL                                        
         MVI   CASHACT,C'N'                                                     
         MVI   TAW2EL,TAW2ELQ                                                   
         MVI   TAW2LEN,TAW2LNQ2                                                 
         MVC   TAW2UNIT,INLOCAL                                                 
         GOTO1 CONVCASH,DMCB,INLGROS,TAW2EARN                                   
         GOTO1 CONVCASH,DMCB,INLTAX,TAW2TAX                                     
         CLI   CASHACT,C'Y'        ADD IF ACTIVE                                
         BNE   *+8                                                              
         BAS   RE,ADDEL                                                         
         B     TREP2                                                            
         SPACE 1                                                                
TREPEOF  BAS   RE,PUTTAPE                                                       
         B     XIT                                                              
         SPACE 1                                                                
CONVCASH NTR1                                                                   
         LM    R2,R3,0(R1)         INPUT/OUTPUT                                 
         ZAP   DUB,0(7,R2)         INPUT IS PL7                                 
         CVB   R1,DUB                                                           
         ST    R1,0(R3)            OUTPUT IS F                                  
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         MVI   CASHACT,C'Y'        RETURN INDIC IF ACTIVE CASH                  
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
*              TAPE ROUTINES                                                    
         SPACE 3                                                                
OPENTAPE NTR1                                                                   
         ZAP   TAPCOUNT,=P'0'                                                   
         CLI   CSCOPT,C'Y'                                                      
         BE    XIT                                                              
         L     R2,=A(W2TAPE)                                                    
         OPEN  ((2),OUTPUT)                                                     
         CLI   INOPT,C'T'          MAYBE AN INPUT TAPE                          
         BNE   XIT                                                              
         L     R2,=A(INTAPE)                                                    
         OPEN  ((2),INPUT)                                                      
         B     XIT                                                              
                                                                                
CLOSTAPE NTR1                                                                   
         BAS   RE,SPLAT                                                         
         LA    R2,P                                                             
         EDIT  (P6,TAPCOUNT),(7,0(R2))                                          
         MVC   P+8(12),=C'TAPE RECORDS'                                         
         BAS   RE,SPLAT                                                         
                                                                                
         CLI   CSCOPT,C'Y'                                                      
         BE    XIT                                                              
         L     R2,=A(W2TAPE)                                                    
         CLOSE ((2))                                                            
         CLI   INOPT,C'T'                                                       
         BNE   XIT                                                              
         L     R2,=A(INTAPE)                                                    
         CLOSE ((2))                                                            
         B     XIT                                                              
         SPACE 1                                                                
PUTTAPE  NTR1                                                                   
         CLI   ANYW2,C'N'                                                       
         BE    XIT                                                              
         MVI   ANYW2,C'N'                                                       
         XC    ELEMENT(64),ELEMENT ADD W2 SUBSID ELEMENT                        
         LA    R5,ELEMENT                                                       
         USING TAWSD,R5                                                         
         MVI   TAWSEL,TAWSELQ                                                   
         MVI   TAWSLEN,TAWSLNQ                                                  
         MVC   TAWSDPRT,TGTODAY1                                                
         BAS   RE,ADDEL                                                         
                                                                                
         XC    ELEMENT(64),ELEMENT ADD ACTIVITY ELEMENT                         
         LA    R5,ELEMENT                                                       
         USING TAACD,R5                                                         
         MVI   TAACEL,TAACELQ                                                   
         MVI   TAACLEN,TAACLNQ                                                  
         MVC   TAACCDTE,TGTODAY1                                                
         BAS   RE,ADDEL                                                         
                                                                                
         MVC   RECTYPE,=CL16'OUTPUT'                                            
         CLI   TRACOPT,C'Y'                                                     
         BNE   PUTTAPE2                                                         
         CP    TAPCOUNT,TRALIMIT                                                
         BH    PUTTAPE2                                                         
         BAS   RE,TRACETAP                                                      
         SPACE 1                                                                
PUTTAPE2 LA    R5,TAPEIO                                                        
         USING TLW2D,R5                                                         
         CLI   CSCOPT,C'Y'         IF CSC OPTION, ADD ON FILE                   
         BE    PUTTAPE5                                                         
                                                                                
**NO-OP**CLC   TLW2SSN,=C'255708108'                                            
*        BNE   *+8                                                              
*        BAS   RE,TRACETAP                                                      
*        CLC   TLW2SSN,=C'041285240'                                            
*        BNE   *+8                                                              
**NO-OP**BAL   RE,TRACETAP                                                      
*                                                                               
         LH    R1,TLW2LEN          NOT CSC, ADD TO TAPE                         
         LA    R1,4(R1)                                                         
         STH   R1,PRETAPIO                                                      
         L     R1,=A(W2TAPE)                                                    
         LA    R0,PRETAPIO                                                      
         PUT   (1),(0)                                                          
         B     PUTTAPE9                                                         
                                                                                
PUTTAPE5 ST    R5,AIO                                                           
         MVC   FILENAME,=CL8'CHKFIL'     ADDING DIRECTLY ON FILE (CSC)          
         GOTO1 ADDREC                                                           
                                                                                
PUTTAPE9 AP    TAPCOUNT,=P'1'                                                   
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO GET FROM INPUT TAPE AND PUT TO SORT                   
         SPACE 1                                                                
GETTAPE  NTR1                                                                   
         LA    R0,INIO                                                          
         L     R1,=A(INTAPE)                                                    
         GET   (1),(0)                                                          
         BAS   RE,SORTPUT                                                       
INEOF    B     XIT                                                              
         EJECT                                                                  
*              SORT UTILITIES                                                   
         SPACE 2                                                                
SORTPUT  NTR1                                                                   
         CLI   SORTFRST,C'Y'                                                    
         BNE   SORTPUT2                                                         
         GOTO1 MYSORTER,DMCB,SORTCARD,RECCARD                                   
         MVI   SORTFRST,C'N'                                                    
         SPACE 1                                                                
SORTPUT2 DS    0H                                                               
         GOTO1 MYSORTER,DMCB,=C'PUT',INIO                                       
         B     XIT                                                              
         SPACE 1                                                                
SORTMOVE NTR1                                                                   
         MOVE  (INIO,490),(R2)                                                  
         B     XIT                                                              
         SPACE 2                                                                
         DS    0F                                                               
SORTFRST DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,11,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(490)'                                 
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 2                                                                
*              TRACING ROUTINES                                                 
         SPACE 1                                                                
TRACEINP NTR1                                                                   
         L     R6,TIAREC                                                        
         MVC   RECTYPE,=CL16'CHECK'                                             
         BAS   RE,TRACEREC                                                      
         B     XIT                                                              
         SPACE 1                                                                
TRACETAP NTR1                                                                   
         LA    R6,TAPEIO                                                        
         MVC   RECTYPE,=CL16'OUTPUT'                                            
         BAS   RE,TRACEREC                                                      
         B     XIT                                                              
         SPACE 1                                                                
TRACEREC NTR1                                                                   
         MVI   P,X'BF'                                                          
         MVC   P+1(131),P                                                       
         MVC   P+60(16),RECTYPE                                                 
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
TRACEM   NTR1                                                                   
         MVI   P,X'BF'                                                          
         MVC   P+1(131),P                                                       
         MVC   P+60(16),=CL16'INPUT TAPE'                                       
         BAS   RE,SPLAT                                                         
         LA    R6,INIO                                                          
         LA    R0,4                                                             
         SPACE 1                                                                
TRACE2   MVC   P(100),0(R2)                                                     
         LA    R2,100                                                           
         BAS   RE,TRACEL                                                        
         LA    R6,100(R6)                                                       
         BCT   R0,TRACE2                                                        
         LA    R2,90                                                            
         BAS   RE,TRACEL                                                        
         B     XIT                                                              
         SPACE 1                                                                
TRACEL   NTR1                                                                   
*                                  R2=LENGTH, R6=ADDRESS                        
         MVC   P,SPACES                                                         
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R6)                                                       
         OC    P,SPACES                                                         
         GOTO1 HEXOUT,DMCB,(R6),P3,132,=C'SEP'                                  
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P2(0),P3                                                         
         MVC   P3,SPACES                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P3(0),P4                                                         
         MVC   P4,SPACES                                                        
         BAS   RE,SPLAT                                                         
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
*              ODD ROUTINES                                                     
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              ADD AN ELEMENT      R5=A(ELEMENT)                                
ADDEL    NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'TALFIL'),TAPEIO,0(R5)                    
         CLI   12(R1),0                                                         
         BE    XIT                                                              
         DC    H'00'                                                            
         SPACE 2                                                                
*              PRINT A LINE                                                     
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINES ETC                                                    
         SPACE 2                                                                
HOOK     NTR1                                                                   
         MVC   H1+52(9),=C'W2 CREATE'                                           
         GOTO1 CENTER,DMCB,H1+52,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
         MVC   H3+58(6),=C'PERIOD'                                              
         MVC   H3+65(4),THISYEAR                                                
         XIT1                                                                   
         SPACE 1                                                                
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         DC    H'0'                                                             
         EJECT                                                                  
*              DCB'S, LTORG, ETC                                                
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         SPACE 2                                                                
ALLLIST  DS    0H                                                               
         DC    C'U',C'PP '                                                      
         DC    C'U',C'P+ '         REPLACED PG WITH P+                          
         DC    C'U',C'TP '                                                      
         DC    X'FF'                                                            
         SPACE 1                                                                
LTPLIST  DS    0H                  LIMIT TP LIST                                
*        DC    C'025628192'            JAGODOWSKI THOMAS                        
         DC    C'034483444'            JOHNSON JEFFREY                          
         DC    C'091744710'            GREGOROFF NIKKI                          
         DC    C'122529448'            MCENROE JOHNKKI                          
         DC    C'155940280'            STEUER MONICA                            
         DC    C'357026524'            HOULT NICHOLAS                           
         DC    C'438250839'            ADKINS TRACE                             
         DC    C'467814893'            JACOBSON ERICTOPHER                      
         DC    C'504901461'            TENDLER ALISONE                          
         DC    C'531845105'            SANDOMIRE IVAN                           
         DC    C'545853269'            FIDLER SEBASTIAN                         
         DC    C'546715314'            FONTANA SANTINON                         
         DC    C'565551560'            GOURLEY MATTHEW                          
         DC    C'579230955'            JACKSON YASHA                            
         DC    C'592732507'            SAUCEDA MEMOBUYUKI                       
         DC    C'598161811'            SANTOS-CORDERO CARLOS                    
         DC    C'603282624'            VAYNTRUB MILANA                          
         DC    C'608830681'            RAMSAY GORDON                            
*                                                                               
* SOME PUERTO RICO PERFORMERS                                                   
*                                                                               
         DC    C'581357924'            ADROVET NORA A                           
         DC    C'581670649'            MEJIAS ANGEL                             
         DC    C'581998966'            BURGOS RICARDO                           
         DC    C'582780487'            ROMAN-RIVERA BET                         
         DC    C'583804296'            GRACIA RAFAEL LO                         
         DC    C'636097538'            CRESPO TATIANA I                         
*                                                                               
         DC    X'FF'                                                            
         SPACE 1                                                                
LPPLIST  DS    0H                  LIMIT PP LIST                                
*        DC    C'052963381'            MARLIER-RIQUIER SANDRINE                 
*        DC    C'063967890'            DONEGAN LOUISE                           
         DC    C'213450792'            BUCH JESSICA                             
         DC    C'243770522'            HEADRICK MADISON                         
         DC    C'243853919'            HARDIN ONDRIA                            
         DC    C'566955526'            MAYEDA HANA                              
         DC    C'597590266'            RIABINKINA EKATERINA                     
         DC    C'613689152'            LOVE LAURA                               
         DC    C'778156863'            ALMEIDA ELI                              
         DC    X'FF'                                                            
         SPACE 1                                                                
LPLLIST  DS    0H                  LIMIT P+ LIST                                
         DC    C'378928240'            KELLY LISA                               
         DC    C'545899088'            CALVIN IVY                               
         DC    C'610449688'            SHEETS BRANDON                           
         DC    X'FF'                                                            
         ENTRY W2TAPE                                                           
         SPACE 1                                                                
W2TAPE   DCB   DDNAME=W2TAPE,DSORG=PS,MACRF=(PM),                      X        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
         ENTRY INTAPE                                                           
         SPACE 1                                                                
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,MACRF=(GM),EODAD=INEOF,          X        
               RECFM=FB,LRECL=449,BUFNO=2,BLKSIZE=0                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 2                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
MYSORTER DS    A                                                                
         SPACE 1                                                                
INOPT    DS    CL1                 D=DISK (SYSIO) T=TAPE (IDC)                  
TRACOPT  DS    CL1                 Y=TRACE                                      
CSCOPT   DS    CL1                 CSC OPTION, ADDREC TO FILE                   
TRALIMIT DS    PL6                                                              
CHKCOUNT DS    PL6                                                              
RECLIMIT DS    PL6                                                              
REPLIMIT DS    PL6                                                              
TAPCOUNT DS    PL6                                                              
         DS    CL6                 SPARE                                        
         SPACE 1                                                                
CASHACT  DS    CL1                                                              
ANYW2    DS    CL1                                                              
THISYEAR DS    CL4                                                              
THISYREX DS    F                                                                
LASTSSN  DS    PL5                                                              
*                                                                               
RECTYPE  DS    CL16                                                             
CEMPLIST DS    CL32                                                             
*                                  OPTIONS                                      
SAVEEL   DS    CL1                                                              
SAVETPKY DS    CL20                                                             
SAVEAMTS DS    CL36                                                             
         SPACE 1                                                                
         DS    0D                                                               
PRETAPIO DS    F                                                                
TAPEIO   DS    1000C                                                            
INIO     DS    1000C                                                            
         SPACE 1                                                                
MYEND    DS    0D                                                               
         EJECT                                                                  
*              DSECT TO COVER INPUT TAPE RECORDS                                
         SPACE 2                                                                
IND      DSECT                                                                  
INYEAR   DS    CL4                                                              
INEMP    DS    CL2                                                              
INSSN    DS    PL5                 (SSN IS PWOS)                                
INCORPID DS    PL5                 (CORP IS SIGNED PACKED)                      
INSTATE  DS    CL2                                                              
INLOCAL  DS    CL3                                                              
INLAST   DS    CL16                                                             
INFIRST  DS    CL16                                                             
INADD1   DS    CL30                                                             
INADD2   DS    CL30                                                             
INADD3   DS    CL30                                                             
INZIP    DS    CL9                                                              
INENAME  DS    CL30                                                             
INEADD1  DS    CL30                                                             
INEADD2  DS    CL30                                                             
INEADD3  DS    CL30                                                             
INEADD4  DS    CL30                                                             
INFGROS  DS    PL7                                                              
INFTXBL  DS    PL7                                                              
INFICA   DS    PL7                                                              
INLGROS  DS    PL7                                                              
INSTAX   DS    PL7                                                              
INCALDIS DS    PL5                                                              
INNYDIS  DS    PL5                                                              
INFTAX   DS    PL7                                                              
INODIS   DS    PL5                                                              
INSGROS  DS    PL7                                                              
INREXP   DS    PL7                                                              
INSDI    DS    PL7                                                              
INSUI    DS    PL7                                                              
INLTAX   DS    PL7                                                              
INTIPS   DS    PL5                                                              
INWAGTIP DS    PL7                                                              
INMSTAT  DS    CL1                                                              
INIDNO   DS    CL10                                                             
INSFTXBL DS    PL7                                                              
INSFICA  DS    PL7                                                              
INFICAR  DS    PL9                                                              
         EJECT                                                                  
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE TAREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPEDD                                                       
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051TAREP2D   11/20/14'                                      
         END                                                                    
