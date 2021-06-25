*          DATA SET TAGENE6    AT LEVEL 106 AS OF 05/02/13                      
*PHASE T702E6A,*                                                                
         TITLE 'T702E6 - TRUST ISSUE'                                           
T702E6   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         NMOD1 0,T702E6                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=SYSTEM STORAGE AREA                       
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INTIALIZE                                    
         TM    TGSYSTAT,TASYSPID                                                
         BZ    *+14                                                             
         MVC   TRUMHED(9),=C'Minor Pid'                                         
         OI    TRUMHEDH+6,X'80'                                                 
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   *+8                                                              
         BAS   RE,VKEY             VALIDATE KEY                                 
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   *+8                                                              
         BAS   RE,VREC             VALIDATE RECORD                              
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE KEY FOR PERFORMER'S W4 RECORD                           
         SPACE 1                                                                
VKEY     NTR1                                                                   
         MVC   TRUWHLD,SPACES      CLEAR AMT WITHHELD                           
         MVC   TRUTRSS,SPACES            TRUSTEE SS#                            
         MVC   TRUIINV,SPACES        AND INVOICE # FLDS                         
         OI    TRUWHLDH+6,X'80'                                                 
         OI    TRUTRSSH+6,X'80'                                                 
         OI    TRUIINVH+6,X'80'                                                 
         SPACE 1                                                                
         NI    TRUR2MPH+1,X'F7'                                                 
         OI    TRUR2MPH+1,X'0C'    HIDE AMT TO REFUND PROMPT                    
         OI    TRUR2MPH+6,X'80'                                                 
         OI    TRUR2MTH+1,X'20'    PROTECT AMT TO REFUND FLD                    
         OI    TRUR2MTH+6,X'80'                                                 
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'28',TRUEMPH),TRUEMPNH   EMPLOYER          
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK05                                                             
         CLI   TRUMINH+5,6                                                      
         BH    VK05                                                             
         MVC   TGPID,TRUMIN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK05                                                             
         MVC   TRUMIN,TGSSN                                                     
         MVI   TRUMINH+5,9                                                      
VK05     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'28',TRUMINH),TRUMINNH   MINOR W4          
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK10                                                             
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   TRUMIN,SPACES                                                    
         MVC   TRUMIN(L'TGPID),TGPID                                            
         MVI   TRUMINH+5,6                                                      
         OI    TRUMINH+6,X'80'                                                  
         SPACE 1                                                                
VK10     LA    R2,TRUMINH          R2=A(MINOR SSN FIELD)                        
         SPACE 1                                                                
         L     R4,AIO              IF INPUTTED SS# IS INDIVIDUAL WITH           
         MVI   ELCODE,TATIELQ      CORPORATE ID ... INVALID                     
         GOTO1 GETL,DMCB,(1,=AL1(TATITYCO))                                     
         BE    ERRINV                                                           
         SPACE 1                                                                
         BAS   RE,W4ISCORP         IS INPUTTED SS# A CORP?                      
*                                                                               
         L     R4,AIO              R4=A(MINOR W4 RECORD)                        
         MVI   ELCODE,TAWXELQ                                                   
         BAS   RE,GETEL            R4=A(MINOR W4 EXTRA DETAILS ELEMENT)         
         BNE   ERRINV                                                           
*                                                                               
         USING TAWXD,R4                                                         
         MVI   RET2MIN,C'N'                                                     
         MVC   TRUSTSS,TAWXTSSN                                                 
*   Allow user to enter amount refunded.  (10/3/2008)                           
         OC    TAWXTSSN,TAWXTSSN   IF NO TRUSTEE SPECIFIED                      
         BNZ   *+8                 MUST BE RETURNING TO MINOR                   
         MVI   RET2MIN,C'Y'                                                     
*                                                                               
         NI    TRUR2MPH+1,X'F3'                                                 
         OI    TRUR2MPH+1,X'08'    HIGHLIGHT AMT TO REFUND PROMPT               
         LA    R2,TRUR2MTH                                                      
         NI    1(R2),X'DF'         UNPROTECT AMOUNT TO RETURN FIELD             
*                                                                               
VK30     BAS   RE,CHKEMP           EMP MUST HAVE MONEY WITHHELD                 
*                                                                               
         CLI   RET2MIN,C'N'        IF NOT RETURNING TO MINOR                    
         BNE   VK50                                                             
         MVC   TRUTRSS,TRUSTSS     DISPLAY TRUSTEE'S SS#                        
         OI    TRUTRSSH+6,X'80'                                                 
*                                                                               
VK50     L     R4,AIO              R4=A(MINOR W4 RECORD)                        
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL            R4=A(EMPLOYEE W4 DETAILS ELEMENT)            
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAW4D,R4                                                         
         MVC   W4TYPE,TAW4TYPE     SAVE EMPLOYEE'S W4 TYPE                      
*                                                                               
         BAS   RE,NOPREV           CANNOT HAVE PREV TRANSACTION TODAY           
*                                                                               
         MVI   CURRENCY,C'U'       DEFAULT US$                                  
         LA    R2,TRUCURRH                                                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         CLI   TRUCURR,C'U'        ONLY ACCEPT US$ OR CAN$                      
         BE    VK60                                                             
         CLI   TRUCURR,C'C'                                                     
         BE    VK60                                                             
         CLI   TRUCURR,C'E'        AND EUROS                                    
         BNE   ERRINV                                                           
VK60     MVC   CURRENCY,TRUCURR                                                 
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              IF INPUTTED SS# BELONGS TO A CORPORATION, READ FIRST             
*              ATTACHED MINOR INTO AIO2 AND SAVE MINOR'S SS# IN MINSSN          
         SPACE 1                                                                
W4ISCORP NTR1                                                                   
         XC    MINSSN,MINSSN                                                    
*                                                                               
         L     R4,AIO              R4=A(W4 RECORD)                              
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL            R4=A(EMPLOYEE W4 DETAILS ELEMENT)            
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         USING TAW4D,R4                                                         
         CLI   TAW4TYPE,TAW4TYCO   IF W4 IS TYPE CORPORATION                    
         BE    *+12                                                             
         CLI   TAW4TYPE,TAW4TYCA   OR CANADIAN                                  
         BNE   W4IX                                                             
*                                                                               
         L     R4,AIO              R4=A(W4 RECORD)                              
         MVI   ELCODE,TAWXELQ                                                   
         BAS   RE,GETEL            R4=A(EMPLOYEE W4 DETAILS ELEMENT)            
         BNE   W4IC10                                                           
*                                                                               
         USING TAWXD,R4            SEE IF THERE'S A TRUSTEE PID                 
         CLC   TAWXTSSN,SPACES     BECAUSE IT MAY BE CANADIAN CORP              
         BNH   W4IC10                                                           
         MVC   MINSSN,TGSSN                                                     
         B     W4IX                                                             
*                                                                               
W4IC10   MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLW4PD,R4                                                        
         MVI   TLW4PCD,TLW4CCDQ   READ FIRST MINOR ATTACHED INTO AIO2           
         MVC   TLW4CCRP,TGSSN                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(TLW4CSSN-TLW4PD),KEYSAVE                                     
         BNE   ERRINV                                                           
*                                                                               
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R4,AIO             SAVE FIRST MINOR'S SSN INTO MINSSN            
         USING TLW4D,R4                                                         
         MVC   MINSSN,TLW4SSN                                                   
W4IX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE CHECKS THAT INPUUTED W4 RECORD HAS MONEY                 
*              WITHHELD FOR INPUT EMPLOYER - DISPLAY AND SAVE IN AMOUNT         
         SPACE 1                                                                
CHKEMP   NTR1                                                                   
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R4,AIO              R4=A(MINOR W4 RECORD)                        
         MVI   ELCODE,TAWXELQ                                                   
         BAS   RE,GETEL            R4=A(MINOR W4 EXTRA DETAILS ELEMENT)         
         BNE   ERRINV                                                           
*                                                                               
         USING TAWXD,R4                                                         
         LA    R2,TRUEMPH          R2=A(EMPLOYER FIELD)                         
         LA    R3,TAWXHTP                                                       
         OC    TRUEMP,SPACES       IF EMPLOYER IS TP, TAWXHTP FIELD             
         CLC   TRUEMP,=CL3'TP'                 MUST CONTAIN A TOTAL             
         BE    CEMP10                                                           
         LA    R3,TAWXHPPL         IF EMPLOYER IS P+, TAWXHPPL FIELD            
         CLC   TRUEMP,=CL3'P+'                 MUST CONTAIN A TOTAL             
         BE    CEMP10                                                           
         LA    R3,TAWXHPP          IF EMPLOYER IS PP, TAWXHPP FIELD             
CEMP10   OC    0(L'TAWXHTP,R3),0(R3)           MUST CONTAIN A TOTAL             
         BZ    ERRNOIS                NOTHING TO ISSUE                          
*                                                                               
         EDIT  (4,(R3)),(10,TRUWHLD),2,ALIGN=LEFT,MINUS=YES                     
         MVC   AMOUNT,0(R3)                         DISP AMT WITHHELD           
         B     XIT                                 AND SAVE IN AMOUNT           
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SEE IF THERE ARE ANY PREVIOUS TRUST ISSUES,           
*              VOIDS, REISSUES, REFUNDS, SS# TRANSFERS FOR TODAY FOR            
*              EPLOYER AND SS# - IF THERE ARE, TRANSACTION INVALID              
         SPACE 1                                                                
NOPREV   NTR1                                                                   
         BAS   RE,SETCHK           SET SYSDIR/SYSFIL FOR CHECKS                 
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLCKPD,R4                                                        
         MVI   TLCKPCD,TLCKECDQ    READ EMPLOYEE'S CHECK PASSIVE PTRS           
         MVC   TLCKESSN,TRUMIN     MINOR'S SS#                                  
*                                                                               
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    NP05                                                             
         MVC   TLCKESSN,SPACES                                                  
         GOTO1 SSNUNPK,DMCB,TRUMIN,TLCKESSN                                     
*                                                                               
NP05     MVC   TLCKECUR,CURRENCY   CURRENCY                                     
         MVC   TLCKEEMP,TRUEMP     EMPLOYER                                     
         GOTO1 HIGH                                                             
         B     NP20                                                             
NP10     GOTO1 SEQ                                                              
NP20     CLC   KEY(TLCKEDTE-TLCKPD),KEYSAVE                                     
         BNE   NP30                                                             
         SPACE 1                                                                
         CLI   TLCKEDTE,0           CHECK WITH TODAY'S DATE                     
         BNE   NP30                                                             
         CLC   TLCKEAGY,=CL6'999999' FOR AGENCY 999999 CANNOT ALREADY           
         BE    ERRFUN                EXIST                                      
         B     NP10                                                             
NP30     BAS   RE,SETTAL             SET SYSDIR/SYSFIL FOR NON-CHECKS           
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 1                                                                
VREC     NTR1                                                                   
*        CLI   RET2MIN,C'N'        IF NOT RETURNING TO MINOR                    
*        BNE   *+14                                                             
*        MVC   TRUR2MT,SPACES      CLEAR AMOUNT TO REFUND FIELD                 
*        B     VREC10                                                           
*                                                                               
         LA    R2,TRUR2MTH         IF RETURNING TO MINOR                        
         ZIC   RF,5(R2)            R2=A(AMOUNT TO REFUND FIELD)                 
         LTR   RF,RF               INPUT REQUIRED                               
         BZ    ERRMIS                                                           
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   0(R1),X'FF'         MUST BE VALID CASH VALUE                     
         BE    ERRINV                                                           
         TM    4(R1),X'80'         CANNOT BE NEGATIVE                           
         BO    ERRINV                                                           
         L     RE,4(R1)                                                         
         C     RE,AMOUNT           CANNOT BE GREATER THAN AMT WITHHELD          
         BH    ERRXWTH                                                          
         ST    RE,AMOUNT           SAVE IN AMOUNT                               
         SPACE 1                                                                
VREC10   CLI   PFAID,19            PF KEY MUST BE 19 OR DO NOTHING              
         BNE   MSGPFVD                                                          
*                                                                               
         BAS   RE,GETINV           GET INVOICE NUMBER                           
         BAS   RE,ADDCHK           ADD THE CHECK                                
         BAS   RE,ADDINV           ADD THE INVOICE                              
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE READS THE AGENCY '999999' RECORD TO GET THE NEXT INVOICE         
* NUMBER, UPDATES IT, AND WRITES THE RECORD BACK.                               
         SPACE 1                                                                
GETINV   NTR1                                                                   
         BAS   RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'B1',=C'999999')                           
         BE    *+6                 DEAD AGENCY '999999' REC FOR UPDATE          
         DC    H'0'                DIE IF NOT FOUND                             
         SPACE 1                                                                
         GOTO1 CHNINV,DMCB,=X'001C',WORK   GET NEXT INVOICE NUMBER              
         BNE   ERRAGY                                                           
         MVC   PINVNUM,6(R1)       SAVE PACKED NUMBER OF INVOICE                
         XC    TGINV,=6X'FF'       UN-COMPLEMENT GLOBAL INVOICE                 
         SPACE 1                                                                
         BAS   RE,SETCHK           RESET SYSDIR/SYSFIL FOR CHECKS               
         B     XIT                                                              
         EJECT                                                                  
*              ADD THE CHECK RECORD                                             
         SPACE 1                                                                
ADDCHK   NTR1                                                                   
         USING TLCKD,R3                                                         
         L     R3,AIO              CLEAR KEY AND ENTER INFORMATION              
         XC    TLCKKEY,TLCKKEY                                                  
         MVC   TLCKLEN,DATADISP                                                 
         XC    TLCKSTAT(10),TLCKSTAT                                            
         MVI   TLCKCD,TLCKCDQ      CHECK CODE                                   
         MVC   TLCKAGY,TGAGY       ADJ AGENCY                                   
         MVC   TLCKINV,TGINV       INVOICE                                      
         GOTO1 SSNUNPK,DMCB,TRUMIN,TLCKSSN                                      
*&&DO                                                                           
         MVC   TLCKSSN,TRUMIN      MINOR'S SS#                                  
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    ADDCHK05                                                         
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   TLCKSSN,SPACES                                                   
         MVC   TLCKSSN,TGPID                                                    
*&&                                                                             
ADDCHK05 OC    MINSSN,MINSSN                                                    
         BZ    *+10                                                             
         MVC   TLCKSSN,MINSSN                                                   
         SPACE 1                                                                
         LA    R4,OIELEM           BUILD ORIGINAL AGENCY/INVOICE                
         USING TAOID,R4            ELEMENT (FOR INVOICE) IN OIELEM              
         XC    OIELEM,OIELEM                                                    
         MVI   TAOIEL,TAOIELQ                                                   
         MVI   TAOILEN,TAOILNQ                                                  
         MVC   TAOIAGY,TLCKAGY                                                  
         MVC   TAOIINV,TLCKINV                                                  
         XC    TAOIINV,=6X'FF'                                                  
         SPACE 1                                                                
         LA    R4,ELEM             BUILD PAYMENT DETAILS ELEMENT                
         USING TAPDD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TAPDEL,TAPDELQ      CODE                                         
         MVI   TAPDLEN,TAPDLNQ     LENGTH                                       
         MVC   TAPDW4TY,W4TYPE     SET PAYMENT TYPE TO W4TYPE                   
         OC    MINSSN,MINSSN                                                    
         BZ    *+8                                                              
         MVI   TAPDW4TY,C'C'                                                    
         MVI   TAPDOFF,C'1'        DEFAULT TO OFFICE 1                          
         CLC   TRUEMP,=CL3'PP'     IF EMPLOYER IS PP                            
         BNE   *+8                                                              
         MVI   TAPDOFF,C'A'        DEFAULT TO OFFICE A                          
         MVC   TAPDEMP,TGEMP       EMPLOYER                                     
         OI    TAPDADJS,TAPDADTR   SET TAX REFUND INDICATOR                     
         CLI   CURRENCY,C'C'       CANADIAN CURRENCY?                           
         BNE   *+8                                                              
         OI    TAPDSTAT,TAPDSCAN   MARK IT AS SUCH                              
         CLI   CURRENCY,C'E'       EUROPEAN CURRENCY?                           
         BNE   *+8                                                              
         OI    TAPDPST2,TAPDPEUR   MARK IT AS SUCH                              
         XC    PDELEM,PDELEM                                                    
         MVC   PDELEM,0(R4)        SAVE ELEMENT IN PDELEM FOR INVOICE           
         GOTO1 ADDELEM             ADD ELEMENT TO CHECK RECORD                  
         SPACE 1                                                                
         LA    R4,ELEM             BUILD CHECK DETAILS ELEMENT                  
         USING TACDD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TACDEL,TACDELQ      CODE                                         
         MVI   TACDLEN,TACDLNQ     LENGTH                                       
         CLI   RET2MIN,C'N'        AND IF REFUNDING TO MINOR                    
         BE    *+10                                                             
         MVC   TACDNET,AMOUNT      AMOUNT                                       
         GOTO1 ADDELEM             ADD ELEMENT TO CHECK RECORD                  
         SPACE 1                                                                
         LA    R4,ELEM             BUILD OTHER DEDUCTION ELEMENT                
         USING TAODD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TAODEL,TAODELQ      CODE                                         
         MVI   TAODLEN,TAODLNQ     LENGTH                                       
         MVI   TAODTYPE,TAODTYPT   TYPE                                         
         MVC   TAODAMT,AMOUNT                                                   
         L     R1,TAODAMT                                                       
         LNR   R1,R1                                                            
         ST    R1,TAODAMT          NEGATIVE AMOUNT                              
         GOTO1 ADDELEM             ADD ELEMENT TO CHECK RECORD                  
         SPACE 1                                                                
         LA    R4,ELEM             BUILD CAST DETAILS ELEMENT                   
         USING TACAD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TACAEL,TACAELQ      CODE                                         
         MVI   TACALEN,TACALNQ     LENGTH                                       
         GOTO1 ADDELEM             ADD ELEMENT TO CHECK RECORD                  
         SPACE 1                                                                
         CLI   RET2MIN,C'Y'        IF NOT RETURNING MONEY TO MINOR              
         BE    ADDCHK10                                                         
         LA    R4,ELEM             BUILD TRUSTEE SS# ELEMENT                    
         USING TANUD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TANUEL,TANUELQ      CODE                                         
         MVI   TANULEN,TANULNQ     LENGTH                                       
         MVI   TANUTYPE,TANUTRST   TYPE                                         
         MVC   TANUMBER(9),TRUSTSS TRUSTEE'S SS#                                
         ZIC   RE,TANULEN                                                       
         AHI   RE,9                                                             
         STC   RE,TANULEN                                                       
         GOTO1 ADDELEM             ADD ELEMENT TO CHECK RECORD                  
         SPACE 1                                                                
ADDCHK10 OC    MINSSN,MINSSN       IF CORPORATION                               
         BZ    ADDCHK20                                                         
         LA    R4,ELEM                                                          
         USING TATID,R4            BUILD TAX ID ELEMENT                         
         XC    ELEM,ELEM                                                        
         MVI   TATIEL,TATIELQ                                                   
         MVI   TATILEN,TATILNQ                                                  
         MVI   TATITYPE,TATITYCO                                                
         MVC   TATIID,SPACES                                                    
         MVC   TATIID(9),TRUMIN       WITH CORP'S SS#                           
         SPACE 1                                                                
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    ADDCHK15                                                         
         GOTO1 SSNUNPK,DMCB,TRUMIN,TGSSN                                        
         MVC   TATIID(9),TGSSN                                                  
         SPACE 1                                                                
ADDCHK15 MVI   TATICRPN,C'1'                                                    
         GOTO1 ADDELEM                                                          
ADDCHK20 GOTO1 ADDREC              ADD NEW CHECK RECORD                         
         SPACE 1                                                                
         LA    R3,PTRBLK           ADD PASSIVE POINTERS                         
         XC    0(L'PTRBLK,R3),0(R3)                                             
         GOTO1 ADDPTRS,DMCB,(R3)                                                
ACX      B     XIT                                                              
         EJECT                                                                  
*              ADD NEW INVOICE RECORD                                           
         SPACE 1                                                                
ADDINV   NTR1                                                                   
         BAS   RE,SETTAL           SET SYSDIR/SYSFIL FOR NON-CHECKS             
         SPACE 1                                                                
         USING TLIND,R4                                                         
         L     R4,AIO              CLEAR KEY AND ENTER INFORMATION              
         XC    TLINKEY,TLINKEY                                                  
         MVC   TLINLEN,DATADISP                                                 
         XC    TLINSTAT(10),TLINSTAT                                            
         MVI   TLINCD,TLINCDQ      INVOICE CODE                                 
         MVC   TLINAGY,TGAGY       ADJUSTMENT AGY/INV                           
         MVC   TLININV,TGINV       INVOICE #                                    
         XC    TLININV,=6X'FF'     COMPLIMENT INVOICE #                         
         SPACE 1                                                                
         MVC   TRUIINV(6),TGAGY                                                 
         MVI   TRUIINV+7,C'/'                                                   
         GOTO1 TINVCON,DMCB,TGINV,TRUIINV+9,DATCON                              
         OI    TRUIINVH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R4,ELEM             BUILD INVOICE STATUS ELEM                    
         USING TAIND,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TAINEL,TAINELQ      CODE                                         
         MVI   TAINLEN,TAINLNQ     LENGTH                                       
         MVC   TAINIID,TWAORIG     USER ID                                      
         MVC   TAINIST,TGCTSTAF    STAFF ID                                     
         MVC   TAINIDTE,TGTODAY1   ASSIGNMENT DATE                              
         TIME  DEC                                                              
         STCM  R0,14,TAINITIM      ASSIGNMENT TIME                              
         MVI   TAINSTA2,TAINSADJ   SET FOR PAYROLL ADJUSTMENT                   
         GOTO1 ADDELEM             ADD ELEMENT TO INVOICE RECORD                
         SPACE 1                                                                
         LA    R4,ELEM             BUILD DUE DATE ELEMENT                       
         USING TADDD,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   TADDEL,TADDELQ      CODE                                         
         MVI   TADDLEN,TADDLNQ     LENGTH                                       
         MVC   TADDDATE,TGTODAY1   SET DATE TO TODAY'S DATE                     
         GOTO1 ADDELEM             ADD ELEMENT TO INVOICE RECORD                
         SPACE 1                                                                
         MVC   ELEM(TAPDLNQ),PDELEM   ADD PAYMENT DETAILS ELEMENT FROM          
         GOTO1 ADDELEM                CHECK RECORD TO INVOICE RECORD            
         SPACE 1                                                                
         XC    ELEM,ELEM           ADD ORIGINAL AGY/INV ELEMENT                 
         MVC   ELEM(TAOILNQ),OIELEM           TO INVOICE RECORD                 
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
         GOTO1 ADDREC              ADD NEW INVOICE RECORD                       
         SPACE 1                                                                
         LA    R3,PTRBLK           ADD PASSIVE POINTERS                         
         XC    0(L'PTRBLK,R3),0(R3)                                             
         GOTO1 ADDPTRS,DMCB,(R3)                                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET SYSDIR/SYSFIL FOR NON-CHECKS                      
         SPACE                                                                  
SETTAL   DS    0H                                                               
         MVC   SYSDIR,SVSYSDIR                                                  
         MVC   SYSFIL,SVSYSFIL                                                  
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO SET SYSDIR/SYSFIL TO CHECKS                           
         SPACE                                                                  
SETCHK   DS    0H                                                               
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         BR    RE                                                               
         EJECT                                                                  
*              INFORMATION AND ERROR MESSAGES                                   
         SPACE 1                                                                
MSGPFVD  MVI   MYMSGNO1,45                                                      
         B     *+8                                                              
MSGPFTR  MVI   MYMSGNO1,46                                                      
         OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
*                                                                               
ERRFUN   MVI   ERROR,81                                                         
         B     THEEND                                                           
ERRAGY   MVI   ERROR,ERAGYERR                                                   
         B     THEEND                                                           
ERRMIS   MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
ERRXWTH  MVC   MYMSGNO,=Y(ERAMTXWH)                                             
         B     *+10                                                             
ERRNOIS  MVC   MYMSGNO,=Y(ERNOISSU)                                             
         MVI   MYMTYP,C'E'                                                      
         OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
*              PF KEY TABLE                                                     
         SPACE 1                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'W4      ',CL8'DISPLAY '                               
PF13X    EQU   *                                                                
         DC    AL1(PF19X-*,19,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF19X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRE6D          SCREEN                                       
         EJECT                                                                  
         DS    D                                                                
AMOUNT   DS    XL4                                                              
PTRBLK   DS    CL(L'TLDRREC*6+1)                                                
RET2MIN  DS    CL1                                                              
PINVNUM  DS    PL3                 PACKED NEXT INVOICE NUMBER                   
PDELEM   DS    XL(TAPDLNQ)                                                      
OIELEM   DS    XL(TAOILNQ)                                                      
W4TYPE   DS    CL1                                                              
TRUSTSS  DS    CL9                                                              
MINSSN   DS    CL9                                                              
CURRENCY DS    CL1                 CURRENCY                                     
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'106TAGENE6   05/02/13'                                      
         END                                                                    
