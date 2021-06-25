*          DATA SET PPLFM01S   AT LEVEL 007 AS OF 05/01/02                      
*PHASE T40401A                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE GETPROF                                                                
         TITLE 'T40401G PRINT LOGICAL FILE MAINT.  **CHANGE LOG**'              
**                                                                              
*                                                                               
* KWAN 07/00     FOR FINANCIAL CLIENTS DO NOT ALLOW COS2                        
*                                                                               
* SMYE 09/23/99  FIX MEDIA NAME OVERRIDE BUG - WOULD NOT DISPLAY                
*                  MORE THAN 9 CHARACTERS EVEN IF 10 WERE USED                  
*                                                                               
* KWAN 04/14/99  ADD CODES FOR PROF READING BEFORE CHKING OPT FLDS              
*                                                                               
* KWAN 02/09/99  REVISE CODES FOR COST2 FACTOR ELEMENT                          
*                                                                               
* KWAN 01/14/99  ADD CODES FOR COST2 FACTOR ELEMENT                             
*                                                                               
* SMYE 09/98     DISALLOW CHANGE TO FROZEN STATUS FOR WESTERN AGENCIES          
*                  IF LIMIT ACCESS BYTE ("CREDIT LIMIT") AT TWA+12              
*                  NOT X'04'  (AT CLTCHG..PROC)                                 
*                  R7 (PREVIOUSLY UNUSED) ADDED AS 3RD BASE                     
*                                                                               
* BPLA 5/98      ACCEPT SFH= FOR ALL AGENCIES                                   
*                ONLY DISPLAY DEFAULT FOR WESTERN (AND SJR)                     
*                ALSO ACCEPT SFH=YES AND SFH=NO                                 
*                                                                               
* SMYE 04/98     CODE FOR 4TH CLIENT OPTION (FRZ FOR "FROZEN")                  
*                                                                               
* SMYE 10/97     CODE FOR 3RD CLIENT OPTION ("SPECIAL FINANCIAL                 
*                   HANDLING" FOR WESTERN AGENCIES ONLY)                        
*                                                                               
* SMYE 5/97      MOVE FINANCIAL FIELD AGENCIES TO TABLE - CHANGE                
*                NOCLR AND EDIT22 AND PUTCFL4D TO USE THIS TABLE                
*                                                                               
* SMYE 2/97      ALLOW ANY CHARACTER EXCEPT '=' IN MEDIA NAME OVERRIDE          
*                                                                               
* SMYE 10/96     FIX BUG - NOT DELETING MEDIA NAME OVERRIDE ELEMENT             
*                IF ZEN CLIENT CODE ELEMENT NOT FOUND                           
*                                                                               
* SMYE 9/96      DISPLAY HEX VALUE OF CLIENT OFFICE (CONDITIONALLY)             
*                                                                               
* SMYE 08/96     DISALLOW CERTAIN CHARACTERS FOR CLIENT OFFICE                  
*                                                                               
* SMYE 5/96      CODE FOR CLIENT OPTIONS (2ND ONE IS MED NAME OVERRIDE)         
*                                                                               
* BPLA 6/95      OFFICE/CLIENT PASSIVE POINTERS (REC CODE = X'A2')              
*                                                                               
* BPLA 5/95      ON ADDS CHECK SVCTAGY AND IF PRESENT,                          
*                DISPLAY NAME FROM CONTROL FILE RECORD                          
*                                                                               
* BPLA 12/94     NEW ACC OFFICE CHECKING                                        
*                                                                               
* BPLA 11/94     CODE FOR CLIENT OPTIONS (FIRST ONE IS ZENITH CLT)              
*                                                                               
* BPLA 8/31/94   FIX BUGS IN DRD OVERRIDE CLT CODE                              
*                                                                               
* SMUR 06/01/94  DRD OVERRIDE CLIENT                                            
*                                                                               
* LWEI 06/01/93  PST CODES                                                      
*                                                                               
* BPLA 10/22/91  PCLTBNAM, PCLTLIN1, PCLTLIN2 NO LONGER REQUIRED                
*                                                                               
* BPLA 8/27/91   ADD A SECOND I/O STANDARD COMMENT                              
*                                                                               
* BPLA 4/16/91   ALLOW FINANCIAL FOR XYZ                                        
*                                                                               
* BPLA 2/4/91    ALLOW 5 DIGITS FOR PCLTNUM - CARRY AS 3 BYTE BINARY            
*                WITH X'80' SET-ON IN HIGH BYTE                                 
*                MAX IS 99999 = X'01869F' WHICH BECOMES X'81869F'               
*                                                                               
* BPLA 12/13/90  GST DISPLAY WILL DEFAULT TO "S" FOR CANADIAN AGY               
*                ALSO GST IS NOW REQUIRED                                       
*                SCREEN WILL BE BROUGHT UP WITH "S" FOR CANADIAN AGY            
* 11/16/90 ROSA ADD GST CODE                                                    
*                                                                               
* 9/6/90 ROSA  INCREASE INTERFACE CODE TO 4 POSITION                            
*              FOR AGENCY LTNY PER SOBI                                         
*                                                                               
* 8/29/90  BPLA    ADD LOGIC FOR I/O STANDARD COMMENT                           
*                                                                               
* 8/7/90   BPLA    AGENCY PROFILE BYTE 12 NEW VALUES                            
*                  1 = OFFICE REQUIRED (OLD VALUE)                              
*                  A = ACC OFFICE REQUIRED                                      
*                  B = BOTH OFFICE AND ACC OFFICE REQUIRED                      
*                                                                               
* 11/29/89 BPLA    ADD ACCOUNTING OFFICE                                        
*                                                                               
* 3/16/89  BPLA    ALLOW FINANCIAL CLIENTS FOR MK                               
*                                                                               
* 2/3/89   BPLA    ALLOW BILLING GROUPS FOR ALL AGENCIES                        
*                                                                               
* 7/11/88  ROSA    ADD BILLING GROUP CODE-- BILL ALL CLIENTS HAVING02           
*             THE SAME CODE ITH ONE REQUEST..  ALSO MODIFY OFFICE               
*             CODE TO ONE BYTE OF ENTRY                                         
* ROSA 4/26/88  ADD LOGIC TO HANDLE EXCEPTION SUBROUTINE FOR FIELD              
*               PCLTNUM  USED FOR INTERFACE                                     
*                                                                               
         TITLE 'T40401G PRINT LOGICAL FILE MAINT.   CLT SCREEN'                 
T40401   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40401,R9,R7    ** NOTE R9 AND R7 AS ADD'L BASE REG'S.         
         L     RC,0(1)                                                          
         USING GENOLD,RC                                                        
         USING T404FFD,RA                                                       
*                                                                               
         RELOC RELO                                                             
******                                                                          
****     NOTE VTWA IS REALLY A(COMFACS)  SAVED IN PPLFM00                       
*****                                                                           
*                                                                               
         EJECT                                                                  
         MVI   SVACCOFC,C'Y'    SET 2 CHAR ACC OFFICE REQUIRED                  
         CLI   SVAGPF12,C'A'    AGY PROF REQUIRES ACC OFFICE                    
         BE    SET10                                                            
         CLI   SVAGPF12,C'B'  AGY PROF REQUIRES OFFICE AND ACC OFFICE           
         BE    SET10                                                            
         MVI   SVACCOFC,C'N'                                                    
*                                                                               
SET10    DS    0H                                                               
         LA    R4,IOAREA                                                        
         LH    R5,=H'1000'                                                      
         BAS   RE,CLEARWRK                                                      
         LA    R4,PRDTAB                                                        
         LH    R5,=H'2000'                                                      
         BAS   RE,CLEARWRK                                                      
*                                                                               
         MVC   PCLTKEY,KEY                                                      
         MVC   PCLTPROF,=32C'0'                                                 
         MVC   PCLTELEM(2),=X'02AA'                                             
         MVC   PCLTLEN,=X'00CB'                                                 
         CLI   SCRNUM,X'F1'         SEE IF I HAVE CLT SCREEN                    
         BE    CLT2                                                             
         MVI   DSPSW,1           SET TO FORMAT                                  
         GOTO1 VCALLOV,DMCB,HDRLAST,X'D90404F1'                                 
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SCRNUM,X'F1'                                                     
* IF AGENCY IS NOT CANADIAN DO NOT DISPLAY GST/PST CODE                         
*                                                                               
         CLI   WNATION,C'C'                                                     
         BE    NOCLR                                                            
         XC    CLTGSTA,CLTGSTA  CLEAR DESCRIPTION                               
         FOUT  CLTGSTAH                                                         
         XC    CLTGST,CLTGST     CLEAR 'S' - DEFAULT                            
         FOUT  CLTGSTH                                                          
         OI    CLTGSTH+1,X'20'                                                  
*                                                                               
         XC    CLTPSTA,CLTPSTA   CLEAR DESCRIPTION                              
         FOUT  CLTPSTAH                                                         
         OI    CLTPSTH+1,X'20'                                                  
**                                                                              
         B     NOCLR                                                            
NOCLR    DS    0H                                                               
         LA    RE,AGYXTBL        AGENCIES TO NOT CLEAR FINANCIAL FIELD          
NOCLRLUP CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    NOCLRX              YES - CLEAR FINANCIAL FIELD                  
         CLC   AGYALPHA,0(RE)      AGENCY ON TABLE?                             
         BE    CLT1                YES - DO NOT CLEAR                           
         LA    RE,2(RE)            NEXT ENTRY                                   
         B     NOCLRLUP                                                         
*****    CLC   AGYALPHA,=C'DM'            IF AGENCY IS NOT DOREMUS              
*****    BE    CLT1                       CLEAR 'FINANCIAL' FIELD OFF           
*****    CLC   AGYALPHA,=C'MK'            IF AGENCY IS NOT MK                   
*****    BE    CLT1                       CLEAR 'FINANCIAL' FIELD OFF           
*****    CLC   AGYALPHA,=C'SJ'            OR SJR                                
*****    BE    CLT1                       CLEAR 'FINANCIAL' FIELD OFF           
*****    CLC   AGYALPHA,=C'XD'            OR XYZ                                
*****    BE    CLT1                       CLEAR 'FINANCIAL' FIELD OFF           
*****    CLC   AGYALPHA,=C'WJ'            OR WITEST                             
*****    BE    CLT1                                                             
NOCLRX   XC    CLTFINL,CLTFINL            CLEAR 'FINANCIAL' FIELD OFF           
         FOUT  CLTFINLH                   SCREEN AND PROTECT INPUT              
         OI    CLTFINH+1,X'20'            FIELD.                                
         OI    CLTFINH+6,X'80'                                                  
*****                                                                           
CLT1     CLI   BACT,1                                                           
         BE    NOTDONE                                                          
*                                                                               
CLT2     DS    0H                                                               
         CLI   BACT,1                                                           
         BE    CLTSCRN                                                          
         MVC   KEY+27(4),CLTADDR   READ CLTHDR                                  
         BAS   RE,GETREC                                                        
CLTSCRN  CLI   DSPSW,0                                                          
         BNE   FORMATC                                                          
*                      CLIENT SCREEN IN TWA  SO EDIT IT UNLESS                  
*                       ACTION=DISPLAY                                          
         CLI   BACT,X'03'                                                       
         BE    FORMATC                                                          
*                                                                               
         CLI   BACT,1         SEE IF ADD                                        
         BNE   CLTS3                                                            
         CLC   SVCTAGY,=C'  '   SEE IF I HAVE A CTFILE ID                       
         BNH   CLTS3                                                            
         BAS   RE,CHKCTCLT     GO DISPLAY NAME FROM CTFILE RECORD               
         LTR   R3,R3            CHECK FOR ERROR RETURN                          
         BNZ   ERROR                                                            
*                                                                               
CLTS3    DS    0H                                                               
         LA    R2,CLTCLTNH                                                      
         BAS   RE,ANY                                                           
         XC    PCLTNAME,PCLTNAME                                                
         MVC   PCLTNAME,CLTCLTN                                                 
         XC    PCLTBNAM,PCLTBNAM                                                
         LA    R2,CLTBRNH                                                       
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   PCLTBNAM,CLTBRN                                                  
*                                                                               
         LA    R2,CLTAL1H                                                       
         XC    PCLTLIN1,PCLTLIN1                                                
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   PCLTLIN1,CLTAL1                                                  
*                                                                               
         LA    R2,CLTAL2H                                                       
         XC    PCLTLIN2,PCLTLIN2                                                
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   PCLTLIN2,CLTAL2                                                  
*                                                                               
         LA    R2,CLTATTNH                                                      
         XC    PCLTATTN,PCLTATTN                                                
         CLI   5(R2),0                                                          
         BE    CKOFF                                                            
         MVC   PCLTATTN,CLTATTN                                                 
CKOFF    LA    R2,CLTOFFH                                                       
         MVC   SVCOFFC,PCLTOFF      SAVE OLD OFFICE CODE                        
*                                                                               
         XC    CLTOFFX,CLTOFFX     CLEAR CLT OFFICE HEXOUT AREA                 
         FOUT  CLTOFFXH                                                         
         XC    PCLTOFF,PCLTOFF                                                  
         CLI   5(R2),0                                                          
         BNE   CKOFF5                                                           
         CLI   SVAGPF12,C'B'       IF 1 OR B OFFICE MUST BE ENTERED             
         BE    CKOFF2                                                           
*                                                                               
         CLI   SVAGPF12,C'1'       IF 1, OFFICE MUST BE ENTERED                 
         BNE   CKACC                                                            
CKOFF2   LA    R3,1                MISSING INPUT ERROR                          
         B     ERROR                                                            
*                                                                               
CKOFF5   CLI   5(R2),1             MAX OF 1 CHAR FOR NOW                        
         BNE   FLDERR              EVEN THOUGH RECORD HAS 3 BYTES               
*                                  AND SCREEN FIELD IS 3 LONG                   
CKOFF6   CLI   CLTOFF,C'='                                                      
         BE    FLDERR              EDIT FOR INVALID CHARACTERS                  
         CLI   CLTOFF,C'-'         FOR CLIENT OFFICE                            
         BE    FLDERR                                                           
         CLI   CLTOFF,C','                                                      
         BE    FLDERR                                                           
         CLI   CLTOFF,C'.'                                                      
         BNE   CKOFF9                                                           
*                                                                               
FLDERR   LA    R3,FLDINV           INVALID INPUT ERROR                          
         B     ERROR                                                            
*                                                                               
CKOFF9   MVC   PCLTOFF(1),CLTOFF                                                
*                                                                               
*                                  CONDITIONALLY SHOW HEX OF CLT OFFICE         
         CLI   CLTOFF,X'40'        SHOW HEX VALUE ?                             
         BNH   CKACC               NO                                           
         CLI   CLTOFF,X'D0'        SHOW HEX VALUE ?                             
         BE    CKOFF9B             YES                                          
         CLI   CLTOFF,X'E0'        SHOW HEX VALUE ?                             
         BE    CKOFF9B             YES                                          
         CLI   CLTOFF,C'A'         SHOW HEX VALUE ?                             
         BL    CKOFF9B             YES                                          
         CLI   CLTOFF,C'9'         SHOW HEX VALUE ?                             
         BNH   CKACC               NO                                           
*                                                                               
CKOFF9B  GOTO1 =V(HEXOUT),DMCB,CLTOFF,CLTOFFX,L'CLTOFF,RR=RELO                  
         FOUT  CLTOFFXH                                                         
*                                                                               
CKACC    CLI   T404FFD+1,C'*'      DDS TERM                                     
         BE    EDITACOF                                                         
         CLI   T404FFD+6,C'*'      OFFICE LIMIT ACCESS                          
         BNE   EDITACOF                                                         
         CLC   PCLTOFF(1),T404FFD+7   ELSE CODES MUST MATCH                     
         BE    EDITACOF                                                         
         LA    R3,CACCERR          ACCESS ERROR                                 
         B     ERROR                                                            
*                                  SO THEY CAN'T ADD TO WRONG OFFICE            
*****                                                                           
EDITACOF LA    R2,CLTAOFFH                                                      
         CLI   BACT,1            SEE IF ADDING                                  
         BE    EDITACO5                                                         
         TM    4(R2),X'20'       HAS FIELD BEEN CHANGED?                        
         BO    CKNUM             NO                                             
*                                                                               
EDITACO5 BAS   RE,CHKAOFF                                                       
         BNE   MYEXIT            ERROR RETURN                                   
         B     CKNUM                                                            
****                                                                            
*****    OLD ACC OFFICE EDIT - NO-OPED                                          
**************************                                                      
*****    XC    PCLTAOFC,PCLTAOFC    CLEAR ACC OFFICE                            
*****    XC    PCLTACCA,PCLTACCA    CLEAR ACC OFFICE AGENCY                     
*****    CLI   5(R2),0          ANY INPUT                                       
*****    BE    EDITAO5                                                          
*****    MVC   PCLTAOFC,CLTAOFF  MOVE ACC OFFICE                                
*****    OC    PCLTAOFC,SPACES                                                  
*****    CLI   PCLTAOFC,C'A'       MUST BE A-Z OR 0-9                           
*****    BL    FLDERR                                                           
*****    CLI   PCLTAOFC+1,C' '                                                  
*****    BE    CKNUM                                                            
*****    CLI   PCLTAOFC+1,C'A'                                                  
*****    BL    FLDERR                                                           
*****    B     CKNUM                                                            
*                                                                               
****AO5  CLI   SVAGPF12,C'A'       ACC OFFICE REQUIRED                          
****     BE    EDITAO7                                                          
****     CLI   SVAGPF12,C'B'       BOTH OFFICE AND ACC OFF REQ                  
****     BNE   CKNUM                                                            
*                                                                               
****AO7  LA    R3,1                MISSING INPUT ERROR                          
****     B     ERROR                                                            
*                                                                               
*                                                                               
CKNUM    LA    R2,CLTNUMH                                                       
         XC    PCLTNUM,PCLTNUM                                                  
         CLI   5(R2),0                                                          
         BE    CKPROF                                                           
         TM    4(R2),X'08'        TEST FOR NUMERICS                             
         BNO   CKNUM1                                                           
         BAS   RE,PACK                                                          
         OI    DUB+7,X'0F'                                                      
         BAS   RE,EXCEPTN          TEST FOR EXCEPTION AGENCIES                  
         B     LOADIT          EXCEPTION AGENCY FOUND LOAD ROUTINE              
         CLI   5(R2),4        IF NON EXCEPTION LENGTH M/B 3 OR LESS             
         BL    DOUNPK                                                           
ERRCEPTN LA    R3,FLDINV    FIELD INVALID                                       
         B     ERROR                                                            
*                                                                               
*                                                                               
*                                                                               
DOUNPK   UNPK  PCLTNUM,DUB+6(2)                                                 
         B     CKPROF                                                           
*                                                                               
*                                 ALPHA/NUMERIC INPUT                           
CKNUM1   BAS   RE,EXCEPTN          TEST FOR EXCEPTION AGENCIES                  
         B     CKNUM2          EXCEPTION AGENCY FOUND ERROR                     
         CLI   5(R2),3        IF NON EXCEPTION LENGTH M/B 3 OR LESS             
         BH    ERRCEPTN                                                         
         B     CKNUMX                                                           
*                                                                               
CKNUM2   CLI   5(R2),2           MUST BE 2 CHARS                                
         BH    ERRCEPTN                                                         
CKNUMX   MVC   PCLTNUM,CLTNUM                                                   
         OC    PCLTNUM,SPACES                                                   
CKPROF   LA    R2,CLTPROFH                                                      
         BAS   RE,ANY                                                           
         LA    R3,PROFERR                                                       
         CLI   5(R2),32                                                         
         BNE   ERROR                                                            
         XC    PCLTPROF,PCLTPROF                                                
         MVC   PCLTPROF,CLTPROF                                                 
         CLI   CLTPROF+5,C'2'                                                   
         BNE   EDIT1                                                            
         LA    R3,MISSMST          MISSING CLT MASTER CODE                      
         CLC   CLTPROF+6(3),=C'000'                                             
         BE    ERROR                                                            
         CLC   CLTPROF+6(3),=C'   '                                             
         BE    ERROR                                                            
         MVC   SAVEKEY(32),KEY                                                  
         MVC   DMWORK1(96),DMWORK                                               
         MVC   KEY+4(3),CLTPROF+6                                               
         LA    R3,MCLTNF           MASTER CLT NOT FD                            
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'DMREAD'),=C'PRTDIR',KEY,      X        
               KEYSAVE,(TERMNAL,0)                                              
         NC    DMCB+8(1),DMOUTBTS                                               
         BNZ   ERROR                                                            
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PRTFILE',         X        
               KEYSAVE+27,IOAREA+500,(TERMNAL,DMWORK)                           
         NC    DMCB+8(1),DMOUTBTS                                               
         BNZ   ERROR                                                            
         CLI   IOAREA+664,C'1'       CLTPROF+5 IN MASTER                        
         BNE   ERROR                                                            
         MVC   DMWORK(96),DMWORK1                                               
         MVC   KEY(32),SAVEKEY                                                  
         B     EDITCSC                                                          
*                                                                               
EDIT1    CLI   CLTPROF+5,C'1'                                                   
         BNE   EDITCSC                                                          
         LA    R3,PROFERR1                                                      
         CLC   CLTPROF+6(3),=C'000'                                             
         BNE   ERROR                                                            
*****                                                                           
EDITCSC  DS    0H                      EDIT CONTRACT STANDARD COMMENT           
         LA    R2,CLTCSCH                                                       
         LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'10'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   EDITC5                                                           
*                                        CHANGE - DELETE OLD ELEM               
         GOTO1 VRECUP,DMCB,(1,PCLTREC),0(R6)                                    
*                                                                               
EDITC5   CLI   5(R2),0                 CHK FOR INPUT                            
         BE    EDITISC                 NO - DONE                                
         LA    R3,FLDINV                                                        
         CLI   5(R2),6                                                          
         BH    ERROR                                                            
         MVC   SAVEKEY(32),KEY                                                  
         MVC   KEY+4(6),SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LCR   RF,R1                                                            
         AH    RF,=H'6'                                                         
         LA    RF,KEY+4(RF)                                                     
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
*                                                                               
         MVI   KEY+3,X'40'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    EDITC8                                                           
         LA    R3,CMNTERR          COMMENT REC NOT FOUND                        
         B     ERROR                                                            
*                                                                               
EDITC8   MVC   ELEM(2),=X'1008'                                                 
         MVC   ELEM+2(6),KEY+4                                                  
         GOTO1 VRECUP,DMCB,(1,PCLTREC),ELEM,0(R6)                               
         MVC   KEY(32),SAVEKEY           RESTORE CLIENTHDR KEY                  
*                                                                               
*****                                                                           
EDITISC  DS    0H                      EDIT I/O STANDARD COMMENT                
         LA    R2,CLTISCH                                                       
EDITI4   LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'11'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   EDITI5                                                           
*                                        CHANGE - DELETE OLD ELEM               
         GOTO1 VRECUP,DMCB,(1,PCLTREC),0(R6)                                    
         B     EDITI4                  GO DELETE ANY MORE                       
*                                                                               
EDITI5   CLI   5(R2),0                 CHK FOR INPUT                            
         BE    EDITI7                  NO - DONE                                
         LA    R3,FLDINV                                                        
         CLI   5(R2),6                                                          
         BH    ERROR                                                            
         MVC   SAVEKEY(32),KEY                                                  
         MVC   KEY+4(6),SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LCR   RF,R1                                                            
         AH    RF,=H'6'                                                         
         LA    RF,KEY+4(RF)                                                     
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
*                                                                               
         MVI   KEY+3,X'40'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    EDITI6                                                           
         LA    R3,CMNTERR          COMMENT REC NOT FOUND                        
         B     ERROR                                                            
*                                                                               
EDITI6   MVC   ELEM(2),=X'1108'                                                 
         MVC   ELEM+2(6),KEY+4                                                  
         GOTO1 VRECUP,DMCB,(1,PCLTREC),ELEM,0(R6)                               
         MVC   KEY(32),SAVEKEY           RESTORE CLIENTHDR KEY                  
*                                                                               
EDITI7   LA    R2,CLTISC2H             2ND I/O STND COM                         
         CLI   5(R2),0                 CHK FOR INPUT                            
         BE    EDITDRD                 NO - DONE                                
         LA    R3,FLDINV                                                        
         CLI   5(R2),6                                                          
         BH    ERROR                                                            
         MVC   SAVEKEY(32),KEY                                                  
         MVC   KEY+4(6),SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LCR   RF,R1                                                            
         AH    RF,=H'6'                                                         
         LA    RF,KEY+4(RF)                                                     
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
*                                                                               
         MVI   KEY+3,X'40'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    EDITI8                                                           
         LA    R3,CMNTERR          COMMENT REC NOT FOUND                        
         B     ERROR                                                            
*                                                                               
EDITI8   MVC   ELEM(2),=X'1108'                                                 
         MVC   ELEM+2(6),KEY+4                                                  
         LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'FF'                                                     
         BAS   RE,PNEXTEL         GETS ME TO END OF RECORD                      
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PCLTREC),ELEM,0(R6)                               
         MVC   KEY(32),SAVEKEY           RESTORE CLIENTHDR KEY                  
*                                                                               
*****                                                                           
EDITDRD  DS    0H                  EDIT DRD OVERRIDE CLIENT                     
         LA    R2,CLTDRDH                                                       
EDITD1   LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'30'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   EDITD2                                                           
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PCLTREC),0(R6)                                    
*                                                                               
EDITD2   CLI   5(R2),0             ANY INPUT                                    
         BE    EDIT2                                                            
         LA    R3,53               RECORD NOT FOUND                             
         MVC   SAVEKEY(32),KEY                                                  
*                                                                               
         LA    R4,KEY                                                           
         MVC   PCLTKCLT-PCLTREC(3,R4),CLTDRD  MOVE DRD CODE                     
         OC    PCLTKCLT-PCLTREC(3,R4),SPACES  BLANK PAD WITH SPACES             
         GOTO1 HIGH                                                             
*                                                                               
EDITD2A  CLC   KEY(20),KEYSAVE                                                  
         BE    EDITD3                                                           
         B     ERROR                                                            
*                                                                               
EDITD3   LA    R3,14               INVALID CLT CODE                             
         MVI   PDIVKRCD-PDIVREC(R4),X'03'  DIVISION RECORD                      
         MVC   PDIVKDIV-PDIVREC(L'PDIVKDIV,R4),=3X'F0'                          
         GOTO1 HIGH                                                             
*                                                                               
EDITD3A  CLC   KEY(PDIVLEN-PDIVKEY),KEYSAVE                                     
         BE    EDITD4                                                           
         B     ERROR                                                            
*                                                                               
EDITD4   XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'3008'                                                 
         MVC   ELEM+2(3),KEY+4     DRD CLT CODE                                 
         GOTO1 VRECUP,DMCB,(1,PCLTREC),ELEM,0(R6) ADD ELEMENT                   
         MVC   KEY(32),SAVEKEY     RESTORE KEY                                  
*                                                                               
*                                                                               
EDIT2    LA    R2,CLTGSTH                                                       
         MVI   PCLTGST,0                                                        
         CLI   WNATION,C'C'        CANADIAN                                     
         BNE   EDIT22                                                           
         LA    R3,2                                                             
***      CLI   5(R2),0             ANY INPUT                                    
***      BE    EDIT22              NO - O.K.                                    
         LA    RF,VALGSTC                                                       
CLI255   CLI   0(RF),255                                                        
         BE    ERROR                                                            
         CLC   0(1,RF),CLTGST                                                   
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     CLI255                                                           
         MVC   PCLTGST,CLTGST      MOVE GST TAX CODE                            
*                                                                               
         B     EDIT22                                                           
*                                                                               
VALGSTC  DC    C'SXZ',X'FF'                                                     
EDIT22   LA    R3,FLDINV                                                        
         LA    RE,AGYXTBL        AGENCIES TO SHOW FINANCIAL FIELD               
EDT22LUP CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    EDITBILG            YES - SKIP TO BILLING GROUP                  
         CLC   AGYALPHA,0(RE)      AGENCY ON TABLE?                             
         BE    EDIT2A              YES - DO NOT SKIP FINANCIAL FIELD            
         LA    RE,2(RE)            NEXT ENTRY                                   
         B     EDT22LUP                                                         
*****    CLC   AGYALPHA,=C'DM'         IF AGENCY NOT DOREMUS SKIP               
*****    BE    EDIT2A                                                           
*****    CLC   AGYALPHA,=C'MK'         OR AGENCY  MK                            
*****    BE    EDIT2A                                                           
*****    CLC   AGYALPHA,=C'XD'         OR AGENCY XYZ                            
*****    BE    EDIT2A                                                           
*****    CLC   AGYALPHA,=C'WJ'         OR AGENCY WITEST                         
*****    BE    EDIT2A                                                           
*****    CLC   AGYALPHA,=C'SJ'         IF AGENCY NOT SJR SKIP                   
*****    BNE   EDITBILG                TO BILLING GROUP                         
EDIT2A   LA    R2,CLTFINH              FINANCIAL FIELD FROM SCREEN              
         MVC   WORK(1),PCLTFIN            SAVE OLD PCLTFIN                      
         CLI   5(R2),0                                                          
         BE    EDIT2B5                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   EDIT2B                                                           
         MVI   PCLTFIN,C'Y'                                                     
         B     EDIT2C                                                           
*                                                                               
EDIT2B   CLI   8(R2),C'N'                                                       
         BNE   EDIT2D                                                           
EDIT2B5  XC    PCLTFIN,PCLTFIN                                                  
EDIT2C   CLI   BACT,1                                                           
         BE    EDITBILG                                                         
         CLC   WORK(1),PCLTFIN   **FINANCIAL IND CANNOT BE CHANGED**            
         BE    EDITBILG                                                         
         B     ERROR                                                            
*                                                                               
EDIT2D   CLI   5(R2),0                                                          
         BNE   ERROR                                                            
         XC    PCLTFIN,PCLTFIN                                                  
*                                                                               
EDITBILG LA    R2,CLTBILH                                                       
         XC    PCLTBLGP,PCLTBLGP    CLEAR BILLING GROUP                         
         CLI   5(R2),0          ANY INPUT                                       
         BE    EDITPST                                                          
*                                                                               
MVCPCL   MVC   PCLTBLGP,CLTBIL   MOVE BILLING GROUP CODE                        
*                                                                               
EDITPST  LA    R2,CLTPSTH                                                       
         CLI   WNATION,C'C'        CANADIAN                                     
         BNE   EDTOP                                                            
         CLI   5(R2),0                                                          
         BE    EDTOP                                                            
         BAS   RE,VALPST                                                        
         BNE   FLDERR                                                           
*                                                                               
EDTOP    DS    0H               EDIT OPTIONS                                    
         LA    R2,CLTOPTSH                                                      
*                                                                               
* GET PROFILE (IN CASE F0PROF IS NULL)                                          
*                                                                               
         MVC   WORK(12),SPACES                                                  
         MVC   WORK(4),=C'P0F0'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         GOTO1 =V(GETPROF),DMCB,WORK,F0PROF,VDATAMGR,RR=RELO                    
*                                                                               
*                                                                               
*                                                                               
*                               FIRST DELETE OLD ZEN CLIENT CODE ELEM           
EDTOP1   LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'32'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   EDTOP2                                                           
         GOTO1 VRECUP,DMCB,(1,PCLTREC),0(R6),0                                  
         B     EDTOP1              CHECK FOR MORE                               
*                                                                               
*                               THEN DELETE MEDIA NAME OVERRIDE ELEM            
EDTOP2   LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'41'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   EDTOP3                                                           
         GOTO1 VRECUP,DMCB,(1,PCLTREC),0(R6),0                                  
         B     EDTOP2              CHECK FOR MORE                               
*                                                                               
* THEN TURN OFF "SPECIAL FINANCIAL HANDLING" AND "FROZEN" OPTIONS               
*                                                                               
EDTOP3   NI    PCLTSTAT,X'FF'-X'03'                                             
*                                                                               
*                                                                               
         MVI   COS2FLAG,0          INIT COST2 FACTOR FLAG                       
*                                                                               
         TM    PCLTSTAT,X'04'      COS2=Y                                       
         BNO   *+12                                                             
         OI    COS2FLAG,X'80'      TURN ON COST2 FACTOR FLAG                    
         B     EDTOP10A                                                         
         TM    PCLTSTAT,X'08'      COS2=9.999999                                
         BNO   *+12                                                             
         OI    COS2FLAG,X'80'      TURN ON COST2 FACTOR FLAG                    
         B     EDTOP10A                                                         
*                                                                               
         LA    R6,PCLTELEM         SEARCH FOR COST2 FACTOR ELEM                 
         MVI   ELCODE,X'45'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   EDTOP10A                                                         
         OI    COS2FLAG,X'80'      COST2 FACTOR ELEM EXIST                      
*                                                                               
*                                                                               
*                                                                               
EDTOP10A DS    0H                                                               
*                                                                               
         CLI   5(R2),0         ANY INPUT                                        
         BE    EDTOP40                                                          
         LA    R3,FLDINV                                                        
         GOTO1 =V(SCANNER),DMCB,(R2),(12,ZZZIO),RR=RELO                         
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         LA    R5,ZZZIO                                                         
         USING SCAND,R5                                                         
         ZIC   R8,DMCB+4     NUMBER OF ENTRIES                                  
*                                                                               
EDTOP10  ZIC   R1,FLD1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,TSTZEN                                                        
         BNE   EDTOP12                                                          
         CLI   FLD2LEN,2                                                        
         BL    ERROR                                                            
         CLI   FLD2LEN,3                                                        
         BH    ERROR                                                            
         ZIC   R1,FLD2LEN                                                       
         LA    RE,FLD2                                                          
EDTOP11A CLI   0(RE),C'A'      CHECK FOR ALPHA NUMERIC                          
         BL    ERROR                                                            
         CLI   0(RE),C'9'                                                       
         BH    ERROR                                                            
         CLI   0(RE),X'CC'     CHECK SOME SPECIAL CHARACTERS                    
         BE    ERROR                                                            
         CLI   0(RE),X'CE'     CHECK SOME SPECIAL CHARACTERS                    
         BE    ERROR                                                            
         CLI   0(RE),X'D0'     CHECK SOME SPECIAL CHARACTERS                    
         BE    ERROR                                                            
         CLI   0(RE),X'E0'     CHECK SOME SPECIAL CHARACTERS                    
         BE    ERROR                                                            
         LA    RE,1(RE)                                                         
         BCT   R1,EDTOP11A                                                      
*                                                                               
         LA    R6,PCLTELEM      CHECK FOR EXISTING ZEN ELEMENT                  
         MVI   ELCODE,X'32'                                                     
         BAS   RE,PNEXTEL                                                       
         BE    ERROR            INVALID - MORE THAN ONE ZEN ENTRY               
*                                                                               
         LA    R6,PCLTELEM      SET R6 TO END OF RECORD                         
         MVI   ELCODE,X'FE'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   *+6                                                              
         DC    H'0'             JUST IN CASE                                    
*                                                                               
         XC    ELEM(10),ELEM                                                    
         MVC   ELEM(2),=X'3205'                                                 
         LA    R4,ELEM                                                          
         USING PCLTZEL,R4                                                       
         ZIC   R1,FLD2LEN                                                       
         BCTR  R1,0                                                             
         EXMVC R1,PCLTZEN,FLD2                                                  
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PCLTREC),ELEM,0(R6)                               
         DROP  R4                                                               
         B     EDTOP30                                                          
*                                                                               
EDTOP12  DS    0H                TEST FOR MEDIA OPTION                          
         ZIC   R1,FLD1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,TSTMED                                                        
         BNE   EDTOP14                                                          
         CLI   FLD2LEN,2                                                        
         BL    ERROR                                                            
         CLI   FLD2LEN,10                                                       
         BH    ERROR                                                            
         ZIC   R1,FLD2LEN                                                       
         LA    RE,FLD2                                                          
*****EDTOP13A CLI   0(RE),C'A'      CHECK FOR ALPHA NUMERIC                     
EDTOP13A CLI   0(RE),C' '      CHECK FOR BLANK                                  
         BL    ERROR                                                            
         CLI   0(RE),C'9'                                                       
         BH    ERROR                                                            
*****    CLI   0(RE),X'CC'     CHECK SOME SPECIAL CHARACTERS                    
*****    BE    ERROR                                                            
*****    CLI   0(RE),X'CE'     CHECK SOME SPECIAL CHARACTERS                    
*****    BE    ERROR                                                            
*****    CLI   0(RE),X'D0'     CHECK SOME SPECIAL CHARACTERS                    
*****    BE    ERROR                                                            
*****    CLI   0(RE),X'E0'     CHECK SOME SPECIAL CHARACTERS                    
*****    BE    ERROR                                                            
         LA    RE,1(RE)                                                         
         BCT   R1,EDTOP13A                                                      
*                                                                               
         LA    R6,PCLTELEM      CHECK FOR EXISTING MED NAME ELEMENT             
         MVI   ELCODE,X'41'                                                     
         BAS   RE,PNEXTEL                                                       
         BE    ERROR            INVALID - MORE THAN ONE MEDIA ENTRY             
*                                                                               
         LA    R6,PCLTELEM      SET R6 TO END OF RECORD                         
         MVI   ELCODE,X'FE'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   *+6                                                              
         DC    H'0'             JUST IN CASE                                    
*                                                                               
         MVC   ELEM(12),SPACES     SET ELEM TO SPACES (NOT X'00'S)              
         MVC   ELEM(2),=X'410C'                                                 
         LA    R4,ELEM                                                          
         USING PCLTMEL,R4                                                       
         ZIC   R1,FLD2LEN                                                       
         BCTR  R1,0                                                             
         EXMVC R1,PCLTMNAM,FLD2                                                 
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PCLTREC),ELEM,0(R6)                               
         DROP  R4                                                               
         B     EDTOP30                                                          
*                                                                               
EDTOP14  DS    0H         TEST FOR "SPECIAL FINANCIAL HANDLING" OPTION          
         ZIC   R1,FLD1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,TSTSFH                                                        
         BNE   EDTOP15                                                          
**NO-OP  TM    SVAGYSW,X'01'       WESTERN OR SJR AGENCY ?                      
**NO-OP  BZ    ERROR               NO - "SFH" NOT ALLOWED                       
*                                                                               
         NI    PCLTSTAT,X'FF'-X'01'     TURN OFF "SFH" OPTION                   
         CLI   FLD2LEN,1                                                        
         BNE   EDTOP14C            MORE THAN 1 CHARACTER INPUT                  
         CLI   FLD2,C'Y'           SHOULD IT BE ON ?                            
         BNE   EDTOP14B            NO                                           
         OI    PCLTSTAT,X'01'      YES                                          
         B     EDTOP30                                                          
EDTOP14B CLI   FLD2,C'N'                                                        
         BNE   ERROR               MUST BE 'Y'' OR 'N'                          
         B     EDTOP30                                                          
*                                                                               
EDTOP14C CLI   FLD2LEN,2         ALSO ACCEPT NO                                 
         BNE   EDTOP14D                                                         
         CLC   FLD2(2),=C'NO'                                                   
         BNE   ERROR                                                            
         B     EDTOP30                                                          
*                                                                               
EDTOP14D CLI   FLD2LEN,3           ALSO ACCEPT YES                              
         BNE   ERROR                                                            
         CLC   FLD2(3),=C'YES'                                                  
         BNE   ERROR                                                            
         OI    PCLTSTAT,X'01'      YES                                          
         B     EDTOP30                                                          
*                                                                               
*                                                                               
*                                                                               
EDTOP15  DS    0H                  COST2 FACTOR ELEMENT                         
*                                                                               
         ZIC   R1,FLD1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,TSTCOS                                                        
         BNE   EDTOP25                                                          
*                                                                               
         CLI   PCLTFIN,C'Y'        FINANCIAL CLIENT?                            
         BE    ERROR               COS2 IS NOT ALLOWED FOR FIN CLIENTS          
*                                                                               
         CLI   F0PROF+4,C'$'       SEE IF PROF ALLOW COS2=Y                     
         BE    EDTOP15H                                                         
         CLI   F0PROF+4,C'F'       SEE IF PROF ALLOW COS2=9.9999999             
         BE    EDTOP15P                                                         
         B     ERROR               PROFILE DOES NOT ALLOW COS2 OPT              
*                                                                               
EDTOP15H DS    0X                                                               
         CLI   FLD2LEN,0                                                        
         BNH   ERROR               NO INPUT                                     
*                                                                               
         CLI   FLD2,C'Y'           COS2=Y?                                      
         BNE   ERROR                                                            
*                                                                               
         CLI   BACT,X'02'          SEE IF ACTION IS CHANGE                      
         BNE   *+12                                                             
         TM    COS2FLAG,X'80'      ADDING COS2 FACTOR ELEM IS NOT               
         BZ    ERROR               ALLOWED ON ACTION CHANGE                     
*                                                                               
         OI    PCLTSTAT,X'04'                COS2=Y        BIT ON               
         NI    PCLTSTAT,X'FF'-X'08'          COS2=9.999999 BIT OFF              
*                                                                               
EDTOP15J LA    R6,PCLTELEM         DELETE COST2 FACTOR ELEM                     
         MVI   ELCODE,X'45'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   EDTOP15K                                                         
         GOTO1 VRECUP,DMCB,(1,PCLTREC),0(R6),0                                  
         B     EDTOP15J                                                         
*                                                                               
EDTOP15K B     EDTOP30             DONE WITH COS2=Y                             
*                                                                               
*                                                                               
*                                                                               
EDTOP15P DS    0X                                                               
         CLI   BACT,X'02'          SEE IF ACTION IS CHANGE                      
         BNE   *+12                                                             
         TM    COS2FLAG,X'80'      ADDING COS2 FACTOR ELEM IS NOT               
         BZ    ERROR               ALLOWED ON ACTION CHANGE                     
*                                                                               
         CLI   FLD2LEN,0                                                        
         BNH   ERROR               NO INPUT                                     
*                                                                               
         ZIC   R4,FLD2LEN                                                       
         GOTO1 VCASHVAL,DMCB,(6,FLD2),(R4)                                      
         CLI   DMCB,0                                                           
         BNE   ERROR                                                            
         L     R6,4(R1)                                                         
*                                                                               
         C     R6,=F'9999999'                                                   
         BH    ERROR               MAX INPUT IS 9.999999                        
         CHI   R6,0                                                             
         BNH   ERROR               LESS THAN ZERO OR ZERO IS NO GOOD            
*                                                                               
         CVD   R6,DUB                                                           
         OI    PCLTSTAT,X'08'                COS2=9.999999 BIT ON               
         NI    PCLTSTAT,X'FF'-X'04'          COSE2=Y       BIT OFF              
*                                                                               
EDTOP15T LA    R6,PCLTELEM         DELETE COST2 FACTOR ELEM                     
         MVI   ELCODE,X'45'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   EDTOP15U                                                         
         GOTO1 VRECUP,DMCB,(1,PCLTREC),0(R6),0                                  
         B     EDTOP15T                                                         
*                                                                               
EDTOP15U CLC   PCLTLEN,=H'360'     CK FOR MAX REC LENGTH                        
         BNH   *+12                                                             
         LA    R3,MAXSERR                                                       
         B     ERROR                                                            
*                                                                               
         LA    R6,PCLTELEM         SET R6 TO END OF RECORD                      
         MVI   ELCODE,X'FE'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   *+6                                                              
         DC    H'0'                JUST IN CASE                                 
*                                                                               
         XC    ELEM(7),ELEM                                                     
         MVC   ELEM(2),=X'4507'    ELEM CODE AND LENGTH                         
         LA    R4,ELEM                                                          
         USING PCLTCFEL,R4                                                      
         MVC   PCLTCF,DUB+3        COST2 FACTOR (PACKED)                        
         GOTO1 VRECUP,DMCB,(1,PCLTREC),ELEM,0(R6)                               
*                                                                               
         DROP  R4                                                               
         B     EDTOP30                                                          
*                                                                               
*                                                                               
*                                                                               
EDTOP25  DS    0H                  TEST FOR "FRZ" (FROZEN)                      
         CLC   FLD1(3),=C'FRZ'                                                  
         BNE   ERROR               UNKNOWN OPTION                               
         CLI   FLD1LEN,3                                                        
         BNE   ERROR               MUST BE FRZ ONLY                             
*                                                                               
         OI    PCLTSTAT,X'02'      INDICATES FROZEN                             
         B     EDTOP30                                                          
*                                                                               
EDTOP28  DS    0H                UNKNOWN OPTION                                 
*                                                                               
EDTOP30  LA    R5,32(R5)         NEXT SCANNER FIELD                             
         BCT   R8,EDTOP10                                                       
*                                                                               
EDTOP40  DS    0H                                                               
*                                                                               
EDIT2Z   CLI   BACT,1                 SEE IF ADDING                             
         BNE   CLTCHG                                                           
         BAS   RE,ADDREC                                                        
         MVC   CLTADDR,KEY            SAVE DISK ADDR                            
         GOTO1 =A(ADDOFC),RR=RELO                                               
         B     DONE                                                             
*                                                                               
CLTCHG   DS    0H                                                               
         TM    SVAGYSW,X'01'       WESTERN AGENCY ?                             
         BNO   CLTC3               NO - CONTINUE                                
         TM    12(RA),X'04'        CREDIT LIMIT ACCESS OK ?                     
         BO    CLTC3               YES - CONTINUE                               
         LA    R3,ACCSERR          "NOT AUTHORIZED FOR THIS FUNCTION"           
         LA    R2,CLTOPTSH         "OPTIONS" FIELD                              
         TM    SVCLSTAT,X'02'      WAS CLIENT "FROZEN" ?                        
         BO    CLTC1               YES                                          
         TM    PCLTSTAT,X'02'      CHANGED TO "FROZEN" ?                        
         BO    ERROR               YES - CHANGE NOT ALLOWED                     
         B     CLTC3               NOT CHANGED - OK                             
CLTC1    TM    PCLTSTAT,X'02'      CHANGED TO NOT "FROZEN" ?                    
         BNO   ERROR               YES - CHANGE NOT ALLOWED                     
CLTC3    CLC   PCLTLEN(2),=H'360'     MAX RECORD LENGTH FOR NEWFILE             
         BNH   CLTC5                                                            
         LA    R3,MAXSERR                                                       
         B     ERROR                                                            
*                                                                               
CLTC5    BAS   RE,PUTREC                                                        
*                                                                               
         GOTO1 =A(OFCPTR),RR=RELO                                               
*                                                                               
DONE     MVI   DONESW,1                                                         
         MVC   CPROFLE,PCLTPROF         SAVE PROFILE - FIRST 20 BYTES           
         B     EXXMOD                                                           
*                                                                               
TSTZEN   CLC   FLD1(0),=C'ZEN'     EXECUTED                                     
*                                                                               
TSTMED   CLC   FLD1(0),=C'MEDIA'   EXECUTED                                     
*                                                                               
TSTSFH   CLC   FLD1(0),=C'SFH'     EXECUTED                                     
*                                                                               
TSTCOS   CLC   FLD1(0),=C'COS2'    EXECUTED  (COST2 FACTOR)                     
*                                                                               
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
CHKCTCLT NTR1                                                                   
         SR    R3,R3              USE TO CHECK ERROR RETURN                     
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING ZENRECD,R4                                                       
*****    MVI   ZENKSYS,ZENKSYSQ    ****  SMYE  02/05/96  ****                   
         MVI   ZENKCODE,ZENKCODQ   ****  SMYE  02/05/96  ****                   
         MVI   ZENKTYP,ZENCLTQ                                                  
         MVC   ZENKAGY,SVCTAGY                                                  
         MVC   ZENKCLT,KCLT                                                     
         BAS   R8,CHKCT                                                         
*                                                                               
         LA    R4,ZZZIO            POINT TO RECORD                              
         LA    R2,CLTCLTNH                                                      
         MVI   5(R2),20            FORCE LENGTH                                 
         OI    6(R2),X'80'         FORCE XMT                                    
         LA    R4,ZENFIRST                                                      
         USING ZENELEM,R4                                                       
         MVC   8(20,R2),ZENCNAME   MOVE ZENITH NAME                             
         B     CHKX                                                             
*                                                                               
CHKX     XIT1  REGS=(R3)                                                        
         DROP  R4                                                               
*                                                                               
CHKCT    DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,ZZZIO                   
         CLC   0(25,R4),ZZZIO                                                   
         BER   R8                                                               
         LA    R3,NOCTCODE                                                      
         B     CHKX                                                             
*                                                                               
NOCTCODE EQU   181                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE PST CODES                                                     
*                                                                               
VALPST   NTR1                                                                   
         LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'25'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   VP10                                                             
*                                        CHANGE - DELETE OLD ELEM               
         GOTO1 VRECUP,DMCB,(1,PCLTREC),0(R6)                                    
*                                                                               
VP10     LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R1,CLTPSTH                                                       
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VTWA        A(COMFACS)                                   
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'  GET PST ADDRESS                          
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         CLI   PSTERR,0                                                         
         BNE   VPNO                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'250C'                                                 
         MVC   ELEM+2(10),PSTOUT                                                
         GOTO1 VRECUP,DMCB,(1,PCLTREC),ELEM,0(R6)                               
         BAS   RE,DISPPST                                                       
*                                                                               
VPYES    SR    R1,R1                                                            
*                                                                               
VPNO     LTR   R1,R1                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        DISPLAY PST CODES                                                      
*                                                                               
DISPPST  NTR1                                                                   
         MVC   CLTPST,SPACES       OUTPUT                                       
         OI    CLTPSTH+6,X'80'                                                  
         LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'25'                                                     
         BAS   RE,PNEXTEL          ANYTHING TO DISPLAY                          
         BNE   DPX                                                              
*                                                                               
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,2(R6)                                                         
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VTWA        A(COMFACS)                                   
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'  GET PST ADDRESS                          
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         MVC   CLTPST,PSTOUT       OUTPUT                                       
*                                                                               
DPX      XIT1                                                                   
         EJECT                                                                  
*                                                                               
FORMATC  DS    0H                                                               
*                                                                               
PUTCFLD  FOUT  CLTCLTNH,PCLTNAME,20                                             
         FOUT  CLTBRNH,PCLTBNAM,20                                              
         FOUT  CLTAL1H,PCLTLIN1,30                                              
         FOUT  CLTAL2H,PCLTLIN2,30                                              
         FOUT  CLTATTNH,PCLTATTN,24                                             
         XC    CLTOFFX,CLTOFFX     CLEAR CLT OFFICE HEXOUT AREA                 
         FOUT  CLTOFFXH                                                         
         FOUT  CLTOFFH,PCLTOFF,1                                                
*                                  CONDITIONALLY SHOW HEX OF CLT OFFICE         
         CLI   PCLTOFF,X'40'       SHOW HEX VALUE ?                             
         BNH   PUTCFLD3            NO                                           
         CLI   PCLTOFF,X'D0'       SHOW HEX VALUE ?                             
         BE    PUTCFLD2            YES                                          
         CLI   PCLTOFF,X'E0'       SHOW HEX VALUE ?                             
         BE    PUTCFLD2            YES                                          
         CLI   PCLTOFF,C'A'        SHOW HEX VALUE ?                             
         BL    PUTCFLD2            YES                                          
         CLI   PCLTOFF,C'9'        SHOW HEX VALUE ?                             
         BNH   PUTCFLD3            NO                                           
*                                                                               
PUTCFLD2 GOTO1 =V(HEXOUT),DMCB,PCLTOFF,CLTOFFX,L'CLTOFF,RR=RELO                 
         FOUT  CLTOFFXH                                                         
*                                                                               
PUTCFLD3 FOUT  CLTPROFH,PCLTPROF,32                                             
         FOUT  CLTBILH,PCLTBLGP,1   MOVE BILLING GROUP CODE                     
         XC    CLTAOFF,CLTAOFF                                                  
         FOUT  CLTAOFFH,PCLTAOFC,2  MOVE ACC OFFICE                             
         OC    PCLTACCA,PCLTACCA    CHECK FOR AGENCY                            
         BZ    PUTCFLD5                                                         
         MVC   WORK(2),PCLTAOFC                                                 
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),PCLTACCA                                               
         FOUT  CLTAOFFH,WORK,5                                                  
*                                                                               
PUTCFLD5 DS    0H                                                               
         CLI   WNATION,C'C'        CANADAIAN                                    
         BNE   PUBCFLDA                                                         
         BAS   RE,DISPPST                                                       
         CLI   PCLTGST,0                                                        
         BNE   *+8                                                              
         MVI   PCLTGST,C'S'        DISPLAY DEFAULT                              
         FOUT  CLTGSTH,PCLTGST,1   GST CODE                                     
*                                                                               
PUBCFLDA XC    CLTNUM,CLTNUM                                                    
         FOUT  CLTNUMH                                                          
         BAS   RE,EXCEPTN      ANY AGENCY EXCEPTIONS                            
         B     PUBCFLDB        FOUND                                            
         FOUT  CLTNUMH,PCLTNUM,3                                                
         B     NOPEX                                                            
*                                                                               
PUBCFLDB CLI   PCLTNUM,X'FF'   SEE IF 2 PWOS BYTES FOLLOW                       
         BNE   PUBCFLDC        NORMAL DISPLAY                                   
         UNPK  WORK(5),PCLTNUM+1(3)                                             
         FOUT  CLTNUMH,WORK,4                                                   
         B     NOPEX                                                            
*                                                                               
PUBCFLDC DS    0H           CHECK FOR SPECIAL 3 BYTE BINARY FORMAT              
*                           WITH X'80' SET ON IN FIRST BYTE                     
*                                                                               
         MVC   BYTE,PCLTNUM                                                     
         NI    BYTE,X'F0'          SET OFF LOWER HALF BYTE BITS                 
         CLI   BYTE,X'80'          ONLY X'80' WILL BE LEFT ON                   
         BNE   NOPEX               IN THIS FORMAT                               
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),PCLTNUM                                                
         NI    FULL+1,X'7F'        SET OFF X'80' IN FULL                        
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         UNPK  WORK(5),DUB                                                      
         OI    WORK+4,X'F0'                                                     
         FOUT  CLTNUMH,WORK,5                                                   
*                                                                               
NOPEX    XC    WORK(6),WORK                                                     
         LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'10'       LOOK FOR CONTRACT STD COM ELEM                
         BAS   RE,PNEXTEL                                                       
         BNE   PUTCFL3                                                          
         MVC   WORK(6),2(R6)                                                    
*                                                                               
PUTCFL3  FOUT  CLTCSCH,WORK,6                                                   
*****                                                                           
         XC    WORK(6),WORK                                                     
         LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'11'       LOOK FOR I/O STD COM ELEM                     
         BAS   RE,PNEXTEL                                                       
         BNE   PUTCFL4                                                          
         MVC   WORK(6),2(R6)                                                    
*                                                                               
PUTCFL4  FOUT  CLTISCH,WORK,6                                                   
*                                                                               
         XC    WORK(6),WORK                                                     
         BAS   RE,PNEXTEL          LOOK FOR ANOTHER                             
         BNE   PUTCFL4C                                                         
         MVC   WORK(6),2(R6)                                                    
*                                                                               
PUTCFL4C FOUT  CLTISC2H,WORK,6                                                  
*****                                                                           
         XC    WORK(3),WORK                                                     
         LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'30'       LOOK FOR DRD CLT ELEM                         
         BAS   RE,PNEXTEL                                                       
         BNE   PUTCFL4D                                                         
         MVC   WORK(3),2(R6)                                                    
*                                                                               
PUTCFL4D FOUT  CLTDRDH,WORK,3                                                   
*****                                                                           
         LA    RE,AGYXTBL        AGENCIES TO UNPROTECT FINANCIAL FIELD          
PUTCFLUP CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    PUTCFL7             YES - SKIP TO FORMAT OPTIONS                 
         CLC   AGYALPHA,0(RE)      AGENCY ON TABLE?                             
         BE    PUTCFL5             YES - UNPROTECT FINANCIAL I/P FIELD          
         LA    RE,2(RE)            NEXT ENTRY                                   
         B     PUTCFLUP                                                         
*****    CLC   AGYALPHA,=C'DM'    IF DOREMUS                                    
*****    BE    PUTCFL5                                                          
*****    CLC   AGYALPHA,=C'MK'    OR MK                                         
*****    BE    PUTCFL5                                                          
*****    CLC   AGYALPHA,=C'XD'    OR XYZ                                        
*****    BE    PUTCFL5                                                          
*****    CLC   AGYALPHA,=C'WJ'    OR WITEST                                     
*****    BE    PUTCFL5                                                          
*****    CLC   AGYALPHA,=C'SJ'    OR SJR                                        
*****    BNE   PUTCFL7                                                          
PUTCFL5  NI    CLTFINH+1,X'DF'    UNPROTECT FINANCIAL INPUT FIELD               
         FOUT  CLTFINH,PCLTFIN,1                                                
*****                                                                           
PUTCFL7  DS    0H                  FORMAT OPTIONS                               
         LA    R1,CLTOPTS                                                       
         XC    CLTOPTS,CLTOPTS                                                  
         LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'32'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   PUTCF7B                                                          
*                                                                               
         USING PCLTZEL,R6                                                       
         OC    PCLTZEN,PCLTZEN                                                  
         BZ    PUTCF7B                                                          
         MVC   0(4,R1),=C'ZEN='                                                 
         LA    RE,1                                                             
         CLI   PCLTZEN+2,X'40'                                                  
         BNH   *+8                                                              
         LA    RE,2                                                             
         EXMVC RE,4(R1),PCLTZEN                                                 
         LA    R1,5(R1,RE)                                                      
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
PUTCF7B  DS    0H                  NEXT OPTION (MEDIA)                          
*                                                                               
         LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'41'                                                     
         BAS   RE,PNEXTEL                                                       
         BNE   PUTCF7C                                                          
*                                                                               
         USING PCLTMEL,R6                                                       
         MVC   0(6,R1),=C'MEDIA='                                               
         SR    RE,RE               FOR CHARACTER COUNT                          
         LA    R5,PCLTMNAM                                                      
PUTCF7BA CLI   0(R5),X'40'                                                      
         BNH   PUTCF7BB                                                         
         LA    RE,1(RE)            BUMP UP CHAR. COUNT                          
         LA    R5,1(R5)            NEXT PCLTMNAM POS'N                          
         CHI   RE,10               MAX LNTH OF PCLTMNAM IS 10                   
         BL    PUTCF7BA                                                         
PUTCF7BB EXMVC RE,6(R1),PCLTMNAM                                                
         LA    R1,6(R1,RE)                                                      
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
PUTCF7C  DS    0H         NEXT OPTION (SPECIAL FINANCIAL HANDLING)              
*                                                                               
         TM    PCLTSTAT,X'01'      SEE IF SFH CLIENT                            
         BO    PUTF7CA                                                          
         TM    SVAGYSW,X'01'       WESTERN OR SJR AGENCY ?                      
         BZ    PUTCF7D             NO - DON'T DISPLAY DEFAULT                   
*                                                                               
PUTF7CA  MVC   0(5,R1),=C'SFH=N'   DEFAULT OPTION                               
         TM    PCLTSTAT,X'01'      DEFAULT ?                                    
         BZ    PUTCF7CC            YES                                          
         MVI   4(R1),C'Y'          REPLACE DEFAULT N WITH Y                     
PUTCF7CC LA    R1,5(R1)            POINT TO NEXT OPTION POSITION                
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
PUTCF7D  DS    0H                  NEXT OPTION (FRZ - "FROZEN")                 
*                                                                               
         TM    PCLTSTAT,X'02'      FROZEN ?                                     
         BZ    PUTCF7E             NO                                           
         MVC   0(3,R1),=C'FRZ'                                                  
         LA    R1,3(R1)            POINT TO NEXT OPTION POSITION                
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
PUTCF7E  DS    0H                  NEXT OPTION (COST2 FACTOR)                   
*                                                                               
         LA    R6,PCLTELEM                                                      
         MVI   ELCODE,X'45'                                                     
         BAS   RE,PNEXTEL                                                       
         BE    PUTCF7EB                                                         
*                                                                               
         TM    PCLTSTAT,X'04'                                                   
         BNO   PUTCF7F                                                          
         MVC   0(5,R1),=C'COS2='                                                
         LA    R1,5(R1)                                                         
         MVI   0(R1),C'Y'                                                       
         LA    R1,1(R1)                                                         
         B     PUTCF7EH                                                         
*                                                                               
         USING PCLTCFEL,R6                                                      
*                                                                               
PUTCF7EB TM    PCLTSTAT,X'08'                                                   
         BO    *+6                                                              
         DC    H'0'                IMPOSSIBLE                                   
*                                                                               
         MVC   0(5,R1),=C'COS2='                                                
         LA    R1,5(R1)                                                         
*                                                                               
         CP    PCLTCF,=P'0'        COST2 FACTOR IS ZERO?                        
         BNE   *+6                                                              
         DC    H'0'                IMPOSSIBLE                                   
*                                                                               
         UNPK  WORK(10),PCLTCF                                                  
         OI    WORK+9,X'F0'                                                     
         LA    RE,WORK                                                          
         MVC   0(1,R1),3(RE)       SKIP FIRST 3 DIGITS                          
         LA    R1,1(R1)                                                         
         LA    RE,4(RE)                                                         
         MVI   0(R1),C'.'          DECIMAL POINT                                
         LA    R1,1(R1)                                                         
         LA    RF,6                SIX DIGITS AFTER DECIMAL PT                  
         MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,*-14                                                          
*                                                                               
         LA    RF,8                MAX LOOP IS 8 TIMES                          
PUTCF7EC BCTR  R1,0                                                             
         CLI   0(R1),C'.'          DECIMAL POINT?                               
         BE    PUTCF7ED                                                         
         CLI   0(R1),C'0'          TRAILING ZERO?                               
         BNE   PUTCF7EF                                                         
         MVI   0(R1),X'00'         CLEAR TRAILING ZERO                          
         BCT   RF,PUTCF7EC                                                      
*                                                                               
         DC    H'0'                SOMETHING IS WRONG...                        
*                                                                               
PUTCF7ED AHI   R1,1                                                             
         MVI   0(R1),C'0'          NON-SIGNIFICANT ZERO                         
         LA    R1,1(R1)                                                         
         B     PUTCF7EH                                                         
*                                                                               
PUTCF7EF AHI   R1,1                                                             
*                                                                               
PUTCF7EH MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
PUTCF7F  DS    0H                  NEXT OPTION                                  
*                                                                               
PUTCFL7X BCTR  R1,0                                                             
         CLI   0(R1),C','          REMOVE LAST COMMA                            
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         FOUT  CLTOPTSH                                                         
*                                                                               
PUTCFL10 CLI   BACT,X'02'          SEE IF ACTION = CHANGE                       
         BNE   DONE                                                             
NOTDONE  DS    0H                                                               
         LA    R2,CLTCLTNH                                                      
         B     EXIT                                                             
*                                                                               
PNEXTEL  CLI   0(R6),0                                                          
         BE    PNEXTELX                                                         
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     PNEXTEL                                                          
*                                                                               
PNEXTELX LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
EXCEPTN  LA    RF,AGTAB                                                         
EXCEPTNA CLI   0(RF),255                                                        
         BE    4(RE)           NO EXCEPTIONS                                    
         CLC   AGYALPHA,0(RF)  CHECK FOR MATCH                                  
         BER   RE              FOUND                                            
         LA    RF,3(RF)                                                         
         B      EXCEPTNA                                                        
*                                                                               
LOADIT   ZIC   RE,2(RF)       DISPLACEMENT                                      
         BCTR  RE,0                                                             
         SLL   RE,2           MULTIPLE OF 4                                     
         B     JUMPT(RE)                                                        
*                                                                               
**                                                                              
**       LIST OF AGENCIES THAT HAVE 4 OR 5 DIGIT INTERFACE CODES                
**       OR 2 ALPHA-NUMERIC                                                     
**       FOR NUMERIC CODES X'FF'  FOLLOWED BY 2 PWOS BYTES                      
**       FOR 2 ALPHA-NUMERIC CARRY 2 CHARACTERS                                 
**                                                                              
**       FOR 5 DIGITS 3 BYTE BINARY WITH HIGH BYTE OI X'80'                     
**                                                                              
AGTAB    DS    0F                                                               
         DC    C'JW',AL1((*+1-AGTAB)/3)                                         
         DC    C'LM',AL1((*+1-AGTAB)/3)                                         
         DC    C'LT',AL1((*+1-AGTAB)/3)                                         
         DC    C'DA',AL1((*+1-AGTAB)/3)                                         
         DC    C'KA',AL1((*+1-AGTAB)/3)                                         
         DC    X'FF'                    END OF TABLE                            
*                                                                               
JUMPT    B     JWTSCHME                                                         
         B     JWTSCHME           LM - SAME AS JW                               
         B     JWTSCHME           LT - SAME AS JW                               
         B     DIGIT5                                                           
         B     DIGIT5                                                           
*                                                                               
JWTSCHME MVI   PCLTNUM,255                                                      
         L     RF,DUB+4                                                         
         SRL   RF,4               SHIFT OUT SIGN                                
         STH   RF,DUB                                                           
         MVC   PCLTNUM+1(2),DUB    PACKED UNSIGNED INTERFACE CODE               
         B     CKPROF                                                           
*                                                                               
DIGIT5   DS    0H                  R0 HAS BINARY VALUE                          
         ST    R0,FULL                                                          
         MVC   PCLTNUM(3),FULL+1                                                
         OI    PCLTNUM,X'80'                                                    
         B     CKPROF                                                           
         EJECT                                                                  
*                                                                               
*    CHECK ACCOUNTING OFFICE  MAKE SURE IT EXISTS                               
*                                                                               
CHKAOFF  NTR1                                                                   
         MVI   COMPCD,0                                                         
         MVI   ASW,0         GETS SET TO X'01' IF I SWTICH TO ACC               
*                                                                               
         L     RF,VTWA             GET SYS NUM TO SWITCH BACK TO                
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,GTFACTB                                                
         LA    R1,GTFACTB                                                       
         USING FACTSD,R1                                                        
         MVC   SYSSW,FASYS                                                      
         DROP  R1                                                               
*                                                                               
         XC    POWCODE,POWCODE                                                  
         XC    ACCOFF,ACCOFF                                                    
         LA    R2,CLTAOFF          WHAT DID THEY ENTER                          
         LA    R1,0                                                             
         CLI   CLTAOFFH+5,0                                                     
         BE    CHKLEN                                                           
         LA    R1,1                                                             
         LA    R2,1(R2)                                                         
         CLI   CLTAOFFH+5,1                                                     
         BE    CHKLEN                                                           
         LA    R1,2                                                             
         LA    R2,2(R2)                                                         
         CLI   CLTAOFFH+5,2                                                     
         BE    CHKLEN                                                           
         LA    R2,CLTAOFF          WHAT DID THEY ENTER                          
         LA    R3,24                                                            
         LA    R1,0                                                             
COLOOP   CLI   0(R2),C','                                                       
         BE    CHKLEN                                                           
         CLI   0(R2),C'/'                                                       
         BE    CHKLEN                                                           
         CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,COLOOP                                                        
*                                                                               
CHKLEN   DS    0H                                                               
         LA    R2,1(R2)            POINT AT AGY CD                              
         STC   R1,OFFLEN                                                        
         MVC   ACCOFF(2),CLTAOFF                                                
         CLI   OFFLEN,2                                                         
         BH    LENERR                                                           
         BE    CHKONE                                                           
         CLI   SVACCOFC,C'Y'       2 CHAR REQUIRED                              
         BE    TWOERR                                                           
         MVI   ACCOFF+1,C' '                                                    
         CLI   OFFLEN,0                                                         
         BNE   ACCSW               SEE IF 2 CHARS MATCH                         
         XC    PCLTAOFC,PCLTAOFC                                                
         XC    PCLTACCA,PCLTACCA                                                
         MVC   PCLTAOFC,PCLTOFF    DEFAULT TO SPOT OFFICE IF <2 CHARS           
         MVI   PCLTAOFC+1,C' '                                                  
         B     COXX                                                             
*                                                                               
CHKONE   CLI   SVACCOFC,C'Y'       2 CHAR REQUIRED                              
         BNE   ONEERR                                                           
         B     ACCSW                                                            
*                                                                               
LENERR   MVI   ERRAREA,X'FF'                                                    
         LA    R2,CLTAOFFH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(32),=C'** ERROR - INVALID OFFICE LENGTH'                  
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
TWOERR   MVI   ERRAREA,X'FF'                                                    
         LA    R2,CLTAOFFH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(38),=C'** ERROR - 2 CHAR OFFICE CODE REQUIRED'            
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
ONEERR   MVI   ERRAREA,X'FF'                                                    
         LA    R2,CLTAOFFH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(38),=C'** ERROR - 1 CHAR OFFICE CODE REQUIRED'            
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
AGYERR   MVI   ERRAREA,X'FF'                                                    
         LA    R2,CLTAOFFH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(31),=C'** ERROR - INVALID AGENCY CODE'                    
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
DIFACC   DS    0H                  VALIDATE AGAINST AGY HDR CODE LIST           
         LA    R3,SVACCAGY                                                      
         LA    R1,4                                                             
CKAGYLP  CLC   0(2,R3),POWCODE     MATCH?                                       
         BE    GETSE                                                            
         CLI   0(R3),C' '                                                       
         BNH   AGYERR                                                           
         LA    R3,2(R3)                                                         
         BCT   R1,CKAGYLP                                                       
         B     AGYERR                                                           
*                                                                               
GETSE    MVC   DATADISP,=H'28'     FIND SE NUMBER FOR SPECIFIED                 
         XC    CTKEY,CTKEY             ACC AGY CODE                             
         LA    R6,CTKEY                                                         
         USING CT5REC,R6                                                        
         MVI   CT5KTYP,CT5KTYPQ    RECORD TYPE '5'                              
         MVC   CT5KALPH,POWCODE                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',CTKEY,PRDTAB             
         LA    R6,PRDTAB                                                        
*                                                                               
         CLI   8(R1),0             ERRORS?                                      
         BE    CO3                                                              
*                                                                               
CO3ERR   DS    0H                  ERROR IF NOT FOUND                           
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,CLTAOFFH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(31),=C'** ERROR - INVALID ACC AGY CODE'                   
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
CO3      MVI   ELCODE,X'21'        GET SE NUM FOR ACC FILE                      
         BAS   RE,GETEL                                                         
         BE    CO3A                                                             
         B     CO3ERR              ERROR IF NOT FOUND                           
CO3NX    BAS   RE,NEXTEL                                                        
CO3A     DS    0H                                                               
         USING CTSYSD,R6                                                        
         CLI   CTSYSEL,X'21'       STILL X'21' EL                               
         BNE   CO3ERR              ERROR IF NOT FOUND                           
*                                                                               
         CLI   CTSYSNUM,X'06'      ACC??                                        
         BNE   CO3NX                                                            
         MVC   COMPCD(1),CTSYSAGB  AGY BINARY CD                                
         XC    DMCB(8),DMCB        YES                                          
         MVC   DMCB(1),CTSYSSE     SE NUM                                       
         L     RF,VTWA             SWITCH TO THAT ACC SYSTEM                    
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB                                                        
         B     ACCSW2                                                           
*                                                                               
ACCSW    DS    0H                                                               
         CLI   0(R2),C' '                                                       
         BNH   ACCSW1                                                           
         MVC   POWCODE,0(R2)                                                    
         B     DIFACC              SWITCH TO DIFFERENT ACC SYSTEM               
*                                                                               
ACCSW1   DS    0H                                                               
         CLI   SVACCOFC,C'Y'       2 CHAR REQUIRED                              
         BNE   CO5                 IF NOT THEN SKIP SWITCH                      
*                                                                               
         L     RF,VTWA             SWITCH TO ACC SYSTEM                         
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'ACC',0                                              
*                                                                               
ACCSW2   CLI   4(R1),2             SYSTEM NOT OPEN                              
         BNE   CO1                                                              
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,CLTAOFFH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(33),=C'** ERROR - ACC SYSTEM IS NOT OPEN'                 
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
CO1      CLI   4(R1),1             ANY OTHER ERRORS?                            
         BNE   CO1A                SHOULDN'T BE A BNH                           
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,CLTAOFFH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(38),=C'** ERROR - CANNOT SWITCH TO ACC SYSTEM'            
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     NO                                                               
*                                                                               
CO1A     DS    0H                                                               
         MVI   ASW,X'01'        SET ASW TO 1 WHEN SWITCHING TO ACC              
*                               SUCCESSFULLY                                    
         CLI   COMPCD,0           SEE IF I ALREADY HAVE                         
         BNE   CO1AA              (FROM CTFILE)                                 
*                                                                               
         CLI   0(R1),0             NO RETURNED CODE                             
         BE    CO1ERR                                                           
         MVC   COMPCD,0(R1)          SAVE RETURNED AGENCY BINARY CODE           
*                                    READ COMPANY REC                           
CO1AA    MVC   MYACCKEY,SPACES                                                  
         MVC   MYACCKEY(1),COMPCD    RETURNED AGENCY BINARY CODE                
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYACCKEY,PRDTAB             
         CLI   8(R1),0                                                          
         BE    CO1AB                                                            
CO1ERR   MVI   ERRAREA,X'FF'                                                    
         LA    R2,CLTAOFFH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(36),=C'** ERROR - ACC COMPANY REC NOT FOUND'              
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         BAS   RE,RETURN            RETURN TO MY SYSTEM                         
         B     NO                                                               
*                                                                               
CO1AB    LA    R6,PRDTAB                                                        
*                                                                               
         CLI   SVACCOFC,C'Y'       2 CHAR REQUIRED?                             
         BNE   CO5                                                              
         LA    R6,MYACCKEY         NEW OFFICE -- LOOK FOR OFFICE REC            
         USING OFFRECD,R6                                                       
         MVC   MYACCKEY,SPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ      X'01'                                      
         MVC   OFFKCPY,COMPCD                                                   
         MVC   OFFKOFF(2),ACCOFF                                                
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYACCKEY,PRDTAB             
         CLI   8(R1),0                                                          
         BE    CO5                                                              
*                                                                               
         DS    0H                  ERROR IF NOT FOUND                           
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,CLTAOFFH                                                      
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(31),=C'** ERROR - INVALID ACC OFF CODE'                   
         FOUT  HDRMSGH                                                          
         OI    6(R2),X'40'                                                      
         BAS   RE,RETURN           RETURN TO MY SYSTEM                          
         B     NO                                                               
*                                                                               
CO5      DS    0H                  OFFICE CODE IS GOOD                          
         MVC   PCLTAOFC,ACCOFF     SAVE OFFICE CODE                             
         MVC   PCLTACCA,POWCODE    SAVE AGY CODE                                
         CLI   ASW,0               SEE IF I SWITCHED TO AN ACC SYSTEM           
         BE    COXX                                                             
         BAS   RE,RETURN           MUST RETURN TO MY SYSTEM                     
*                                                                               
COXX     XIT1                                                                   
*                                                                               
YES      SR    RE,RE                                                            
NO       LTR   RE,RE                                                            
MYEXIT   XIT1  REGS=(R2)                                                        
*                                                                               
         EJECT                                                                  
*==============================================================*                
* ADD/DELETE OFFICE PASSIVE POINTERS                           *                
*==============================================================*                
         SPACE 1                                                                
         DS    0D                                                               
OFCPTR   NTR1                                                                   
         B     OFCPTR0                                                          
         DC    CL8'*OFCPTR*'                                                    
*                                                                               
OFCPTR0  CLC   SVCOFFC,PCLTOFF     TEST OFFICE CHANGED                          
         BE    OFCPTRX             NO                                           
* DELETE OLD OFFICE (IF ANY)                                                    
         CLI   SVCOFFC,C' '                                                     
         BNH   OFCPTR10                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(3),PCLTKEY                                                   
         MVI   KEY+3,POFCKIDQ                                                   
         MVC   KEY+4(1),SVCOFFC                                                 
         MVC   KEY+5(3),PCLTKCLT                                                
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   OFCPTR2                                                          
         OI    KEY+25,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
OFCPTR2  DS    0H                                                               
* NOW ADD NEW (OR UNDELETE)                                                     
OFCPTR10 CLI   PCLTOFF,C' '        TEST NEW OFFICE PRESENT                      
         BNH   OFCPTRX                                                          
         BAS   RE,ADDOFC                                                        
*                                                                               
*                                                                               
OFCPTRX  NI    DMINBTS,X'F7'                                                    
         XIT1                                                                   
*                                                                               
ADDOFC   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(3),PCLTKEY                                                   
         MVI   KEY+3,POFCKIDQ                                                   
         MVC   KEY+4(1),PCLTOFF                                                 
         MVC   KEY+5(3),PCLTKCLT                                                
         MVC   KEY+27(4),CLTADDR   SET DISK ADDRESS                             
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   ADDOFC10                                                         
         NI    KEY+25,X'7F'                                                     
         GOTO1 WRITE                                                            
         B     ADDOFCX                                                          
*                                                                               
ADDOFC10 MVC   KEY,KEYSAVE         RESTORE KEY                                  
         GOTO1 ADD                                                              
*                                                                               
ADDOFCX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         DS    F                                                                
RETURN   ST    RE,RETURN-4                                                      
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SYSSW       ORIGINAL SYS                                 
         L     RF,VTWA             SWITCH BACK                                  
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0             ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATADISP,=H'33'     FOR PRINT                                    
         L     RE,RETURN-4                                                      
         BR    RE                                                               
*                                                                               
***********************************************************************         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
SVACCOFC DS    CL1       SET TO 'Y' IF SVAGPF12 IS A OR B                       
COMPCD   DS    CL1                                                              
ASW      DS    XL1       SET OT X'01' IF I SWITCHED TO ACC                      
MYACCKEY DS    CL42                                                             
CTKEY    DS    CL28                                                             
SENUM    DS    XL1                                                              
SYSSW    DS    XL1                                                              
DATADISP DS    H                                                                
POWCODE  DS    CL2                                                              
ACCOFF   DS    CL2                                                              
OFFLEN   DS    XL1                                                              
GTFACTB  DS    CL88                                                             
*                                                                               
COS2FLAG DS    XL1                 FLAG FOR COST2 FACTOR ELEM                   
*                                                                               
ZEROS    DC    40C'0'                                                           
SPACES   DC    CL50' '                                                          
ELCODE   DS    CL1                                                              
SVCOFFC  DS    CL1                 SAVE OFFICE CODE                             
ELEM     DS    CL30                                                             
DMWORK1  DS    12D                                                              
SAVEKEY  DS    CL32                                                             
PSTOUT   DS    CL64                                                             
FLDINV   EQU   2                                                                
PROFERR  EQU   60                                                               
MISSMST  EQU   62                                                               
MCLTNF   EQU   63                                                               
ACCSERR  EQU   96                  NOT AUTH FOR THIS FUNCTION                   
PROFERR1 EQU   176                                                              
CACCERR  EQU   207                 LIMIT ACCESS ERROR                           
MAXSERR  EQU   208                 MAXIMUM RECORD SIZE EXCEEDED                 
CMNTERR  EQU   53                                                               
VIRERR   DC    H'0'                                                             
RELO     DS    F                                                                
AGYXTBL  DC    C'DMMKMXSJWIWJWTXD'      TABLE OF AGENCIES FOR WHICH             
         DC    X'FF'                    FINANCIAL FIELD NOT CLEARED             
         EJECT                                                                  
       ++INCLUDE PLFMWRK                                                        
*                                                                               
         ORG   HDRLAST                                                          
       ++INCLUDE PPLFMF1D                                                       
         ORG   HDRLAST+2000                                                     
*                                                                               
*                                                                               
WNATION  DS    CL1                                                              
*                                                                               
SVP1USER DS    CL20                PRD USER DESCRIPTION FIELD 1                 
SVP1TYPE DS    CL1                          TYPE                                
SVP1LEN  DS    XL1                          LENGTH                              
SVP1FLG1 DS    XL1                          FLAG                                
SVP1FLG2 DS    XL1                          FLAG                                
SVP2USER DS    CL20                PRD USER DESCRIPTION FIELD 2                 
SVP2TYPE DS    CL1                          TYPE                                
SVP2LEN  DS    XL1                          LENGTH                              
SVP2FLG1 DS    XL1                          FLAG                                
SVP2FLG2 DS    XL1                          FLAG                                
SVE1USER DS    CL20                EST USER DESCRIPTION FIELD 1                 
SVE1TYPE DS    CL1                          TYPE                                
SVE1LEN  DS    XL1                          LENGTH                              
SVE1FLG1 DS    XL1                          FLAG                                
SVE1FLG2 DS    XL1                          FLAG                                
SVE2USER DS    CL20                EST USER DESCRIPTION FIELD 2                 
SVE2TYPE DS    CL1                          TYPE                                
SVE2LEN  DS    XL1                          LENGTH                              
SVE2FLG1 DS    XL1                          FLAG                                
SVE2FLG2 DS    XL1                          FLAG                                
SVULNQ   EQU   *-SVP1USER                                                       
*                                                                               
F0PROF   DS    CL16               F0 PROFILE READ IN 00                         
*                                 WHEN CLIENT IS VALIDATED                      
SVACCAGY DS    CL24              ROOM FOR 12 ACC AGENCYS                        
SVCTAGY  DS    CL2               CTFILE ID                                      
*                                                                               
SVXFRSY  DS    CL3              TRANSFERRED FROM SYSTEM                         
SVXFRPR  DS    CL3              TRANSFERRED FROM PROGRAM                        
*                                                                               
SVCLSTAT DS    XL1              PCLTSTAT FROM PCLTREC SET IN 00                 
*                                 WHEN CLIENT IS VALIDATED                      
SVAGYSW  DS    XL1           AGENCY "SWITCH" SET IN 00 AT CKVALC..              
*                               X'01' = WESTERN AGENCY (OR SJR)                 
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
SCAND    DSECT                                                                  
*         DSECT TO COVER SCANNER LINES                                          
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
*                                                                               
*                                                                               
POFCKEYD DSECT                                                                  
       ++INCLUDE POFFCLTPP                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PPLFM01S  05/01/02'                                      
         END                                                                    
