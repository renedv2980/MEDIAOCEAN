*          DATA SET PPAFM01    AT LEVEL 051 AS OF 06/17/08                      
*PHASE T40701B                                                                  
         TITLE 'T40701  CHANGE LOG'                                             
*                                                                               
***********************************************************************         
*                                                                               
* SMYE 01/08    NEW FIELD - FOREIGN EXCHANGE REP                                
*                                                                               
* SMYE 01/04/01 NEW FIELDS FOR DDS BILLING DOWNLOAD                             
*                                                                               
* KWAN 04/00    CORRECT ADD RECORD LOGIC                                        
*                                                                               
* KWAN 03/00    ADD NEW FIELD RFP ID TO SCREEN AND STORE IN PAGYREC             
*                                                                               
* SMYE 02/11/98 DO NOT CLEAR SCREEN DATA ON ADD                                 
*                                                                               
* BPLA 6/96     FIX BUG                                                         
*                                                                               
* BPLA 5/24/95  NEW FIELD (CTFILE ID)                                           
*                                                                               
* BPLA 12/6/94  CHANGE FOR ACC AGENCY LIST                                      
*                                                                               
* ROSA 6/29/88  ALLOW ONLY 'U' (USA) OR 'C' (CANADA) AS VALID                   
*                                                                               
* ROSA 5/31/88  ADD NEW FIELD -- AGENCY NATIONALITY  BIN 0=USA                  
*               ACCEPT ANYTHING ELSE                                            
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'T40701  PRINTPAK  AGYREC  SPECIAL FILE MAINTENANCE'             
T40701   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40701                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T407FFD,RA                                                       
         USING PAGYREC,R9                                                       
*                                                                               
         LA    R8,T40701+4095                                                   
         AHI   R8,1                                                             
         USING T40701+4096,R8          ** NOTE USE OF SECOND BASE REG *         
*                                                                               
         EJECT                                                                  
*                                                                               
         MVC   PAGYKEY(25),KEY                                                  
         MVC   PAGYPROF(30),=30C'0'                                             
         MVC   PAGYELEM(2),=X'01AA'                                             
         MVC   PAGYLEN(2),=X'00CB'                                              
         CLI   BACT,1                                                           
         BE    AGYSCRN                                                          
         MVC   KEY+27(4),AGYADDR        READ AGYREC                             
         BAS   RE,GETREC                                                        
AGYSCRN  CLI   BYTE2,0                                                          
         BNE   FORMATA                                                          
*                                                                               
* AGENCY SCREEN IN TWA SO EDIT IT UNLESS ACTION= DISPLAY                        
*                                                                               
         CLI   BACT,3                                                           
         BE    FORMATA                                                          
EDTFLDS  LA    R2,AGYNAMEH                                                      
         BAS   RE,ANY                                                           
         XC    PAGYNAME,PAGYNAME                                                
         MVC   PAGYNAME,AGYNAME                                                 
         LA    R2,AGYADRSH                                                      
         BAS   RE,ANY                                                           
         XC    PAGYADDR,PAGYADDR                                                
         MVC   PAGYADDR,AGYADRS                                                 
         LA    R2,AGYPROFH                                                      
         BAS   RE,ANY                                                           
         LA    R3,APROFERR                                                      
         CLI   5(R2),30                                                         
         BNE   ERROR                                                            
         XC    PAGYPROF,PAGYPROF                                                
         MVC   PAGYPROF,AGYPROF                                                 
         LA    R2,AGYABBRH                                                      
         BAS   RE,ANY                                                           
         XC    PAGYABBR,PAGYABBR                                                
         MVC   PAGYABBR,AGYABBR                                                 
         OC    PAGYABBR,SPACES                                                  
         LA    R2,AGYEQUH                                                       
         MVC   PAGYEQU,=C'  '                                                   
         CLI   5(R2),0                                                          
         BE    EDITA1                                                           
         LA    R3,AGYERR                                                        
         CLI   5(R2),2                                                          
         BNE   ERROR                                                            
         XC    KEY,KEY             SEE IF AGY ON FILE                           
         MVC   KEY(25),PAGYKEY                                                  
         MVC   KAGY,AGYEQU                                                      
         BAS   RE,READ                                                          
         MVC   PAGYEQU,AGYEQU                                                   
EDITA1   LA    R2,AGYMDESH                                                      
         BAS   RE,ANY                                                           
         XC    PAGYMED,PAGYMED                                                  
         MVC   PAGYMED,AGYMDES                                                  
         LA    R2,AGYVDESH                                                      
         XC    PAGYVEN,PAGYVEN                                                  
         CLI   5(R2),0                                                          
         BE    EDITA2                                                           
         MVC   PAGYVEN,AGYVDES                                                  
EDITA2   LA    R2,AGYACCTH                                                      
         BAS   RE,ANY                                                           
         MVC   PAGYACCT,AGYACCT                                                 
         OC    PAGYACCT,SPACES                                                  
*                                                                               
         LA    R2,AGYNATIH         NATIONALITY HEADER                           
         BAS   RE,ANY                                                           
         MVI   PAGYNAT,0           INITIALIZE TO USA                            
         CLI   AGYNATI,C'U'        IF USA DEFAULT TO BIN ZERO                   
         BE    EDTACC                                                           
         CLI   AGYNATI,C'C'                                                     
         BNE   ERROR                                                            
         MVC   PAGYNAT,AGYNATI     MOVE TO RECORD                               
*                                                                               
EDTACC   DS    0H                                                               
         LA    R2,AGYACCH                                                       
         XC    ELEM(30),ELEM                                                    
         MVC   ELEM(2),=X'030A'                                                 
         LA    R6,ELEM                                                          
         USING PAGYACCEL,R6                                                     
         CLI   5(R2),0             NO INPUT                                     
         BE    EDTACC1                                                          
         LA    R3,AGYACC                                                        
         LA    R4,PAGYACCAG                                                     
EDTACCLP CLI   0(R3),C' '                                                       
         BNH   EDTACC2             GO UPDATE RECORD                             
         MVC   0(2,R4),0(R3)                                                    
         LA    R3,3(R3)                                                         
         LA    R4,2(R4)                                                         
         B     EDTACCLP                                                         
*                                                                               
EDTACC1  DS    0H                                                               
         MVI   ELEM,0              SET FOR NO ELEM TO ADD                       
*                                                                               
EDTACC2  DS    0H                                                               
         LA    R6,PAGYREC+33       TRY TO FIND OLD ELEM                         
         MVI   ELCODE,X'03'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   EDTACC4             NOT FOUND                                    
         CLI   ELEM,0              SEE IF I NEED TO DELETE IT                   
         BE    EDTACC3             YES                                          
         MVC   0(10,R6),ELEM       NO -  CAN SWITCH ELEMS                       
         B     EDTACCX                                                          
*                                                                               
EDTACC3  DS    0H                                                               
         GOTO1 VRECUP,DMCB,(1,PAGYREC),0(R6),0                                  
         B     EDTACCX                                                          
*                                                                               
EDTACC4  DS    0H                                                               
         CLI   ELEM,0              SEE IF I NEED TO ADD ELEM                    
         BE    EDTACCX             NO                                           
         LA    R6,PAGYREC+33       GET ADDRESS OF WHERE TO PUT 03               
         MVI   ELCODE,X'FE'        TO END OF RECORD                             
         BAS   RE,NEXTEL                                                        
*                                                                               
EDTACC5  GOTO1 VRECUP,DMCB,(1,PAGYREC),ELEM,0(R6)                               
*                                                                               
EDTACCX  DS    0H                                                               
*                                                                               
EDTCID   DS    0H                                                               
         LA    R2,AGYCTIH                                                       
         XC    ELEM(30),ELEM                                                    
         MVC   ELEM(2),=X'0504'                                                 
         LA    R6,ELEM                                                          
         USING PAGYCIDEL,R6                                                     
         CLI   5(R2),0             NO INPUT                                     
         BE    EDTCID1                                                          
         CLI   5(R2),2             OR 2 CHARACTERS                              
         BE    EDTCID0                                                          
         LA    R3,INVERR                                                        
         B     ERROR                                                            
*                                                                               
EDTCID0  MVC   PAGYCID,AGYCTI                                                   
         B     EDTCID2                                                          
*                                                                               
EDTCID1  DS    0H                                                               
         MVI   ELEM,0              SET FOR NO ELEM TO ADD                       
*                                                                               
EDTCID2  DS    0H                                                               
         LA    R6,PAGYREC+33       TRY TO FIND OLD ELEM                         
         MVI   ELCODE,X'05'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   EDTCID4             NOT FOUND                                    
         CLI   ELEM,0              SEE IF I NEED TO DELETE IT                   
         BE    EDTCID3             YES                                          
         MVC   0(4,R6),ELEM       NO -  CAN SWITCH ELEMS                        
         B     EDTCIDX                                                          
*                                                                               
EDTCID3  DS    0H                                                               
         GOTO1 VRECUP,DMCB,(1,PAGYREC),0(R6),0                                  
         B     EDTCIDX                                                          
*                                                                               
EDTCID4  DS    0H                                                               
         CLI   ELEM,0              SEE IF I NEED TO ADD ELEM                    
         BE    EDTCIDX             NO                                           
         LA    R6,PAGYREC+33       GET ADDRESS OF WHERE TO PUT 03               
         MVI   ELCODE,X'FE'        TO END OF RECORD                             
         BAS   RE,NEXTEL                                                        
*                                                                               
EDTCID5  GOTO1 VRECUP,DMCB,(1,PAGYREC),ELEM,0(R6)                               
*                                                                               
EDTCIDX  DS    0H                                                               
*                                                                               
EDTRID   DS    0H                  RFP ID VALIDATION                            
*                                                                               
         XC    PAGYPINI,PAGYPINI   PREPARE FIELD FOR EDITING                    
         XC    WORK(2),WORK                                                     
         LA    R3,2                INVALID ERROR MSG                            
         LA    R2,AGYRFPIH         RFP ID                                       
         CLI   5(R2),0                                                          
         BE    EDTRIDX             INPUT IS NOT REQUIRED                        
*                                                                               
         MVC   WORK+2(10),8(R2)                                                 
         BAS   RE,VALID            VALIDATE ID                                  
         BNE   ERROR                                                            
*                                                                               
         MVC   PAGYPINI,WORK       BINARY RFP ID                                
*                                                                               
EDTRIDX  DS    0H                  RFP ID VALIDATION ENDS                       
*                                                                               
EDTSKP   DS    0H                  SKIP AGENCY VALIDATION                       
         NI    PAGYSTAT,X'FF'-X'01'     TURN OFF "YES" SWITCH                   
         LA    R3,INVERR           INVALID ERROR MSG                            
         LA    R2,AGYSKIPH         SKIP AGENCY                                  
         CLI   5(R2),0                                                          
         BE    EDTSKPX             INPUT IS NOT REQUIRED                        
         CLI   8(R2),C'N'                                                       
         BE    EDTSKPX                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   ERROR               MUST BE 'N' OR 'Y' IF ENTERED                
         OI    PAGYSTAT,X'01'      TURN ON "YES" SWITCH                         
*                                                                               
EDTSKPX  DS    0H                  SKIP AGENCY VALIDATION ENDS                  
*                                                                               
EDTDTL   DS    0H                  PRD DETAIL VALIDATION                        
         NI    PAGYSTAT,X'FF'-X'02'     TURN OFF "YES" SWITCH                   
         LA    R3,INVERR           INVALID ERROR MSG                            
         LA    R2,AGYPDTLH         PRD DETAIL LEVEL                             
         CLI   5(R2),0                                                          
         BE    EDTDTLX             INPUT IS NOT REQUIRED                        
         CLI   8(R2),C'N'                                                       
         BE    EDTDTLX                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   ERROR               MUST BE 'N' OR 'Y' IF ENTERED                
         OI    PAGYSTAT,X'02'      TURN ON "YES" SWITCH                         
*                                                                               
EDTDTLX  DS    0H                  PRD DETAIL VALIDATION ENDS                   
*                                                                               
EDTREP   DS    0H                  EDIT FOREIGN EXCH REP ENTRY                  
         LA    R2,AGYFXRPH                                                      
         XC    ELEM(PAGFXLNQ),ELEM                                              
         LA    R6,ELEM                                                          
         USING PAGFXEL,R6                                                       
         MVI   PAGFXCOD,PAGFXIDQ   X'10'                                        
         MVI   PAGFXLEN,PAGFXLNQ                                                
         CLI   5(R2),0             INPUT ?                                      
         BNE   EDTREPD             YES                                          
         XC    AGYFXNM,AGYFXNM     NO - CLEAR REP NAME AREA                     
         FOUT  AGYFXNMH                                                         
         B     EDTREP1                                                          
*                                                                               
EDTREPD  DS    0H                                                               
         LA    R3,NOTNUM                                                        
         TM    4(R2),X'08'         NUMERIC ?                                    
         BNO   ERROR               NO - INVALID                                 
         LA    R3,TOOLONG                                                       
         CLI   5(R2),4             MAX 4 CHARACTERS ?                           
         BH    ERROR               NO - INVALID                                 
         LA    R3,INVERR                                                        
         CLI   AGYNATI,C'C'        CANADIAN AGENCY ?                            
         BNE   ERROR               NO - INVALID                                 
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  PAGFXREP,DUB+5(3)                                                
         CLC   PAGFXREP,=C'0000'                                                
         BE    ERROR                                                            
         LA    R3,NOTFND                                                        
*                                                                               
         XC    WORK,WORK           WORK USED AS KEY IN GETREP                   
         MVC   WORK(3),PAGYREC     AGY/MED                                      
         MVI   WORK+3,X'11'        REP RECORD CODE                              
         MVC   WORK+4(L'PAGFXREP),PAGFXREP                                      
         BAS   RE,GETREP                                                        
         BE    EDTREPH             RECORD FOUND                                 
         XC    AGYFXNM,AGYFXNM     CLEAR REP NAME AREA                          
         FOUT  AGYFXNMH                                                         
         B     ERROR               RECORD NOT FOUND                             
*                                                                               
EDTREPH  DS    0H                                                               
         CLI   BACT,1              ADD ?                                        
         BE    EDTREPM             YES - DO NOT REPEAT GETREC                   
*                                                                               
*  REPEAT GETREC FOR UPDATE (RECORD IN IOAREA STILL HAS UPDATED FIELDS)         
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'PRTFILE',KEY+27,TEMPIO,     X        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EDTREPM  DS    0H                                                               
         XC    AGYFXRP,AGYFXRP                                                  
         MVC   AGYFXRP(L'PAGFXREP),PAGFXREP                                     
         OI    6(R2),X'80'         XMIT                                         
         FOUT  AGYFXNMH,WORK,30    WORK HAS REP NAME FROM GETREP                
*                                                                               
         B     EDTREP2                                                          
         DROP  R6                                                               
*                                                                               
EDTREP1  DS    0H                                                               
         MVI   ELEM,0              SET FOR NO ELEM TO ADD                       
*                                                                               
EDTREP2  DS    0H                                                               
         LA    R6,PAGYREC+33       TRY TO FIND OLD ELEM                         
         MVI   ELCODE,PAGFXIDQ                                                  
         BAS   RE,NEXTEL                                                        
         BNE   EDTREP4             NOT FOUND                                    
         CLI   ELEM,0              SEE IF I NEED TO DELETE IT                   
         BE    EDTREP3             YES                                          
         MVC   0(PAGFXLNQ,R6),ELEM       NO -  CAN SWITCH ELEMS                 
         B     EDTREPX                                                          
*                                                                               
EDTREP3  DS    0H                                                               
         GOTO1 VRECUP,DMCB,(1,PAGYREC),0(R6),0                                  
         B     EDTREPX                                                          
*                                                                               
EDTREP4  DS    0H                                                               
         CLI   ELEM,0              SEE IF I NEED TO ADD ELEM                    
         BE    EDTREPX             NO                                           
         LA    R6,PAGYREC+33       GET ADDRESS OF WHERE TO PUT ELEM             
         MVI   ELCODE,X'FE'        TO END OF RECORD                             
         BAS   RE,NEXTEL                                                        
*                                                                               
EDTREP5  GOTO1 VRECUP,DMCB,(1,PAGYREC),ELEM,0(R6)                               
*                                                                               
EDTREPX  DS    0H                                                               
*                                                                               
*                                                                               
EDITX    CLI   BACT,1                                                           
         BNE   AGYCHG                                                           
         MVC   KEY(25),PAGYKEY                                                  
         BAS   RE,ADDREC                                                        
         B     DONE                                                             
AGYCHG   MVC   KEY+27(4),AGYADDR                                                
         BAS   RE,PUTREC                                                        
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
FORMATA  DS    0H                                                               
         CLI   SCRNUM,X'F1'        CLIENT SCREEN PRESENT ?                      
         BNE   FORMATC             NO                                           
         CLI   BACT,1              DOING ADD ?                                  
         BE    EDTFLDS             YES                                          
*                                                                               
FORMATC  LA    R6,HDRLAST                                                       
         GOTO1 VCALLOV,WORK,(R6),X'D90407F1'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRERR                                                           
         MVI   SCRNUM,X'F1'                                                     
         CLI   BACT,1                                                           
         BNE   PUTFLDS                                                          
         FOUT  AGYPROFH,ZEROS,30                                                
FORMATG  LA    R2,AGYNAMEH                                                      
         B     EXIT                                                             
*                                                                               
PUTFLDS  FOUT  AGYNAMEH,PAGYNAME,33                                             
         FOUT  AGYADRSH,PAGYADDR,33                                             
         FOUT  AGYPROFH,PAGYPROF,30                                             
         FOUT  AGYABBRH,PAGYABBR,7                                              
         FOUT  AGYEQUH,PAGYEQU,2                                                
         FOUT  AGYMDESH,PAGYMED,10                                              
         FOUT  AGYVDESH,PAGYVEN,20                                              
         FOUT  AGYACCTH,PAGYACCT,10                                             
*                                                                               
         FOUT  AGYNATIH,PAGYNAT,1                                               
         CLI   PAGYNAT,0           IF 0, IS USA DEFAULT                         
         BNE   *+8                                                              
         MVI   AGYNATI,C'U'        FORCE TO USA                                 
*                                                                               
         MVI   AGYSKIP,C'N'        DEFAULT IS N                                 
         TM    PAGYSTAT,X'01'      SKIP AGENCY ?                                
         BNO   *+8                 NO                                           
         MVI   AGYSKIP,C'Y'        YES                                          
         FOUT  AGYSKIPH                                                         
*                                                                               
         MVI   AGYPDTL,C'N'        DEFAULT IS N                                 
         TM    PAGYSTAT,X'02'      DDS BILLING AT PRD LVL DTL ?                 
         BNO   *+8                 NO                                           
         MVI   AGYPDTL,C'Y'        YES                                          
         FOUT  AGYPDTLH                                                         
*                                                                               
         FOUT  AGYRFPIH,SPACES,2                                                
         OC    PAGYPINI,PAGYPINI   SEE IF DISPLAYING ID IS NEEDED               
         BZ    FMTACC1                                                          
         CLC   PAGYPINI,=C'00'     GARBAGE ZEROS?                               
         BE    FMTACC1                                                          
         CLC   PAGYPINI,SPACES     GARBAGE SPACES?                              
         BE    FMTACC1                                                          
         MVC   WORK(2),PAGYPINI                                                 
         BAS   RE,DISPID           DISPLAY RFP ID                               
*                                                                               
         USING PAGYACCEL,R6                                                     
FMTACC1  LA    R2,AGYACCH          ACC AGY CODE LIST                            
         XC    AGYACC,AGYACC                                                    
         OI    4(R2),II1C                                                       
         OI    6(R2),X'80'                                                      
         LA    R6,PAGYREC+33       TRY TO FIND OLD ELEM                         
         MVI   ELCODE,X'03'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   FMTACCX             NOT FOUND                                    
         LA    R2,8(R2)                                                         
         LA    R3,PAGYACCAG                                                     
         LA    R1,4                4 AGENCYS FOR NOW                            
         CLI   0(R3),C' '                                                       
         BNH   FMTACCX                                                          
FMTACC   MVC   0(2,R2),0(R3)                                                    
         LA    R3,2(R3)                                                         
         CLI   0(R3),C' '                                                       
         BNH   FMTACCX                                                          
         LA    R2,2(R2)                                                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         BCT   R1,FMTACC                                                        
FMTACCX  DS    0H                                                               
         DROP  R6                                                               
*                                                                               
         USING PAGYCIDEL,R6                                                     
FMTCID   LA    R2,AGYCTIH          ACC AGY CODE LIST                            
         XC    AGYCTI,AGYCTI                                                    
         OI    4(R2),II1C                                                       
         OI    6(R2),X'80'                                                      
         LA    R6,PAGYREC+33       TRY TO FIND OLD ELEM                         
         MVI   ELCODE,X'05'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   FMTCIDX             NOT FOUND                                    
         MVC   AGYCTI(2),PAGYCID                                                
FMTCIDX  DS    0H                                                               
         DROP  R6                                                               
*                                                                               
         USING PAGFXEL,R6                                                       
FMTREP   DS    0H                  FOREIGN EXCHANGE REP AND NAME                
         XC    AGYFXRP,AGYFXRP     CLEAR                                        
         XC    AGYFXNM,AGYFXNM                                                  
         OI    AGYFXRPH+6,X'80'    XMIT                                         
         OI    AGYFXNMH+6,X'80'                                                 
         LA    R6,PAGYREC+33       TRY TO FIND OLD ELEM                         
         MVI   ELCODE,PAGFXIDQ     X'10'                                        
         BAS   RE,NEXTEL                                                        
         BNE   FMTREPX             NOT FOUND                                    
*                                                                               
         MVC   AGYFXRP(L'PAGFXREP),PAGFXREP                                     
         XC    WORK,WORK           WORK USED AS KEY IN GETREP                   
         MVC   WORK(3),PAGYREC     AGY/MED                                      
         MVI   WORK+3,X'11'        REP RECORD CODE                              
         MVC   WORK+4(L'PAGFXREP),PAGFXREP    REP CODE                          
         BAS   RE,GETREP                                                        
         FOUT  AGYFXNMH,WORK,30    WORK HAS REP NAME FROM GETREP                
*                                    OR "REC NO LONGER ON FILE" MSG             
FMTREPX  DS    0H                                                               
         DROP  R6                                                               
*                                                                               
         CLI   BACT,3              SEE IF DISPLAY                               
         BE    DONE                                                             
         LA    R2,AGYNAMEH                                                      
         B     EXIT                                                             
*                                                                               
PROTECT  OI    AGYNAMEH+1,X'20'                                                 
         OI    AGYADRSH+1,X'20'                                                 
         OI    AGYPROFH+1,X'20'                                                 
         OI    AGYABBRH+1,X'20'                                                 
         OI    AGYEQUH+1,X'20'                                                  
         OI    AGYMDESH+1,X'20'                                                 
         OI    AGYVDESH+1,X'20'                                                 
         OI    AGYACCTH+1,X'20'                                                 
         OI    AGYCTIH+1,X'20'                                                  
         B     DONE                                                             
*                                                                               
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
         CLI   0(R6),0                                                          
         BNE   *-18                                                             
NEXTELX  LTR   R6,R6                                                            
         BR    RE                                                               
*                                                                               
SPACES   DC    CL40' '                                                          
ZEROS    DC    40C'0'                                                           
AGYERR   EQU   35                                                               
APROFERR EQU   94                                                               
INVERR   EQU   2                                                                
NOTNUM   EQU   3                   ONLY NUMERIC DATA ALLOWED                    
TOOLONG  EQU   32                  INPUT IS TOO LONG                            
NOTFND   EQU   53                  RECORD NOT FOUND                             
VIRERR   DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
* VALIDATE ID IN WORK+2(10) AND RETURN ID NUMBER IN WORK(2)                     
*                                                                               
***********************************************************************         
*                                                                               
VALID    NTR1                                                                   
         XC    ELEM2,ELEM2         VALIDATE ID AND GET ID NUMBER                
         LA    R3,ELEM2                                                         
         USING CTIREC,R3                                                        
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKID,WORK+2       ID                                           
         OC    CTIKID,SPACES                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',ELEM2,TEMPIO                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ELEM2(L'CTIKEY),TEMPIO                                           
         BNE   VIDERR                                                           
         LA    R3,TEMPIO                                                        
         LA    R3,CTIDATA          FIRST ELEM                                   
         DROP  R3                                                               
*                                                                               
VID10    CLI   0(R3),0                                                          
         BE    VIDERR              CAN'T FIND ID NUMBER                         
         CLI   0(R3),X'02'                                                      
         BE    VID20                                                            
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     VID10                                                            
VID20    MVC   WORK(2),2(R3)       SAVE ID NUMBER                               
         SR    RC,RC                                                            
VIDERR   LTR   RC,RC                                                            
EXITIT   XIT1                                                                   
*                                                                               
***********************************************************************         
*                                                                               
* DISPLAY PRINCIPAL ID FOR RFP FROM ID NUMBER IN WORK(2)                        
*                                                                               
***********************************************************************         
*                                                                               
DISPID   NTR1                                                                   
         XC    ELEM2,ELEM2         GET ID NAME FROM NUMBER                      
         LA    R3,ELEM2                                                         
         USING CTIREC,R3                                                        
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKNUM,WORK                                                     
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',ELEM2,TEMPIO                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ELEM2(L'CTIKEY),TEMPIO                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,TEMPIO                                                        
         LA    R3,CTIDATA          FIRST ELEM                                   
         DROP  R3                                                               
*                                                                               
DID10    CLI   0(R3),0                                                          
         BNE   *+6                 CAN'T FIND ID NAME                           
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    DID20                                                            
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DID10                                                            
DID20    MVC   AGYRFPI,2(R3)       DISPLAY ID                                   
         OI    AGYRFPIH+6,X'80'                                                 
         B     EXITIT                                                           
*                                                                               
*                                                                               
***********************************************************************         
*                                                                               
*                VALIDATE REP EXISTS AND . . .                                  
*   KEY IN WORK (25) - RETURN REP NAME OR "NOT FOUND" IN WORK (30)              
*                                                                               
***********************************************************************         
*                                                                               
GETREP   NTR1                                                                   
         XC    ELEM2,ELEM2         USE AS O/P KEY                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',WORK,ELEM2                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ELEM2(L'PAGYKEY),WORK                                            
         BE    GETREPR             FOUND - NOW GET THE RECORD                   
         MVC   WORK(30),=C'*REP RECORD NO LONGER ON FILE*'                      
         B     GETREPEX            NOT FOUND                                    
*                                                                               
GETREPR  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'PRTFILE',ELEM2+27,TEMPIO,   X        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,TEMPIO                                                        
         USING PREPRECD,R3                                                      
         MVC   WORK(L'PREPNAME),PREPNAME                                        
         DROP  R3                                                               
*                                                                               
GETREPX  DS    0H                                                               
         SR    RC,RC                                                            
GETREPEX DS    0H                                                               
         LTR   RC,RC                                                            
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
ELEM2    DS    XL256                                                            
TEMPIO   DS    2000X               TEMPORARY IO AREA FOR DATAMGR USES           
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PGENEROL                                                       
*                                                                               
       ++INCLUDE GENOLD                                                         
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND                                                         
*                                                                               
         ORG   KEY                                                              
KAGY     DS    CL2                                                              
KMED     DS    CL1                                                              
KRCD     DS    CL1                                                              
         DS    CL28                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE PPAFMFFD                                                       
         ORG   HDRLAST                                                          
       ++INCLUDE PPAFMF1D                                                       
         ORG   T407FFD                                                          
         DS    CL16                                                             
BREC     DS    CL1                                                              
BACT     DS    CL1                                                              
OLNUM    DS    CL1                                                              
AGYADDR  DS    F                                                                
SCRNUM   DS    CL1                                                              
*                                                                               
         ORG   T407FFD+1300        PAST END OF SCREEN                           
*                                                                               
ELCODE   DS    CL1                                                              
ELEM     DS    XL50                                                             
PUBIO    DS    0X                  ** NOT USED - TAG HERE TO PREVENT            
*                                  ASSEMBLY ERR (PGENEROL USES PUBIO)           
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
*                                                                               
         PRINT ON                                                               
PREPRECD DSECT                                                                  
       ++INCLUDE PREPREC                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051PPAFM01   06/17/08'                                      
         END                                                                    
