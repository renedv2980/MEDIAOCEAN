*          DATA SET ACSCR0F    AT LEVEL 094 AS OF 03/13/19                      
*PHASE T60C0FA                                                                  
*&&ONLIN SET   Y                                                                
*INCLUDE ACJBEXC                                                                
*                                                                               
***********************************************************************         
* GHOA 094 SPEC-28585  LIMIT AGEING TO DEFAULT AND OPEN IN AGEMTHTB             
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 91 AS OF 06/10/14         *         
*                                                                     *         
***********************************************************************         
         TITLE 'Prod profile elements extra maintance'                          
T60C0F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C0F,RA,RR=RE                                                 
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         USING RESRECD,R2                                                       
*                                                                               
         L     RC,APALOCAL                                                      
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*&&UK                                                                           
         L     R1,=A(WCTYTAB)                                                   
         A     R1,APRELO                                                        
         ST    R1,AWCTYTAB                                                      
*                                                                               
         L     R1,=A(CHKTABL)                                                   
         A     R1,APRELO                                                        
         ST    R1,ACHKTABL                                                      
*                                                                               
         L     R1,=A(AGEMTHTB)                                                  
         A     R1,APRELO                                                        
         ST    R1,AAGEMTHT                                                      
*&&                                                                             
         CLI   INSCRN,SCRJBPF2     2ND  SCREEN ?                                
         BE    SCR02               YES, SET CURSOR FOR SECOND SCREEN            
         CLC   ATWA,ACURSOR        INSURE CURSOR WAS IN A FIELD                 
         BE    SCR01               NO,  SET CURSOR                              
         CLI   TWALREC,RECJOBPF    IF   FIRST TIME IN, THEN SET CURSOR          
         BE    SCR04               NO,  SKIP                                    
*                                                                               
SCR01    LA    RE,JOBCDEH          FORMAT CODE FIELD                            
         ST    RE,ACURSOR                                                       
         B     SCR04                                                            
*                                                                               
SCR02    DS    0H                                                               
         OI    JB2CDEH+6,FVOXMT+X'01'   NON-SENSE (MODIFY FIELD BIT TO          
*                                       REFRESH SCREEN ON HIT OF ENTER)         
         CLC   ATWA,ACURSOR        INSURE CURSOR WAS IN A FIELD                 
         BE    SCR03               NO,  SET CURSOR                              
         CLI   TWALREC,RECJBPF2    IF   FIRST TIME IN, THEN SET CURSOR          
         BE    SCR04               NO,  FILL IN JOB FIELDS                      
*                                                                               
SCR03    LA    RE,JB2CDEH          FORMAT CODE FIELD                            
         ST    RE,ACURSOR                                                       
*                                                                               
SCR04    DS    0H                                                               
         CLI   APMODE,APMVALK                                                   
         BNE   SCR10                                                            
         LA    R2,SCRTXT           TABLE     OF   SCREEN    UPDATES   1         
         CLI   INSCRN,SCRJBPF2     2ND  SCREEN ?                                
         BNE   SCR05               NO,  SKIP                                    
         LA    R2,SCRTXT2          TABLE     OF   SCREEN    UPDATES   2         
*                                                                               
SCR05    CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    SCR10               FINISHED                                     
         L     R4,0(,R2)           GET HEADER ADDRESS                           
         AR    R4,R5                                                            
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
         L     R3,4(,R2)           GET SCREEN TEXT NUMBER                       
         GOTO1 TEXTGET,APPARM,(R3),(R4),0                                       
         LA    R2,8(,R2)           BUMP TO NEXT                                 
         B     SCR05                                                            
*                                                                               
SCR10    LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     EXIT                                                             
         B     EXIT                RESTORE                                      
         B     EXIT                VALSEL                                       
         B     EXIT                GETSEL                                       
         B     EXIT                DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
*                                                                               
         TM    TWASWPST,TWASWAP    SWAP TO NEW RECORD ACTION?                   
         BZ    EXIT95              NO                                           
         XC    APCURSOR,APCURSOR   DON'T SET CURSOR ON WRONG SCREEN             
         MVI   APMODE,APMSWP           SWAP                                     
         MVC   APPARM(1),TWASWPRE      SWAP RECORD                              
         MVC   APPARM+1(1),TWASWPAC    SWAP ACTION                              
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   DS    0H                                                               
         CLI   APMODE,APMVALR      IN   VALIDATE  RECORD                        
         BNE   EXIT999             NO,  SKIP                                    
         CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS    FOUND ?                       
         BNE   EXIT999             YES, SKIP                                    
         CLI   APPFKEY,0           ANY  PF   KEY  DEPRESSED ?                   
         BNE   EXIT999             YES, SKIP                                    
         TM    TWASWPST,TWASWAP    SWAP TO   NEW  RECORD ACTION ?               
         BO    EXIT999             YES, SKIP                                    
         MVC   APCURSOR,ACURSOR    NO,  SET  APPLICATION CURSOR                 
*                                                                               
EXIT999  CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS    FOUND ?                       
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
         SPACE 1                                                                
VALKEY   DS    0H                                                               
         MVI   NEWKEY,NO                                                        
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         GOTO1 AFVAL,JOBCDEH                                                    
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    JOBCDE+4,FVITHIS    ANY INPUT?                                   
         BZ    *+10                NO                                           
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   JOBCDE,SAVFORM                                                   
         OI    JOBCDEH+6,FVOXMT    TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 MVC   SAVFORM,RESKFORM                                                 
         MVC   APRECKEY(L'RESKEY),RESKEY                                        
         LA    R1,IORD+IOACCFIL+IO1                                             
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR                                     
         BNE   VALKEY20                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
         OI    SCRTYPH+6,FVOXMT                                                 
         CLC   APREPCDE,AC@PROD    IS IT PROD?                                  
         BNE   IVALRPTY                                                         
*                                                                               
VALKEY18 MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         TM    TWAMODE,TWAMDFR     2ND PASS IN ACTION COPY?                     
         BZ    VALKEY98                                                         
         CLI   APACTN,ACTCPY                                                    
         BNE   VALKEY98                                                         
         OI    APINDS,APIOKADD     TURN ON TO TRICK ACTION COPY                 
         B     VALKEY98                                                         
*                                                                               
VALKEY20 TM    IOERR,IOEDEL        IS RECORD MARKED DELETED                     
         BNZ   VALKEY99            OK TO ADD RECORD                             
         MVI   APINDS,APIOKADD     RECORD NOT ON FILE, SO OK TO ADD             
         L     R2,AIOAREA1                                                      
         XC    RESKEY(256),RESKEY  RESET AIO AREA                               
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEY99 B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
         SPACE 1                                                                
DISKEY   NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         EJECT ,                                                                
**********************************************************************          
*  VALREC - CHECK SCREEN NUMBER                                      *          
*          IF SCREEN 2, USE DIFFERENT ROUTINE, VR2                   *          
**********************************************************************          
         SPACE 1                                                                
VALREC   DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 AFVAL,JOBNMEH                                                    
         BNE   VALREC05            NAME HAS  NOT  BEEN INPUT                    
         GOTO1 ADDNAME,APPARM,(R2),JOBNMEH   ADD  FORMAT    NAME                
         BNE   VALREC99            ON   ERROR,    EXIT                          
*                                                                               
VALREC05 CLI   INSCRN,SCRJBPF2     CHECK SCREEN #                               
         BE    VR2                 NOT 1ST SCREEN                               
*                                                                               
         USING REPTABD,R3                                                       
VALREC07 GOTO1 AFVAL,JOBTYPEH                                                   
         L     R3,ACTYPTAB                                                      
*                                                                               
VALREC10 CLI   REPCODE,EOT                                                      
         BE    IVALTYPE                                                         
         LR    R1,R3                                                            
         BAS   RE,EXPRPTY                                                       
         CLC   APREPCDE,JOBRPCDE   MATCH REPORT TYPE                            
         BNE   VALREC15                                                         
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         EXCLC R1,JOBRPTYP,FVIFLD                                               
         BNE   VALREC15                                                         
         TM    REPFLAG,REPDDS      DDS ONLY?                                    
         BZ    VALREC20                                                         
         TM    CUSTAT,CUSDDS       IS IT A DDS TERMINAL?                        
         BNZ   VALREC20                                                         
*                                                                               
VALREC15 LA    R3,REPLNQ(,R3)      BUMP TO NEXT                                 
         B     VALREC10                                                         
*                                                                               
VALREC20 MVC   APREPNUM,REPNUM                                                  
         MVC   APREPCDE,JOBRPCDE                                                
         GOTO1 ADDREPTY,APPARM,AIOAREA1                                         
         BNE   VALREC99            ON   ERROR, EXIT                             
         DROP  R3                                                               
*                                  ************************************         
*                                  * VALIDATE UNIT/LEDGER             *         
*                                  ************************************         
         GOTO1 GETTYPE,(R2)                                                     
         MVI   LDGFLDH,L'LDGFLDH+L'LDGFLD                                       
         MVC   LDGFLD,APREPUL                                                   
         GOTO1 MAKELIST,APPARM,('RFLLDG',LDGFLDH),(R2),                X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         GOTO1 ADDEL,(R2)                                                       
         BNE   VALREC99                                                         
*                                                                               
         USING RESRECD,R2                                                       
         MVC   RESKEY,APRECKEY                                                  
         MVC   SAVEKEY1,IOKEY                                                   
         DROP  R2                                                               
*                                  ************************************         
*                                  * VALIDATE SPECIFIC ACCOUNT(S)     *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLACC',JOBACCTH),(R2),               X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC34            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALLIST,APPARM,=C'SJ',(JOBBLOCK,JOBBLOCK+12),LITTACT             
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC31            NO   LIST                                    
         BP    VALREC99            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VALRC33A            YES, ADD ACCOUNT LIST                        
         B     IVAL2MNY            NO,  TOO MANY LISTS                          
*                                                                               
VALREC31 DS    0H                  VALIDATE ACCOUNT(S)                          
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,JOBBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
VALREC32 DS    0H                                                               
         CLI   0(R4),LACCOUNT      IS   THE LENGTH OF ACCOUNT > 12 ?            
         BH    IVALACCT            YES, INVALID ACCOUNT                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         MVC   ACTKUNT(2),=C'SJ'   UNIT/LEDGER                                  
         MVC   ACTKACT(12),12(R4)  ACCOUNT IS IN JOBBLOCK                       
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VALREC33            VALID ACCOUNT                                
         MVI   BYTE,C'N'           CHECK FOR WILDCARD                           
         BAS   RE,WILDCARD                                                      
         BNE   IVALACCT            INVALID ACCOUNT                              
*                                                                               
VALREC33 DS    0H                                                               
         LA    R4,32(,R4)          NEXT BLOCK IN JOBBLOCK                       
         BCT   R6,VALREC32                                                      
         DROP  R2                                                               
*                                                                               
VALRC33A DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  SPECIFIC ACCOUNT ELEMENT                
         BNE   VALREC99                                                         
*                                                                               
*                                  ************************************         
VALREC34 DS    0H                  * VALIDATE CONTRA ACCOUNT(S)       *         
*                                  ************************************         
         L     R2,AIOAREA1                                                      
         GOTO1 MAKELIST,APPARM,('RFLCNTR',JOBCNTRH),(R2),              X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC38            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         MVC   APWORK(2),SPACES    INDICATE CONTRA ACCOUNT                      
         GOTO1 VALLIST,APPARM,APWORK,(JOBBLOCK,JOBBLOCK+12),1                   
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC35            NO   LIST                                    
         BP    VALRC34A            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VALREC37            YES, ADD CONTRA LIST                         
         B     IVAL2MNY            NO,  TOO MANY LISTS                          
*                                                                               
VALRC34A DS    0H                  SHOW WHICH CONTRA CODE IS BAD                
*                                  INSERT NAME                                  
         MVC   FVXTRA(20),JOBBLOCK+12                                           
         B     VALREC99            GENERATE INVALID LIST                        
*                                                                               
VALREC35 DS    0H                  VALIDATE CONTRA ACCOUNT(S)                   
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,JOBBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
*                                                                               
VALREC36 DS    0H                  VALIDATE A CONTRA ACCOUNT                    
         CLI   0(R4),0             ANY  ACCOUNT REQUESTED ?                     
         BE    VALRC36H            NO,  CHECK NEXT BLOCK                        
         CLI   0(R4),LULACNT       IS   THE LENGTH OF CONTRA  > 14 ?            
         BH    IVALCNTR            YES, INVALID CONTRA ACCOUNT                  
         CLI   0(R4),LUNLG         IS   THE LENGTH OF CONTRA  <  2 ?            
         BL    IVALCNTR            YES, INVALID CONTRA ACCOUNT                  
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
*                                                                               
*                                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
*                                  ! NOTE: SINCE THIS MODULE IS FOR   !         
*                                  !       PRODUCTION, WE WILL NOT    !         
*                                  !       VALIDATE AGAINST THE       !         
*                                  !       CONTRA TABLE.  ALL         !         
*                                  !       UNIT/LEDGERS ARE VALID.    !         
*                                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
*                                                                               
*                                  VALIDATE THAT THE RECORD EXISTS              
         MVC   ACTKUNT(14),12(R4)  CONTRA ACCOUNT IS IN JOBBLOCK                
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
*                                                                               
*                                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
*                                  ! NOTE: WILD CARD CHARACTERS ARE   !         
*                                  !       NOT VALID FOR CONTRA       !         
*                                  !       ACCOUNTS.                  !         
*                                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
*                                                                               
         BNE   IVALCNTR            NOT VALID, INVALID CONTRA ACCOUNT            
*                                                                               
*                                                                               
VALRC36H DS    0H                  CHECK NEXT BLOCK                             
         LA    R4,32(,R4)          NEXT BLOCK IN JOBBLOCK                       
         BCT   R6,VALREC36                                                      
*                                                                               
         DROP  R2                                                               
*                                                                               
VALREC37 DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  CONTRA ACCOUNT ELEMENT                  
         BNE   VALREC99                                                         
*                                                                               
*                                  ************************************         
VALREC38 DS    0H                  * VALIDATE BILLING GROUP           *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLBLGP',JOBBGH),(R2),                X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC40            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         CLI   NPARMS,1            ONLY ONE INPUT ?                             
         BH    IVAL2MNY            NO,  TOO MANY INPUT PARAMETERS               
         LA    R4,JOBBLOCK                                                      
         CLI   0(R4),LENFBGRP-1    IS   THE LENGTH > 3 ?                        
         BH    IVALIPUT            YES, INVALID INPUT                           
         CLI   12(R4),C'+'         IS   THE FIRST CHARACTER + ?                 
         BE    IVALIPUT            YES, INVALID INPUT                           
         CLI   12(R4),C'-'         IS   THE FIRST CHARACTER - ?                 
         BE    IVALIPUT            YES, INVALID INPUT                           
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  BILLING GROUP ELEMENT                   
         BNE   VALREC99                                                         
*                                  ************************************         
VALREC40 DS    0H                  * VALIDATE OFFICE GROUP(S)         *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLOFGP',JOBOFGH),(R2),               X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC46            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 CHKLIST,APPARM,(1,0)                                             
         BNE   IVALIPUT            INVALID INPUT                                
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  OFFICE GROUP ELEMENT                    
         BNE   VALREC99                                                         
*                                                                               
*                                  ************************************         
VALREC46 DS    0H                  * VALIDATE OFFICE(S)               *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLOFF',JOBOFFH),(R2),                X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC58            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
*                                                                               
         USING RFLELD,R4                                                        
         MVI   BYTE,NO             ASSUME INCLUDE                               
         LA    R4,APELEM           ->     ELEMENT                               
         TM    RFLIND,RFLXCLD      EXCLUDE     FILTER ?                         
         BZ    *+8                 NO,    SKIP                                  
         MVI   BYTE,YES            SAY    EXCLUDE                               
         DROP  R4                                                               
*                                                                               
         ZIC   R4,NPARMS           NUMBER OF   OFFICES                          
         GOTO1 VOFFICE,APPARM,('VOFTYPEB',JOBBLOCK),(BYTE,(R4))                 
         BNE   VALREC99            BAD    INPUT                                 
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD    OFFICE    LIST ELEMENT                
         BNE   VALREC99                                                         
*                                                                               
*                                  ************************************         
VALREC58 DS    0H                  * VALIDATE MEDIA GROUP(S)          *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLMDGP',JOBMGH),(R2),                X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC60            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 CHKLIST,APPARM,(3,0)                                             
         BNE   IVALIPUT            INVALID INPUT                                
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  MEDIA GROUP ELEMENT                     
         BNE   VALREC99                                                         
*                                                                               
*                                  ************************************         
VALREC60 DS    0H                  * VALIDATE MEDIA(S)                *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLMED',JOBMEDH),(R2),                X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC62            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 CHKLIST,APPARM,(4,0)                                             
         BNE   IVALIPUT            INVALID INPUT                                
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  MEDIA ELEMENT                           
         BNE   VALREC99                                                         
*                                                                               
*                                  ************************************         
VALREC62 DS    0H                  * VALIDATE WORK CODE GROUP(S)      *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLWCGP',JOBWCGH),(R2),               X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC64            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 CHKLIST,APPARM,(5,0)                                             
         BNE   IVALIPUT            INVALID INPUT                                
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  WORK CODE GROUP ELEMENT                 
         BNE   VALREC99                                                         
*                                                                               
*                                  ************************************         
VALREC64 DS    0H                  * VALIDATE WORK CODE(S)            *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLWC',JOBWCH),AIOAREA1,              X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC67            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALLIST,APPARM,SPACES,(JOBBLOCK,JOBBLOCK+12),LITTWRK             
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC65            NO   LIST                                    
         BP    VALREC99            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VALREC66            YES, ADD WORKCODE LIST                       
         B     IVAL2MNY            NO,  TOO MANY LISTS                          
*                                                                               
VALREC65 DS    0H                  VALIDATE WORK CODE(S)                        
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,JOBBLOCK                                                      
*                                  *** SPECIAL CODE FOR CASE IN WHICH           
*                                  *** THE FIRST WORK CODE IS *X                
         CLI   0(R4),1             ONE     CHARACTER WORK CODE ?                
         BNE   VALRC65A            NO,     SKIP                                 
*                                                                               
         USING RFLELD,R9                                                        
         LA    R9,APELEM           ->      FILTER DATA ELEMENT                  
         TM    RFLIND,RFLXCLD      EXCLUDE EXCEPTION ?                          
         BZ    IVALWCDE            YES,    INVALID INPUT                        
*                                                                               
*                                  *** FOOL MAKELIST BY INSERTING               
*                                  *** AN EXTRA '*' IN FRONT OF THE             
*                                  *** INITIAL INPUT FIELD                      
*                                  INIT   DUMMY WORK CODE FIELD HDR             
         MVI   DWCFLDH,L'DWCFLDH+L'DWCFLD                                       
         MVC   DWCFLD,SPACES       CLEAR  DUMMY WORK CODE FIELD                 
         MVI   DWCFLD,C'*'         INSERT EXTRA '*'                             
         ZIC   RE,FVXLEN           GET    EXMVC LENGTH                          
         EXMVC RE,DWCFLD+1,JOBWC   INSERT INITIAL INPUT                         
         GOTO1 MAKELIST,APPARM,('RFLWC',DWCFLDH),AIOAREA1,             X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
*                                  TURN    OFF EXCLUDE EXCEPTION                
         NI    RFLIND,TURNOFF-RFLXCLD                                           
         DROP  R9                                                               
*                                                                               
         USING WCORECD,R1                                                       
VALRC65A DS    0H                  VALIDATE A WORK CODE                         
         CLI   0(R4),L'WCOKWRK     IS   THE LENGTH = 2 ?                        
         BNE   IVALWCDE            NO,  INVALID WORK CODE                       
         CLC   12(2,R4),=C'99'     SPECIAL WORK CODE  ?                         
         BE    VALRC65B            YES, SKIP VALIDATION                         
         CLC   12(2,R4),=C'**'     SPECIAL WORK CODE  ?                         
         BE    VALRC65B            YES, SKIP VALIDATION                         
         LA    R1,IOKEY                                                         
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ    X'0A'                                        
         MVC   WCOKCPY,CUABIN      COMPANY                                      
         MVC   WCOKUNT(2),=C'SJ'   UNIT/LEDGER FOR WORK CODE                    
         MVC   WCOKWRK(2),12(R4)   WORK CODE                                    
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   IVALWCDE            INVALID WORK CODE                            
         DROP  R1                                                               
*                                                                               
VALRC65B DS    0H                  VALIDATE A WORK CODE                         
         LA    R4,32(,R4)          GET  NEXT WORK CODE                          
         BCT   R6,VALRC65A         TEST NEXT WORK CODE                          
*                                                                               
VALREC66 DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  WORK CODE ELEMENT                       
         BNE   VALREC99                                                         
*                                                                               
*                                  ************************************         
VALREC67 DS    0H                  * VALIDATE BILLING TYPE            *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLBTYP',JOBBTYH),(R2),               X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC70            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,JOBBLOCK                                                      
*                                                                               
VALREC68 DS    0H                                                               
         LA    RF,BILLTY                                                        
*                                                                               
VALREC69 DS    0H                                                               
         CLI   0(RF),EOT           END  OF TABLE ?                              
         BE    IVALBTYP            YES, INVALID BILLING TYPE                    
         CLI   0(R4),LENFBILT-1    IS   LENGTH > 1 ?                            
         BH    IVALBTYP            YES, INVALID BILLING TYPE                    
         CLC   0(1,RF),12(R4)      MATCH BILLING TYPE ?                         
         BE    VALRC69A            YES, NEXT INPUT FIELD                        
         LA    RF,1(,RF)           BUMP TO NEXT BILL TYPE                       
         B     VALREC69                                                         
*                                                                               
VALRC69A DS    0H                                                               
         LA    R4,32(,R4)          BUMP TO NEXT PARAMETER                       
         BCT   R6,VALREC68                                                      
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  BILLING TYPE ELEMENT                    
         BNE   VALREC99                                                         
*                                                                               
*                                  ************************************         
VALREC70 DS    0H                  * VALIDATE ESTIMATE STATUS         *         
*                                  ************************************         
         XC    SAVEESTS,SAVEESTS                                                
         GOTO1 AFVAL,JOBESTSH                                                   
         BNE   VALREC71                                                         
         CLI   FVIFLD,C'U'         NOT APPROVED ONLY                            
         BE    *+8                                                              
         CLI   FVIFLD,C'A'         APPROVED/NO NEED EST BILL                    
         BE    *+8                                                              
         CLI   FVIFLD,C'R'         REVISED AWAITING APPROVAL                    
         BE    *+8                                                              
         CLI   FVIFLD,C'N'         NO ESTIMATES                                 
         BNE   IVALIPUT                                                         
         MVC   SAVEESTS,FVIFLD                                                  
*                                                                               
*                                  ************************************         
VALREC71 DS    0H                  * VALIDATE STUDIO TYPE(S)          *         
*                                  ************************************         
*&&US                                                                           
         GOTO1 MAKELIST,APPARM,('RFLSTTY',JOBSTTYH),(R2),              X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC72            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         CLI   NPARMS,1            ONLY ONE STUDIO TYPE REQUESTED ?             
         BNE   IVAL2MNY            NO,  TOO MANY INPUT PARAMETERS               
*                                                                               
         GOTO1 CHKLIST,APPARM,(6,0)                                             
         BNE   IVALIPUT            INVALID STUDIO TYPE                          
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  STUDIO TYPE ELEMENT                     
         BNE   VALREC99                                                         
*&&                                                                             
*                                                                               
*                                  ************************************         
VALREC72 DS    0H                  * VALIDATE USER FIELD              *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLUFLD',JOBUFLDH),(R2),              X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC73            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
*        BAS   RE,CHKUFLD                                                       
*        BNE   VALREC73                                                         
*                                                                               
         CLI   NPARMS,1            ONLY ONE INPUT ?                             
         BH    IVAL2MNY            NO,  TOO MANY INPUT PARAMETERS               
         LA    R4,JOBBLOCK                                                      
         CLI   0(R4),LENFUSRF-1    IS   THE LENGTH > 2 ?                        
         BH    IVALIPUT            YES, INVALID INPUT                           
         CLI   12(R4),C'+'         IS   THE FIRST CHARACTER + ?                 
         BE    IVALIPUT            YES, INVALID INPUT                           
         CLI   12(R4),C'-'         IS   THE FIRST CHARACTER - ?                 
         BE    IVALIPUT            YES, INVALID INPUT                           
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD SPECIFIC ACCOUNT ELEMENT                 
         BNE   VALREC99                                                         
*                                                                               
*                                  ************************************         
VALREC73 DS    0H                  * VALIDATE TYPE(S) OF TIME         *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLTTIME',JOBTIMEH),(R2),             X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC75            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         USING RFLELD,R9                                                        
         LA    R9,APELEM           ->   FILTER DATA ELEMENT                     
         TM    RFLIND,RFLXCLD      EXCLUDE EXCEPTION ?                          
         BO    IVALIPUT            YES, INVALID INPUT                           
         DROP  R9                                                               
*                                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,JOBBLOCK                                                      
*&&UK*&& MVI   BYTE,0                                                           
*                                                                               
VALREC74 DS    0H                                                               
         CLI   0(R4),1             HAS  TO BE 1 CHARACTER                       
         BH    IVALIPUT                                                         
*                                                                               
         CLI   12(R4),C'B'         CHECK VALID TYPE OF TIME                     
         BNE   VREC74A                                                          
*&&UK                                                                           
         TM    BYTE,X'02'                                                       
         BNZ   IVALIPUT                                                         
         OI    BYTE,X'02'                                                       
*&&                                                                             
         B     VREC74C                                                          
*                                                                               
VREC74A  CLI   12(R4),C'N'                                                      
         BNE   VREC74B                                                          
*&&UK                                                                           
         TM    BYTE,X'04'                                                       
         BNZ   IVALIPUT                                                         
         OI    BYTE,X'04'                                                       
*&&                                                                             
         B     VREC74C                                                          
*                                                                               
VREC74B  CLI   12(R4),C'R'                                                      
         BNE   IVALIPUT                                                         
*&&UK                                                                           
         TM    BYTE,X'08'                                                       
         BNZ   IVALIPUT                                                         
         OI    BYTE,X'08'                                                       
*&&                                                                             
VREC74C  LA    R4,32(,R4)                                                       
         BCT   R6,VALREC74                                                      
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  TYPE OF TIME ELEMENT                    
         BNE   VALREC99                                                         
*                                                                               
*                                  ************************************         
VALREC75 DS    0H                  * VALIDATE TRANSACTION TYPE(S)     *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLTTYPE',JOBITTYH),(R2),             X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALRC75A            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         USING RFLELD,R9                                                        
         LA    R9,APELEM                                                        
         SR    R6,R6                                                            
         IC    R6,NPARMS           SCAN ENTRIES = NUMBER OF TYPES               
         LA    RF,RFLLNQ(,R6)      NEW  LENGTH OF ELEMENT                       
         STC   RF,RFLLN            STORE IN ELEMENT                             
         GOTO1 CNVTTYPE,APPARM,(C'N',JOBBLOCK),(NPARMS,RFLDATA)                 
         BNE   VALREC99                                                         
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  TRANSACTION TYPE ELEMENT                
         BNE   VALREC99                                                         
         DROP  R9                                                               
*                                  ************************************         
VALRC75A DS    0H                  * AUTHORIZATION STATUS             *         
*                                  ************************************         
*&&UK                                                                           
         GOTO1 MAKELIST,APPARM,('RFLAUTH',JOBAUTHH),(R2),              X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC76            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         LA    R9,APELEM                                                        
         USING RFLELD,R9                                                        
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BH    IVAL2MNY            NO,  TOO MANY LISTS                          
         TM    RFLIND,RFLXCLD      EXCLUDE EXCEPTION ?                          
         BO    IVALIPUT            YES, INVALID INPUT                           
         LA    R4,JOBBLOCK                                                      
         ZIC   RF,0(R4)                                                         
         BCTR  RF,0                LENGTH OF INPUT - 1                          
         EXCLC RF,12(R4),AC@BOTH   DEFAULT - BOTH?                              
         BE    VALREC76                                                         
         MVI   BYTE,AUTHQ                                                       
         EXCLC RF,12(R4),AC@ATHED  AUTH?                                        
         BE    VALRC75B                                                         
         MVI   BYTE,UNAUTHQ                                                     
         EXCLC RF,12(R4),AC@UATH   UNAUTH?                                      
         BNE   IVALIPUT            NO, INVALID INPUT                            
*                                                                               
VALRC75B MVC   RFLDATA(L'BYTE),BYTE                                             
         MVI   RFLLN,RFLLNQ+L'BYTE                                              
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  AUTH STATUS ELEMENT                     
         BNE   VALREC99                                                         
         DROP  R9                                                               
*                                  ************************************         
VALREC76 DS    0H                  * VALIDATE VAT REGION              *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLVATRG',JOBVATRH),(R2),             X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALRC76B            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         USING RFLELD,R9                                                        
         LA    R9,APELEM           ->     ELEMENT                               
         TM    RFLIND,RFLXCLD      EXCLUDE     FILTER ?                         
         BO    IVALIPUT            YES, INVALID INPUT                           
*                                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,JOBBLOCK                                                      
*                                                                               
VALRC76A DS    0H                                                               
         CLI   0(R4),1             HAS  TO BE 1 CHARACTER                       
         BH    IVALIPUT                                                         
         CLI   12(R4),C'N'         NATIONAL                                     
         BE    *+8                                                              
         CLI   12(R4),C'E'         EU                                           
         BE    *+12                                                             
         CLI   12(R4),C'X'         FOREIGN BUT NOT EU                           
         BNE   IVALIPUT                                                         
*                                                                               
         LA    R4,32(,R4)                                                       
         BCT   R6,VALRC76A                                                      
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  VAT REGION ELEMENT                      
         BNE   VALREC99                                                         
         DROP  R9                                                               
*&&                                                                             
*                                  ************************************         
VALRC76B DS    0H                  * BUILD FILTER PROFILE DATA        *         
*                                  ************************************         
         USING RPFELD,R9                                                        
         LA    R9,APELEM                                                        
         BAS   RE,BLDRPRF          BUILD OR SAVE OFF PROFILE ELEMENT            
         NI    RPFROPT,RPFXLOCK+RPFOLOCK                                        
         NI    RPFOPT1,RPFXHELD+RPFIDRFT+RPFOHELD+RPFODRFT                      
         MVI   RPFOPT2,0                                                        
         MVI   RPFOPT3,0                                                        
         NI    RPFOPT4,RPFXESTO+RPFXCWOT+RPFOESTO+RPFOCWOT                      
         NI    RPFOPT5,RPFMRGPO+RPFXPORD+RPFOPORD+RPFIWC99                      
         MVI   RPFBLTRN,YES                                                     
         MVC   RPFESTST,SAVEESTS                                                
*&&US*&& MVI   RPFOPT7,0                                                        
*&&UK*&& NI    RPFOPT7,RPFIDJBS+RPFODJBS                                        
*                                                                               
         XC    RPFFLT1(4),RPFFLT1  CLEAR FILTER AREA                            
         MVI   RPFFLT5,0           CLEAR FILTER 5                               
*                                                                               
         LA    R4,5                FIVE FILTER TYPES                            
         LA    R3,RPFFLT1                                                       
         LA    R1,JOBF1H                                                        
VALREC77 GOTO1 AFVAL                                                            
         BNE   VALREC78                                                         
         MVC   0(1,R3),FVIFLD                                                   
         CLI   FVIFLD,C'*'         EXCLUDE FILTER?                              
         BNE   VALREC78                                                         
         MVC   0(1,R3),FVIFLD+1                                                 
         NI    0(R3),TURNOFF-X'40' MAKE LOWER CASE                              
*                                                                               
VALREC78 SR    RE,RE                                                            
         IC    RE,0(,R1)           BUMP TO PROTECTED FIELD                      
         AR    R1,RE                                                            
         IC    RE,0(,R1)           BUMP TO NEXT INPUT FILTER                    
         AR    R1,RE                                                            
         LA    R3,1(,R3)           BUMP TO NEXT FILTER IN PROFILE ELEM          
         CLM   R4,1,=AL1(2)        F1-F4 DONE YET?                              
         BNE   *+8                 NOT YET                                      
         LA    R3,RPFFLT5          SWITCH TO F5                                 
         BCT   R4,VALREC77                                                      
*                                  ************************************         
*                                  * INCLUDE UTILIZED TRANSACTIONS    *         
*                                  ************************************         
         GOTO1 AFVAL,JOBIUTRH                                                   
         BNE   IVALMISS            NO INPUT, MISSING                            
*                                                                               
         LA    RF,IUTRLST          LIST OF VALID INPUT                          
*&&UK*&& CLI   CUCTRY,CTRYGER                                                   
*&&UK*&& BNE   *+8                                                              
*&&UK*&& LA    RF,IUTRLSTG                                                      
VR078A   CLI   0(RF),X'FF'         END OF LIST                                  
         BE    IVALIPUT                                                         
         CLC   JOBIUTR,0(RF)                                                    
         BE    VALREC79                                                         
         LA    RF,1(,RF)                                                        
         B     VR078A                                                           
*                                                                               
VALREC79 MVC   RPFBLTRN,JOBIUTR                                                 
*                                                                               
*                                  ************************************         
VALREC80 DS    0H                  * INCLUDE CLOSED JOBS              *         
*                                  ************************************         
         GOTO1 VALYNO,JOBCLSEH                                                  
         BNE   VALREC99                                                         
         CLC   JOBCLSE,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT4,RPFXCLSE    EXCLUDE CLOSED JOBS                          
         CLC   JOBCLSE,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT4,RPFOCLSE    CLOSED JOBS ONLY                             
*                                                                               
*                                  ************************************         
*                                  * INCLUDE EXPENSE JOBS             *         
*                                  ************************************         
*&&US                                                                           
         GOTO1 VALYNO,JOBEXPSH                                                  
         BNE   VALREC99                                                         
         CLC   JOBEXPS,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT4,RPFIEXPS    INCLUDE EXPENSE JOBS                         
         CLC   JOBEXPS,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT4,RPFOEXPS    EXPENSE JOBS ONLY                            
*&&                                                                             
*                                  ************************************         
*                                  * INCLUDE REVERSALS                *         
*                                  ************************************         
         GOTO1 VALYNO,JOBRVRSH                                                  
         BNE   VALREC99                                                         
         CLC   JOBRVRS,APNO                                                     
         BNE   *+8                                                              
         OI    RPFROPT,RPFXRVRS    EXCLUDE REVERESED ITEMS                      
         CLC   JOBRVRS,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFROPT,RPFORVRS    REVERESED ITEMS ONLY                         
*                                  ************************************         
*                                  * TYPE OF BALANCE                  *         
*                                  ************************************         
         GOTO1 AFVAL,JOBTBALH                                                   
         BNE   VALREC83            NO INPUT, OKAY                               
         CLI   JOBTBAL,C'C'        CREDIT BALANCE                               
         BNE   VALRC82A                                                         
         OI    RPFROPT,RPFBALCR                                                 
         B     VALREC83                                                         
*                                                                               
VALRC82A CLI   JOBTBAL,C'D'        DEBIT BALANCE                                
         BNE   VALRC82B                                                         
         OI    RPFROPT,RPFBALDR                                                 
         B     VALREC83                                                         
*                                                                               
VALRC82B CLI   JOBTBAL,C'S'        OUT STANDING BALANCES ONLY                   
         BNE   VALRC82C                                                         
         OI    RPFROPT,RPFBALO                                                  
         B     VALREC83                                                         
*                                                                               
VALRC82C CLI   JOBTBAL,C'Z'        ZERO BALANCE                                 
         BNE   IVALIPUT                                                         
         OI    RPFROPT,RPFBALZR                                                 
*                                                                               
VALREC83 DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  PROFILE ELEMENT X'C4'                   
         BNE   VALREC99                                                         
         DROP  R9                                                               
*                                                                               
VALREC90 DS    0H                                                               
         MVC   IOKEY,SAVEKEY1      RESTORE THE KEY                              
*                                                                               
*                                  ************************************         
VALREC95 DS    0H                  * UPDATE THE RECORD WITH ALL THE   *         
*                                  * NEW ELEMENTS                     *         
*                                  ************************************         
         GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VALREC99            ON     ERROR, EXIT                           
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD     ADD    A      RECORD ?                       
         BO    VALREC97                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA     CHANGE A      RECORD ?                       
         BO    VALREC97                                                         
         DC    H'0'                WHAT   THE    HELL - ABEND !                 
*                                                                               
VALREC97 GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD    WRITE  OR     SOMETHING DUDE          
*                                                                               
VALREC99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    DISREC                                                           
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  VR2 - VALIDATE RECORD FOR PROF2 SCREEN                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R9                                                        
VR2      DS    0H                                                               
         LA    R9,APELEM           BUILD DEFAULT PROFILE DATA                   
         BAS   RE,BLDRPRF                                                       
*                                                                               
VR2010   MVI   APBYTE,RPFBALO+RPFTYP56+RPFBALCR+RPFBALDR                        
         OI    APBYTE,RPFXRVRS+RPFORVRS                                         
         NC    RPFROPT,APBYTE                                                   
         NI    RPFOPT1,RPFXOFFS+RPFXAPRV+RPFOOFFS+RPFOAPRV                      
         NI    RPFOPT4,RPFXCLSE+RPFIEXPS+RPFOCLSE+RPFOEXPS                      
         MVI   RPFOPT5,0           RESET                                        
         MVI   RPFOPT6,0           RESET                                        
         MVI   RPFOPT7,0           RESET                                        
*&&UK*&& MVI   RPFOPT8,0           RESET                                        
*&&UK*&& MVI   RPFOPT9,0           RESET                                        
*                                                                               
*                                  ************************************         
*                                  * INCLUDE LOCKED ACCOUNTS          *         
*                                  ************************************         
         GOTO1 VALYNO,JB2LOCKH                                                  
         BNE   VALREC99                                                         
         CLC   JB2LOCK,APNO                                                     
         BNE   *+8                                                              
         OI    RPFROPT,RPFXLOCK    EXCLUDE LOCKED ACCOUNTS                      
         CLC   JB2LOCK,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFROPT,RPFOLOCK    LOCKED ACCOUNTS ONLY                         
*                                                                               
*                                  ************************************         
*                                  * INCLUDE PURCHASE ORDERS          *         
*                                  ************************************         
         GOTO1 VALYNO,JB2PORDH                                                  
         BNE   VALREC99                                                         
         CLC   JB2PORD,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT5,RPFXPORD    EXCLUDE PURCHASE ORDERS                      
         CLC   JB2PORD,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT5,RPFOPORD    PURCHASE ORDERS ONLY                         
*                                                                               
*                                  ************************************         
*                                  * MERGE   PURCHASE ORDERS          *         
*                                  ************************************         
         GOTO1 VALYN,JB2MGPOH                                                   
         BNE   VALREC99                                                         
         CLC   JB2MGPO,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT5,RPFMRGPO    MERGER PURCHASE ORDERS WC                    
*                                                                               
*                                  ************************************         
*                                  * INCLUDE DRAFT TRANSACTIONS       *         
*                                  ************************************         
         GOTO1 VALYNO,JB2DRFTH                                                  
         BNE   VALREC99                                                         
         CLC   JB2DRFT,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT1,RPFIDRFT    INCLUDE DRAFT ITEMS                          
         CLC   JB2DRFT,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT1,RPFODRFT    DRAFT ITEMS ONLY                             
*                                                                               
*                                  ************************************         
*                                  * INCLUDE HELD TRANSACTIONS        *         
*                                  ************************************         
         GOTO1 VALYNO,JB2HELDH                                                  
         BNE   VALREC99                                                         
         CLC   JB2HELD,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT1,RPFXHELD    EXCLUDE HELD ITEMS                           
         CLC   JB2HELD,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT1,RPFOHELD    HELD ITEMS ONLY                              
*                                                                               
*                                  ************************************         
*                                  * INCLUDE CBIL W/O AND TRANSFERS   *         
*                                  ************************************         
         GOTO1 VALYNO,JB2CWOTH                                                  
         BNE   VALREC99                                                         
         CLC   JB2CWOT,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT4,RPFXCWOT    EXCLUDE CBIL W/O'S AND TRANSFERS             
         CLC   JB2CWOT,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT4,RPFOCWOT    CBIL W/O'S AND TRANSFERS ONLY                
*                                                                               
*                                  ************************************         
*                                  * INCLUDE ESTIMATES                *         
*                                  ************************************         
         GOTO1 VALYNO,JB2ESTOH                                                  
         BNE   VALREC99                                                         
         CLC   JB2ESTO,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT4,RPFXESTO    JOBS WITH NO ESTIMATES                       
         CLC   JB2ESTO,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT4,RPFOESTO    JOBS ONLY WITH ESTIMATES                     
*                                                                               
*                                  ************************************         
*                                  * INCLUDE WC99 W/GRP OR TYPE FILTS *         
*                                  ************************************         
         GOTO1 VALYN,JB2WC99H                                                   
         BNE   VALREC99                                                         
         CLC   JB2WC99,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT5,RPFIWC99    INCLUDE WC99                                 
*                                                                               
*&&US                                                                           
*                                  ************************************         
*                                  * INCLUDE INACTIVE AUTHORIZATIONS  *         
*                                  ************************************         
         GOTO1 VALAUTH,JB2AUTHH    THE DEFAULT IS NO (DO NOT INCLUDE)           
         BNE   VALREC99                                                         
         CLC   JB2AUTH,APNO        IF YES, INCLUDE                              
         BNE   *+8                                                              
         OI    RPFOPT6,RPFAUTHE    JOB WITH NO AUTHRIZATION AT ALL              
         CLI   JB2AUTH,C'A'        ACTIVE AUTHORIZATION ONLY ?                  
         BNE   *+8                                                              
         OI    RPFOPT6,RPFAUTHA    ACTIVE AUTHORIZATIONS                        
         CLI   JB2AUTH,C'I'        INACTIVE AUTHORIZATION ONLY ?                
         BNE   *+8                                                              
         OI    RPFOPT6,RPFAUTHI    INACTIVE AUTHORIZATIONS                      
         CLI   JB2AUTH,C'B'        BOTH INACTIVE AND ACTIVE AUTHS               
         BNE   *+8                                                              
         OI    RPFOPT6,RPFAUTHA+RPFAUTHI                                        
         CLC   JB2AUTH,APYES       IF YES, INCLUDE ALL                          
         BE    VR2011Z                                                          
         TM    RPFOPT6,RPFAUTHA+RPFAUTHI+RPFAUTHE                               
         BZ    IVALIPUT            MUST HAVE ENTERED INVALID CHAR               
***********************************************************************         
* Only Show External Vendor Invoices - CASHFLOW                       *         
***********************************************************************         
VR2011Z  DS    0H                                                               
         GOTO1 VALYNO,JB2CFLWH                                                  
         BNE   VALREC99                                                         
         CLC   JB2CFLW,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT6,RPFOEXIV    Only show                                    
***********************************************************************         
* Use Bill Due Date as Days Calculation Date - CASHFLOW               *         
***********************************************************************         
         GOTO1 VALYNO,JB2CFDCH                                                  
         BNE   VALREC99                                                         
         CLC   JB2CFDC,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT6,RPFBDDC                                                  
***********************************************************************         
* Include previous bills                                              *         
***********************************************************************         
                                                                                
         GOTO1 VALYNO,JB2IPBH                                                   
         BNE   VALREC99                                                         
         CLC   JB2IPB,APYES                                                     
         BNE   *+8                                                              
         OI    RPFOPT6,RPFSPBIL    Inlcude previous bills                       
***********************************************************************         
* Billing by workcode for % of Est. bill                              *         
***********************************************************************         
                                                                                
         GOTO1 VALYNO,JB2BYWCH                                                  
         BNE   VALREC99                                                         
         CLC   JB2BYWC,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT6,RPFBYWC     % est billing reported by workcode           
*&&                                                                             
*                                  ************************************         
*                                  * INCLUDE DRAFT JOBS               *         
*                                  ************************************         
         GOTO1 VALYNO,JB2DJOBH                                                  
         BNE   VALREC99                                                         
         CLC   JB2DJOB,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT7,RPFIDJBS   INCLUDE DRAFT JOBS                            
         CLC   JB2DJOB,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT7,RPFODJBS   DRAFT JOBS ONLY                               
*                                                                               
*&&UK                                                                           
*                                  ************************************         
*                                  * Include Jobs locked - Estimates  *         
*                                  ************************************         
         GOTO1 VALYNO,JB2JLESH                                                  
         BNE   VALREC99                                                         
         CLC   JB2JLES,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT9,RPFXJLES    JOBS ALLOW FOR ESTIMATES                     
         CLC   JB2JLES,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT8,RPFOJLES    JOBS ONLY WITH LOCKED FROM ESTIMATES         
*                                                                               
*                                  ************************************         
*                                  * Include Jobs locked - Orders     *         
*                                  ************************************         
         GOTO1 VALYNO,JB2JLORH                                                  
         BNE   VALREC99                                                         
         CLC   JB2JLOR,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT9,RPFXJLOR    JOBS ALLOW FOR ORDERS                        
         CLC   JB2JLOR,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT8,RPFOJLOR    JOBS ONLY WITH LOCKED FROM ORDERS            
*                                                                               
*                                  ************************************         
*                                  * Include Jobs locked - Billing    *         
*                                  ************************************         
         GOTO1 VALYNO,JB2JLBIH                                                  
         BNE   VALREC99                                                         
         CLC   JB2JLBI,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT9,RPFXJLBI    JOBS ALLOW FOR BILLING                       
         CLC   JB2JLBI,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT8,RPFOJLBI    JOBS ONLY WITH LOCKED FROM BILLING           
*                                                                               
*                                  ************************************         
*                                  * Include Jobs locked - Time       *         
*                                  ************************************         
         GOTO1 VALYNO,JB2JLTIH                                                  
         BNE   VALREC99                                                         
         CLC   JB2JLTI,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT9,RPFXJLTI    JOBS ALLOW FOR TIME                          
         CLC   JB2JLTI,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT8,RPFOJLTI    JOBS ONLY WITH LOCKED FROM TIME              
*                                                                               
*                                  ************************************         
*                                  * Include Jobs locked - Adjust     *         
*                                  ************************************         
         GOTO1 VALYNO,JB2JLADH                                                  
         BNE   VALREC99                                                         
         CLC   JB2JLAD,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT9,RPFXJLAD    JOBS ALLOW FOR Adjustments                   
         CLC   JB2JLAD,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT8,RPFOJLAD    JOBS ONLY WITH LOCKED FROM Adjust            
*                                                                               
*                                  ************************************         
*                                  * Include Jobs locked - 3rd party  *         
*                                  ************************************         
         GOTO1 VALYNO,JB2JLEXH                                                  
         BNE   VALREC99                                                         
         CLC   JB2JLEX,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT9,RPFXJLEX    JOBS ALLOW FOR (3rd PARTY)                   
         CLC   JB2JLEX,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT8,RPFOJLEX    JOBS ONLY WITH LOCKED FROM 3rd PARTY         
*                                                                               
*                                  ************************************         
*                                  * Include sub-jobs                 *         
*                                  ************************************         
         GOTO1 VALYNO,JB2SUBJH                                                  
         BNE   VALREC99                                                         
         CLC   JB2SUBJ,APNO                                                     
         BNE   *+8                                                              
         OI    RPFOPT9,RPFXSUBJ    Exclude sub-jobs                             
         CLC   JB2SUBJ,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT8,RPFOSUBJ    Sub-jobs only                                
*&&                                                                             
*&&US                                                                           
*                                  ************************************         
*                                  * INCLUDE $0 TRANSACTION           *         
*                                  ************************************         
         GOTO1 VALYNO,JB2ZTRNH                                                  
         BNE   VALREC99                                                         
         CLC   JB2ZTRN,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT6,RPFZPST    INCLUDE $0 TRANSACTIONS                       
*                                                                               
*&&                                                                             
*                                                                               
*                                  ************************************         
*                                  * VALIDATE AGEING METHOD           *         
*                                  ************************************         
*                                                                               
         USING AGEMTHDD,R3         AGEING    METHODS                            
*                                  **** TEMPORARY CODE START     ****           
         TM    CUSTAT,CUSDDS       DDS  TERMNINAL ?                             
         BZ    VR2020              NO,  SKIP ANY  UPDATES                       
*                                  **** TEMPORARY CODE END       ****           
*&&US*&& LA    R3,AGEMTHTB         ->   AGEING    TABLE                         
*&&UK*&& L     R3,AAGEMTHT         ->   AGEING    TABLE                         
         MVC   RPFAGMTH,AGEMTHC    DEFAULT   METHOD                             
         GOTO1 AFVAL,JB2AGEMH      ANY  AGEING    METHOD SPECIFIED ?            
         BNE   VR2020              NO,  SKIP                                    
         ZIC   R4,FVXLEN           GET  EXECUTE   LENGTH                        
*                                                                               
VR2012   DS    0H                  VALIDATE  METHOD                             
         CLI   0(R3),EOT           END  OF   TABLE                              
         BE    IVALIPUT            YES, INVALID   INPUT                         
         LR    R1,R3               ->   METHOD                                  
         BAS   RE,EXPAGEMT         EXPAND    AGEING    METHOD                   
         EXCLC R4,AGEMETHN,FVIFLD  FOUND     AGEING    METHOD ?                 
         BE    VR2014              YES, USE  AGEING    METHOD                   
         LA    R3,AGEMTHLQ(,R3)    NEXT AGEING    METHOD                        
         B     VR2012              TEST AGEING    METHOD                        
*                                                                               
VR2014   DS    0H                  FOUND     AGEING    METHOD                   
         MVC   RPFAGMTH,AGEMETHC   SAVE AGEING    METHODCODE                    
         DROP  R3                                                               
*                                                                               
VR2020   DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  PROFILE ELEMENT X'C4'                   
         BNE   VALREC99                                                         
*                                                                               
*                                  ************************************         
*                                  * VALIDATE EXCEPTION REASON(S)     *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLXCPT',JB2XCPTH),(R2),              X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VR2050              NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         USING RFLELD,R9                                                        
         TM    RFLIND,RFLXCLD      EXCLUDE EXCEPTION ?                          
         BO    IVALIPUT            YES, INVALID INPUT                           
         DROP  R9                                                               
*                                                                               
         USING JXCEPTD,R6                                                       
         ZIC   R0,NPARMS           # OF PARMS FROM SCANNER                      
         LA    R2,JOBBLOCK         SCANNER BLOCK FROM MAKELIST                  
         LA    R6,JBEXPBLK                                                      
         MVI   JXMODE,JXMLIT       LITERAL                                      
         XC    SAVEXP,SAVEXP                                                    
*                                                                               
VR2030   MVC   JXCODE,12(R2)       EXCEPTION CODE TO BE CHECKED                 
         MVC   FVXTRA(12),12(R2)   SHOW EXCEPTION CODE ERROR                    
         CLI   0(R2),1             CHECK LENGTH                                 
         BNE   IVALIPUT                                                         
         GOTO1 =V(ACEXCP),APPARM,JBEXPBLK,RR=APRELO                             
*                                                                               
         USING JXMSGD,RE                                                        
         ICM   RE,15,JXALIT                                                     
         BZ    IVALIPUT            INVALID INPUT                                
         SR    RF,RF                                                            
         IC    RF,JXMSEQ                                                        
         BCTR  RF,0                SUBTRACT ONE                                 
         LA    R1,1                                                             
         SLL   R1,0(RF)                                                         
         L     RF,SAVEXP                                                        
         NR    RF,R1                                                            
         BNZ   IVALEDUP            DUPLICATE ENTRY                              
         O     R1,SAVEXP           SAVE OFF THIS EXCEPTION AS A BIT             
         ST    R1,SAVEXP                                                        
         LA    R2,32(,R2)          BUMP TO NEXT ENTRY                           
         BCT   R0,VR2030                                                        
         MVC   FVXTRA,SPACES                                                    
         DROP  RE,R6                                                            
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  EXCEPTION REASONS ELEMENT               
         BNE   VALREC99                                                         
*                                                                               
*                                  ************************************         
VR2050   DS    0H                  * VALIDATE TIME WORKCODE TYPE(S)   *         
*                                  ************************************         
*&&US                                                                           
         MVC   WCTYLIST,SPACES                                                  
         GOTO1 MAKELIST,APPARM,('RFLTWTP',JB2TWTPH),(R2),              X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VR2100              NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         BAS   RE,CHKTYPES                                                      
         BNE   VALREC99                                                         
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  TIME WORKCODE TYPE ELEMENT              
         BNE   VALREC99                                                         
*&&                                                                             
*                                                                               
*                                  ************************************         
VR2100   DS    0H                  * VALIDATE OUT OF POCKET           *         
*                                  *          WORK CODE TYPE(S)       *         
*                                  ************************************         
*&&US                                                                           
         GOTO1 MAKELIST,APPARM,('RFLOPTP',JB2OPTPH),(R2),              X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VR2150              NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         BAS   RE,CHKTYPES                                                      
         BNE   VALREC99                                                         
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  OUT OF POCKET WORKCODE TYPES            
         BNE   VALREC99                                                         
*&&                                                                             
*                                                                               
*                                  ************************************         
VR2150   DS    0H                  * VALIDATE PRICE LEVELS            *         
*                                  ************************************         
*&&US                                                                           
         GOTO1 MAKELIST,APPARM,('RFLPRCLV',JB2PRLVH),(R2),             X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST  THE  RETURN    CODE                    
         BM    VR2200              NO    INPUT                                  
         BP    VALREC99            BAD   INPUT                                  
*                                  GOOD  INPUT                                  
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,JOBBLOCK                                                      
         XC    PRCLVLS,PRCLVLS     CLEAR THE  PRICE     LEVELS    USED          
*                                                                               
VR2160   DS    0H                  VALIDATE   THE  PRICE     LEVELS             
         MVC   FVXTRA(20),12(R4)   SAVE  THE  PRICE     LEVEL                   
         CLI   0(R4),1             ONE   BYTE PRICE     LEVEL ?                 
         BNE   IVALIPUT            NO,   INVALID   INPUT                        
*                                                                               
         CLI   12(R4),C'A'         PRICE LEVEL     A-D  ONLY                    
         BL    IVALIPUT                                                         
         CLI   12(R4),C'D'                                                      
         BH    IVALIPUT                                                         
*                                                                               
         LA    RE,PRCLVLS          ->    PRICE     LEVELS    USED               
         ZIC   RF,12(,R4)          GET   PRICE     LEVEL                        
         SH    RF,=XL2'00C1'       MINUS C'A'                                   
         AR    RE,RF               ->    PRICE     LEVEL     SLOT               
         CLI   0(RE),0             WAS   PRICE     LEVEL     SLOT USED          
         BNE   IVALEDUP            YES,  DUPLICATE ENTRY                        
         MVC   0(1,RE),12(R4)      NO,   USE  THE  SLOT                         
*                                                                               
         LA    R4,32(,R4)          CHECK NEXT BLOCK                             
         BCT   R6,VR2160                                                        
         MVC   FVXTRA,SPACES                                                    
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD   PRICE     LEVELS                       
         BNE   VALREC99                                                         
*&&                                                                             
*                                  ************************************         
VR2200   DS    0H                  * BrandOcean Estimate Status       *         
*                                  ************************************         
*&&UK                                                                           
         GOTO1 MAKELIST,APPARM,('RFLBESTA',JB2BESTH),(R2),             X        
               ('MAXPARM',JOBBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VR2300              NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,JOBBLOCK                                                      
*                                                                               
VR2250   DS    0H                                                               
         CLI   0(R4),1             HAS  TO BE 1 CHARACTER                       
         BH    IVALIPUT                                                         
         CLI   12(R4),PRGRSS       IN PROGRESS                                  
         BE    *+8                                                              
         CLI   12(R4),SUBINA       SUBMITTED TO INTERNAL APPROVER               
         BE    *+8                                                              
         CLI   12(R4),INTAPP       INTERNALLY APPROVED                          
         BE    *+8                                                              
         CLI   12(R4),SUBCLI       SUBMITTED TO CLIENT APPROVER                 
         BE    *+8                                                              
         CLI   12(R4),CLIAPP       CLIENT APPROVED                              
         BE    *+8                                                              
         CLI   12(R4),REJCTD       REJECTED                                     
         BE    *+8                                                              
         CLI   12(R4),DELTED       DELETED                                      
         BE    *+8                                                              
         CLI   12(R4),MERGD        MERGED                                       
         BNE   IVALIPUT                                                         
*                                                                               
         LA    R4,32(,R4)                                                       
         BCT   R6,VR2250                                                        
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  BRANDOCEAN ESTIMATE STATUS ELE          
         BNE   VALREC99                                                         
*&&                                                                             
VR2300   DS    0H                  SPACE FOR  FUTURE    EXPANSION               
*                                                                               
         B     VALREC95            UPDATE THE RECORD WITH THE NEW EL.S          
         EJECT ,                                                                
***********************************************************************         
*  WILDCARD                                                           *         
***********************************************************************         
         SPACE 1                                                                
WILDCARD NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,1,0(R4)          LENGTH                                       
         LA    RF,12(,R4)          START OF ACCOUNT                             
*                                                                               
         CLI   BYTE,C'U'           CHECK UNIT/LEDGER                            
         BNE   WILD200                                                          
*                                                                               
         LA    R2,2                JUST CHECKING U/L                            
*                                                                               
WILD100  CLI   0(RF),C'?'          U/L HAS WILDCARD?                            
         BE    WILDNO              WILDCARD INVALID                             
         LA    RF,1(,RF)           NEXT CHAR                                    
         BCTR  R1,0                                                             
         BCT   R2,WILD100                                                       
*                                                                               
WILD200  CLI   0(RF),C'?'          CONTRA U/L HAS WILDCARD?                     
         BE    WILDYES             WILDCARD INVALID                             
         LA    RF,1(,RF)           NEXT CHAR                                    
         BCT   R1,WILD200                                                       
*                                                                               
WILDNO   LTR   RE,RE               WILDCARD INVALID / U/L HAS WILDCARD          
         B     XIT                                                              
*                                                                               
WILDYES  LA    R1,APELEM                                                        
*                                                                               
         USING RFLELD,R1                                                        
         OI    RFLIND,RFLWILD      TURN ON THE BIT                              
         CR    RE,RE               WILDCARD IS GOOD                             
         B     XIT                                                              
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  BLDRPRF - BUILD RPFELD ELEMENT X'C4'                               *         
***********************************************************************         
         SPACE 1                                                                
         USING RPFELD,R9                                                        
BLDRPRF  NTR1                                                                   
         LA    R9,APELEM           BUILD DEFAULT PROFILE DATA                   
         XC    APELEM,APELEM                                                    
         MVI   RPFEL,RPFELQ        ELEMENT CODE                                 
*&&US*&& MVI   RPFLN,RPFLNQ        LENGTH OF ELEMENT                            
*&&UK*&& MVI   RPFLN,RPFLN2Q                                                    
         OI    RPFPOPT,RPFBOX      TURN ON BIT FOR BOXES                        
         OI    RPFEDOPT,RPFEDCMA+RPFEDTRL      COMMAS, TRAILING MINUS           
         MVI   RPFPCTS,C'2'                                                     
         MVI   RPFRND,PENNY                                                     
         MVI   APELCODE,RPFELQ     GET PROFILE ELEMENT                          
         GOTO1 GETEL,(R2)                                                       
         BNE   XIT                                                              
         SR    RF,RF                                                            
         IC    RF,1(,R1)                                                        
         BCTR  RF,0                                                             
         EXMVC RF,APELEM,0(R1)                                                  
*&&UK*&& MVI   RPFLN,RPFLN2Q       NEW LENGTH FOR OPTION 7 & 8                  
         GOTO1 DELEL,(R2)                                                       
         B     XIT                                                              
         DROP  R9                                                               
         EJECT ,                                                                
***********************************************************************         
*  CHKLIST - CHECK FOR OFFICE GROUP, MEDIA, ... AFTER MAKELIST CALL   *         
***********************************************************************         
         SPACE 1                                                                
         USING CHKTABD,R2                                                       
CHKLIST  NTR1                                                                   
*&&US*&& LA    R2,CHKTABL                                                       
*&&UK*&& L     R2,ACHKTABL                                                      
*                                                                               
CHKL005  CLI   CHKTYPE,EOT                                                      
         BE    CHKCCNE                                                          
         CLC   CHKTYPE,0(R1)       RIGHT     TYPE OF   CHECK ?                  
         BE    CHKL010             YES, CONTINUE                                
         LA    R2,CHKTABLN(,R2)                                                 
         B     CHKL005                                                          
*                                                                               
CHKL010  DS    0H                                                               
         LA    RE,APELEM           GET  CURRENT   FILTER DATA ELEMENT           
*                                                                               
         USING RFLELD,RE           SET  ADDRESSABILITY                          
         CLI   RFLEL,RFLELQ        DO   WE   HAVE A    FILTER    EL ?           
         BNE   CHKL015             NO,  SKIP THE  TEST                          
         TM    RFLIND,RFLXCLD      EXCLUDE   FILTER    ALREADY   ON ?           
         BZ    CHKL015             NO,  SKIP THE  TEST                          
         TM    CHKFIND,CHKFXLD     EXCLUDE   SUPPORTED ?                        
         BZ    CHKCCNE             NO,  INVALID                                 
         DROP  RE                                                               
*                                                                               
CHKL015  DS    0H                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,JOBBLOCK                                                      
*                                                                               
CHKL020  CLC   0(1,R4),CHKMLN      VALID     LENGTH    ?                        
         BH    CHKL090             NO,  ERROR                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         TM    CHKFIND,CHKFSPC     Space filled key?                            
         BZ    *+10                No                                           
         MVC   IOKEY,SPACES        Yes                                          
*                                                                               
         MVC   IOKEY(L'CHKKEY),CHKKEY        BUILD     KEY                      
         ZIC   RE,CHKCPY                                                        
         LA    RE,IOKEY(RE)        ->   TO   COMPANY   CODE LOCATION            
         MVC   0(1,RE),CUABIN                                                   
         ZIC   RE,CHKDSP                                                        
         LA    RE,IOKEY(RE)        ->   TO   DATA           LOCATION            
         ZIC   R1,0(,R4)           LENGTH    OF   FIELD     IN SCANBLK          
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),12(R4)                                                  
         LA    R1,IOHIGH+IOACCFIL+IO2                                           
         GOTO1 AIO                                                              
         BNE   CHKL090                                                          
         L     RF,AIOAREA2         CHECK     IF   KEYS ARE  THE  SAME           
         CLC   IOKEY(ACCORLEN),0(RF)                                            
         BNE   CHKL090             NO   GOOD,     SHOW INPUT                    
         LA    R4,32(,R4)                                                       
         BCT   R6,CHKL020                                                       
         B     CHKCCEQ                                                          
*                                                                               
CHKL090  ZIC   R1,0(,R4)                                                        
         BCTR  R1,0                                                             
         EXMVC R1,FVXTRA,12(R4)    INSERT    INVALID INPUT  INTO TEXT           
         B     CHKCCNE                                                          
*                                                                               
CHKCCEQ  SR    RE,RE                                                            
*                                                                               
CHKCCNE  LTR   RE,RE                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  CHKTYPES - CHECK VALID WORKCODE TYPES                              *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,RF                                                        
CHKTYPES NTR1                                                                   
         MVC   FVMSGNO,=AL2(FVFNOTV)     ASSUME INVALID INPUT                   
         LA    RF,APELEM           ->   FILTER DATA ELEMENT                     
         TM    RFLIND,RFLXCLD      EXCLUDE EXCEPTION ?                          
         BO    CKTY98              YES, INVALID INPUT                           
         DROP  RF                                                               
*                                                                               
         ZIC   R6,NPARMS                                                        
         LA    R4,JOBBLOCK                                                      
*                                                                               
CKTY10   DS    0H                                                               
*&&US*&& LA    RF,WCTYTAB          TABLE OF WORKCODES                           
*&&UK*&& L     RF,AWCTYTAB         TABLE OF WORKCODES                           
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         SR    R1,R1                                                            
         IC    R1,0(,R4)                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVXTRA(0),12(R4)                                                 
         CLI   0(R4),1             SHOULD ONLY BE ONE CHARACTER                 
         BH    CKTY98                                                           
*                                                                               
CKTY20   CLI   0(RF),X'FF'         EOT  ?                                       
         BE    CKTY98              YES, INVALID WORKCODE                        
         CLC   0(1,RF),12(R4)      FOUND A MATCH ?                              
         BE    CKTY30                                                           
         LA    RF,1(,RF)           NEXT VALID WORKCODE                          
         B     CKTY20                                                           
*                                                                               
CKTY30   MVC   FVMSGNO,=AL2(ACEDUPR)                                            
         LA    R1,L'WCTYLIST                                                    
         LA    RF,WCTYLIST         CHECK FOR DUPLICATE                          
*                                                                               
CKTY32   CLI   0(RF),C' '          END                                          
         BE    CKTY50                                                           
         CLC   0(1,RF),12(R4)                                                   
         BE    CKTY98              YES, SEND OUT TO ERROR                       
         LA    RF,1(,RF)                                                        
         BCT   R1,CKTY32                                                        
         MVC   FVMSGNO,=AL2(ACE2MANY)                                           
         B     CKTY98                                                           
*                                                                               
CKTY50   MVC   0(1,RF),12(R4)      SAVE USED WC TYPE                            
         LA    R4,32(,R4)          BUMP TO NEXT WORKCODE                        
         BCT   R6,CKTY10                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   FVXTRA,SPACES                                                    
*                                                                               
CKTY98   CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE A FIELD FOR YES OR NO                                     *         
*                                                                     *         
*        ON ENTRY - R1      = ADDRESS OF HEADER FOR FIELD             *         
*        ON EXIT  - FVMSGNO = FVFOK OR FVFNOTV OR FVMISS              *         
*                   CC      = EQUAL     I.E. FVMSGNO = FVFOK          *         
*                   CC      = NOT EQUAL I.E. FVMSGNO = FVFNOTV|FVMISS *         
***********************************************************************         
         SPACE 1                                                                
VALYN    NTR1                                                                   
         MVC   FVMSGNO,=AL2(FVFOK) ASSUME OKAY                                  
         LR    R2,R1               SAVE   REGISTER                              
         GOTO1 AFVAL               ANY    DATA ?                                
         BNE   VALYNMIS            NO,    MISSING                               
         CLC   8(1,R2),APYES       'YES'  ?                                     
         BE    VALYNEX             YES,   OKAY                                  
         CLC   8(1,R2),APNO        'NO'   ?                                     
         BE    VALYNEX             YES,   OKAY                                  
*                                                                               
VALYNNG  DS    0H                  NOT    VALID                                 
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALYNEX             RETURN                                       
*                                                                               
VALYNMIS DS    0H                  MISSING                                      
         MVC   FVMSGNO,=AL2(FVFMISS)                                            
*        B     VALYNEX             RETURN                                       
*                                                                               
VALYNEX  DS    0H                  RETURN                                       
         CLC   FVMSGNO,=AL2(FVFOK) SET    CONDITION CODE                        
         B     XIT                 EXIT                                         
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE A FIELD FOR YES, NO, OR ONLY                              *         
*                                                                     *         
*        ON ENTRY - R1      = ADDRESS OF HEADER FOR FIELD             *         
*        ON EXIT  - FVMSGNO = FVFOK OR FVFNOTV OR FVMISS              *         
*                   CC      = EQUAL     I.E. FVMSGNO = FVFOK          *         
*                   CC      = NOT EQUAL I.E. FVMSGNO = FVFNOTV|FVMISS *         
***********************************************************************         
         SPACE 1                                                                
VALYNO   NTR1                                                                   
         MVC   FVMSGNO,=AL2(FVFOK) ASSUME OKAY                                  
         LR    R2,R1               SAVE   REGISTER                              
         GOTO1 AFVAL               ANY    DATA ?                                
         BNE   VALYNOMS            NO,    MISSING                               
         CLC   8(1,R2),APYES       'YES'  ?                                     
         BE    VALYNOEX            YES,   OKAY                                  
         CLC   8(1,R2),APNO        'NO'   ?                                     
         BE    VALYNOEX            YES,   OKAY                                  
         CLC   8(1,R2),APONLY      'ONLY' ?                                     
         BE    VALYNOEX            YES,   OKAY                                  
*                                                                               
VALYNONG DS    0H                  NOT    VALID                                 
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALYNOEX            RETURN                                       
*                                                                               
VALYNOMS DS    0H                  MISSING                                      
         MVC   FVMSGNO,=AL2(FVFMISS)                                            
*        B     VALYNOEX            RETURN                                       
*                                                                               
VALYNOEX DS    0H                  RETURN                                       
         CLC   FVMSGNO,=AL2(FVFOK) SET    CONDITION CODE                        
         B     XIT                 EXIT                                         
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE A FIELD FOR AUTHORIZATION STATUS VALUES                   *         
*                                                                     *         
*        ON ENTRY - R1      = ADDRESS OF HEADER FOR FIELD             *         
*        ON EXIT  - FVMSGNO = FVFOK OR FVFNOTV OR FVMISS              *         
*                   CC      = EQUAL     I.E. FVMSGNO = FVFOK          *         
*                   CC      = NOT EQUAL I.E. FVMSGNO = FVFNOTV|FVMISS *         
***********************************************************************         
         SPACE 1                                                                
VALAUTH  NTR1                                                                   
         MVC   FVMSGNO,=AL2(FVFOK) ASSUME OKAY                                  
         LR    R2,R1               SAVE   REGISTER                              
         GOTO1 AFVAL               ANY    DATA ?                                
         BNE   VALAUTMS            NO,    MISSING                               
         CLC   8(1,R2),APYES       'YES'  ?                                     
         BE    VALAUTEX            YES,   OKAY                                  
         CLC   8(1,R2),APNO        'NO'   ?                                     
         BE    VALAUTEX            YES,   OKAY                                  
         CLI   8(R2),C'A'          'A'    ?                                     
         BE    VALAUTEX            YES,   OKAY                                  
         CLI   8(R2),C'B'          'B'    ?                                     
         BE    VALAUTEX            YES,   OKAY                                  
         CLI   8(R2),C'I'          'I'    ?                                     
         BE    VALAUTEX            YES,   OKAY                                  
*                                                                               
VALAUTNG DS    0H                  NOT    VALID                                 
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALAUTEX            RETURN                                       
*                                                                               
VALAUTMS DS    0H                  MISSING                                      
         MVC   FVMSGNO,=AL2(FVFMISS)                                            
*        B     VALAUTEX            RETURN                                       
*                                                                               
VALAUTEX DS    0H                  RETURN                                       
         CLC   FVMSGNO,=AL2(FVFOK) SET    CONDITION CODE                        
         B     XIT                 EXIT                                         
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY PROD PROFILE DATA                                          *         
*                                                                     *         
*        LIKE VALREC, WE CHECK SCREEN NUMBER AND USE MATCHING ROUTINE.*         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1         PROFILE ELEMENT                              
         CLI   INSCRN,SCRJBPF2     CHECK SCREEN #                               
         BE    DR2                 NOT 1ST SCREEN                               
         TWAXC JOBNMEH,JOBTABH                                                  
         GOTO1 GETNAME,APPARM,(R2),JOBNMEH                                      
         GOTO1 GETPER,APPARM,(R2),JOBOWNH                                       
         MVC   JOBTYPE(L'APREPTYP),APREPTYP                                     
*                                                                               
         MVC   JOBIUTR,APYES       DEFAULT, BILLED TRANSACTIONS                 
         MVC   JOBCLSE,APYES       DEFAULT, CLOSED JOBS                         
*&&US*&& MVC   JOBEXPS,APNO        DEFAULT, NO EXPENSE JOBS                     
         MVC   JOBRVRS,APYES       DEFAULT, INLCUDE REVERSALS                   
*&&US*&& MVC   JOBIUTR,APYES       DEFAULT, BILL TRANSACTION                    
         MVI   JOBESTS,C' '                                                     
*&&US*&& MVI   JOBTIME,C'B'        DEFAULT, TYPE OF TIME                        
*&&UK                                                                           
         MVC   JOBTIME,=C'B,N,R'   DEFAULT, TYPE OF TIME                        
         CLI   CUCTRY,CTRYGER                                                   
         BNE   *+10                                                             
         MVC   JOBTIME,=C'B#N#R'                                                
         MVC   JOBAUTH(L'AC@BOTH),AC@BOTH     DEFAULT, INCLUDE BOTH             
         MVC   JOBVATR,SPACES      DEFAULT, SHOW ALL                            
*&&                                                                             
         USING RFLELD,R1                                                        
         MVI   APELCODE,RFLELQ     FILTER/LIST ELEMENT X'C5'                    
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC50                                                         
*                                                                               
DISREC10 CLI   RFLSEQ,0            HIGH LEVEL FILTER ONLY                       
         BNE   DISREC40                                                         
         SR    RF,RF                                                            
         SR    R6,R6                                                            
         IC    R6,RFLLN                                                         
         SH    R6,=Y(RFLLNQ+1)                                                  
         BM    DISREC40            SKIP ELEMENT IF LENGTH TOO SMALL             
*                                                                               
*&&UK                                                                           
         CLI   RFLTYPE,RFLAUTH                                                  
         BNE   DISREC20                                                         
         CLI   RFLDATA,C' '        NO INPUT                                     
         BNH   DISREC40                                                         
         MVC   JOBAUTH(L'AC@ATHED),AC@ATHED                                     
         CLI   RFLDATA,AUTHQ       AUTH STATUS?                                 
         BE    *+10                                                             
         MVC   JOBAUTH(L'AC@UATH),AC@UATH                                       
         OI    JOBAUTHH+6,FVOXMT   TURN    TRANSMIT  ON                         
         B     DISREC40                                                         
*&&                                                                             
DISREC20 CLI   RFLTYPE,RFLACC      ACCOUNT FILTER LIST                          
         BNE   *+8                                                              
         LA    RF,JOBACCTH                                                      
*                                                                               
*        CLI   RFLTYPE,RFLLDG      LEDGER FILTER LIST                           
*        BNE   *+8                                                              
*        LA    RF,JOBTYPEH                                                      
*                                                                               
         CLI   RFLTYPE,RFLCNTR     CONTRA FILTER LIST                           
         BNE   *+8                                                              
         LA    RF,JOBCNTRH                                                      
*                                                                               
         CLI   RFLTYPE,RFLBLGP     BILLING GROUP                                
         BNE   *+8                                                              
         LA    RF,JOBBGH                                                        
*                                                                               
         CLI   RFLTYPE,RFLOFGP     OFFICE GROUP                                 
         BNE   *+8                                                              
         LA    RF,JOBOFGH                                                       
*                                                                               
         CLI   RFLTYPE,RFLOFF      OFFICE FILTER LIST                           
         BNE   *+8                                                              
         LA    RF,JOBOFFH                                                       
*                                                                               
         CLI   RFLTYPE,RFLMDGP     MEDIA GROUP                                  
         BNE   *+8                                                              
         LA    RF,JOBMGH                                                        
*                                                                               
         CLI   RFLTYPE,RFLMED      MEDIA                                        
         BNE   *+8                                                              
         LA    RF,JOBMEDH                                                       
*                                                                               
         CLI   RFLTYPE,RFLWCGP     WORK CODE GROUP                              
         BNE   *+8                                                              
         LA    RF,JOBWCGH                                                       
*                                                                               
         CLI   RFLTYPE,RFLWC       WORK CODE                                    
         BNE   *+8                                                              
         LA    RF,JOBWCH                                                        
*                                                                               
         CLI   RFLTYPE,RFLBTYP     BILL TYPE                                    
         BNE   *+8                                                              
         LA    RF,JOBBTYH                                                       
*                                                                               
*&&US                                                                           
         CLI   RFLTYPE,RFLSTTY     STUDIO TYPE                                  
         BNE   *+8                                                              
         LA    RF,JOBSTTYH                                                      
*&&                                                                             
*                                                                               
         CLI   RFLTYPE,RFLUFLD     USER FIELD                                   
         BNE   *+8                                                              
         LA    RF,JOBUFLDH                                                      
*                                                                               
         CLI   RFLTYPE,RFLTTIME    TYPE OF TIME                                 
*&&US*&& BNE   *+8                                                              
*&&UK*&& BNE   *+14                                                             
         LA    RF,JOBTIMEH                                                      
*&&UK*&& XC    JOBTIME,JOBTIME     CLEAR DEFAULT VALUES                         
*&&UK                                                                           
         CLI   RFLTYPE,RFLVATRG    VAT REGION                                   
         BNE   *+8                                                              
         LA    RF,JOBVATRH                                                      
*&&                                                                             
         CLI   RFLTYPE,RFLTTYPE    TRANSACTION TYPE                             
         BNE   DISREC30                                                         
         LR    R0,R1                                                            
         GOTO1 CNVTTYPE,APPARM,(C'S',(R0)),(X'00',JOBITTY)                      
         LR    R1,R0                                                            
         OI    JOBITTYH+6,FVOXMT                                                
         SR    RF,RF                                                            
*                                                                               
DISREC30 DS    0H                                                               
         LTR   RF,RF               ANY     DATA TO   DISPLAY   ?                
         BZ    DISREC40            NO,     SKIP                                 
         OI    6(RF),FVOXMT        TURN    TRANSMIT  ON                         
         TM    RFLIND,RFLXCLD      EXCLUDE BIT  ON   ?                          
         BZ    DISREC35            NO,     MOVE THE  DATA                       
         MVI   8(RF),C'*'          OUTPUT  C'*'                                 
         LA    RF,1(,RF)           BUMP    DATA LOCATION                        
*                                                                               
DISREC35 DS    0H                                                               
         EXMVC R6,8(RF),RFLDATA    INSERT  THE  DATA                            
*                                                                               
DISREC40 DS    0H                                                               
         GOTO1 NEXTEL                                                           
         BE    DISREC10                                                         
*                                                                               
         USING RPFELD,R9                                                        
DISREC50 DS    0H                                                               
         MVI   APELCODE,RPFELQ                                                  
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC99                                                         
         LR    R9,R1                                                            
*                                                                               
         MVC   JOBESTS,RPFESTST    ESTIMATE STATUS                              
         OI    JOBESTSH+6,FVOXMT                                                
*                                                                               
         TM    RPFROPT,RPFBALCR    TYPE OF BALANCE                              
         BZ    *+8                                                              
         MVI   JOBTBAL,C'C'        SHOW CREDIT (NEGATIVE) BALANCES ONLY         
         TM    RPFROPT,RPFBALDR                                                 
         BZ    *+8                                                              
         MVI   JOBTBAL,C'D'        SHOW DEBIT (POSITIVE) BALANCES ONLY          
         TM    RPFROPT,RPFBALO                                                  
         BZ    *+8                                                              
         MVI   JOBTBAL,C'S'        SHOW + OR - BALANCES ONLY                    
         TM    RPFROPT,RPFBALZR                                                 
         BZ    *+8                                                              
         MVI   JOBTBAL,C'Z'        SHOW ZERO BALANCES ONLY                      
         OI    JOBTBALH+6,FVOXMT                                                
*                                                                               
         CLI   RPFBLTRN,C' '       INCLUDE UTILIZED TRANSACTIONS                
         BNH   *+10                                                             
         MVC   JOBIUTR,RPFBLTRN    BILL TRANSACTION                             
         OI    JOBIUTRH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT4,RPFXCLSE                                                 
         BZ    *+10                                                             
         MVC   JOBCLSE,APNO        EXCLUDE CLOSED JOBS                          
         TM    RPFOPT4,RPFOCLSE                                                 
         BZ    *+10                                                             
         MVC   JOBCLSE,APONLY      CLOSED JOBS ONLY                             
         OI    JOBCLSEH+6,FVOXMT                                                
*                                                                               
*&&US                                                                           
         TM    RPFOPT4,RPFIEXPS                                                 
         BZ    *+10                                                             
         MVC   JOBEXPS,APYES       INCLUDE EXPENSE JOBS                         
         TM    RPFOPT4,RPFOEXPS                                                 
         BZ    *+10                                                             
         MVC   JOBEXPS,APONLY      EXPENSE JOBS ONLY                            
         OI    JOBEXPSH+6,FVOXMT                                                
*&&                                                                             
*                                                                               
         TM    RPFROPT,RPFXRVRS                                                 
         BZ    *+10                                                             
         MVC   JOBRVRS,APNO        EXCLUDE REVERSALS                            
         TM    RPFROPT,RPFORVRS                                                 
         BZ    *+10                                                             
         MVC   JOBRVRS,APONLY      REVERSALS ONLY                               
         OI    JOBRVRSH+6,FVOXMT                                                
*                                                                               
         LA    R4,5                FIVE FILTER TYPES                            
         LA    R3,RPFFLT1                                                       
         LA    R1,JOBF1H                                                        
DISREC52 CLI   0(R3),0             ANY CHARACTER ?                              
         BE    DISREC54                                                         
         OI    6(R1),FVOXMT        TRANSMIT                                     
         MVC   8(1,R1),0(R3)                                                    
         TM    0(R3),X'40'         LOWER OR UPPER CASE?                         
         BO    DISREC54            UPPER                                        
         MVI   8(R1),C'*'          EXCLUDE FILTER                               
         MVC   9(1,R1),0(R3)                                                    
         OI    9(R1),X'40'         MAKE UPPER CASE                              
*                                                                               
DISREC54 SR    RE,RE                                                            
         IC    RE,0(,R1)           BUMP TO PROTECTED FIELD                      
         AR    R1,RE                                                            
         IC    RE,0(,R1)           BUMP TO NEXT INPUT FILTER                    
         AR    R1,RE                                                            
         LA    R3,1(,R3)           BUMP TO NEXT FILTER IN PROFILE ELEM          
         CLM   R4,1,=AL1(2)        F1-F4 DONE YET?                              
         BNE   *+8                 NOT YET                                      
         LA    R3,RPFFLT5          SWITCH TO F5                                 
         BCT   R4,DISREC52                                                      
*                                                                               
DISREC99 DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   DISRC999                                                         
         CLI   APACTN,ACTDIS       IN   DISPLAY MODE ?                          
         BE    DISRC999            YES, SKIP                                    
         CLI   TWALREC,RECJOBPF    IF   FIRST TIME IN, THEN SET CURSOR          
         BE    DISRC999            NO,  SKIP                                    
*                                                                               
         LA    RE,JOBCDEH          FORMAT CODE FIELD                            
         ST    RE,APCURSOR         SET  APPLICATION CURSOR                      
*                                                                               
DISRC999 DS    0H                                                               
         B     EXIT                                                             
         DROP  R9                                                               
         EJECT ,                                                                
**********************************************************************          
*  DR2 - DISPLAY REC FOR 2ND SCREEN                                  *          
**********************************************************************          
         SPACE 1                                                                
DR2      DS    0H                                                               
         TWAXC JB2NMEH,JB2TABH                                                  
         GOTO1 GETNAME,APPARM,(R2),JB2NMEH                                      
         GOTO1 GETPER,APPARM,(R2),JB2OWNH                                       
         MVC   JB2LOCK,APYES       DEFAULT, SHOW LOCKED ACCOUNTS                
         MVC   JB2DRFT,APNO        DEFAULT, EXCLUDE DRAFT ITEMS                 
         MVC   JB2HELD,APYES       DEFAULT, INCLUDE HELD ITEMS                  
         MVC   JB2CWOT,APYES       DEFAULT, INCLUDE CBIL W/O'S & TRANS          
         MVC   JB2ESTO,APYES       DEFAULT, INCLUDE ALL JOBS                    
         MVC   JB2PORD,APYES       DEFAULT, INCLUDE PO'S                        
         MVC   JB2MGPO,APNO        DEFAULT, MERGE PO WORKCODES                  
         MVC   JB2WC99,APNO        DEFAULT, WC99 W/GRP OR TYP FILTERS           
*&&US*&& MVC   JB2AUTH,APYES       DEFAULT, INCLUDE ALL JOBS                    
*&&US*&& MVC   JB2CFLW,APNO        DEFAULT, Only show (No)                      
*&&US*&& MVC   JB2IPB,APNO         DEFAULT, Don't include prior bills           
*&&US*&& MVC   JB2CFDC,APNO        DEFAULT, Don't use Due Date                  
*&&US*&& MVC   JB2BYWC,APNO        DEFAULT, %Est by workcode                    
*&&UK*&& MVC   JB2JLES,APYES       DEFAULT, Include all jobs Estimates          
*&&UK*&& MVC   JB2JLOR,APYES       DEFAULT, Include all jobs Orders             
*&&UK*&& MVC   JB2JLBI,APYES       DEFAULT, Include all jobs Billing            
*&&UK*&& MVC   JB2JLTI,APYES       DEFAULT, Include all jobs Time               
*&&UK*&& MVC   JB2JLAD,APYES       DEFAULT, Include all jobs Adjustment         
*&&UK*&& MVC   JB2JLEX,APYES       DEFAULT, Include all jobs 3rd Party          
*&&UK*&& MVC   JB2SUBJ,APYES       DEFAULT, Include all sub-jobs                
         MVC   JB2DJOB,APNO        DEFAULT, Exclude draft jobs                  
*&&US*&& MVC   JB2ZTRN,APNO        DEFAULT, EXCLUDE $0 TRANSACTIONS             
*&&UK                                                                           
         MVC   JB2BEST,=C'P,B,I,S,A,R,D,M'  DEFAULT, Include all ests           
         CLI   CUCTRY,CTRYGER                                                   
         BNE   *+10                                                             
         MVC   JB2BEST,=C'P#B#I#S#A#R#D#M'  DEFAULT, Include all ests           
*&&                                                                             
*                                                                               
         USING RFLELD,R1                                                        
         MVI   APELCODE,RFLELQ     FILTER/LIST ELEMENT X'C5'                    
         GOTO1 GETEL,(R2)                                                       
         BNE   DR2050                                                           
*                                                                               
DR2010   DS    0H                                                               
         CLI   RFLSEQ,0            HIGH LEVEL FILTER?                           
         BNE   DR2020                                                           
         SR    R6,R6                                                            
         IC    R6,RFLLN                                                         
         SHI   R6,RFLLNQ+1                                                      
         BM    DR2020              SKIP ELEMENT IF LENGTH TOO SMALL             
*                                                                               
         LA    RF,JB2XCPTH                                                      
         CLI   RFLTYPE,RFLXCPT     EXCEPTION REASON                             
         BE    DR2019                                                           
*&&UK                                                                           
         LA    RF,JB2BESTH                                                      
         CLI   RFLTYPE,RFLBESTA    BRANDOCEAN ESTIMATE STATUS                   
         BNE   DR2015                                                           
         XC    JB2BEST,JB2BEST                                                  
         B     DR2019                                                           
                                                                                
DR2015   DS    0H                                                               
*&&                                                                             
*&&US                                                                           
         LA    RF,JB2TWTPH                                                      
         CLI   RFLTYPE,RFLTWTP     TIME WORKCODE TYPES                          
         BE    DR2019                                                           
         LA    RF,JB2OPTPH                                                      
         CLI   RFLTYPE,RFLOPTP     OUT OF POCKET WORKCODE TYPES                 
         BE    DR2019                                                           
         LA    RF,JB2PRLVH                                                      
         CLI   RFLTYPE,RFLPRCLV    PRICE LEVEL                                  
         BE    DR2019                                                           
*&&                                                                             
         B     DR2020                                                           
*                                                                               
DR2019   DS    0H                                                               
         OI    6(RF),FVOXMT        TRANSMIT                                     
         TM    RFLIND,RFLXCLD      EXCLUDE ?                                    
         BZ    *+12                                                             
         MVI   8(RF),C'*'                                                       
         LA    RF,1(,RF)                                                        
         EXMVC R6,8(RF),RFLDATA    INSERT THE DATA                              
*                                                                               
DR2020   GOTO1 NEXTEL                                                           
         BE    DR2010                                                           
         DROP  R1                                                               
*                                                                               
         USING RPFELD,R9                                                        
DR2050   MVI   APELCODE,RPFELQ     GET R.L. PROFILE DATA                        
         GOTO1 GETEL,(R2)                                                       
         BNE   DR2X                                                             
         LR    R9,R1                                                            
*                                                                               
         TM    RPFROPT,RPFXLOCK                                                 
         BZ    *+10                                                             
         MVC   JB2LOCK,APNO         EXCLUDE LOCKED ACCOUNTS                     
         TM    RPFROPT,RPFOLOCK                                                 
         BZ    *+10                                                             
         MVC   JB2LOCK,APONLY       LOCKED ACCOUNTS ONLY                        
         OI    JB2LOCKH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT1,RPFIDRFT                                                 
         BZ    *+10                                                             
         MVC   JB2DRFT,APYES        INCLUDE DRAFT ITEMS                         
         TM    RPFOPT1,RPFODRFT                                                 
         BZ    *+10                                                             
         MVC   JB2DRFT,APONLY      DRAFT ITEMS ONLY                             
         OI    JB2DRFTH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT1,RPFXHELD                                                 
         BZ    *+10                                                             
         MVC   JB2HELD,APNO        EXCLUDE HELD ITEMS                           
         TM    RPFOPT1,RPFOHELD                                                 
         BZ    *+10                                                             
         MVC   JB2HELD,APONLY      HELD ITEMS ONLY                              
         OI    JB2HELDH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT4,RPFXCWOT                                                 
         BZ    *+10                                                             
         MVC   JB2CWOT,APNO        EXCLUDE CBIL W/O'S & TRANSFERS               
         TM    RPFOPT4,RPFOCWOT                                                 
         BZ    *+10                                                             
         MVC   JB2CWOT,APONLY      CBIL W/O'S & TRANSFERS ONLY                  
         OI    JB2CWOTH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT4,RPFXESTO                                                 
         BZ    *+10                                                             
         MVC   JB2ESTO,APNO        EXCLUDE JOB WITH ESTIMATES                   
         TM    RPFOPT4,RPFOESTO                                                 
         BZ    *+10                                                             
         MVC   JB2ESTO,APONLY      ONLY JOB WITH ESTIMATES                      
         OI    JB2ESTOH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT5,RPFXPORD                                                 
         BZ    *+10                                                             
         MVC   JB2PORD,APNO        EXCLUDE PURCHASE ORDERS                      
         TM    RPFOPT5,RPFOPORD                                                 
         BZ    *+10                                                             
         MVC   JB2PORD,APONLY      PURCHASE ORDERS ONLY                         
         OI    JB2PORDH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT5,RPFMRGPO                                                 
         BZ    *+10                                                             
         MVC   JB2MGPO,APYES       MERGE PURCHASE ORDERS WORKCODES              
         OI    JB2MGPOH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT5,RPFIWC99                                                 
         BZ    *+10                                                             
         MVC   JB2WC99,APYES       INCLUDE WC99 W/GRP OR TYP FILTERS            
         OI    JB2WC99H+6,FVOXMT                                                
*&&US                                                                           
***********************************************************************         
*        Include inactive aurhorizations                              *         
****************************************8******************************         
         TM    RPFOPT6,RPFAUTHA                                                 
         BZ    *+8                                                              
         MVI   JB2AUTH,C'A'        ACTIVE AUTHORIZATION ONLY                    
         TM    RPFOPT6,RPFAUTHI                                                 
         BZ    *+8                                                              
         MVI   JB2AUTH,C'I'        INACTIVE AUTHORIZATION ONLY                  
         TM    RPFOPT6,RPFAUTHA+RPFAUTHI                                        
         BNO   *+8                                                              
         MVI   JB2AUTH,C'B'        BOTH INACTIVE AND ACTIVE                     
         TM    RPFOPT6,RPFAUTHE                                                 
         BZ    *+10                                                             
         MVC   JB2AUTH,APNO                                                     
*=====================================================================*         
*        Only Show External Vendor Invoices                           *         
*=====================================================================*         
         TM    RPFOPT6,RPFOEXIV                                                 
         BZ    *+10                                                             
         MVC   JB2CFLW,APYES       Only show external invoices                  
         OI    JB2CFLWH+6,FVOXMT                                                
*=====================================================================*         
*        Use Due Date as Days Calculation Date                        *         
*=====================================================================*         
         TM    RPFOPT6,RPFBDDC                                                  
         BZ    *+10                                                             
         MVC   JB2CFDC,APYES       Use Bill Due Date as Days Calc Date          
         OI    JB2CFDCH+6,FVOXMT                                                
*=====================================================================*         
*        Include previous bills                                       *         
*=====================================================================*         
         TM    RPFOPT6,RPFSPBIL                                                 
         BZ    *+10                                                             
         MVC   JB2IPB,APYES        Yes include previous bills                   
         OI    JB2IPBH+6,FVOXMT                                                 
*=====================================================================*         
*        Billing by workcode for % of Est. bill                       *         
*=====================================================================*         
         TM    RPFOPT6,RPFBYWC                                                  
         BZ    *+10                                                             
         MVC   JB2BYWC,APYES       Yes report by workcode                       
         OI    JB2BYWCH+6,FVOXMT                                                
*&&                                                                             
         TM    RPFOPT7,RPFIDJBS                                                 
         BZ    *+10                                                             
         MVC   JB2DJOB,APYES        INCLUDE DRAFT JOBS                          
         TM    RPFOPT7,RPFODJBS                                                 
         BZ    *+10                                                             
         MVC   JB2DJOB,APONLY      DRAFT JOBS ONLY                              
         OI    JB2DJOBH+6,FVOXMT                                                
*&&US                                                                           
         TM    RPFOPT6,RPFZPST                                                  
         BZ    *+10                                                             
         MVC   JB2ZTRN,APYES        INCLUDE $0 TRANSACTIONS                     
*&&                                                                             
         USING AGEMTHDD,R3         AGEING    METHODS                            
*&&US*&& LA    R3,AGEMTHTB         ->   AGEING    METHOD    TABLE               
*&&UK*&& L     R3,AAGEMTHT         ->   AGEING    METHOD    TABLE               
DR2055   DS    0H                  FIND AGEING    METHOD                        
         LR    R1,R3                                                            
         BAS   RE,EXPAGEMT         EXPAND    AGEING    METHOD                   
         CLI   RPFAGMTH,0          ANY  METHOD ?                                
         BE    DR2060              NO,  USE  DEFAULT   METHOD                   
         CLI   RPFAGMTH,C' '       ANY  METHOD ?                                
         BE    DR2060              NO,  USE  DEFAULT   METHOD                   
         CLC   RPFAGMTH,AGEMETHC   FOUND     AGEING    METHOD ?                 
         BE    DR2060              YES, USE  AGEING    METHOD                   
         LA    R3,AGEMTHLQ(,R3)    NEXT AGEING    METHOD                        
         CLI   0(RE),EOT           END  OF   TABLE ?                            
         BNE   DR2055              NO,  LOOP                                    
*&&US*&& LA    R3,AGEMTHTB         USE  DEFAULT   AGEING    METHOD              
*&&UK*&& L     R3,AAGEMTHT         USE  DEFAULT   AGEING    METHOD              
         MVI   RPFAGMTH,0          SAY  NO   AGEING    METHOD                   
         B     DR2055              EXPAND    DEFAULT   METHOD                   
*                                                                               
DR2060   DS    0H                  FOUND     AGEING    METHOD                   
         MVC   JB2AGEM,AGEMETHN    SHOW AGEING    METHOD                        
         DROP  R3                                                               
*                                                                               
*&&UK                                                                           
*=====================================================================*         
*        Jobs locked from applications                                *         
*=====================================================================*         
         CLI   RPFLN,RPFLN2Q                                                    
         BL    DR2X                                                             
         TM    RPFOPT9,RPFXJLES                                                 
         BZ    *+10                                                             
         MVC   JB2JLES,APNO        EXCLUDE JOBS LOCKED ESTIMATES                
         TM    RPFOPT8,RPFOJLES                                                 
         BZ    *+10                                                             
         MVC   JB2JLES,APONLY      ONLY JOBS LOCKED FROM ESTIMATES              
         OI    JB2JLESH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT9,RPFXJLOR                                                 
         BZ    *+10                                                             
         MVC   JB2JLOR,APNO        EXCLUDE JOBS LOCKED ORDERS                   
         TM    RPFOPT8,RPFOJLOR                                                 
         BZ    *+10                                                             
         MVC   JB2JLOR,APONLY      ONLY JOBS LOCKED FROM ORDERS                 
         OI    JB2JLORH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT9,RPFXJLBI                                                 
         BZ    *+10                                                             
         MVC   JB2JLBI,APNO        EXCLUDE JOBS LOCKED BILLING                  
         TM    RPFOPT8,RPFOJLBI                                                 
         BZ    *+10                                                             
         MVC   JB2JLBI,APONLY      ONLY JOBS LOCKED FROM BILLING                
         OI    JB2JLBIH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT9,RPFXJLTI                                                 
         BZ    *+10                                                             
         MVC   JB2JLTI,APNO        EXCLUDE JOBS LOCKED TIME                     
         TM    RPFOPT8,RPFOJLTI                                                 
         BZ    *+10                                                             
         MVC   JB2JLTI,APONLY      ONLY JOBS LOCKED FROM TIME                   
         OI    JB2JLTIH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT9,RPFXJLAD                                                 
         BZ    *+10                                                             
         MVC   JB2JLAD,APNO        EXCLUDE JOBS LOCKED ADJUSTMENTS              
         TM    RPFOPT8,RPFOJLAD                                                 
         BZ    *+10                                                             
         MVC   JB2JLAD,APONLY      ONLY JOBS LOCKED FROM ADJUSTMENTS            
         OI    JB2JLADH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT9,RPFXJLEX                                                 
         BZ    *+10                                                             
         MVC   JB2JLEX,APNO        EXCLUDE JOBS LOCKED 3rd PARTY                
         TM    RPFOPT8,RPFOJLEX                                                 
         BZ    *+10                                                             
         MVC   JB2JLEX,APONLY      ONLY JOBS LOCKED FROM 3rd PARTY              
         OI    JB2JLEXH+6,FVOXMT                                                
*                                                                               
         TM    RPFOPT9,RPFXSUBJ                                                 
         BZ    *+10                                                             
         MVC   JB2SUBJ,APNO        EXCLUDE SUB-JOBS                             
         TM    RPFOPT8,RPFOSUBJ                                                 
         BZ    *+10                                                             
         MVC   JB2SUBJ,APONLY      SUB-JOB ONLY                                 
         OI    JB2SUBJH+6,FVOXMT                                                
*&&                                                                             
DR2X     DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   DR2XX                                                            
         CLI   APACTN,ACTDIS       IN   DISPLAY MODE ?                          
         BE    DR2XX               YES, SKIP                                    
         CLI   TWALREC,RECJBPF2    IF   FIRST TIME IN, THEN SET CURSOR          
         BE    DR2XX               NO,  SKIP                                    
*                                                                               
         LA    RE,JB2CDEH          FORMAT CODE FIELD                            
         ST    RE,APCURSOR         SET  APPLICATION CURSOR                      
*                                                                               
DR2XX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R9                                                               
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO EXPAND REPORT TYPE                                      *         
*        ON ENTRY - R1=A(PARAMETER ENTRY)                             *         
*        ON EXIT  - JOBRPCDE CONTAINS THE REPORT TYPE CODE            *         
*                 - JOBRPTYP CONTAINS THE REPORT TYPE NAME            *         
***********************************************************************         
         SPACE 1                                                                
         USING REPTABD,R1                                                       
EXPRPTY  MVC   JOBRPCDE,REPCODE                                                 
         MVC   JOBRPTYP,REPSTYPE                                                
         CLI   JOBRPCDE,ESCHIGHQ   TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
         DROP  R1                                                               
*                                                                               
         STM   RE,R1,SVREGS                                                     
         GOTO1 VDICTAT,APPARM,C'TU  ',('JOBRPLNQ',JOBRPCDE),0                   
         LM    RE,R1,SVREGS                                                     
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO EXPAND AGEING METHOD                                    *         
*        ON ENTRY - R1=A(PARAMETER ENTRY)                             *         
*        ON EXIT  - AGEMETHC CONTAINS THE AGEING METHOD CODE          *         
*                 - AGEMETHN CONTAINS THE AGEING METHOD NAME          *         
***********************************************************************         
         SPACE 1                                                                
         USING AGEMTHDD,R1                                                      
         SPACE 1                                                                
EXPAGEMT MVC   AGEMETHC,AGEMTHC                                                 
         MVC   AGEMETHN,AGEMTHN                                                 
         CLI   AGEMETHN,ESCHIGHQ   TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
         DROP  R1                                                               
*                                                                               
         STM   RE,R1,SVREGS                                                     
         GOTO1 VDICTAT,APPARM,C'TU  ',(L'AGEMETHN,AGEMETHN),0                   
         LM    RE,R1,SVREGS                                                     
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  ERRORS TO DISPLAY ON TOP OF SCREEN                                 *         
***********************************************************************         
         SPACE 1                                                                
IVALBTYP MVC   FVMSGNO,=AL2(ACEBTYPE)                                           
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALTYPE MVC   FVMSGNO,=AL2(ACERPTYP)                                           
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALRPTY MVC   FVMSGNO,=AL2(ACERPTYP)     INVALID REPORT TYPE                   
         MVC   FVXTRA(3),FFNUMBER-FFNELD+1(R1)                                  
         LA    R1,SCRTYPH                                                       
         ST    R1,FVADDR                                                        
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEKEY MVC   FVMSGNO,=AL2(FVFEKEY)      ENTER KEY                             
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEDUP MVC   FVMSGNO,=AL2(ACEDUPR)      DUPLICATE PARAMETER                   
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALCNTR MVC   FVMSGNO,=AL2(ACEICNTR)     INVALID CONTRA ACCOUNT                
         MVC   FVXTRA(20),12(R4)                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVAL2MNY MVC   FVMSGNO,=AL2(ACE2MANY)     TOO MANY PARAMETERS                   
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALACCT MVC   FVMSGNO,=AL2(ACEACTLV)     INVALID ACCOUNT OR LEVEL              
         MVC   FVXTRA(20),12(R4)                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALIPUT MVC   FVMSGNO,=AL2(FVFNOTV)      INVALID INPUT                         
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALMISS MVC   FVMSGNO,=AL2(FVFMISS)      MISSING                               
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALWCDE MVC   FVMSGNO,=AL2(19)           INVALID WORKCODE                      
         MVC   FVXTRA(20),12(R4)                                                
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  TABLES                                                             *         
***********************************************************************         
         SPACE 1                                                                
BILLTY   DC    C'ACEPSTU1',AL1(EOT)                                             
         SPACE 4                                                                
IUTRLST  DC    C'YNOPAUDB'                                                      
         DC    X'FF'                                                            
*&&UK                                                                           
IUTRLSTG DC    C'JNOPAUDB'                                                      
         DC    X'FF'                                                            
*&&                                                                             
         SPACE 4                                                                
SCRTXT   DS    0F                                                               
         DC    AL4(JOB001H-TWAD,1652)    ACCOUNTS                               
         DC    AL4(JOB002H-TWAD,1671)    CONTRAS                                
         DC    AL4(JOB012H-TWAD,5658)    ESTIMATE STATUS                        
         DC    AL4(JOB015H-TWAD,5659)    TYPE     OF   TIME                     
         DC    AL4(JOB016H-TWAD,5660)    TYPE     OF   BALANCE                  
*&&UK*&& DC    AL4(JOB017H-TWAD,5683)    AUTHORIZATION STATUS                   
*&&US*&& DC    AL4(JOB018H-TWAD,5661)    INCLUDE  UTILIZED TRANSACTIONS         
*&&UK*&& DC    AL4(JOB018H-TWAD,5681)    INCLUDE  BILLED CHARGES                
         DC    AL4(JOB019H-TWAD,5662)    INCLUDE  CLOSED   JOBS                 
*&&US*&& DC    AL4(JOB020H-TWAD,5663)    INCLUDE  EXPENSE  JOBS                 
         DC    AL4(JOB021H-TWAD,1647)    INCLUDE  REVERSALS                     
*&&UK*&& DC    AL4(JOB022H-TWAD,5691)    VAT REGION                             
         DC    X'FF'                                                            
         SPACE 4                                                                
SCRTXT2  DS    0F                                                               
         DC    AL4(JB2001H-TWAD,1646)    INCLUDE LOCKED ACCOUNTS                
         DC    AL4(JB2002H-TWAD,1649)    INCLUDE DRAFT TRANSACTIONS             
         DC    AL4(JB2003H-TWAD,5647)    INCLUDE HELD TRANSACTIONS              
         DC    AL4(JB2004H-TWAD,5648)    INCLUDE CBIL W/OS & TRANSFERS          
         DC    AL4(JB2005H-TWAD,5649)    INCLUDE ESTIMATES                      
         DC    AL4(JB2006H-TWAD,5650)    INCLUDE PUCHASE ORDERS                 
         DC    AL4(JB2007H-TWAD,5651)    MERGE PURCHASE ORDERS                  
*&&US*&& DC    AL4(JB2008H-TWAD,5677)    INCLUDE AUTHORIZATION STATUS           
         DC    AL4(JB2009H-TWAD,5655)    AGEING METHOD                          
*        DC    AL4(JB2009H-TWAD,5900)    FOR FUTURE USE                         
         DC    AL4(JB2010H-TWAD,5667)    INCLUDE 99 W/WC GRP/TYP FILTS          
*&&US*&& DC    AL4(JB2011H-TWAD,5678)    Only show external - Cashflow          
*&&US*&& DC    AL4(JB2012H-TWAD,5679)    Include previous bills                 
*&&US*&& DC    AL4(JB2013H-TWAD,5680)    Use Bill Due Dte as Dys Calc           
*&&US*&& DC    AL4(JB2014H-TWAD,5681)    Billing by wc for % of Est             
*&&US*&& DC    AL4(JB2015H-TWAD,5653)    Time workcode types                    
*&&US*&& DC    AL4(JB2016H-TWAD,5654)    Out of pocket workcodes types          
         DC    AL4(JB2017H-TWAD,5652)    Exception reasons                      
*&&US*&& DC    AL4(JB2018H-TWAD,5656)    Price levels                           
         DC    AL4(JB2019H-TWAD,5682)    Include draft jobs                     
*&&US*&& DC    AL4(JB2020H-TWAD,5683)    Include 0 transactions                 
*&&UK*&& DC    AL4(JB2020H-TWAD,5684)    Include Jobs locked Estimates          
*&&UK*&& DC    AL4(JB2021H-TWAD,5685)    Include Jobs locked Orders             
*&&UK*&& DC    AL4(JB2022H-TWAD,5686)    Include Jobs locked Billing            
*&&UK*&& DC    AL4(JB2023H-TWAD,5687)    Include Jobs locked Time               
*&&UK*&& DC    AL4(JB2024H-TWAD,5688)    Include Jobs locked Adjustment         
*&&UK*&& DC    AL4(JB2025H-TWAD,5689)    Include Jobs locked 3rd party          
*&&UK*&& DC    AL4(JB2026H-TWAD,5690)    Include Sub-jobs                       
*&&UK*&& DC    AL4(JB2027H-TWAD,0058)    BrandOcean Estimate Status             
         DC    X'FF'                                                            
         SPACE 4                                                                
WCTYTAB  DC    C'TOPRM',X'FF'                                                   
         EJECT ,                                                                
         SPACE 1                                                                
CHKTABL  DS    0C                                                               
         DC    AL1(1),AL1(1,2,5),X'2C0200E2D14000' Office group                 
         DC    AL1(CHKFXLD)                                                     
*        DC    AL1(2),AL1(2,2,5),X'2C0400E2D14040' Office (OBSOLETE)            
*        DC    AL1(CHKFXLD)                                                     
         DC    AL1(3),AL1(1,2,5),X'2C0600E2D14000' Media group                  
         DC    AL1(CHKFXLD)                                                     
         DC    AL1(4),AL1(1,1,2),X'09004040404040' Media                        
         DC    AL1(CHKFXLD+CHKFSPC)                                             
         DC    AL1(5),AL1(1,2,5),X'2C0800E2D14000' Workcode group               
         DC    AL1(CHKFXLD)                                                     
         DC    AL1(6),AL1(4,2,3),X'2C230040404040' Studio type                  
         DC    AL1(CHKFXLD)                                                     
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  AGEING METHODS TABLE                                               *         
***********************************************************************         
         SPACE 1                                                                
AGEMTHTB DS    0C                                                               
*                                                                               
*                                  NOTE:    LEAVE   DEFAULT FIRST               
         DC    AL1(RPFAGDLT)       SYSTEM   DEFAULT AGEING                      
         DCDD  AC#RSAGD,L'JB2AGEM                                               
*                                                                               
*        DC    AL1(RPFAGADT)       ACTIVITY DATE    AGEING                      
*        DCDD  AC#RSAGB,L'JB2AGEM                                               
*                                                                               
*        DC    AL1(RPFAGFIF)       FIFO             AGEING                      
*        DCDD  AC#RSAGG,L'JB2AGEM                                               
*                                                                               
         DC    AL1(RPFAGOPN)       OPEN     ITEM    AGEING                      
         DCDD  AC#RSAGO,L'JB2AGEM                                               
*                                                                               
*        DC    AL1(RPFAGUBL)       UNBILLED ITEM    AGEING                      
*        DCDD  AC#RSAGU,L'JB2AGEM                                               
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*  EQUATES - INCLUDING EXCLUDE CHARACTER WHERE APPLICABLE             *         
***********************************************************************         
         SPACE 1                                                                
LENFBGRP EQU   4                   BILLING GROUP                                
LENFBILT EQU   2                   BILLING TYPE                                 
LENFUSRF EQU   3                   USER    FIELD                                
         SPACE 1                   BRANDOCEAN ESTIMATE STATUSES                 
PRGRSS   EQU   C'P'                IN PROGRESS                                  
SUBINA   EQU   C'B'                SUBMITTED TO INTERNAL APPROVER               
INTAPP   EQU   C'I'                INTERNALLY APPROVED                          
SUBCLI   EQU   C'S'                SUBMITTED TO CLIENT APPROVER                 
CLIAPP   EQU   C'A'                CLIENT APPROVED                              
REJCTD   EQU   C'R'                REJECTED                                     
DELTED   EQU   C'D'                DELETED                                      
MERGD    EQU   C'M'                MERGED                                       
         SPACE 1                                                                
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
MAXPARM  EQU   21                                                               
PENNY    EQU   C'P'                                                             
AUTHQ    EQU   C'A'                                                             
UNAUTHQ  EQU   C'U'                                                             
*                                                                               
DUB      DS    D                                                                
*                                                                               
SVREGS   DS    6A                                                               
SAVEXP   DS    F                   SAVED EXCEPTION REASONS                      
*                                                                               
WORK     DS    CL20                                                             
BYTE     DS    CL1                                                              
*                                                                               
*&&UK                                                                           
AWCTYTAB DS    A                                                                
ACHKTABL DS    A                                                                
AAGEMTHT DS    A                                                                
*&&                                                                             
JOBRPCDE DS    CL(L'REPCODE)                                                    
JOBRPTYP DS    CL(L'REPSTYPE)                                                   
JOBRPLNQ EQU   *-JOBRPCDE                                                       
*                                                                               
AGEMETHC DS    CL(L'AGEMTHC)       AGEING METHOD CODE                           
AGEMETHN DS    CL(L'AGEMTHN)       AGEING METHOD NAME                           
*                                                                               
JBEXPBLK DS    CL(JXLEN)           JOB EXCEPTION BLOCK                          
*                                                                               
WCTYLIST DS    CL10                                                             
ONEXONLY DS    XL1                 ONE TIME ONLY FLAG                           
PARM_N   DS    XL1                 CURRENT PARAMETER WORKING ON                 
SAVEESTS DS    CL1                 ESTIMATE STATUS                              
SAVEKEY1 DS    CL(L'RESKEY)                                                     
TEMPCHAR DS    CL1                                                              
TYPECDE  DS    CL4                                                              
MULTLDG  DS    AL4                                                              
*                                                                               
LDGFLDH  DS    CL8                 DUMMY FIELD HEADER                           
LDGFLD   DS    CL2                                                              
*                                                                               
DWCFLDH  DS    CL8                 DUMMY WORK CODE FIELD                        
DWCFLD   DS    CL(L'JOBWC+1)                                                    
*                                                                               
*&&US                                                                           
PRCLVLS  DS    CL((L'JB2PRLV)/2) PRICE LEVELS                                   
*&&                                                                             
*                                                                               
JOBBLOCK DS    (MAXPARM)CL32                                                    
*                                                                               
LWSX     DS    0C                                                               
         EJECT ,                                                                
         SPACE 1                                                                
CHKTABD  DSECT                                                                  
CHKTYPE  DS    AL1                 TYPE OF CHECKING                             
CHKMLN   DS    XL1                 MAXIMUM LENGTH                               
CHKCPY   DS    XL1                 COMPANY DISPLACEMENT FROM KEY                
CHKDSP   DS    XL1                 DISPLACEMENT WHERE DATA GOES                 
CHKKEY   DS    CL7                 KEY TO USE                                   
CHKFIND  DS    XL1                 FIELD INDICATORS                             
CHKFXLD  EQU   X'20'               .  Exclude supported                         
CHKFSPC  EQU   X'10'               .  Spaced filled key                         
CHKTABLN EQU   *-CHKTYPE                                                        
         EJECT ,                                                                
***********************************************************************         
*  AGEING METHODS DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
AGEMTHDD DSECT                                                                  
AGEMTHC  DS    CL1                 AGEING METHOD CODE                           
AGEMTHN  DS    CL(L'JB2AGEM)       AGEING METHOD NAME                           
AGEMTHLQ EQU   *-AGEMTHDD          ELEMENT LENGTH                               
         EJECT ,                                                                
* ACSCRWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
***********************************************************************         
*  DSECTS FOR PRODUCTION PROFILE DEFINITIONS                          *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCREED                                                       
         SPACE 3                                                                
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRECD                                                       
         PRINT ON                                                               
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094ACSCR0F   03/13/19'                                      
         END                                                                    
