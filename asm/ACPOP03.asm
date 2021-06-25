*          DATA SET ACPOP03    AT LEVEL 007 AS OF 05/21/13                      
*PHASE T60A03C                                                                  
         TITLE 'ACPOP03 - PRODUCTION ORDERS PLUS - PRINT'                       
ACPOP03  CSECT                                                                  
         PRINT NOGEN               ON ENTRY TO OVERLAY ORDER RECORD IS          
         NMOD1 0,**POP3**,RA                                                    
         USING POPWORKD,R9                                                      
         USING TWAD,R8                                                          
         L     RC,ASAVE            RC=A(LOCAL WORKING STORAGE)                  
         USING LWSD,RC                                                          
         LR    RE,RC                                                            
         LA    RF,LWSX-LWSD        CLEAR OUT WORKING STORAGE                    
         SR    R1,R1               ADD/PRINT USES STORAGE TWICE                 
         MVCL  RE,R0                                                            
*                                  INITIALIZE LOCAL W/S                         
         ST    RD,AWORK1                                                        
         MVC   AORDER,AIOAREA1     USE AIO1                                     
         CLI   APOACT,C'A'         IS THIS ADD AND PRINT?                       
         BNE   *+10                NO                                           
         MVC   AORDER,AIOAREA3     YES, USE AIO3 INSTEAD                        
         ZAP   TOTAL,=P'0'                                                      
         MVI   LASTPC,TRMINATE                                                  
         GOTO1 VGETFACT,DMCB,0                                                  
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         AP    FATIME,=P'80000'    USE NEW YORK TIME IN US                      
         UNPK  DUB,FATIME                                                       
         MVC   TIMENOW,DUB+2                                                    
         DROP  RF                                                               
         MVC   MSG,SPACES                                                       
         CLI   APONUM,C'L'         LINEUP                                       
         BNE   ACO20                                                            
                                                                                
         BAS   RE,INITPRT                                                       
         BAS   RE,LINEUP                                                        
         BAS   RE,PRTMSG                                                        
         B     EXIT                                                             
                                                                                
ACO20    MVI   ELCODE,ORDELQ       GET ORDELQ FROM AORDER                       
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                MUST HAVE IT                                 
                                                                                
         CLI   TYPE,PRODN          PRODUCTION ORDER                             
         BNE   *+8                                                              
         BAS   RE,GETMED           GET MEDIA FOR REPORT                         
                                                                                
         BAS   RE,GETPARMS         SEE IF PASSED ANY PARMS                      
         CLI   FERN,INVALID        ERROR IN PARMS                               
         BE    ERRXIT                                                           
                                                                                
         BAS   RE,VALPARMS         MAKE SURE PARMS ARE ALLOWED                  
         BNE   ERRXIT                                                           
                                                                                
         BAS   RE,SETSTAT          SEE IF PRINT OR FAX OR BOTH                  
                                                                                
         TM    STATUS,FAX_IT                                                    
         BNO   ACO40                                                            
         BAS   RE,CHKFXNUM         ENSURE I HAVE A FAX NUMBER                   
         BNE   ACO40               NO                                           
         MVI   PSTAT,FAXING                                                     
         BAS   RE,INITFAX                                                       
         BAS   RE,REPORT                                                        
         BAS   RE,FAXMSG                                                        
                                                                                
ACO40    TM    STATUS,EMAIL_IT                                                  
         BNO   ACO50                                                            
         BAS   RE,CHKEMAIL         ENSURE I HAVE A FAX NUMBER                   
         BNE   ACO50               NO                                           
         MVI   PSTAT,EMAILING                                                   
         BAS   RE,INITEML                                                       
         BAS   RE,REPORT                                                        
         BAS   RE,EMAILMSG                                                      
                                                                                
ACO50    TM    STATUS,FAX_IT+EMAIL_IT   IF NO FAX OR EMAIL THEN PRINT           
         BZ    *+12                                                             
         TM    STATUS,PRINT_IT                                                  
         BNO   ACOX                                                             
         MVI   PSTAT,PRINTING                                                   
         BAS   RE,INITPRT                                                       
         BAS   RE,REPORT           CREATE REPORT                                
         BAS   RE,PRTMSG           DO MESSAGE                                   
                                                                                
         TM    STATUS,FAX_IT       WAS THIS FAXED ALSO?                         
         BNO   *+8                 NO                                           
         BAS   RE,PRTFXMSG         DO BOTH STYLE MESSAGE                        
                                                                                
         TM    STATUS,EMAIL_IT     WAS THIS EMAILED ALSO?                       
         BNO   *+8                 NO                                           
         BAS   RE,PRTEMMSG         DO BOTH STYLE MESSAGE                        
                                                                                
ACOX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE PRINT QUEUE CALL                                  *         
***********************************************************************         
                                                                                
INITPRT  NTR1                                                                   
         LA    R7,PC                                                            
         USING PQPLD,R7                                                         
         XC    P,P                                                              
         MVI   PC,INITIAL                                                       
         MVI   QLEXTRA,X'FF'       MUST SET TO X'FF' FOR EXTRA VALUES           
         MVC   QLDESC,SPACES                                                    
         MVC   QLDESC(2),=C'PO'                                                 
         MVC   QLDESC+2(6),APONUM                                               
         MVC   QLSUBID,=C'APO'                                                  
         MVI   QLLPP,68                                                         
         MVC   QLSRCID,TWAUSRID                                                 
         MVI   QLCLASS,C'A'                                                     
         MVI   QLSTAT,ACTV                                                      
         MVI   QLSYS,C'A'                                                       
         MVC   QLPRG,=C'PO'                                                     
         CLI   APONUM,C'L'         LINEUP                                       
         BNE   INITP02                                                          
         MVC   QLSUBID,=C'LU1'                                                  
         MVC   QLDESC(11),=C'*LINEUP#01*'                                       
         MVI   QLCLASS,0                                                        
         MVI   QLSTAT,KEEP                                                      
                                                                                
INITP02  BAS   RE,PRNT                                                          
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE PRINT QUEUE CALL FOR FAX (CLASS G, *HDR* AND DDS+)*         
***********************************************************************         
                                                                                
INITFAX  NTR1                                                                   
         BAS   RE,OPENPQFX         SET PQ HEADER, OPEN PQ                       
         BAS   RE,SETFXHDR         CREATE *HDR*                                 
         BAS   RE,SETDDS           CREATE DDS+                                  
         B     EXIT                                                             
                                                                                
OPENPQFX NTR1                                                                   
         LA    R7,PC                                                            
         USING PQPLD,R7                                                         
         MVI   PC,INITIAL                                                       
         MVI   QLEXTRA,X'FF'       MUST SET TO X'FF' FOR EXTRA VALUES           
         MVC   QLDESC,SPACES                                                    
         MVC   QLDESC(2),=C'PO'                                                 
         MVC   QLDESC+2(6),APONUM                                               
         MVC   QLSUBID,=C'APO'                                                  
         MVI   QLLPP,68                                                         
         MVC   QLSRCID,TWAUSRID                                                 
         MVI   QLCLASS,C'G'                                                     
         MVI   QLSTAT,ACTV                                                      
         MVI   QLSYS,C'A'                                                       
         MVC   QLPRG,=C'PO'                                                     
         BAS   RE,PRNT                                                          
         B     EXIT                                                             
         DROP  R7                                                               
                                                                                
SETFXHDR NTR1                                                                   
         MVI   PC,EDICCC           PRINT *HDR* EDICT CONTROL CARD               
         LA    R7,P                                                             
         MVC   P,SPACES                                                         
         USING HDRD,R7                                                          
         MVC   HDR,=C'*HDR*'                                                    
         MVC   HDFX,=C'FAX '      11'/80 CHRS                                   
         MVC   HDNUM,FAXNUM                                                     
         OC    FAXPARM,FAXPARM     PASSED A FAX NUMBER                          
         BZ    *+10                                                             
         MVC   HDNUM,FAXPARM                                                    
         MVI   HDW,C' '            W I D E                                      
         MVI   HDP,C'P'            USE /PAGE FOR T-O-F                          
         MVC   HDDEST,SPACES                                                    
         LA    R1,FAXNUM           FORMAT FAX NUMBER                            
         OC    FAXPARM,FAXPARM     PASSED A FAX NUMBER                          
         BZ    *+8                                                              
         LA    R1,FAXPARM                                                       
         LA    R2,HDDEST                                                        
         BAS   RE,FORMFONE                                                      
         MVC   HDBILL,SPACES       CLEAR BILLING AREA                           
         MVC   HDBCLI,APOCLI                                                    
         MVC   HDBPRO,APOPRO                                                    
         MVC   HDBJOB,APOJOB                                                    
         BAS   RE,PRNT             SENT TO PQ                                   
         B     EXIT                                                             
         DROP  R7                                                               
                                                                                
SETDDS   NTR1                                                                   
         MVI   PC,EDICCC           PRINT ++DDS EDICT CONTROL CARD               
         LA    R7,P                                                             
         MVC   P,SPACES                                                         
         USING DDSD,R7                                                          
         MVC   DDD,=C'++DDS'                                                    
         MVC   DDSYS,=C'AC'        SYSTEM                                       
         MVC   DDREP,=C'ORD'       REPORT                                       
         MVC   DDTRN,=C'TRN'       MUST BE TRN                                  
                                                                                
         LA    R2,DDUSER                                                        
         USING ACPOPFXD,R2                                                      
         MVC   ACFXCOMP,COMPANY                                                 
         MVI   ACFXTYPE,C'E'                                                    
         MVC   ACFXEXP,LEXP                                                     
         CLI   TYPE,PRODN          PRODUCTION ORDER                             
         BNE   SETD02                                                           
         MVI   ACFXTYPE,C'P'                                                    
         MVC   ACFXUL,=C'SJ'                                                    
         MVC   ACFXCLI,APOCLI                                                   
         MVC   ACFXPRO,APOPRO                                                   
         MVC   ACFXJOB,APOJOB                                                   
                                                                                
SETD02   MVC   ACFXVEND,LSUPP+1    ASSUME OVERRIDE                              
         CLI   LSUPP,C'*'          UL OVERRIDE IN SUPPLIER                      
         BE    SETD04              YES                                          
                                                                                
         MVC   ACFXVEND(2),SUPPUL  12 BYTE SUPPLIER I/P                         
         MVC   ACFXVEND+2(12),LSUPP                                             
                                                                                
SETD04   MVC   ACFXONUM,APONUM                                                  
         BAS   RE,PRNT             SENT TO PQ                                   
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*      INITIALIZE PRINT QUEUE CALL FOR EMAIL (CLASS M, *HDR* AND DDS+)*         
***********************************************************************         
                                                                                
INITEML  NTR1                                                                   
         BAS   RE,OPENPQEM         SET PQ HEADER, OPEN PQ                       
         BAS   RE,SETEMHDR         CREATE EMAIL *HDR*                           
         BAS   RE,SETEMDDS         CREATE DDS+                                  
         BAS   RE,SETEMRCP         CREATE DDS+ RECIEVE RECORD                   
         BAS   RE,SETEMSUB         CREATE DDS+ SUBJECT RECORD                   
         B     EXIT                                                             
                                                                                
OPENPQEM NTR1                                                                   
         LA    R7,PC                                                            
         USING PQPLD,R7                                                         
         MVI   PC,INITIAL                                                       
         MVI   QLEXTRA,X'FF'       MUST SET TO X'FF' FOR EXTRA VALUES           
         MVC   QLDESC,SPACES                                                    
         MVC   QLDESC(2),=C'PO'                                                 
         MVC   QLDESC+2(6),APONUM                                               
         MVC   QLSUBID,=C'APO'                                                  
         MVI   QLLPP,68                                                         
         MVC   QLSRCID,TWAUSRID                                                 
         MVI   QLCLASS,C'G'                                                     
         MVI   QLSTAT,ACTV                                                      
         MVI   QLSYS,C'A'                                                       
         MVC   QLPRG,=C'PO'                                                     
         BAS   RE,PRNT                                                          
         B     EXIT                                                             
         DROP  R7                                                               
                                                                                
SETEMHDR NTR1                                                                   
         MVI   PC,EDICCC           PRINT *HDR* EDICT CONTROL CARD               
         LA    R7,P                                                             
         MVC   P,SPACES                                                         
         USING HDRD,R7                                                          
         MVC   HDR,=C'*HDR*'                                                    
         MVI   HDEMID,HDEMIDQ      C'M'  EMAIL IDENTIFIER                       
         BAS   RE,PRNT             SENT TO PQ                                   
         B     EXIT                                                             
         DROP  R7                                                               
                                                                                
SETEMDDS NTR1                                                                   
         MVI   PC,EDICCC           PRINT ++DDS EDICT CONTROL CARD               
         LA    R7,P                                                             
         MVC   P,SPACES                                                         
         USING DDSD,R7                                                          
         MVC   DDD,=C'++DDS'                                                    
         MVC   DDSYS,=C'AC'        SYSTEM                                       
         MVC   DDREP,=C'ORD'       REPORT                                       
         MVC   DDTRN,=C'TRN'       MUST BE TRN                                  
         BAS   RE,PRNT             SENT TO PQ                                   
         B     EXIT                                                             
         DROP  R7                                                               
                                                                                
SETEMRCP NTR1                                                                   
         MVI   PC,EDICCC           PRINT ++DDS EDICT CONTROL CARD               
         LA    R7,P                                                             
         MVC   P,SPACES                                                         
         USING DDSD,R7                                                          
         MVC   DDD,=C'++DDS'                                                    
         MVC   DDTRN,=C'RCP'                                                    
         SR    R2,R2                                                            
         LA    R1,EMLPARM          OVERRIDE EMAIL PARAMETER                     
         ICM   R2,1,EMLPARML                                                    
         BNZ   SETE02                                                           
         LA    R1,EMAILAD          EMAIL ADDRESS SET ON ACCOUNT RECORD          
         IC    R2,EMAILADL                                                      
                                                                                
SETE02   EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   DDUSER(0),0(R1)                                                  
         BAS   RE,PRNT             SENT TO PQ                                   
         B     EXIT                                                             
         DROP  R7                                                               
                                                                                
SETEMSUB NTR1                                                                   
         MVI   PC,EDICCC           PRINT ++DDS EDICT CONTROL CARD               
         LA    R7,P                                                             
         MVC   P,SPACES                                                         
         USING DDSD,R7                                                          
         MVC   DDD,=C'++DDS'                                                    
         MVC   DDTRN,=C'SUB'                                                    
         MVC   DDUSER(16),=C'EMAILED ORDER - '                                  
         MVC   DDUSER+16(L'APONUM),APONUM                                       
         BAS   RE,PRNT             SENT TO PQ                                   
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*        SET FAXPARM AND EMAIL PARM FROM VALUES PASSED IN ACTSCAN     *         
***********************************************************************         
                                                                                
GETPARMS NTR1                                                                   
         MVI   FERN,OK                                                          
         XC    FAXPARM,FAXPARM                                                  
         XC    EMLPARM,EMLPARM                                                  
         XC    EMLPARML,EMLPARML                                                
         CLI   ACTSCAN+82,0        ANYTHING IN PARM                             
         BE    GETPX               NO                                           
         CLC   ACTSCAN+94(3),=C'FAX'                                            
         BNE   GETP02                                                           
         CLI   ACTSCAN+82,3        C'FAX'                                       
         BNE   GETPER              NO                                           
         MVC   FAXPARM,ACTSCAN+104                                              
         B     GETPX                                                            
                                                                                
GETP02   CLC   ACTSCAN+94(2),=C'EM'                                             
         BNE   GETPER                                                           
         CLI   ACTSCAN+82,2               C'EM'                                 
         BNE   GETPER                      NO                                   
         MVC   EMLPARML,ACTSCAN+83        LENGTH OF EMAIL ADDRESS               
         MVC   EMLPARM,ACTSCAN+104                                              
                                                                                
GETPX    B     EXIT                                                             
                                                                                
GETPER   MVI   FERN,INVALID                                                     
         B     ERRXIT                                                           
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PARMS                                               *         
***********************************************************************         
                                                                                
VALPARMS NTR1                                                                   
         OC    FAXPARM,FAXPARM          DO I HAVE A FAXPARM?                    
         BZ    VALP04                    NO                                     
         CLI   FAXPARM,C'N'             JUST WANT THE REPORT?                   
         BE    VALPX                     OK                                     
         CLI   PRAUTOFX,C'Y'            CAN THEY USE IT?                        
         BE    VALP02                    YES, VALIDATE                          
         MVI   FERN,NOFAX                                                       
         B     ERRXIT                                                           
                                                                                
VALP02   LA    RF,9                     ASSUME LEN OF 10                        
         CLI   FAXPARM,C'1'             LEADING 1                               
         BNE   *+8                                                              
         LA    RF,10                    LEN IS 11                               
         MVC   WORK(11),=CL11'00000000000'                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),FAXPARM                                                  
         CLC   WORK(11),=CL11'00000000000'                                      
         BE    VALPX                                                            
         MVI   FERN,BADFAX                                                      
         B     ERRXIT                                                           
                                                                                
VALP04   CLI   EMLPARML,0               AN EMAIL ADDRESS GIVEN?                 
         BE    VALPX                     NO                                     
         CLI   EMLPARML,2               POSSIBLE EMAIL ADDRESS?                 
         BH    VALP06                    YES                                    
         CLI   EMLPARM,C'N'             'NO' EMAIL WANTED?                      
         BE    VALPX                     YES, EXIT                              
         MVI   FERN,BADEMAIL                                                    
         B     ERRXIT                                                           
                                                                                
VALP06   CLI   PRAUTOFX,C'E'            CAN THEY USE IT?                        
         BNE   VALP08                    YES, VALIDATE                          
         BRAS  RE,EMVAL                                                         
         BE    VALPX                                                            
                                                                                
VALP08   MVI   FERN,NOEMAIL                                                     
         B     ERRXIT                                                           
                                                                                
VALPX    B     OKXIT                                                            
         EJECT                                                                  
***********************************************************************         
*        SET STATUS BYTE BASED ON PARMS/PROFILE OPTIONS               *         
***********************************************************************         
                                                                                
SETSTAT  NTR1                                                                   
         MVI   STATUS,NOTHING                                                   
         CLI   PRAUTOFX,C'E'       CAN THEY EMAIL?                              
         BNE   SETS04              NO                                           
         CLI   EMLPARML,2          HAVE THEY DECIDED NOT TO                     
         BH    SETS02               NO                                          
         CLI   EMLPARM,C'N'        HAVE THEY DECIDED NOT TO                     
         BE    SETS04               YES                                         
                                                                                
SETS02   OI    STATUS,EMAIL_IT                                                  
         B     SETS06                                                           
                                                                                
SETS04   CLI   PRAUTOFX,C'Y'       CAN THEY FAX?                                
         BNE   SETSX               NO                                           
         CLI   FAXPARM,C'N'        HAVE THEY DECIDED NOT TO                     
         BE    SETSX               YES, JUST PRINT                              
         OI    STATUS,FAX_IT                                                    
                                                                                
SETS06   CLI   PRFXNPRT,C'Y'       WANT A HARD COPY ALSO                        
         BNE   *+8                 YES                                          
         OI    STATUS,PRINT_IT                                                  
                                                                                
SETSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        PRINT LINE-UP PATTERN                                        *         
***********************************************************************         
                                                                                
LINEUP   NTR1                                                                   
         MVI   PC,HEADOFF                                                       
         BAS   RE,PRNT             THROW TO HEAD OF FORM                        
         ZAP   LINE,=P'0'                                                       
         MVI   PC,SP2                                                           
         BAS   RE,PRNT                                                          
         BAS   RE,PRNT                                                          
         MVI   P,C'X'                                                           
         MVC   P+1(84),P                                                        
         MVC   P+44(17),=CL17' LINE UP PATTERN'                                 
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRNT             HEADS ON LINES 3 & 4                         
         MVI   P+45,C'-'                                                        
         MVC   P+46(14),P+45                                                    
         BAS   RE,PRNT                                                          
         LA    R0,5                                                             
         MVI   PC,SP2                                                           
         BAS   RE,PRNT             5 SPACE LINES                                
         BCT   R0,*-4                                                           
         MVI   PC,PR1SP1                                                        
         LA    R0,2                                                             
                                                                                
LINE02   MVI   P,C'X'                                                           
         MVC   P+1(84),P                                                        
         BAS   RE,PRNT             LINE UP PATTERN ON LINES 10 & 11             
         BCT   R0,LINE02                                                        
                                                                                
         MVI   PC,TRMINATE                                                      
         BAS   RE,PRNT                                                          
                                                                                
LINEX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        GET MEDIA DESCRIPTION INTO LMEDNAME                          *         
***********************************************************************         
                                                                                
GETMED   NTR1                                                                   
         CLC   LMEDIA,APOJOB       IS THIS THE SAME MEDIA?                      
         BE    GETMX               YES, WE HAVE DESCRIPTION IN TWA              
         MVC   LMEDIA,APOJOB                                                    
         MVC   LMEDNAME,SPACES                                                  
                                                                                
         USING PMDRECD,R6                                                       
         LA    R6,KEY              NO, READ FOR IT                              
         MVC   PMDKEY,SPACES                                                    
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,COMPANY                                                  
         MVC   PMDKMED,LMEDIA                                                   
         GOTO1 AREAD,AIOAREA2                                                   
         BNE   GETMX                                                            
                                                                                
         L     R6,AIOAREA2                                                      
         MVI   ELCODE,PMDELQ       FOUND IT, SAVE THE DESCRIPTION               
         BAS   RE,GETEL                                                         
         BNE   GETMX                                                            
                                                                                
         USING PMDELD,R6                                                        
         MVC   LMEDNAME(L'PMDDESC),PMDDESC                                      
                                                                                
GETMX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        PRINT THE REPORT                                             *         
*        PRINT NARRATIVE, WORK CODE AMOUNTS, TOTAL AND AUTHORIZATION  *         
***********************************************************************         
                                                                                
REPORT   NTR1                                                                   
         CLI   PROGPROF+9,C'P'     ARE WE SUPPOSTED TO PRINT TAX ??             
         BNE   REPO04                                                           
         MVC   P+1(19),=C'IS TAX APPLICABLE ?'                                  
         MVC   P+22(1),APOTAX                                                   
         CLI   P+22,C' '                                                        
         BE    REPO02                                                           
         CLI   P+22,C'N'                                                        
         MVC   P+22(2),=C'NO'                                                   
         BE    REPO02                                                           
         MVC   P+22(3),=C'YES'                                                  
                                                                                
REPO02   MVI   PC,PR1SP1                                                        
         BAS   RE,PRNT                                                          
         MVI   PC,SP2              1 BLANK                                      
         BAS   RE,PRNT                                                          
                                                                                
REPO04   MVI   FLAG,0                                                           
         MVI   CLEN,60                                                          
         GOTO1 PRINARR,AORDER      PRINT NARRATIVE                              
         MVI   PC,SP2              MAKE SURE HEADS ROUTINE IS EXECUTED          
         BAS   RE,PRNT                                                          
         CP    TOTAL,=P'0'         PRINT ORDER TOTAL                            
         BE    REPO06              UNLESS VALUES SUPPRESSED OR ZERO             
         MVI   PC,SP2                                                           
         BAS   RE,PRNT                                                          
         MVC   P+54(11),=C'ORDER TOTAL'                                         
         EDIT  TOTAL,(10,P+72),2                                                
         ZAP   TOTAL,=P'0'                                                      
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRNT                                                          
                                                                                
REPO06   MVI   PC,SP2              PRINT FOOTLINES                              
         BAS   RE,PRNT                                                          
         MVI   FLAG,FOOTLINE                                                    
         MVI   CLEN,60                                                          
         GOTO1 PRINARR,AORDER                                                   
                                                                                
REPO08   ZAP   DUB,MAXLINES                                                     
         SP    DUB,LINE                                                         
         SP    DUB,=P'1'                                                        
         CP    DUB,=P'0'           IF NO ROOM FOR BOTTOM LINE                   
         BH    *+12                                                             
         BAS   RE,PRNT             SET NEXT PAGE                                
         B     REPO08              AND TRY AGAIN                                
                                                                                
         CVB   R1,DUB                                                           
         MVI   PC,SP2                                                           
         BAS   RE,PRNT             SKIP TO BOTTOM OF FORM                       
         BCT   R1,*-8                                                           
         MVC   P+1(9),=C'DUE DATE:'                                             
         MVC   P+11(L'APODDTE),APODDTE                                          
         MVC   P+39(13),=C'AUTHORIZATION'                                       
         MVC   P+54(15),APOAUT                                                  
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRNT                                                          
         MVI   PC,TRMINATE                                                      
         BAS   RE,PRNT                                                          
                                                                                
REPOX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        HANDLE AMOUNT OAMAMNT (PRINT SET-UP AND TOTALLING)           *         
*        ORDER AMOUNT ELEMENT IS ADDRESSED BY R6                      *         
***********************************************************************         
                                                                                
         USING OAMELD,R6                                                        
AMOUNT   CLI   PROGPROF,C'N'       VALUES SUPPRESSED                            
         BER   RE                                                               
         CP    OAMAMNT,=P'0'       OR ZERO                                      
         BER   RE                                                               
         CLC   APOACT(2),=C'PN'    OR NO CASH WANTED THIS TIME                  
         BER   RE                                                               
         AP    TOTAL,OAMAMNT       ADD TO TOTAL AND SET IN PRINT LINE           
         CLI   PROGPROF+6,0                                                     
         BNER  RE                                                               
         EDIT  OAMAMNT,(10,P+40),2                                              
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        PRINT THE NARRATIVE                                          *         
* RECURSIVE ROUTINE TO SET UP AND PRINT NARRATIVE LINES FROM A RECORD *         
* (ASSUMES ONLY ONE LEVEL OF NESTING IN USE OF IOAREAS)               *         
*                                                                     *         
* RECORD (ORDER OR COMMENT) IS ADDRESSED BY WORD AT R1                *         
* FLAG = FOOTLINE IF FOOTLINE COMMENTS REQUIRED, IF NOT = NULL        *         
***********************************************************************         
                                                                                
PRINARR  NTR1                                                                   
         L     R6,0(R1)                                                         
         LA    R3,CHOPIN                                                        
         AHI   R6,ACCORFST                                                      
                                                                                
         USING SCMELD,R6                                                        
PRIN02   CLI   SCMEL,0                                                          
         BNE   PRIN04                                                           
         BAS   RE,ANYPRIN          NONE OR DONE                                 
         B     EXIT                                                             
                                                                                
PRIN04   CLI   SCMEL,SCMELQ                                                     
         BNE   PRIN06                                                           
         MVI   FLAG1,FOOTLINE      CHECK FOOTLINE                               
         NC    FLAG1,SCMTYPE                                                    
         CLC   FLAG1,FLAG                                                       
         BE    PRIN08                                                           
                                                                                
PRIN06   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PRIN02                                                           
                                                                                
PRIN08   TM    SCMTYPE,SCMTOMOC    SKIP ORDER MATCHING TEXT                     
         BO    PRIN06                                                           
         TM    SCMTYPE,SCMTPRAD    NESTED - READ COMMENT RECORD                 
         BNO   PRIN10              AND MAKE RECURSIVE CALL                      
         BAS   RE,ANYPRIN                                                       
         BAS   RE,PRBILINF                                                      
                                                                                
         USING SCMRECD,KEY                                                      
         XC    SCMKEY,SCMKEY                                                    
         MVI   SCMKTYP,SCMKTYPQ                                                 
         MVC   SCMKCPY,COMPANY                                                  
         MVC   SCMKCODE,SCMCODE                                                 
         GOTO1 AREAD,AIOAREA2                                                   
         BNE   PRIN10              ANY PROBLEM - TREAT AS STANDARD NARR         
         MVC   SFLAG,FLAG          SAVE/CLEAR FLAG FOR NESTED CALL              
         MVI   FLAG,0                                                           
         MVI   CLEN,72                                                          
         GOTO1 PRINARR                                                          
         MVC   FLAG,SFLAG                                                       
         B     PRIN06                                                           
                                                                                
PRIN10   CLC   SCMCODE(2),=C'S='   CHECK FOR SPACE LINE REQUESIS                
         BNE   PRIN12                                                           
         BAS   RE,ANYPRIN                                                       
         SR    R1,R1                                                            
         IC    R1,SCMLN                                                         
         SH    R1,=H'7'                                                         
         BM    PRIN12              MISSING OR NON-NUMERIC TREATED AS            
         MVC   WORK(3),=3C'0'      STANDARD                                     
         EX    R1,PRINARRM                                                      
         CLC   WORK(3),=3C'0'                                                   
         BNE   PRIN12                                                           
         EX    R1,PRINARRP                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    *+12                                                             
         CH    R1,=H'9'                                                         
         BNH   *+8                                                              
         LA    R1,1                0 OR GTR THAN 9 TREATED AS 1                 
         MVI   PC,SP2                                                           
         MVC   SAMT,P+70                                                        
         MVC   SCAT,P+54                                                        
         BAS   RE,PRNT                                                          
         BCT   R1,*-4                                                           
         MVC   P+70(12),SAMT                                                    
         MVC   P+54(15),SCAT                                                    
         B     PRIN06                                                           
PRINARRM MVZ   WORK(0),SCMCODE+2                                                
PRINARRP PACK  DUB,SCMCODE+2(0)                                                 
                                                                                
PRIN12   CLI   SCMCODE,C'#'        ALWAYS START A NEW LINE UNLESS               
         BNE   *+12                FIRST CHAR IS A '#'                          
         MVI   SCMCODE,0                                                        
         B     *+8                                                              
         BAS   RE,ANYPRIN                                                       
         SR    R1,R1                                                            
         IC    R1,SCMLN                                                         
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SCMCODE     MOVE COMMENT TO PRINT BUFFER AND             
         CLI   0(R3),C' '          IF LEADING SPACE THEN CHANGE                 
         BNE   *+8                 TO ZERO FOR INDENT.                          
         MVI   0(R3),0                                                          
         AR    R3,R1               BUMP POINTER                                 
         MVI   1(R3),C' '                                                       
         LA    R3,2(R3)                                                         
         B     PRIN06                                                           
                                                                                
ANYPRIN  LA    RF,CHOPIN           CHOP AND PRINT CONTENTS OF CHOPIN            
         CR    R3,RF               IF ANY                                       
         BER   RE                                                               
         NTR1                                                                   
         BCTR  R3,0                                                             
         LR    R0,R3                                                            
         LA    R3,CHOPIN                                                        
         SR    R0,R3                                                            
         LA    R2,CHOPOUT                                                       
         GOTO1 VCHOPPER,DMCB,(R3),(CLEN,(R2)),20,C'LEN=',(R0)                   
         L     R1,8(R1)                                                         
         MVI   PC,PR1SP1                                                        
                                                                                
ANYP02   ZIC   RF,CLEN                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P+1(0),0(R2)                                                     
         BAS   RE,PRNT                                                          
         ZIC   RF,CLEN                                                          
         LA    R2,0(RF,R2)                                                      
         BCT   R1,ANYP02                                                        
                                                                                
ANYPX    XIT1  REGS=(R3)           PASS BACK A(PRINT BUFFER)                    
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PRINT TO PRINT QUEUE                              *         
* HANDLES LINE AND PAGE CONTROL AND CALLS HEADS FOR HEADINGS          *         
* ON ENTRY PC CONTAINS PRINT COMMAND CODE AND P CONTAINS PRINT LINE   *         
* ON EXIT P IS SPACE-FILLED                                           *         
* ON ERROR CONTROL IS RETURNED DIRECT TO ROOT WITH PRINT SEQUENCE     *         
* TERMINATED                                                          *         
***********************************************************************         
                                                                                
PRNT     NTR1                                                                   
         CLI   PC,INITIAL          INITIAL                                      
         BNE   PRNT04                                                           
         ZAP   PAGE,=P'0'                                                       
         ZAP   MAXLINES,=P'55'                                                  
         CLI   PROGPROF+1,0        PROFILE FOR MAXIMUM LINES                    
         BE    PRNT02                                                           
         ZIC   RF,PROGPROF+1                                                    
         CVD   RF,DUB                                                           
         ZAP   MAXLINES,DUB                                                     
                                                                                
PRNT02   ZAP   LINE,MAXLINES       FORCE HEAD CALL NEXT TIME THROUGH            
         B     PRNT10                                                           
                                                                                
PRNT04   CLI   PC,EDICCC           PRINT EDICT CONTROL CARDS                    
         BNE   PRNT06                                                           
         MVI   PC,PR1SP1                                                        
         B     PRNT10                                                           
                                                                                
PRNT06   CLI   PC,TRMINATE        END                                           
         BE    PRNT10                                                           
         CLI   PC,HEADOFF          NO HEADS (OR DOING HEADS)                    
         BE    PRNT10                                                           
         CP    LINE,MAXLINES       NEED HEADS                                   
         BL    PRNT08              NOT YET                                      
         AP    PAGE,=P'1'          THROW TO NEW PAGE & PRINT HEADS              
         ZAP   LINE,=P'0'                                                       
         MVC   SPC(133),PC                                                      
         BAS   RE,HEADS                                                         
         MVC   PC(133),SPC                                                      
                                                                                
PRNT08   AP    LINE,=P'1'                                                       
                                                                                
PRNT10   GOTO1 VDATAMGR,DMCB,=C'DMPRINT',=C'PRTQUE',0,PC,ATIA                   
         CLI   PC,INITIAL                                                       
         BNE   PRNT12                                                           
         LA    RF,PC               SAVE REPORT ID AFTER INITIAL CALL            
         USING PQPLD,RF                                                         
         MVC   SUBID,QLSUBID                                                    
         MVC   REPNO,QLREPRNO                                                   
         DROP  RF                                                               
                                                                                
PRNT12   MVC   P,SPACES                                                         
         CLC   SUBID,=C'APO'                                                    
         BE    PRNT14                                                           
         CLC   SUBID,=C'LU1'                                                    
         BE    PRNT14                                                           
         DC    H'0'                                                             
                                                                                
PRNT14   TM    DMCB+8,X'FE'                                                     
         BNZ   PRNT16                                                           
         MVC   LASTPC,PC                                                        
         B     EXIT                                                             
                                                                                
PRNT16   CLI   LASTPC,TRMINATE     IF ERROR ENSURE PRINT SEQUENCE               
         BE    PRNT18              TERMINATED                                   
         MVI   LASTPC,TRMINATE                                                  
         MVI   PC,TRMINATE                                                      
         MVC   WORK(1),DMCB+8                                                   
         BAS   RE,PRNT                                                          
         MVC   DMCB+8(1),WORK                                                   
                                                                                
PRNT18   L     RD,AWORK1           AND EXIT TO ROOT                             
         LM    RE,RC,12(RD)                                                     
         MVI   FERN,IOERROR                                                     
                                                                                
PRNTX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        PRINT HEADINGS                                               *         
***********************************************************************         
                                                                                
HEADS    NTR1  WORK=(R5,IOBUFLN)   IN CASE YOU NEED A CT FILE READ              
         MVI   PC,HEADOFF                                                       
         XC    P,P                                                              
         BAS   RE,PRNT                                                          
         MVI   PC,SP2              HEADS ON LINE 3 & 4                          
         BAS   RE,PRNT                                                          
         BAS   RE,PRNT                                                          
         MVI   PC,PR1SP1                                                        
         CLI   PROGPROF+4,0                                                     
         BE    HEAD06              PRINT NEITHER NAME/ADDR                      
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAUSRID                                             
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R5),(R5)                    
         CLI   DMCB+8,0                                                         
         BNE   HEAD06                                                           
         LA    R4,CTIDATA                                                       
         SR    R0,R0                                                            
                                                                                
HEAD02   CLI   0(R4),X'36'         FROM 'CTGENFILE'                             
         BE    HEAD04                                                           
         CLI   0(R4),0                                                          
         BE    HEAD06                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     HEAD02                                                           
                                                                                
HEAD04   LA    R1,CHOPIN           PRINT COMPANY NAME/ADDRESS                   
         MVC   CHOPIN(110),SPACES                                               
         CLI   PROGPROF+4,1                                                     
         BE    *+14                                                             
         MVC   0(L'CTORGNAM,R1),CTORGNAM-CTORGD(R4)  NAME                       
         LA    R1,L'CTORGNAM+1(R1)                                              
         MVC   0(L'CTORGADD,R1),CTORGADD-CTORGD(R4)  ADDRESS                    
         GOTO1 VSQUASH,DMCB,CHOPIN,80                                           
         MVC   P+1(76),CHOPIN                                                   
         BAS   RE,PRNT                                                          
                                                                                
HEAD06   LA    RF,P+78                                                          
         CLI   PSTAT,PRINTING      AM I PRINTING                                
         BE    *+8                  YES, DO NOT MOVE PAGE                       
         LA    RF,P+70              NO, MOVE PAGE BACK TO FIT 80 COLS           
         MVC   0(4,RF),=C'PAGE'                                                 
         EDIT  PAGE,(1,5(RF))                                                   
         BAS   RE,PRNT                                                          
         MVC   P+58(6),APONUM                                                   
         BAS   RE,PRNT                                                          
                                                                                
HEAD08   ZIC   R1,PROGPROF+8       START LINE NO.                               
         CVD   R1,DUB                                                           
         CP    DUB,LINE                                                         
         BNH   HEAD10                                                           
         MVI   PC,SP2                                                           
         BAS   RE,PRNT                                                          
         B     HEAD08                                                           
                                                                                
HEAD10   MVI   PC,PR1SP1                                                        
                                                                                
         USING ORDELD,R6                                                        
         MVI   ELCODE,ORDELQ       GET ORDELQ FROM AORDER                       
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   PROGPROF+5,C'L'                                                  
         BNE   HEAD22                                                           
         EJECT                                                                  
* PUT SUPPLIER ON THE LEFT & CLIENT ON THE RIGHT                                
                                                                                
         MVC   P+1(3),=C'TO:'             SUPPLIER NAME - DATE                  
         MVC   P+7(L'LSUPNAME),LSUPNAME                                         
         MVC   P+39(4),=C'DATE'                                                 
         GOTO1 VDATCON,DMCB,(1,ORDDATE),(8,P+47)                                
         BAS   RE,PRNT                                                          
                                                                                
         LA    R5,SUPADDEL                SUPPLIER ADDRESS1 - CLIENT?           
         USING ADRELD,R5                                                        
         SR    R4,R4                                                            
         IC    R4,ADRNUM                  NUMBER OF ADDRESS LINES               
         LTR   R4,R4                                                            
         BZ    *+12                                                             
         MVC   P+7(L'ADRADD1),ADRADD1                                           
         BCTR  R4,0                                                             
         CLI   TYPE,PRODN                                                       
         BNE   HEAD12                                                           
         MVC   P+39(6),=C'CLIENT'                                               
         MVC   P+47(6),APOCLI                                                   
         MVC   P+54(L'LCLINAME),LCLINAME                                        
                                                                                
HEAD12   BAS   RE,PRNT                    SUPPLIER ADDRESS2 - PRODUCT?          
         LTR   R4,R4                                                            
         BZ    *+12                                                             
         MVC   P+7(L'ADRADD2),ADRADD2                                           
         BCTR  R4,0                                                             
         CLI   TYPE,PRODN                                                       
         BNE   HEAD14                                                           
         MVC   P+39(7),=C'PRODUCT'                                              
         MVC   P+47(6),APOPRO                                                   
         MVC   P+54(L'LPRONAME),LPRONAME                                        
                                                                                
HEAD14   BAS   RE,PRNT                    SUPPLIER ADDRESS3 - JOB?              
         LTR   R4,R4                                                            
         BZ    *+12                                                             
         MVC   P+7(L'ADRADD3),ADRADD3                                           
         BCTR  R4,0                                                             
         CLI   TYPE,PRODN                                                       
         BNE   HEAD16                                                           
         MVC   P+39(3),=C'JOB'                                                  
         MVC   P+47(6),APOJOB                                                   
         MVC   P+54(L'LJOBNAME),LJOBNAME                                        
         BAS   RE,PRNT                                                          
                                                                                
         LTR   R4,R4                      SUPPLIER ADDRESS4                     
         BZ    *+10                                                             
         MVC   P+7(L'ADRADD4),ADRADD4                                           
         BAS   RE,PRBILINF                                                      
         CLC   PRBILLS,SPACES                                                   
         BNH   HEAD16                                                           
         MVC   P+39(L'PRBILLS),PRBILLS    PRINT ON BILLS                        
                                                                                
HEAD16   BAS   RE,PRNT                                                          
         CLC   APOATTN,SPACES                                                   
         BNH   HEAD18                                                           
         MVC   P+1(5),=C'ATTN:'                                                 
         MVC   P+7(L'APOATTN),APOATTN     ATTENTION                             
         BAS   RE,PRNT                                                          
                                                                                
HEAD18   CLI   PROGPROF+12,C'N'                                                 
         BE    HEAD20                                                           
         LA    RE,P+1                                                           
         CLI   PROGPROF+12,C'L'                                                 
         BE    *+8                                                              
         LA    RE,P+39                                                          
         MVC   0(12,RE),LSUPP+3           SUPPLIER CODE                         
         CLI   LSUPP,C'*'                                                       
         BE    *+10                                                             
         MVC   0(12,RE),LSUPP                                                   
         BAS   RE,PRNT                                                          
         B     *+8                                                              
                                                                                
HEAD20   BAS   RE,PRNT                                                          
         MVI   PC,SP2                                                           
         BAS   RE,PRNT                                                          
         B     HEAD36                                                           
         DROP  R5                                                               
         EJECT                                                                  
* SUPPLIER ON THE RIGHT & CLIENT ON THE LEFT                                    
                                                                                
HEAD22   LA    R5,SUPADDEL                                                      
         USING ADRELD,R5                                                        
         ZIC   R4,ADRNUM                  DATE - SUPPLIER NAME                  
         MVC   P+52(3),=C'TO:'                                                  
         MVC   P+58(22),LSUPNAME                                                
         MVC   P+1(4),=C'DATE'                                                  
                                                                                
         GOTO1 VDATCON,DMCB,(1,ORDDATE),(8,P+8)                                 
         DROP  R6                                                               
                                                                                
         BAS   RE,PRNT                    CLIENT? - SUPPLIER ADDRESS1           
         LTR   R4,R4                                                            
         BZ    *+12                                                             
         MVC   P+58(L'ADRADD1),ADRADD1                                          
         BCTR  R4,0                                                             
         CLI   TYPE,PRODN                                                       
         BNE   HEAD24                                                           
         MVC   P+1(6),=C'CLIENT'                                                
         MVC   P+8(3),APOCLI                                                    
         MVC   P+16(L'LCLINAME),LCLINAME                                        
                                                                                
HEAD24   BAS   RE,PRNT                    PRODUCT? - SUPPLIER ADDRESS2          
         LTR   R4,R4                                                            
         BZ    *+12                                                             
         MVC   P+58(L'ADRADD2),ADRADD2                                          
         BCTR  R4,0                                                             
         CLI   TYPE,PRODN                                                       
         BNE   HEAD26                                                           
         MVC   P+1(5),=C'PROD.'                                                 
         MVC   P+8(3),APOPRO                                                    
         MVC   P+16(L'LPRONAME),LPRONAME                                        
                                                                                
HEAD26   BAS   RE,PRNT                    JOB - SUPPLIER ADDRESS3               
         LTR   R4,R4                                                            
         BZ    *+12                                                             
         MVC   P+58(L'ADRADD3),ADRADD3                                          
         BCTR  R4,0                                                             
         CLI   TYPE,PRODN                                                       
         BNE   HEAD28                                                           
         MVC   P+1(3),=C'JOB'                                                   
         MVC   P+8(6),APOJOB                                                    
         MVC   P+16(L'LJOBNAME),LJOBNAME                                        
                                                                                
HEAD28   BAS   RE,PRNT                    SUPPLIER ADDRESS4                     
         LTR   R4,R4                                                            
         BZ    *+10                                                             
         MVC   P+58(L'ADRADD4),ADRADD4                                          
         BAS   RE,PRBILINF                                                      
         CLC   PRBILLS,SPACES             PRINT ON BILLS                        
         BNH   HEAD30                                                           
         MVC   P+1(L'PRBILLS),PRBILLS                                           
         BAS   RE,PRNT                                                          
                                                                                
HEAD30   CLC   APOATTN,SPACES                                                   
         BNH   HEAD32                                                           
         MVC   P+52(5),=C'ATTN:'          ATTENTION                             
         MVC   P+58(L'APOATTN),APOATTN                                          
         BAS   RE,PRNT                                                          
                                                                                
HEAD32   CLI   PROGPROF+12,C'N'                                                 
         BE    HEAD34                                                           
         LA    RE,P+1                                                           
         CLI   PROGPROF+12,C'L'                                                 
         BE    *+8                                                              
         LA    RE,P+58                                                          
         MVC   0(12,RE),LSUPP+3           SUPPLIER CODE                         
         CLI   LSUPP,C'*'                                                       
         BE    *+10                                                             
         MVC   0(12,RE),LSUPP                                                   
         BAS   RE,PRNT                                                          
         MVI   PC,SP2                                                           
         B     *+8                                                              
                                                                                
HEAD34   MVI   PC,SP3              3 BLANKS                                     
         BAS   RE,PRNT                                                          
         MVI   PC,SP2              2 BLANKS                                     
         BAS   RE,PRNT                                                          
                                                                                
HEAD36   MVI   PC,PR1SP1                                                        
         CLI   TYPE,PRODN          HANDLE PRODUCTION ORDER                      
         BE    HEAD38                                                           
                                                                                
*              EXPENSE ORDERS ARE DIFFERENT                                     
                                                                                
         CLI   PROGPROF,C'N'       SUPPRESS ORDER TOTAL                         
         BE    HEADX                                                            
                                                                                
         USING OAMELD,R6                                                        
         MVI   ELCODE,OAMELQ       GET OAMELQ FROM AORDER                       
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   TOTAL,OAMAMNT                                                    
         B     HEAD46              ONLY ONE AMOUNT FOR EXPENSE                  
         DROP  R6                                                               
                                                                                
HEAD38   CP    PAGE,=P'1'          W/C DETAIL ON PAGE 1 ONLY                    
         BNE   HEADX                                                            
         CLI   PROGPROF+6,2                                                     
         BNE   HEAD40                                                           
         CLI   PROGPROF,C'Y'                                                    
         BNE   HEADX                                                            
         B     HEAD42                                                           
                                                                                
HEAD40   BAS   RE,PRNT                                                          
         MVC   P+1(13),=C'WORK CATEGORY'                                        
         CLI   PROGPROF+6,0                                                     
         BNE   HEAD42              SHOW MONEY AND CODES                         
         CLC   =C'PN',APOACT                                                    
         BE    HEAD42              SHOW NO MONEY THIS TIME                      
         MVC   P+44(6),=C'AMOUNT'                                               
                                                                                
         USING OAMELD,R6                                                        
HEAD42   BAS   RE,PRNT                                                          
         MVI   ELCODE,OAMELQ       GET OAMELQ FROM AORDER                       
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
HEAD44   TM    OAMSTAT,OAMSXTRA    DON'T PRINT IF ON                            
         BNZ   HEAD45                                                           
         GOTO1 AGETWC,OAMWORK                                                   
         MVC   P+1(2),OAMWORK                                                   
         MVC   P+6(15),WORK                                                     
         BAS   RE,AMOUNT                                                        
         MVI   PC,PR1SP1                                                        
         CLI   PROGPROF+6,2                                                     
         BNE   *+14                                                             
         MVC   P,SPACES                                                         
         B     *+8                                                              
         BAS   RE,PRNT                                                          
                                                                                
HEAD45   BAS   RE,NEXTEL                                                        
         BE    HEAD44                                                           
         CLI   PROGPROF+6,1                                                     
         BNL   HEADX                                                            
         DROP  R6                                                               
                                                                                
HEAD46   CP    TOTAL,=P'0'                                                      
         BE    HEADX                                                            
         EDIT  TOTAL,(10,P+40),2                                                
         ZAP   TOTAL,=P'0'                                                      
         MVC   P+25(12),=C'TOTAL AMOUNT'                                        
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRNT                                                          
                                                                                
HEADX    BAS   RE,PRNT             SUB1                                         
         MVI   PC,SP2                                                           
         BAS   RE,PRNT             SUB2                                         
         BAS   RE,PRNT                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        EXITS TO ROOT                                                *         
***********************************************************************         
                                                                                
CHKFXNUM NTR1                                                                   
         MVI   FERN,OK                                                          
         OC    FAXNUM,FAXNUM       ANYTHING ON SUPPLIER?                        
         BNZ   OKXIT               YES                                          
         OC    FAXPARM,FAXPARM     ANYTHING IN ACTION PARMS                     
         BNZ   OKXIT               YES                                          
         NI    STATUS,X'FF'-FAX_IT CANT FAX WITH NO NUMBER                      
         B     ERRXIT                                                           
                                                                                
CHKEMAIL NTR1                                                                   
         MVI   FERN,OK                                                          
         CLI   EMAILADL,0              ANYTHING ON SUPPLIER?                    
         BNE   OKXIT                    YES                                     
         CLI   EMLPARML,0              ANYTHING IN ACTION PARMS                 
         BNE   OKXIT                    YES                                     
         NI    STATUS,X'FF'-EMAIL_IT   CANT EMAIL WITH NO ADDRESS               
         B     ERRXIT                                                           
                                                                                
PRTMSG   NTR1                                                                   
         MVI   FERN,OK                                                          
         MVC   MSG,ENDMESS                                                      
         MVC   MSG+21(3),SUBID     MOVE REPORT ID INTO MSG                      
         EDIT  REPNO,(4,MSG+25),ALIGN=LEFT                                      
         LR    R1,R0                                                            
         LA    R1,MSG+25(R1)                                                    
         MVC   0(25,R1),MSG+29                                                  
         B     EXIT                                                             
ENDMESS  DC    CL60'ORDER SENT TO PQ (ID=APO,1234) - ENTER NEXT ACTION'         
                                                                                
FAXMSG   NTR1                                                                   
         MVI   FERN,OK                                                          
         MVC   MSG,FAXMESS                                                      
         MVC   MSG+6(6),APONUM                                                  
         LA    R2,MSG+22                                                        
         LA    R1,FAXNUM           FORMAT FAX NUMBER                            
         OC    FAXPARM,FAXPARM     PASSED A FAX NUMBER                          
         BZ    *+8                                                              
         LA    R1,FAXPARM                                                       
         LA    R2,MSG+22                                                        
         BAS   RE,FORMFONE                                                      
         B     EXIT                                                             
FAXMESS  DC    CL60'ORDER NNNNNN FAXED TO                - ENTER NEXT AX        
               CTION'                                                           
                                                                                
EMAILMSG NTR1                                                                   
         MVI   FERN,OK                                                          
         MVC   MSG,EMAILMES                                                     
         MVC   MSG+6(6),APONUM                                                  
         B     EXIT                                                             
                                                                                
EMAILMES DC    CL60'ORDER NNNNNN EMAILED - ENTER NEXT ACTION'                   
                                                                                
PRTFXMSG NTR1                                                                   
         MVI   FERN,OK                                                          
         MVC   MSG,PFXMESS                                                      
         MVC   MSG+6(6),APONUM                                                  
         LA    R2,MSG+22                                                        
         LA    R1,FAXNUM                 FORMAT FAX NUMBER                      
         OC    FAXPARM,FAXPARM           PASSED A FAX NUMBER                    
         BZ    *+8                                                              
         LA    R1,FAXPARM                                                       
         BAS   RE,FORMFONE                                                      
         MVC   MSG+51(3),SUBID           MOVE REPORT ID INTO MSG                
         EDIT  REPNO,(4,MSG+55),ALIGN=LEFT                                      
         B     EXIT                                                             
                                                                                
PFXMESS  DC    CL60'ORDER NNNNNN FAXED TO                - AND QUEUED- X        
                  ,     '                                                       
                                                                                
PRTEMMSG NTR1                                                                   
         MVI   FERN,OK                                                          
         MVC   MSG,PEMMESS                                                      
         MVC   MSG+6(6),APONUM                                                  
         MVC   MSG+35(3),SUBID     MOVE REPORT ID INTO MSG                      
         EDIT  REPNO,(4,MSG+39),ALIGN=LEFT                                      
                                                                                
         B     EXIT                                                             
PEMMESS  DC    CL60'ORDER NNNNNN EMAILED - AND QUEUED-    ,     '               
         EJECT                                                                  
***********************************************************************         
*        FORMAT THE PHONE NUMBER AT 0(R1) INTO 0(R2)                  *         
***********************************************************************         
                                                                                
FORMFONE CLI   0(R1),C'1'                                                       
         BNE   FORM02                                                           
         MVI   0(R2),C'1'                                                       
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
                                                                                
FORM02   MVC   0(09,R2),=C'(   )   -'                                           
         MVC   1(03,R2),0(R1)                                                   
         MVC   5(03,R2),3(R1)                                                   
         MVC   9(04,R2),6(R1)                                                   
         BR    RE                                                               
                                                                                
GETELIO  L     R6,AORDER                                                        
         GETEL (R6),DATADISP,ELCODE                                             
                                                                                
OKXIT    SR    RB,RB               CC = EQU                                     
ERRXIT   LTR   RB,RB               CC = NEQ                                     
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        GET 'PRINT  ON BILLS INF' FROM JOB                           *         
***********************************************************************         
                                                                                
PRBILINF NTR1                                                                   
         CLI   PROGPROF+7,C'Y'                                                  
         BNE   PRBILX                                                           
         OC    PRBILLS,PRBILLS                                                  
         BNZ   PRBILX                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),PRODUL                                                  
         SR    RE,RE                                                            
         LA    RF,KEY+3(RE)                                                     
         MVC   0(6,RF),APOCLI      CLIENT KEY                                   
         IC    RE,PRODHEIR                                                      
         LA    RF,KEY+3(RE)                                                     
         MVC   0(6,RF),APOPRO      PRODUCT KEY                                  
         IC    RE,PRODHEIR+1                                                    
         LA    RF,KEY+3(RE)                                                     
         MVC   0(6,RF),APOJOB      JOB KEY                                      
         GOTO1 AREAD,AIOAREA4                                                   
         BNE   ERRXIT                                                           
         L     RE,AIOAREA4                                                      
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
                                                                                
PRBI02   CLI   0(RE),PPRELQ        PROFILE ELEMENT                              
         BE    PRBI04                                                           
         CLI   0(RE),0                                                          
         BE    PRBI06                                                           
         IC    RF,1(RE)                                                         
         LA    RE,0(RF,RE)                                                      
         B     PRBI02                                                           
                                                                                
         USING PPRELD,RE                                                        
PRBI04   MVC   PRBILLS,PPRBILLP                                                 
         B     PRBILX                                                           
                                                                                
PRBI06   MVC   PRBILLS,SPACES                                                   
                                                                                
PRBILX   B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CHECK IF VALID EMAIL ADDRESS                                 *         
***********************************************************************         
                                                                                
EMVAL    NTR1  BASE=*,LABEL=*                                                   
         ZIC   R2,EMLPARML         EMAIL LENGTH                                 
         LA    R3,EMLPARM          POINT TO E-MAIL ADDRESS.                     
         MVI   BYTE,0              INIT FLAG                                    
         CHI   R2,60               IS E-MAIL MORE THAN 60 CHARS                 
         BH    EMVALER             YES, IT'S AN ERROR                           
         CHI   R2,7                SHLD ATLST LOOK LIKE X@X.XXX                 
         BL    EMVALER                                                          
         LR    R4,R3               EXTRA POINTER TO EMAIL ADDRS                 
         SHI   R2,4                WANT TO POINT TO .XXX                        
         AR    R4,R2                                                            
         CLI   0(R4),C'.'          IS IT DOT SOMETHING                          
         BNE   EMVALER                                                          
         LHI   R0,3                VALIDATE .COM, .EDU ETC                      
                                                                                
EMVA02   LA    R4,1(R4)                                                         
         CLI   0(R4),C'A'          IS IT HIGHER THAN A                          
         BL    EMVALER                                                          
         CLI   0(R4),C'Z'          SHOULD BE Z OR LOWER                         
         BH    EMVALER                                                          
         BCT   R0,EMVA02                                                        
         AHI   R2,4                E-MAIL'S LENGTH                              
         OI    BYTE,SPCCHAR        CAN'T START WITH SPECIAL CHAR                
                                                                                
EMVA04   CLI   0(R3),C'0'          IS IT LESS THAN F0                           
         BL    EMVA06              YES CHK NEXT                                 
         CLI   0(R3),C'9'          IS IT HIGHER THAN F9                         
         BNH   EMVA08              IT IS A NUMBER                               
         B     EMVALER                                                          
                                                                                
EMVA06   CLI   0(R3),C'A'          IS IT HIGHER THAN A                          
         BL    EMVA12              CHECK FOR SPECIAL CHARS                      
         CLI   0(R3),C'Z'          SHOULD BE Z OR LOWER                         
         BH    EMVALER                                                          
                                                                                
EMVA08   NI    BYTE,X'FF'-SPCCHAR  LAST CHAR WAS ALL PURPOSE CHAR               
                                                                                
EMVA10   LA    R3,1(R3)            GET NEXT CHAR                                
         BCT   R2,EMVA04                                                        
         B     EMVA20                                                           
                                                                                
EMVA12   CLI   0(R3),C'@'          HAVE WE REACHED @ YET                        
         BNE   EMVA14                                                           
         TM    BYTE,ATFOUND        MAKE SURE NO MORE THAN ONE @ SIGN            
         BO    EMVALER                                                          
         OI    BYTE,ATFOUND        NOW WE HAVE ONE @ IN E-MAIL                  
                                                                                
EMVA14   LA    R1,EXCTAB           POINT TO SPECIAL CHARS TABLE                 
                                                                                
EMVA16   CLI   0(R1),X'FF'         DID WE FIND SPECIAL CHARS                    
         BE    EMVALER             NO SPCL CHAR FND, ERROR.                     
         CLC   0(1,R1),0(R3)       IS IT SPCL CHAR                              
         BNE   EMVA18                                                           
         TM    BYTE,SPCCHAR        NO TWO SPECIAL CHARS IN ROW                  
         BO    EMVALER                                                          
         OI    BYTE,SPCCHAR        LAST CHAR WAS SPEC CHAR                      
         B     EMVA10                                                           
                                                                                
EMVA18   LA    R1,1(R1)            POINT TO NEXT TABLE ENTRY                    
         B     EMVA16                                                           
                                                                                
EMVA20   TM    BYTE,ATFOUND        SHOULD HAVE ONE @ IN E-MAIL                  
         BZ    EMVALER             NO @ FOUND ERROR                             
         CR    RB,RB                                                            
         B     EMVALX                                                           
                                                                                
EMVALER  CR    RB,RD                                                            
                                                                                
EMVALX   XIT1                                                                   
                                                                                
EXCTAB   DC    C'@'                @ SIGN                                       
         DC    C'_'                UNDERSCORE                                   
         DC    C'.'                DOT                                          
         DC    X'FF'               END OF TABLE                                 
                                                                                
ATFOUND  EQU   X'01'               @ SYMBOL FOUND IN EMAIL ADDRESS              
SPCCHAR  EQU   X'02'               LAST CHAR WAS A SPECIAL EMAIL CHAR           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACPOPDSECT                                                     
ACPOPFXD DSECT                                                                  
       ++INCLUDE ACPOPFAX                                                       
         EJECT                                                                  
***********************************************************************         
*        OVERLAY WORKING STORAGE AT ASAVE                             *         
***********************************************************************         
                                                                                
LWSD     DSECT                                                                  
AWORK1   DS    A                   A(RD)                                        
AORDER   DS    A                   A(ORDER RECORD) - IOAREA1 OR 3               
PC       DS    CL1                 PRINT COMMAND CODE                           
P        DS    CL132               PRINT LINE                                   
SPC      DS    CL133               SAVED PC + P                                 
LASTPC   DS    CL1                 LAST SUCCESSFUL PRINT COMMAND CODE           
SUBID    DS    CL3                 3-CHAR REPORT ID                             
REPNO    DS    CL2                 REPORT NUMBER                                
LINE     DS    PL3                 LINE NUMBER                                  
MAXLINES DS    PL3                 MAX PAGE LENGTH                              
PAGE     DS    PL3                 PAGE NUMBER                                  
TOTAL    DS    PL6                 ORDER TOTAL VALUE                            
TIMENOW  DS    CL4                 TIME AS EBCDIC HHMM                          
SCAT     DS    CL15                SAVED WC DESC (PRINARR)                      
SAMT     DS    CL12                SAVED AMOUNT (PRINARR)                       
SFLAG    DS    CL1                 SAVED FLAG (PRINARR)                         
CLEN     DS    CL1                 COMMENT LINE CHOP WIDTH                      
                                                                                
STATUS   DS    CL1                 WHAT TO DO                                   
NOTHING  EQU   X'00'                                                            
FAX_IT   EQU   X'01'                                                            
PRINT_IT EQU   X'02'                                                            
EMAIL_IT EQU   X'04'                                                            
                                                                                
PSTAT    DS    CL1                 WHAT I AM DOING                              
FAXING   EQU   X'01'                                                            
PRINTING EQU   X'02'                                                            
EMAILING EQU   X'04'                                                            
                                                                                
FAXPARM  DS    CL11                FAX= VALUE PASSED IN ACTSCAN                 
EMLPARM  DS    CL(MAXESCAN)        EMAIL= VALUE PASSED IN ACTSCAN (60)          
EMLPARML DS    X                   LENGTH OF EMAIL IN ACTSCAN                   
                                                                                
PRBILLS  DS    CL50                PRINT ON BILLS FROM PROFILE                  
                                                                                
CHOPIN   DS    1000C               CHOPPER IN                                   
CHOPOUT  DS    (CHOPOLN)C          CHOPPER OUT                                  
CHOPOLN  EQU   1440                                                             
                                                                                
IOBUFLN  EQU   2000                                                             
LWSX     DS    0C                                                               
*                                  PRINT COMMAND CODES                          
INITIAL  EQU   X'00'                                                            
EDICCC   EQU   X'01'               PRINT AN EDICT CONTROL CARD                  
HEADOFF  EQU   X'89'                                                            
PR1SP1   EQU   X'09'                                                            
SP2      EQU   X'0B'                                                            
SP3      EQU   X'19'                                                            
TRMINATE EQU   X'FF'                                                            
                                                                                
KEEP     EQU   X'08'               PQ STATUS (KEEP AFTER PRINTING)              
ACTV     EQU   X'80'               PQ STAT, CREATION COMPLETE, ACTIVE           
         EJECT                                                                  
HDRD     DSECT                                                                  
         DS    CL4                 ORIGIN ID FOR REP                            
HDR      DS    CL5                                                              
HDFX     DS    CL4                 FAX                                          
         DS    CL1                                                              
HDNUM    DS    CL11                NUMBER                                       
         ORG   HDFX                                                             
         DS    CL25                                                             
HDW      DS    CL1                                                              
HDP      DS    CL1                                                              
         DS    CL2                                                              
HDDEST   DS    CL16                FORMATED DESTINATION (FOR =ETI)              
HDBILL   DS    0CL13                                                            
HDBCLI   DS    CL3                                                              
HDBPRO   DS    CL3                                                              
HDBJOB   DS    CL6                                                              
         DS    CL3                                                              
HDEMID   DS    CL1                 EMAIL IDENTIFIER                             
HDEMIDQ  EQU   C'M'                EMAIL IDENTIFIER                             
         DS    CL3                 SPARE                                        
         EJECT                                                                  
DDSD     DSECT                                                                  
DDD      DS    CL5                                                              
         DS    CL1                                                              
DDSYS    DS    CL2                                                              
DDREP    DS    CL3                                                              
DDTRN    DS    CL3                                                              
         DS    CL1                                                              
DDUSER   DS    CL58                                                             
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACPOP03   05/21/13'                                      
         END                                                                    
