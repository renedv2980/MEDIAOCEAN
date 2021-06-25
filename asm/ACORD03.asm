*          DATA SET ACORD03    AT LEVEL 054 AS OF 10/17/18                      
*PHASE T60F03A                                                                  
*---------------------------------------------------------                      
*        PRODUCTION ORDERS - PRINT OVERLAY                                      
*---------------------------------------------------------                      
*                                                                               
         TITLE 'ACORD03 - PRODUCTION ORDERS - PRINT'                            
ACORD03  CSECT                                                                  
         PRINT NOGEN               ON ENTRY TO OVERLAY ORDER RECORD IS          
         NMOD1 0,**ORD3**,RA                                                    
         USING ORDWORKD,R9                                                      
         USING TWAD,R8                                                          
         L     RC,ASAVE            RC=A(LOCAL WORKING STORAGE)                  
         USING LWSD,RC                                                          
         LR    RE,RC                                                            
         LA    RF,LWSX-LWSD        CLEAR OUT WORKING STORAGE                    
         SR    R1,R1               ADD/PRINT USES STORAGE TWICE                 
         MVCL  RE,R0                                                            
*                                  INITIALIZE LOCAL W/S                         
         ST    RD,AWORK1                                                        
         MVC   AORDER,AIOAREA1                                                  
         CLI   APOACT,C'A'                                                      
         BNE   *+10                                                             
         MVC   AORDER,AIOAREA3                                                  
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
*                                                                               
         BAS   RE,INITPRT                                                       
         BAS   RE,LINEUP                                                        
         BAS   RE,PRTMSG                                                        
         B     EXIT                                                             
*                                                                               
ACO20    L     R6,AORDER           SET R6 = A(ORDER ELEMENT) THROUGHOUT         
         AH    R6,DATADISP                                                      
         SR    R0,R0                                                            
         AR    R6,R0               CRAFTY, AS LONG AS YOUR SURE THE             
         IC    R0,1(R6)            ELEMENT'S ON THE RECORD                      
         CLI   0(R6),X'67'                                                      
         BNE   *-10                                                             
         USING ACORDRD,R6                                                       
         CLI   TYPE,PRODN          PRODUCTION ORDER                             
         BNE   *+8                                                              
         BAS   RE,GETMED           GET MEDIA FOR REPORT                         
*                                                                               
         BAS   RE,GETPARMS         SEE IF PASSED ANY PARMS                      
         CLI   FERN,INVALID        ERROR IN PARMS                               
         BE    ERRXIT                                                           
*                                                                               
         BAS   RE,VALPARMS         MAKE SURE PARMS ARE ALLOWED                  
         BNE   ERRXIT                                                           
*                                                                               
         BAS   RE,SETSTAT          SEE IF PRINT OR FAX OR BOTH                  
*                                                                               
         TM    STATUS,FAX_IT                                                    
         BNO   ACO40                                                            
         BAS   RE,CHKFXNUM         ENSURE I HAVE A FAX NUMBER                   
         BNE   ACO40               NO                                           
         MVI   PSTAT,FAXING                                                     
         BAS   RE,INITFAX                                                       
         BAS   RE,REPORT                                                        
         BAS   RE,FAXMSG                                                        
*                                                                               
ACO40    TM    STATUS,EMAIL_IT                                                  
         BNO   ACO50                                                            
         BAS   RE,CHKEMAIL         ENSURE I HAVE A FAX NUMBER                   
         BNE   ACO50               NO                                           
         MVI   PSTAT,EMAILING                                                   
         BAS   RE,INITEML                                                       
         BAS   RE,REPORT                                                        
         BAS   RE,EMAILMSG                                                      
*                                                                               
ACO50    TM    STATUS,FAX_IT+EMAIL_IT   IF NO FAX OR EMAIL THEN PRINT           
         BZ    *+12                                                             
         TM    STATUS,PRINT_IT                                                  
         BNO   ACOX                                                             
         MVI   PSTAT,PRINTING                                                   
         BAS   RE,INITPRT                                                       
         BAS   RE,REPORT           CREATE REPORT                                
         BAS   RE,PRTMSG           DO MESSAGE                                   
*                                                                               
         TM    STATUS,FAX_IT          WAS THIS FAXED ALSO?                      
         BNO   *+8                     NO                                       
         BAS   RE,PRTFXMSG             DO BOTH STYLE MESSAGE                    
*                                                                               
         TM    STATUS,EMAIL_IT        WAS THIS EMAILED ALSO?                    
         BNO   *+8                     NO                                       
         BAS   RE,PRTEMMSG             DO BOTH STYLE MESSAGE                    
*                                                                               
ACOX     B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        INITIALIZE PRINT QUEUE CALL                                            
*---------------------------------------------------------                      
*                                                                               
INITPRT  NTR1                                                                   
         LA    R7,PC                                                            
         USING PQPLD,R7                                                         
*                                                                               
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
         BNE   INPR2                                                            
         MVC   QLSUBID,=C'LU1'                                                  
         MVC   QLDESC(11),=C'*LINEUP#01*'                                       
         MVI   QLCLASS,0                                                        
         MVI   QLSTAT,KEEP                                                      
*                                                                               
INPR2    BAS   RE,PRIN                                                          
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*        INITIALIZE PRINT QUEUE CALL FOR FAX (CLASS G, *HDR* AND DDS+)          
*---------------------------------------------------------------------          
*                                                                               
INITFAX  NTR1                                                                   
         BAS   RE,OPENPQFX         SET PQ HEADER, OPEN PQ                       
         BAS   RE,SETFXHDR         CREATE *HDR*                                 
         BAS   RE,SETDDS           CREATE DDS+                                  
         B     EXIT                                                             
         EJECT                                                                  
OPENPQFX NTR1                                                                   
         LA    R7,PC                                                            
         USING PQPLD,R7                                                         
         MVI   PC,INITIAL                                                       
         MVI   QLEXTRA,X'FF'       MUST SET TO X'FF' FOR EXTRA VALUES           
         MVC   QLDESC,SPACES                                                    
         MVC   QLDESC(2),=C'PO'                                                 
         MVC   QLDESC+2(6),APONUM                                               
**       MVI   QLDESC+10,1         1 COPY                                       
         MVC   QLSUBID,=C'APO'                                                  
         MVI   QLLPP,68                                                         
         MVC   QLSRCID,TWAUSRID                                                 
         MVI   QLCLASS,C'G'                                                     
         MVI   QLSTAT,ACTV                                                      
         MVI   QLSYS,C'A'                                                       
         MVC   QLPRG,=C'PO'                                                     
         BAS   RE,PRIN                                                          
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
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
         BAS   RE,PRIN             SENT TO PQ                                   
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
SETDDS   NTR1                                                                   
         MVI   PC,EDICCC           PRINT ++DDS EDICT CONTROL CARD               
         LA    R7,P                                                             
         MVC   P,SPACES                                                         
         USING DDSD,R7                                                          
         MVC   DDD,=C'++DDS'                                                    
         MVC   DDSYS,=C'AC'        SYSTEM                                       
         MVC   DDREP,=C'ORD'       REPORT                                       
         MVC   DDTRN,=C'TRN'       MUST BE TRN                                  
*                                                                               
         LA    R2,DDUSER                                                        
         USING ACORDFXD,R2                                                      
         MVC   ACFXCOMP,COMPANY                                                 
         MVI   ACFXTYPE,C'E'                                                    
         MVC   ACFXEXP,LEXP                                                     
         CLI   TYPE,PRODN          PRODUCTION ORDER                             
         BNE   SETDD30                                                          
         MVI   ACFXTYPE,C'P'                                                    
         MVC   ACFXUL,=C'SJ'                                                    
         MVC   ACFXCLI,APOCLI                                                   
         MVC   ACFXPRO,APOPRO                                                   
         MVC   ACFXJOB,APOJOB                                                   
*                                                                               
SETDD30  MVC   ACFXVEND,LSUPP+1    ASSUME OVERRIDE                              
         CLI   LSUPP,C'*'          UL OVERRIDE IN SUPPLIER                      
         BE    SETDD50             YES                                          
*                                                                               
         MVC   ACFXVEND(2),SUPPUL  12 BYTE SUPPLIER I/P                         
         MVC   ACFXVEND+2(12),LSUPP                                             
*                                                                               
SETDD50  EQU   *                                                                
         MVC   ACFXONUM,APONUM                                                  
         BAS   RE,PRIN             SENT TO PQ                                   
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
*-------------------------------------------------- - - - - - - - - -           
*      INITIALIZE PRINT QUEUE CALL FOR EMAIL (CLASS M, *HDR* AND DDS+)          
*--------------------------------------------------------------------           
*                                                                               
INITEML  NTR1                                                                   
         BAS   RE,OPENPQEM         SET PQ HEADER, OPEN PQ                       
         BAS   RE,SETEMHDR         CREATE EMAIL *HDR*                           
         BAS   RE,SETEMDDS         CREATE DDS+                                  
         BAS   RE,SETEMRCP         CREATE DDS+ RECIEVE RECORD                   
         BAS   RE,SETEMSUB         CREATE DDS+ SUBJECT RECORD                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
OPENPQEM NTR1                                                                   
         LA    R7,PC                                                            
         USING PQPLD,R7                                                         
         MVI   PC,INITIAL                                                       
         MVI   QLEXTRA,X'FF'       MUST SET TO X'FF' FOR EXTRA VALUES           
         MVC   QLDESC,SPACES                                                    
         MVC   QLDESC(2),=C'PO'                                                 
         MVC   QLDESC+2(6),APONUM                                               
**       MVI   QLDESC+10,1         1 COPY                                       
         MVC   QLSUBID,=C'APO'                                                  
         MVI   QLLPP,68                                                         
         MVC   QLSRCID,TWAUSRID                                                 
         MVI   QLCLASS,C'G'                                                     
         MVI   QLSTAT,ACTV                                                      
         MVI   QLSYS,C'A'                                                       
         MVC   QLPRG,=C'PO'                                                     
         BAS   RE,PRIN                                                          
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
SETEMHDR NTR1                                                                   
         MVI   PC,EDICCC           PRINT *HDR* EDICT CONTROL CARD               
         LA    R7,P                                                             
         MVC   P,SPACES                                                         
         USING HDRD,R7                                                          
         MVC   HDR,=C'*HDR*'                                                    
         MVI   HDEMID,HDEMIDQ      C'M'  EMAIL IDENTIFIER                       
         BAS   RE,PRIN             SENT TO PQ                                   
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
SETEMDDS NTR1                                                                   
         MVI   PC,EDICCC           PRINT ++DDS EDICT CONTROL CARD               
         LA    R7,P                                                             
         MVC   P,SPACES                                                         
         USING DDSD,R7                                                          
         MVC   DDD,=C'++DDS'                                                    
         MVC   DDSYS,=C'AC'        SYSTEM                                       
         MVC   DDREP,=C'ORD'       REPORT                                       
         MVC   DDTRN,=C'TRN'       MUST BE TRN                                  
*                                                                               
         BAS   RE,PRIN             SENT TO PQ                                   
         B     EXIT                                                             
         DROP  R7                                                               
*                                                                               
SETEMRCP NTR1                                                                   
         MVI   PC,EDICCC           PRINT ++DDS EDICT CONTROL CARD               
         LA    R7,P                                                             
         MVC   P,SPACES                                                         
         USING DDSD,R7                                                          
         MVC   DDD,=C'++DDS'                                                    
         MVC   DDTRN,=C'RCP'                                                    
*                                                                               
*                                                                               
         SR    R2,R2                                                            
         LA    R1,EMLPARM          OVERRIDE EMAIL PARAMETER                     
         ICM   R2,1,EMLPARML                                                    
         BNZ   SETEMR10                                                         
         LA    R1,EMAILAD          EMAIL ADDRESS SET ON ACCOUNT RECORD          
         IC    R2,EMAILADL                                                      
*                                                                               
SETEMR10 EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   DDUSER(0),0(R1)                                                  
*                                                                               
         BAS   RE,PRIN             SENT TO PQ                                   
         B     EXIT                                                             
         DROP  R7                                                               
*                                                                               
SETEMSUB NTR1                                                                   
         MVI   PC,EDICCC           PRINT ++DDS EDICT CONTROL CARD               
         LA    R7,P                                                             
         MVC   P,SPACES                                                         
         USING DDSD,R7                                                          
         MVC   DDD,=C'++DDS'                                                    
         MVC   DDTRN,=C'SUB'                                                    
*                                                                               
         MVC   DDUSER(16),=C'EMAILED ORDER - '                                  
         MVC   DDUSER+16(L'APONUM),APONUM                                       
*                                                                               
         BAS   RE,PRIN             SENT TO PQ                                   
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
*-------------------------------------------------- - - - - - - - - -           
*        SET FAXPARM AND EMAIL PARM FROM VALUES PASSED IN ACTSCAN               
*--------------------------------------------------------------------           
*                                                                               
GETPARMS NTR1                                                                   
         MVI   FERN,OK                                                          
         XC    FAXPARM,FAXPARM                                                  
         XC    EMLPARM,EMLPARM                                                  
         XC    EMLPARML,EMLPARML                                                
         CLI   ACTSCAN+82,0        ANYTHING IN PARM                             
         BE    GETPX               NO                                           
*                                                                               
         CLC   ACTSCAN+94(3),=C'FAX'                                            
         BNE   GETP10                                                           
         CLI   ACTSCAN+82,3        C'FAX'                                       
         BNE   GETPERR             NO                                           
         MVC   FAXPARM,ACTSCAN+104                                              
         B     GETPX                                                            
*                                                                               
GETP10   CLC   ACTSCAN+94(2),=C'EM'                                             
         BNE   GETPERR                                                          
         CLI   ACTSCAN+82,2               C'EM'                                 
         BNE   GETPERR                     NO                                   
         MVC   EMLPARML,ACTSCAN+83        LENGTH OF EMAIL ADDRESS               
         MVC   EMLPARM,ACTSCAN+104                                              
*                                                                               
GETPX    B     EXIT                                                             
*                                                                               
GETPERR  MVI   FERN,INVALID                                                     
         B     ERRXIT                                                           
         EJECT                                                                  
*                                                                               
*-------------------------------------------------- - - - - - - - - -           
*        VALIDATE PARMS                                                         
*--------------------------------------------------------------------           
*                                                                               
VALPARMS NTR1                                                                   
         OC    FAXPARM,FAXPARM          DO I HAVE A FAXPARM?                    
         BZ    VALP100                   NO                                     
*                                                                               
         CLI   FAXPARM,C'N'             JUST WANT THE REPORT?                   
         BE    VALPX                     OK                                     
*                                                                               
         CLI   PRAUTOFX,C'Y'            CAN THEY USE IT?                        
         BE    VALP50                    YES, VALIDATE                          
*                                                                               
         MVI   FERN,NOFAX                                                       
         B     ERRXIT                                                           
*                                                                               
VALP50   LA    RF,9                     ASSUME LEN OF 10                        
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
*                                                                               
VALP100  CLI   EMLPARML,0               AN EMAIL ADDRESS GIVEN?                 
         BE    VALPX                     NO                                     
         CLI   EMLPARML,2               POSSIBLE EMAIL ADDRESS?                 
         BH    VALP110                   YES                                    
         CLI   EMLPARM,C'N'             'NO' EMAIL WANTED?                      
         BE    VALPX                     YES, EXIT                              
*                                                                               
VALP109  MVI   FERN,BADEMAIL                                                    
         B     ERRXIT                                                           
*                                                                               
VALP110  CLI   PRAUTOFX,C'E'            CAN THEY USE IT?                        
         BNE   VALP120                   YES, VALIDATE                          
*                                                                               
         BRAS  RE,EMVAL                                                         
         BE    VALPX                                                            
*                                                                               
VALP120  MVI   FERN,NOEMAIL                                                     
         B     ERRXIT                                                           
*                                                                               
VALPX    B     OKXIT                                                            
         EJECT                                                                  
*---------------------------------------------------------------------          
*        SET STATUS BYTE BASED ON PARMS/PROFILE OPTIONS                         
*---------------------------------------------------------------------          
*                                                                               
SETSTAT  NTR1                                                                   
         MVI   STATUS,NOTHING                                                   
*                                                                               
         CLI   PRAUTOFX,C'E'       CAN THEY EMAIL?                              
         BNE   SET10               NO                                           
*                                                                               
         CLI   EMLPARML,2          HAVE THEY DECIDED NOT TO                     
         BH    SET05                NO                                          
         CLI   EMLPARM,C'N'        HAVE THEY DECIDED NOT TO                     
         BE    SET10                YES                                         
*                                                                               
SET05    OI    STATUS,EMAIL_IT                                                  
         B     SET20                                                            
*                                                                               
SET10    CLI   PRAUTOFX,C'Y'       CAN THEY FAX?                                
         BNE   SETX                NO                                           
*                                                                               
         CLI   FAXPARM,C'N'        HAVE THEY DECIDED NOT TO                     
         BE    SETX                YES, JUST PRINT                              
*                                                                               
         OI    STATUS,FAX_IT                                                    
*                                                                               
SET20    CLI   PRFXNPRT,C'Y'       WANT A HARD COPY ALSO                        
         BNE   *+8                 YES                                          
         OI    STATUS,PRINT_IT                                                  
*                                                                               
SETX     B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        PRINT LINE-UP PATTERN                                                  
*---------------------------------------------------------                      
*                                                                               
LINEUP   NTR1                                                                   
         MVI   PC,HEADOFF                                                       
         BAS   RE,PRIN             THROW TO HEAD OF FORM                        
         ZAP   LINE,=P'0'                                                       
         MVI   PC,SP2                                                           
         BAS   RE,PRIN                                                          
         BAS   RE,PRIN                                                          
         MVI   P,C'X'                                                           
         MVC   P+1(84),P                                                        
         MVC   P+44(17),=CL17' LINE UP PATTERN'                                 
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN             HEADS ON LINES 3 & 4                         
         MVI   P+45,C'-'                                                        
         MVC   P+46(14),P+45                                                    
         BAS   RE,PRIN                                                          
         LA    R0,5                                                             
         MVI   PC,SP2                                                           
         BAS   RE,PRIN             5 SPACE LINES                                
         BCT   R0,*-4                                                           
         MVI   PC,PR1SP1                                                        
         LA    R0,2                                                             
LINEUP2  MVI   P,C'X'                                                           
         MVC   P+1(84),P                                                        
         BAS   RE,PRIN             LINE UP PATTERN ON LINES 10 & 11             
         BCT   R0,LINEUP2                                                       
         MVI   PC,TRMINATE                                                      
         BAS   RE,PRIN                                                          
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        GET MEDIA DESCRIPTION INTO LMEDNAME                                    
*---------------------------------------------------------                      
*                                                                               
GETMED   NTR1                                                                   
         CLC   LMEDIA,APOJOB       MEDIA IS 1ST CHAR OF JOB CODE                
         BE    GETMX               DESC IS ALREADY IN TWA                       
         MVC   LMEDIA,APOJOB                                                    
         MVC   LMEDNAME,SPACES                                                  
         MVC   KEY,SPACES          IT ISN'T SO READ MEDIA REC                   
         MVI   KEY,X'09'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(1),LMEDIA                                                  
         GOTO1 AREAD,AIOAREA2                                                   
         BNE   GETMX                                                            
         L     RF,AIOAREA2                                                      
         AH    RF,DATADISP                                                      
         SR    R0,R0                                                            
GETMED2  CLI   0(RF),0                                                          
         BE    GETMX                                                            
         CLI   0(RF),X'11'                                                      
         BE    *+14                                                             
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     GETMED2                                                          
         USING ACMEDIAD,RF                                                      
         MVC   LMEDNAME(12),ACMDDESC+3  SAVE IT                                 
GETMX    B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        PRINT THE REPORT                                                       
*        PRINT NARRATIVE, WORK CODE AMOUNTS, TOTAL AND AUTHORIZATION            
*---------------------------------------------------------                      
*                                                                               
REPORT   NTR1                                                                   
         CLI   PROGPROF+9,C'P'     ARE WE SUPPOSTED TO PRINT TAX ??             
         BNE   REP20                                                            
*                                                                               
         MVC   P+1(19),=C'IS TAX APPLICABLE ?'                                  
         MVC   P+22(1),APOTAX                                                   
         CLI   P+22,C' '                                                        
         BE    REP10                                                            
         CLI   P+22,C'N'                                                        
         MVC   P+22(2),=C'NO'                                                   
         BE    REP10                                                            
         MVC   P+22(3),=C'YES'                                                  
*                                                                               
REP10    MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
         MVI   PC,SP2              1 BLANK                                      
         BAS   RE,PRIN                                                          
*                                                                               
REP20    MVI   FLAG,0                                                           
         MVI   CLEN,60                                                          
         GOTO1 PRINARR,AORDER      PRINT NARRATIVE                              
*                                                                               
         MVI   PC,SP2              MAKE SURE HEADS ROUTINE IS EXECUTED          
         BAS   RE,PRIN                                                          
         CP    TOTAL,=P'0'         PRINT ORDER TOTAL                            
         BE    REP40               UNLESS VALUES SUPPRESSED OR ZERO             
         MVI   PC,SP2                                                           
         BAS   RE,PRIN                                                          
         MVC   P+54(11),=C'ORDER TOTAL'                                         
         EDIT  TOTAL,(10,P+72),2                                                
         ZAP   TOTAL,=P'0'                                                      
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
*                                                                               
REP40    MVI   PC,SP2              PRINT FOOTLINES                              
         BAS   RE,PRIN                                                          
         MVI   FLAG,FOOTLINE                                                    
         MVI   CLEN,60                                                          
         GOTO1 PRINARR,AORDER                                                   
*                                                                               
REP50    ZAP   DUB,MAXLINES                                                     
         SP    DUB,LINE                                                         
         SP    DUB,=P'1'                                                        
         CP    DUB,=P'0'           IF NO ROOM FOR BOTTOM LINE                   
         BH    *+12                                                             
         BAS   RE,PRIN             SET NEXT PAGE                                
         B     REP50               AND TRY AGAIN                                
*                                                                               
         CVB   R1,DUB                                                           
         MVI   PC,SP2                                                           
         BAS   RE,PRIN             SKIP TO BOTTOM OF FORM                       
         BCT   R1,*-8                                                           
         MVC   P+1(9),=C'DUE DATE:'                                             
         MVC   P+11(L'APODDTE),APODDTE                                          
         MVC   P+39(13),=C'AUTHORIZATION'                                       
         MVC   P+54(15),APOAUT                                                  
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
         MVI   PC,TRMINATE                                                      
         BAS   RE,PRIN                                                          
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        HANDLE AMOUNT ACOAMT (PRINT SET-UP AND TOTALLING)                      
*---------------------------------------------------------                      
*                                                                               
*        ORDER AMOUNT ELEMENT IS ADDRESSED BY R7                                
*                                                                               
         USING OAMELD,R7                                                        
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
         DROP  R7                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        PRINT THE NARRATIVE                                                    
*---------------------------------------------------------                      
*                                                                               
* RECURSIVE ROUTINE TO SET UP AND PRINT NARRATIVE LINES FROM A RECORD           
* (ASSUMES ONLY ONE LEVEL OF NESTING IN USE OF IOAREAS)                         
*                                                                               
* RECORD (ORDER OR COMMENT) IS ADDRESSED BY WORD AT R1                          
* FLAG = FOOTLINE IF FOOTLINE COMMENTS REQUIRED, IF NOT = NULL                  
*                                                                               
PRINARR  NTR1                                                                   
         L     R5,0(R1)                                                         
         AH    R5,DATADISP                                                      
         SR    R0,R0                                                            
         LA    R3,CHOPIN                                                        
*                                                                               
PRINARR2 CLI   0(R5),0                                                          
         BNE   *+12                                                             
         BAS   RE,ANYPRIN                                                       
         B     EXIT                                                             
         CLI   0(R5),X'3E'                                                      
         BNE   PRINARR3                                                         
         USING ACOMMD,R5                                                        
         MVI   FLAG1,FOOTLINE      CHECK FOOTLINE REQUIREMENT                   
         NC    FLAG1,ACOMTYPE                                                   
         CLC   FLAG1,FLAG                                                       
         BE    PRINARR4                                                         
PRINARR3 ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     PRINARR2                                                         
*                                                                               
PRINARR4 TM    ACOMTYPE,4          NESTED - READ COMMENT RECORD                 
         BNO   PRINARR6            AND MAKE RECURSIVE CALL                      
         BAS   RE,ANYPRIN                                                       
         BAS   RE,PRBILINF                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(6),ACOMMENT                                                
         GOTO1 AREAD,AIOAREA2                                                   
         BNE   PRINARR6            ANY PROBLEM - TREAT AS STANDARD NARR         
         MVC   SFLAG,FLAG          SAVE/CLEAR FLAG FOR NESTED CALL              
         MVI   FLAG,0                                                           
         MVI   CLEN,72                                                          
         GOTO1 PRINARR                                                          
         MVC   FLAG,SFLAG                                                       
         B     PRINARR3                                                         
*                                                                               
PRINARR6 DS    0H                  STANDARD NARRATIVE                           
         CLC   ACOMMENT(2),=C'S='  CHECK FOR SPACE LINE REQUESIS                
         BNE   PRINARR8                                                         
         BAS   RE,ANYPRIN                                                       
         ZIC   R1,ACOMLEN                                                       
         SH    R1,=H'7'                                                         
         BM    PRINARR8            MISSING OR NON-NUMERIC TREATED AS            
         MVC   WORK(3),=3C'0'      STANDARD                                     
         EX    R1,PRINARRM                                                      
         CLC   WORK(3),=3C'0'                                                   
         BNE   PRINARR8                                                         
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
         BAS   RE,PRIN                                                          
         BCT   R1,*-4                                                           
         MVC   P+70(12),SAMT                                                    
         MVC   P+54(15),SCAT                                                    
         B     PRINARR3                                                         
PRINARRM MVZ   WORK(0),ACOMMENT+2                                               
PRINARRP PACK  DUB,ACOMMENT+2(0)                                                
*                                                                               
PRINARR8 CLI   ACOMMENT,C'#'       ALWAYS START A NEW LINE UNLESS               
         BNE   *+12                FIRST CHAR IS A '#'                          
         MVI   ACOMMENT,0                                                       
         B     *+8                                                              
         BAS   RE,ANYPRIN                                                       
         ZIC   R1,ACOMLEN                                                       
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ACOMMENT    MOVE COMMENT TO PRINT BUFFER AND             
         CLI   0(R3),C' '          IF LEADING SPACE THEN CHANGE                 
         BNE   *+8                 TO ZERO FOR INDENT.                          
         MVI   0(R3),0                                                          
         AR    R3,R1               BUMP POINTER                                 
         MVI   1(R3),C' '                                                       
         LA    R3,2(R3)                                                         
         B     PRINARR3                                                         
*                                                                               
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
ANYP2    ZIC   RF,CLEN                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P+1(0),0(R2)                                                     
         BAS   RE,PRIN                                                          
         ZIC   RF,CLEN                                                          
         LA    R2,0(RF,R2)                                                      
         BCT   R1,ANYP2                                                         
         XIT1  REGS=(R3)           PASS BACK A(PRINT BUFFER)                    
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        ROUTINE TO PRINT TO PRINT QUEUE                                        
*---------------------------------------------------------                      
*                                                                               
* HANDLES LINE AND PAGE CONTROL AND CALLS HEADS FOR HEADINGS                    
* ON ENTRY PC CONTAINS PRINT COMMAND CODE AND P CONTAINS PRINT LINE             
* ON EXIT P IS SPACE-FILLED                                                     
* ON ERROR CONTROL IS RETURNED DIRECT TO ROOT WITH PRINT SEQUENCE               
* TERMINATED                                                                    
*                                                                               
PRIN     NTR1                                                                   
         CLI   PC,INITIAL          INITIAL                                      
         BNE   PRINT2                                                           
         ZAP   PAGE,=P'0'                                                       
         ZAP   MAXLINES,=P'55'                                                  
         CLI   PROGPROF+1,0        PROFILE FOR MAXIMUM LINES                    
         BE    PRIN1                                                            
         ZIC   RF,PROGPROF+1                                                    
         CVD   RF,DUB                                                           
         ZAP   MAXLINES,DUB                                                     
PRIN1    ZAP   LINE,MAXLINES       FORCE HEAD CALL NEXT TIME THROUGH            
         B     PRINT5                                                           
*                                                                               
PRINT2   CLI   PC,EDICCC           PRINT EDICT CONTROL CARDS                    
         BNE   PRINT3                                                           
         MVI   PC,PR1SP1                                                        
         B     PRINT5                                                           
*                                                                               
PRINT3   CLI   PC,TRMINATE        END                                           
         BE    PRINT5                                                           
         CLI   PC,HEADOFF          NO HEADS (OR DOING HEADS)                    
         BE    PRINT5                                                           
*                                                                               
         CP    LINE,MAXLINES       NEED HEADS                                   
         BL    PRINT4              NOT YET                                      
*                                                                               
         AP    PAGE,=P'1'          THROW TO NEW PAGE & PRINT HEADS              
         ZAP   LINE,=P'0'                                                       
         MVC   SPC(133),PC                                                      
         BAS   RE,HEADS                                                         
         MVC   PC(133),SPC                                                      
PRINT4   AP    LINE,=P'1'                                                       
*                                                                               
PRINT5   GOTO1 VDATAMGR,DMCB,=C'DMPRINT',=C'PRTQUE',0,PC,ATIA                   
         CLI   PC,INITIAL                                                       
         BNE   PRINT7                                                           
*                                                                               
PRINT6   LA    RF,PC               SAVE REPORT ID AFTER INITIAL CALL            
         USING PQPLD,RF                                                         
         MVC   SUBID,QLSUBID                                                    
         MVC   REPNO,QLREPRNO                                                   
         DROP  RF                                                               
*                                                                               
PRINT7   MVC   P,SPACES                                                         
         CLC   SUBID,=C'APO'                                                    
         BE    PRIN7A                                                           
         CLC   SUBID,=C'LU1'                                                    
         BE    PRIN7A                                                           
         DC    H'0'                                                             
PRIN7A   TM    DMCB+8,X'FE'                                                     
         BNZ   PRINT8                                                           
         MVC   LASTPC,PC                                                        
         B     EXIT                                                             
*                                                                               
PRINT8   CLI   LASTPC,TRMINATE     IF ERROR ENSURE PRINT SEQUENCE               
         BE    PRINT9              TERMINATED                                   
         MVI   LASTPC,TRMINATE                                                  
         MVI   PC,TRMINATE                                                      
         MVC   WORK(1),DMCB+8                                                   
         BAS   RE,PRIN                                                          
         MVC   DMCB+8(1),WORK                                                   
PRINT9   L     RD,AWORK1           AND EXIT TO ROOT                             
         LM    RE,RC,12(RD)                                                     
         MVI   FERN,IOERROR                                                     
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        PRINT HEADINGS                                                         
*---------------------------------------------------------                      
IOBUFLN  EQU   2000                                                             
*                                                                               
HEADS    NTR1  WORK=(R5,IOBUFLN)   IN CASE YOU NEED A CT FILE READ              
         MVI   PC,HEADOFF                                                       
         XC    P,P                                                              
         BAS   RE,PRIN                                                          
         MVI   PC,SP2              HEADS ON LINE 3 & 4                          
         BAS   RE,PRIN                                                          
         BAS   RE,PRIN                                                          
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
*                                                                               
HEAD02   CLI   0(R4),X'36'         FROM 'CTGENFILE'                             
         BE    HEAD04                                                           
         CLI   0(R4),0                                                          
         BE    HEAD06                                                           
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     HEAD02                                                           
*                                                                               
HEAD04   LA    R1,CHOPIN           PRINT COMPANY NAME/ADDRESS                   
         MVC   CHOPIN(110),SPACES                                               
         CLI   PROGPROF+4,1                                                     
         BE    *+14                                                             
         MVC   0(L'CTORGNAM,R1),CTORGNAM-CTORGD(R4)  NAME                       
         LA    R1,L'CTORGNAM+1(R1)                                              
         MVC   0(L'CTORGADD,R1),CTORGADD-CTORGD(R4)  ADDRESS                    
         GOTO1 VSQUASH,DMCB,CHOPIN,80                                           
         MVC   P+1(76),CHOPIN                                                   
         BAS   RE,PRIN                                                          
*                                                                               
HEAD06   LA    RF,P+78                                                          
         CLI   PSTAT,PRINTING      AM I PRINTING                                
         BE    *+8                  YES, DO NOT MOVE PAGE                       
         LA    RF,P+70              NO, MOVE PAGE BACK TO FIT 80 COLS           
         MVC   0(4,RF),=C'PAGE'                                                 
         EDIT  PAGE,(1,5(RF))                                                   
         BAS   RE,PRIN                                                          
         MVC   P+58(6),APONUM                                                   
         BAS   RE,PRIN                                                          
*                                                                               
HEAD08   ZIC   R1,PROGPROF+8       START LINE NO.                               
         CVD   R1,DUB                                                           
         CP    DUB,LINE                                                         
         BNH   HEAD10                                                           
         MVI   PC,SP2                                                           
         BAS   RE,PRIN                                                          
         B     HEAD08                                                           
*                                                                               
HEAD10   MVI   PC,PR1SP1                                                        
         CLI   PROGPROF+5,C'L'                                                  
         BNE   HEAD22                                                           
         EJECT                                                                  
* PUT SUPPLIER ON THE LEFT & CLIENT ON THE RIGHT                                
*                                                                               
         MVC   P+1(3),=C'TO:'             SUPPLIER NAME - DATE                  
         MVC   P+7(L'LSUPNAME),LSUPNAME                                         
         MVC   P+39(4),=C'DATE'                                                 
         GOTO1 VDATCON,DMCB,(1,ACORDATE),(8,P+47)                               
         BAS   RE,PRIN                                                          
*                                                                               
         LA    R5,SUPADDEL                SUPPLIER ADDRESS1 - CLIENT?           
         USING ACADDD,R5                                                        
         ZIC   R4,ACADLNES                NUMBER OF ADDRESS LINES               
         LTR   R4,R4                                                            
         BZ    *+16                                                             
         MVC   P+7(L'ACADADD),ACADADD                                           
         BCTR  R4,0                                                             
         LA    R5,L'ACADADD(R5)                                                 
         CLI   TYPE,PRODN                                                       
         BNE   HEAD12                                                           
         MVC   P+39(6),=C'CLIENT'                                               
         MVC   P+47(6),APOCLI                                                   
         MVC   P+54(L'LCLINAME),LCLINAME                                        
*                                                                               
HEAD12   BAS   RE,PRIN                    SUPPLIER ADDRESS2 - PRODUCT?          
         LTR   R4,R4                                                            
         BZ    *+16                                                             
         MVC   P+7(L'ACADADD),ACADADD                                           
         BCTR  R4,0                                                             
         LA    R5,L'ACADADD(R5)                                                 
         CLI   TYPE,PRODN                                                       
         BNE   HEAD14                                                           
         MVC   P+39(7),=C'PRODUCT'                                              
         MVC   P+47(6),APOPRO                                                   
         MVC   P+54(L'LPRONAME),LPRONAME                                        
*                                                                               
HEAD14   BAS   RE,PRIN                    SUPPLIER ADDRESS3 - JOB?              
         LTR   R4,R4                                                            
         BZ    *+16                                                             
         MVC   P+7(L'ACADADD),ACADADD                                           
         BCTR  R4,0                                                             
         LA    R5,L'ACADADD(R5)                                                 
         CLI   TYPE,PRODN                                                       
         BNE   HEAD16                                                           
         MVC   P+39(3),=C'JOB'                                                  
         MVC   P+47(6),APOJOB                                                   
         MVC   P+54(L'LJOBNAME),LJOBNAME                                        
         BAS   RE,PRIN                                                          
*                                                                               
         LTR   R4,R4                      SUPPLIER ADDRESS4                     
         BZ    *+10                                                             
         MVC   P+7(L'ACADADD),ACADADD                                           
         BAS   RE,PRBILINF                                                      
         CLC   PRBILLS,SPACES                                                   
         BNH   HEAD16                                                           
         MVC   P+39(L'PRBILLS),PRBILLS    PRINT ON BILLS                        
*                                                                               
HEAD16   BAS   RE,PRIN                                                          
         CLC   APOATTN,SPACES                                                   
         BNH   HEAD18                                                           
         MVC   P+1(5),=C'ATTN:'                                                 
         MVC   P+7(L'APOATTN),APOATTN     ATTENTION                             
         BAS   RE,PRIN                                                          
*                                                                               
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
         BAS   RE,PRIN                                                          
         B     *+8                                                              
*                                                                               
HEAD20   BAS   RE,PRIN                                                          
         MVI   PC,SP2                                                           
         BAS   RE,PRIN                                                          
         B     HEAD36                                                           
         DROP  R5                                                               
         EJECT                                                                  
* SUPPLIER ON THE RIGHT & CLIENT ON THE LEFT                                    
*                                                                               
HEAD22   LA    R5,SUPADDEL                                                      
         USING ACADDD,R5                                                        
         ZIC   R4,ACADLNES                DATE - SUPPLIER NAME                  
         MVC   P+52(3),=C'TO:'                                                  
         MVC   P+58(22),LSUPNAME                                                
         MVC   P+1(4),=C'DATE'                                                  
         GOTO1 VDATCON,DMCB,(1,ACORDATE),(8,P+8)                                
*                                                                               
         BAS   RE,PRIN                    CLIENT? - SUPPLIER ADDRESS1           
         LTR   R4,R4                                                            
         BZ    *+16                                                             
         MVC   P+58(L'ACADADD),ACADADD                                          
         BCTR  R4,0                                                             
         LA    R5,L'ACADADD(R5)                                                 
         CLI   TYPE,PRODN                                                       
         BNE   HEAD24                                                           
         MVC   P+1(6),=C'CLIENT'                                                
         MVC   P+8(3),APOCLI                                                    
         MVC   P+16(L'LCLINAME),LCLINAME                                        
*                                                                               
HEAD24   BAS   RE,PRIN                    PRODUCT? - SUPPLIER ADDRESS2          
         LTR   R4,R4                                                            
         BZ    *+16                                                             
         MVC   P+58(L'ACADADD),ACADADD                                          
         BCTR  R4,0                                                             
         LA    R5,L'ACADADD(R5)                                                 
         CLI   TYPE,PRODN                                                       
         BNE   HEAD26                                                           
         MVC   P+1(5),=C'PROD.'                                                 
         MVC   P+8(3),APOPRO                                                    
         MVC   P+16(L'LPRONAME),LPRONAME                                        
*                                                                               
HEAD26   BAS   RE,PRIN                    JOB - SUPPLIER ADDRESS3               
         LTR   R4,R4                                                            
         BZ    *+16                                                             
         MVC   P+58(L'ACADADD),ACADADD                                          
         BCTR  R4,0                                                             
         LA    R5,L'ACADADD(R5)                                                 
         CLI   TYPE,PRODN                                                       
         BNE   HEAD28                                                           
         MVC   P+1(3),=C'JOB'                                                   
         MVC   P+8(6),APOJOB                                                    
         MVC   P+16(L'LJOBNAME),LJOBNAME                                        
*                                                                               
HEAD28   BAS   RE,PRIN                    SUPPLIER ADDRESS4                     
         LTR   R4,R4                                                            
         BZ    *+10                                                             
         MVC   P+58(L'ACADADD),ACADADD                                          
         BAS   RE,PRBILINF                                                      
         CLC   PRBILLS,SPACES             PRINT ON BILLS                        
         BNH   HEAD30                                                           
         MVC   P+1(L'PRBILLS),PRBILLS                                           
         BAS   RE,PRIN                                                          
*                                                                               
HEAD30   CLC   APOATTN,SPACES                                                   
         BNH   HEAD32                                                           
         MVC   P+52(5),=C'ATTN:'          ATTENTION                             
         MVC   P+58(L'APOATTN),APOATTN                                          
         BAS   RE,PRIN                                                          
*                                                                               
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
         BAS   RE,PRIN                                                          
         MVI   PC,SP2                                                           
         B     *+8                                                              
*                                                                               
HEAD34   MVI   PC,SP3              3 BLANKS                                     
         BAS   RE,PRIN                                                          
         MVI   PC,SP2              2 BLANKS                                     
         BAS   RE,PRIN                                                          
*                                                                               
HEAD36   MVI   PC,PR1SP1                                                        
         CLI   TYPE,PRODN          HANDLE PRODUCTION ORDER                      
         BE    HEAD38                                                           
*                                                                               
*              EXPENSE ORDERS ARE DIFFERENT                                     
*                                                                               
         CLI   PROGPROF,C'N'       SUPPRESS ORDER TOTAL                         
         BE    HEADX                                                            
*                                                                               
         USING OAMELD,R7                                                        
         MVI   ELCODE,OAMELQ       GET OAMELQ FROM AORDER                       
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   TOTAL,OAMAMNT                                                    
         B     HEAD46              ONLY ONE AMOUNT FOR EXPENSE                  
         DROP  R7                                                               
*                                                                               
HEAD38   CP    PAGE,=P'1'          W/C DETAIL ON PAGE 1 ONLY                    
         BNE   HEADX                                                            
         CLI   PROGPROF+6,2                                                     
         BNE   HEAD40                                                           
         CLI   PROGPROF,C'Y'                                                    
         BNE   HEADX                                                            
         B     HEAD42                                                           
*                                                                               
HEAD40   BAS   RE,PRIN                                                          
         MVC   P+1(13),=C'WORK CATEGORY'                                        
         CLI   PROGPROF+6,0                                                     
         BNE   HEAD42              SHOW MONEY AND CODES                         
         CLC   =C'PN',APOACT                                                    
         BE    HEAD42              SHOW NO MONEY THIS TIME                      
         MVC   P+44(6),=C'AMOUNT'                                               
*                                                                               
         USING OAMELD,R7                                                        
HEAD42   BAS   RE,PRIN                                                          
         MVI   ELCODE,OAMELQ       GET OAMELQ FROM AORDER                       
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
HEAD44   GOTO1 AGETWC,OAMWORK                                                   
         MVC   P+1(2),OAMWORK                                                   
         MVC   P+6(15),WORK                                                     
         BAS   RE,AMOUNT                                                        
         MVI   PC,PR1SP1                                                        
         CLI   PROGPROF+6,2                                                     
         BNE   *+14                                                             
         MVC   P,SPACES                                                         
         B     *+8                                                              
         BAS   RE,PRIN                                                          
         BAS   RE,NEXTEL                                                        
         BE    HEAD44                                                           
         CLI   PROGPROF+6,1                                                     
         BNL   HEADX                                                            
         DROP  R7                                                               
*                                                                               
HEAD46   CP    TOTAL,=P'0'                                                      
         BE    HEADX                                                            
         EDIT  TOTAL,(10,P+40),2                                                
         ZAP   TOTAL,=P'0'                                                      
         MVC   P+25(12),=C'TOTAL AMOUNT'                                        
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
HEADX    BAS   RE,PRIN             SUB1                                         
         MVI   PC,SP2                                                           
         BAS   RE,PRIN             SUB2                                         
         BAS   RE,PRIN                                                          
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        EXITS TO ROOT                                                          
*-----------------------------------------------------------                    
CHKFXNUM NTR1                                                                   
         MVI   FERN,OK                                                          
         OC    FAXNUM,FAXNUM       ANYTHING ON SUPPLIER?                        
         BNZ   OKXIT               YES                                          
         OC    FAXPARM,FAXPARM     ANYTHING IN ACTION PARMS                     
         BNZ   OKXIT               YES                                          
         NI    STATUS,X'FF'-FAX_IT CANT FAX WITH NO NUMBER                      
         B     ERRXIT                                                           
*                                                                               
*-----------------------------------------------------------                    
CHKEMAIL NTR1                                                                   
         MVI   FERN,OK                                                          
         CLI   EMAILADL,0              ANYTHING ON SUPPLIER?                    
         BNE   OKXIT                    YES                                     
         CLI   EMLPARML,0              ANYTHING IN ACTION PARMS                 
         BNE   OKXIT                    YES                                     
         NI    STATUS,X'FF'-EMAIL_IT   CANT EMAIL WITH NO ADDRESS               
         B     ERRXIT                                                           
*                                                                               
*-----------------------------------------------------------                    
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
*                                                                               
*-----------------------------------------------------------                    
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
*                                                                               
*-----------------------------------------------------------                    
EMAILMSG NTR1                                                                   
         MVI   FERN,OK                                                          
         MVC   MSG,EMAILMES                                                     
         MVC   MSG+6(6),APONUM                                                  
         B     EXIT                                                             
EMAILMES DC    CL60'ORDER NNNNNN EMAILED - ENTER NEXT ACTION'                   
*                                                                               
*-----------------------------------------------------------                    
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
*                                                                               
         MVC   MSG+51(3),SUBID           MOVE REPORT ID INTO MSG                
         EDIT  REPNO,(4,MSG+55),ALIGN=LEFT                                      
*                                                                               
         B     EXIT                                                             
PFXMESS  DC    CL60'ORDER NNNNNN FAXED TO                - AND QUEUED- X        
                  ,     '                                                       
*-----------------------------------------------------------                    
PRTEMMSG NTR1                                                                   
         MVI   FERN,OK                                                          
         MVC   MSG,PEMMESS                                                      
         MVC   MSG+6(6),APONUM                                                  
*                                                                               
         MVC   MSG+35(3),SUBID     MOVE REPORT ID INTO MSG                      
         EDIT  REPNO,(4,MSG+39),ALIGN=LEFT                                      
*                                                                               
         B     EXIT                                                             
PEMMESS  DC    CL60'ORDER NNNNNN EMAILED - AND QUEUED-    ,     '               
*                                                                               
*----------------------------------------------------------------------         
*        FORMAT THE PHONE NUMBER AT 0(R1) INTO 0(R2)                            
*----------------------------------------------------------------------         
*                                                                               
FORMFONE CLI   0(R1),C'1'                                                       
         BNE   FF50                                                             
*                                                                               
         MVI   0(R2),C'1'                                                       
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
*                                                                               
FF50     MVC   0(09,R2),=C'(   )   -'                                           
         MVC   1(03,R2),0(R1)                                                   
         MVC   5(03,R2),3(R1)                                                   
         MVC   9(04,R2),6(R1)                                                   
         BR    RE                                                               
*                                                                               
GETELIO  L     R7,AORDER                                                        
         GETEL (R7),DATADISP,ELCODE                                             
*                                                                               
OKXIT    SR    RB,RB               CC = EQU                                     
ERRXIT   LTR   RB,RB               CC = NEQ                                     
EXIT     XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------                      
*        GET 'PRINT  ON BILLS INF' FROM JOB                                     
*---------------------------------------------------------                      
*                                                                               
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
PRBIL100 CLI   0(RE),X'24'         PROFILE ELEMENT                              
         BE    PRBIL200                                                         
         CLI   0(RE),0                                                          
         BE    PRBIL300                                                         
         IC    RF,1(RE)                                                         
         LA    RE,0(RF,RE)                                                      
         B     PRBIL100                                                         
PRBIL200 MVC   PRBILLS,ACPRBLPR-ACPROFD(RE)                                     
         B     PRBILX                                                           
PRBIL300 MVC   PRBILLS,SPACES                                                   
PRBILX   B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        LITERALS                                                               
*---------------------------------------------------------                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------- - - - - - - - - - - - - *                   
*        CHECK IF VALID EMAIL ADDRESS                                           
*-----------------------------------------------------------*                   
EMVAL    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZIC   R2,EMLPARML         EMAIL LENGTH                                 
         LA    R3,EMLPARM          POINT TO E-MAIL ADDRESS.                     
         MVI   BYTE,0              INIT FLAG                                    
*                                                                               
         CHI   R2,60               IS E-MAIL MORE THAN 60 CHARS                 
         BH    EMVALERR            YES, IT'S AN ERROR                           
         CHI   R2,7                SHLD ATLST LOOK LIKE X@X.XXX                 
         BL    EMVALERR                                                         
*                                                                               
EMVAL10  DS    0H                                                               
         LR    R4,R3               EXTRA POINTER TO EMAIL ADDRS                 
         SHI   R2,4                WANT TO POINT TO .XXX                        
         AR    R4,R2                                                            
         CLI   0(R4),C'.'          IS IT DOT SOMETHING                          
         BNE   EMVALERR                                                         
         LHI   R0,3                VALIDATE .COM, .EDU ETC                      
EMVAL15  LA    R4,1(R4)                                                         
         CLI   0(R4),C'A'          IS IT HIGHER THAN A                          
         BL    EMVALERR                                                         
         CLI   0(R4),C'Z'          SHOULD BE Z OR LOWER                         
         BH    EMVALERR                                                         
         BCT   R0,EMVAL15                                                       
         AHI   R2,4                E-MAIL'S LENGTH                              
*                                                                               
         OI    BYTE,SPCCHAR        CAN'T START WITH SPECIAL CHAR                
*                                                                               
EMVAL20  CLI   0(R3),C'0'          IS IT LESS THAN F0                           
         BL    EMVAL30             YES CHK NEXT                                 
         CLI   0(R3),C'9'          IS IT HIGHER THAN F9                         
         BNH   EMVAL40             IT IS A NUMBER                               
         B     EMVALERR                                                         
EMVAL30  CLI   0(R3),C'A'          IS IT HIGHER THAN A                          
         BL    EMVAL50             CHECK FOR SPECIAL CHARS                      
         CLI   0(R3),C'Z'          SHOULD BE Z OR LOWER                         
         BH    EMVALERR                                                         
*                                                                               
EMVAL40  NI    BYTE,X'FF'-SPCCHAR  LAST CHAR WAS ALL PURPOSE CHAR               
*                                                                               
EMVAL41  LA    R3,1(R3)            GET NEXT CHAR                                
         BCT   R2,EMVAL20                                                       
         B     EMVAL100                                                         
*      ----------------------------                                             
EMVAL50  DS    0H                                                               
         CLI   0(R3),C'@'          HAVE WE REACHED @ YET                        
         BNE   EMVAL54                                                          
         TM    BYTE,ATFOUND        MAKE SURE NO MORE THAN ONE @ SIGN            
         BO    EMVALERR                                                         
         OI    BYTE,ATFOUND        NOW WE HAVE ONE @ IN E-MAIL                  
*                                                                               
EMVAL54  LA    R1,EXCTAB           POINT TO SPECIAL CHARS TABLE                 
EMVAL55  CLI   0(R1),X'FF'         DID WE FIND SPECIAL CHARS                    
         BE    EMVALERR            NO SPCL CHAR FND, ERROR.                     
         CLC   0(1,R1),0(R3)       IS IT SPCL CHAR                              
         BNE   EMVAL60                                                          
*                                                                               
         TM    BYTE,SPCCHAR        NO TWO SPECIAL CHARS IN ROW                  
         BO    EMVALERR                                                         
         OI    BYTE,SPCCHAR        LAST CHAR WAS SPEC CHAR                      
         B     EMVAL41                                                          
*                                                                               
EMVAL60  LA    R1,1(R1)            POINT TO NEXT TABLE ENTRY                    
         B     EMVAL55                                                          
*                                                                               
EMVAL100 TM    BYTE,ATFOUND        SHOULD HAVE ONE @ IN E-MAIL                  
         BZ    EMVALERR            NO @ FOUND ERROR                             
         CR    RB,RB                                                            
         B     EMVALX                                                           
*                                                                               
EMVALERR CR    RB,RD                                                            
EMVALX   XIT1                                                                   
*                                                                               
EXCTAB   DC    C'@'                @ SIGN                                       
         DC    C'_'                UNDERSCORE                                   
         DC    C'.'                DOT                                          
         DC    X'FF'               END OF TABLE                                 
*                                                                               
ATFOUND  EQU   X'01'               @ SYMBOL FOUND IN EMAIL ADDRESS              
SPCCHAR  EQU   X'02'               LAST CHAR WAS A SPECIAL EMAIL CHAR           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*---------------------------------------------------------                      
*        ACORDDSECT                                                             
*---------------------------------------------------------                      
*                                                                               
       ++INCLUDE ACORDDSECT                                                     
*                                                                               
ACORDFXD DSECT                                                                  
       ++INCLUDE ACORDFAX                                                       
         EJECT                                                                  
*---------------------------------------------------------                      
*        OVERLAY WORKING STORAGE AT ASAVE                                       
*---------------------------------------------------------                      
*                                                                               
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
*                                                                               
STATUS   DS    CL1                 WHAT TO DO                                   
NOTHING  EQU   X'00'                                                            
FAX_IT   EQU   X'01'                                                            
PRINT_IT EQU   X'02'                                                            
EMAIL_IT EQU   X'04'                                                            
*                                                                               
PSTAT    DS    CL1                 WHAT I AM DOING                              
FAXING   EQU   X'01'                                                            
PRINTING EQU   X'02'                                                            
EMAILING EQU   X'04'                                                            
*                                                                               
FAXPARM  DS    CL11                FAX= VALUE PASSED IN ACTSCAN                 
EMLPARM  DS    CL(MAXESCAN)        EMAIL= VALUE PASSED IN ACTSCAN (60)          
EMLPARML DS    X                   LENGTH OF EMAIL IN ACTSCAN                   
*                                                                               
PRBILLS  DS    CL38                PRINT ON BILLS FROM PROFILE                  
*                                                                               
CHOPIN   DS    1000C               CHOPPER IN                                   
CHOPOUT  DS    (CHOPOLN)C          CHOPPER OUT                                  
CHOPOLN  EQU   1440                                                             
*                                                                               
LWSX     DS    0C                                                               
*                                  PRINT COMMAND CODES                          
INITIAL  EQU   X'00'                                                            
EDICCC   EQU   X'01'               PRINT AN EDICT CONTROL CARD                  
HEADOFF  EQU   X'89'                                                            
PR1SP1   EQU   X'09'                                                            
SP2      EQU   X'0B'                                                            
SP3      EQU   X'19'                                                            
TRMINATE EQU   X'FF'                                                            
*                                                                               
KEEP     EQU   X'08'               PQ STATUS (KEEP AFTER PRINTING)              
ACTV     EQU   X'80'               PQ STAT, CREATION COMPLETE, ACTIVE           
         EJECT                                                                  
*----------------------------------------                                       
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
*----------------------------------------                                       
DDSD     DSECT                                                                  
DDD      DS    CL5                                                              
         DS    CL1                                                              
DDSYS    DS    CL2                                                              
DDREP    DS    CL3                                                              
DDTRN    DS    CL3                                                              
         DS    CL1                                                              
DDUSER   DS    CL58                                                             
*----------------------------------------                                       
*                                                                               
* ACGENBOTH                                                                     
* CTGENFILE                                                                     
* FAFACTS                                                                       
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054ACORD03   10/17/18'                                      
         END                                                                    
