*          DATA SET SPSFM51    AT LEVEL 006 AS OF 03/08/10                      
*PHASE T21751A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21751  -- PRODUCT LIST                              *         
*                                                                     *         
*  COMMENTS:     LISTS PRODUCT RECORDS                                *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SPSFM71 (LIST)                                *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21751 - PRODUCT LIST'                                          
T21751   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1751**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT BASED ON LIST                   
         BNE   XIT                                                              
         LA    R5,HDHOOK                                                        
         ST    R5,HEADHOOK                                                      
         CLI   SVAPROF+7,C'C'                                                   
         BE    *+16                                                             
         LA    R5,HEDSPECS                                                      
         ST    R5,SPECS                                                         
         B     LR                                                               
*                                                                               
         LA    R5,HEDSPEC2         IF CANADIAN AGENCY                           
         ST    R5,SPECS                                                         
         B     LR                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
*                                                                               
VK       XC    FILTFLAG,FILTFLAG                                                
         XC    PROKEY,PROKEY                                                    
         MVC   PLSMEDN,SPACES        CLEAR MEDIA NAME, CLIENT NAME AND          
         OI    PLSMEDNH+6,X'80'      PRODUCT NAME FROM SCREEN                   
         MVC   PLSCLIN,SPACES                                                   
         OI    PLSCLINH+6,X'80'                                                 
         MVC   PLSPRON,SPACES                                                   
         OI    PLSPRONH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,PLSMEDKH           MEDIA                                      
         GOTO1 VALIMED               VALIDATE MEDIA CODE AND TRANSMIT           
         MVC   PLSMEDN,MEDNM         MEDIA NAME                                 
         OI    PLSMEDNH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         XC    KEY,KEY               BEGIN TO SET UP KEY FOR READHIGH           
         LA    R6,KEY                                                           
         USING PRDHDR,R6                                                        
         MVI   PKEYTYPE,X'00'        RECORD TYPE X'00'                          
         MVC   PKEYAM,BAGYMD         VALIDATED BINARY MEDIA CODE                
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,PLSCLIKH           CLIENT                                     
         GOTO1 VALICLT               VALIDATE CLIENT CODE, STORE BINARY         
         MVC   PKEYCLT,BCLT          REP INTO KEY AND TRANSMIT CLIENT           
         MVC   PLSCLIN,CLTNM         NAME                                       
         OI    PLSCLINH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
* VALIDATE FILTERS                                                              
*                                                                               
         CLI   PLSFILH+5,0           IS FILTER OPTION BLANK?                    
         BE    VK17                  YES                                        
         BAS   RE,VALFLTR                                                       
*                                                                               
*                                    INITIALIZE PRODUCT PART OF KEY FOR         
VK17     MVC   PKEYPRD(2),=C'AA'     READHIGH                                   
*                                                                               
         ZICM  R1,PLSPROKH+5,1       USER FILTERING ON PRODUCT?                 
         BZ    VK20                                                             
         LA    RE,2                  YES ... MOVE PRODUCT CODE INTO KEY         
         CR    R1,RE                                                            
         BNE   *+8                                                              
         MVI   PKEYPRD+2,C' '                                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PKEYPRD(0),PLSPROK                                               
*                                                                               
***********************************************************************         
*                                                                               
VK20     MVC   PLSPRON,SPACES        INITIALIZE PRODUCT NAME TO SPACES          
         OI    PLSPRONH+6,X'80'                                                 
*                                                                               
         CLI   PLSPROKH+5,0          USER FILTERING ON PRODUCT?                 
         BE    VKX                   NO                                         
*                                                                               
         GOTO1 HIGH                  YES ... IF VALID PRODUCT CODE              
         CLC   KEY(4),KEYSAVE                                                   
         BNE   LRX                                                              
         CLC   KEY(7),KEYSAVE        MOVE PRODUCT NAME TO SCREEN                
         BNE   VKX                                                              
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R6,AIO                                                           
         MVC   PLSPRON,PNAME                                                    
         OI    PLSPRONH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
***********************************************************************         
*                                                                               
VKX      MVC   PROKEY,KEY            PREPARE TO LIST RECORDS                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
*                                                                               
LR       MVI   NLISTS,13             LIST 14 RECORDS PER SCREEN                 
*                                                                               
         USING PKEY,R4                                                          
         LA    R4,KEY                                                           
*                                                                               
         CLI   SVAPROF+7,C'C'        IF CANADIAN AGENCY ... ADD                 
         BNE   LR05                  HEADERS FOR GST AND PST CODES              
         MVC   PLSGST,=C'GST'                                                   
         MVC   PLSPST,=C'PST'                                                   
         MVC   PLSGSTU,=C'---'                                                  
         MVC   PLSPSTU,=C'---'                                                  
         OI    PLSGSTH+6,X'80'                                                  
         OI    PLSPSTH+6,X'80'                                                  
         OI    PLSPSTUH+6,X'80'                                                 
         OI    PLSGSTUH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
LR05     OC    KEY,KEY                                                          
         BNZ   LR10                                                             
*                                                                               
         MVC   KEY,PROKEY            READHIGH FIRST PRODUCT IN LIST             
LR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R4,KEY                READ SEQUENTIAL ALL RECORDS AFTER          
         GOTO1 SEQ                   FIRST PRODUCT                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
*                                                                               
***********************************************************************         
*                                                                               
LR30     CLC   KEY(4),PROKEY         LIST ALL PRODUCT RECORDS UNTIL             
         BNE   LRX                   CHANGE OF CLIENT                           
*                                                                               
         OC    KEY+4(3),KEY+4        FILTER OUT CLIENT RECORDS                  
         BZ    LR20                                                             
         OC    KEY+7(6),KEY+7        FILTER OUT ESTIMATE RECORDS                
         BNZ   LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
         GOTO1 GETREC                GET PRODUCT RECORD                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
*                                                                               
         TM    FILTFLAG,FFCPPYQ      DID USER WANT A FILTER ON CPPRS?           
         BNO   *+12                  NO                                         
         CLI   PCPPRS,0                                                         
         BNE   LR20                                                             
*                                                                               
         TM    FILTFLAG,FFCPPNQ                                                 
         BNO   *+12                                                             
         CLI   PCPPRS,C'N'                                                      
         BNE   LR20                                                             
*                                                                               
         TM    FILTFLAG,FFTHTRYQ   DO WE WANT THEATRICAL ONLY?                  
         BZ    *+12                NO - SKIP THIS CHECK                         
         TM    POPT1,POPT1_THTR    PRODUCT THEATRICAL?                          
         BZ    LR20                NO - SKIP IT                                 
*                                                                               
         TM    FILTFLAG,FFTHTRNQ   NON-THEATRICAL ONLY?                         
         BZ    *+12                                                             
         TM    POPT1,POPT1_THTR                                                 
         BO    LR20                SKIP IF THEATRICAL                           
*                                                                               
         TM    FILTFLAG,FFTHNTYQ   DO WE WANT THNT ONLY?                        
         BZ    *+12                NO - SKIP THIS CHECK                         
         TM    POPT1,POPT1_THNT    PRODUCT THNT?                                
         BZ    LR20                NO - SKIP IT                                 
*                                                                               
         TM    FILTFLAG,FFTHNTNQ   NON-THNT ONLY?                               
         BZ    *+12                                                             
         TM    POPT1,POPT1_THNT                                                 
         BO    LR20                SKIP IF THNT                                 
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LSPRDC,PKEYPRD        COPY PRODUCT CODE AND PRODUCT NAME         
         MVC   LSPRDN,PNAME          INTO LIST LINE                             
*                                                                               
         CLI   PACCT,X'FF'                                                      
         BNE   LR40                                                             
         UNPK  LSPRCD(5),PACCT+1(3)                                             
         B     LR50                                                             
LR40     MVC   LSPRCD(4),PACCT       COPY CLT/PRD CODE INTO LIST LINE           
*                                                                               
LR50     MVC   FULL,SPACES                                                      
         CLI   PCLASS,0                                                         
         BE    LR60                                                             
         MVC   FULL(1),PCLASS                                                   
         MVI   FULL+1,C' '                                                      
         CLI   PCLASS,X'99'                                                     
         BH    LR60                                                             
         PACK  FULL(1),PCLASS                                                   
         NI    FULL,X'0F'                                                       
         OI    FULL,X'C0'                                                       
         MVC   FULL+1(1),PCLASS                                                 
         NI    FULL+1,X'0F'                                                     
         OI    FULL+1,X'C0'                                                     
LR60     MVC   LSPRCLS,FULL          COPY PRODUCT CLASS INTO LIST LINE          
*                                                                               
         CLI   SVAPROF+7,C'C'        IF CANADIAN AGENCY...                      
         BNE   LR65                  COPY GST CODE AND WHETHER OR NOT           
         MVC   LSPRGST,PGSTCODE      THERE'S A PST CODE INTO LIST LINE          
         OC    PPST,PPST                                                        
         BZ    LR65                                                             
         MVC   LSPRPST,=C'YES'                                                  
*                                                                               
LR65     CLI   MODE,PRINTREP                                                    
         BNE   LR70                                                             
         MVC   P,SPACES                                                         
         MVC   PPRD,LSPRDC                                                      
         MVC   PPRDNAME,LSPRDN                                                  
         MVC   PCPCODE,LSPRCD                                                   
         MVC   PRCLASS,LSPRCLS                                                  
         CLI   SVAPROF+7,C'C'        IF CANADIAN AGENCY ...                     
         BNE   LR68                  COPY GST CODE AND WHETHER OR NOT           
         MVC   PRGST,LSPRGST         THERE'S A PST CODE INTO REPORT             
         MVC   PRPST,LSPRPST                                                    
*                                                                               
LR68     GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR20                                                             
*                                                                               
LR70     GOTO1 LISTMON                                                          
         B     LR20                                                             
*                                                                               
***********************************************************************         
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                          VALFLTR                                    *         
***********************************************************************         
VALFLTR  NTR1                                                                   
*                                                                               
         LA    R2,PLSFILH            CURSOR ON FILTER HEADER FOR ERROR          
         GOTO1 SCANNER,DMCB,PLSFILH,(2,SBLOCK)                                  
         ZIC   R4,4(R1)              TOTAL NUMBER TO PROCESS                    
         LA    R3,SBLOCK                                                        
         USING SCANBLKD,R3                                                      
*                                                                               
VFTR10   CLI   0(R3),X'00'          DID WE REACH THE END OF THE TABLE?          
         BE    VFTRX                YES...ERROR                                 
         ZIC   R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
         CHI   R1,0                                                             
         BL    ERRFNF                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),=C'CPPRS'                                            
         BNE   VFTR20                                                           
*                                                                               
         TM    FILTFLAG,FFCPPYQ+FFCPPNQ FILTER VALUE ALREADY SET?               
         BNZ   ERRDUP               YES - DISPLAY ERROR                         
         CLI   SC2NDLEN,1           DOES L'RIGHT OPERAND =1?                    
         BNE   ERRLEN2              NO...ERROR                                  
*                                                                               
         CLI   SC2NDFLD,C'Y'                                                    
         BNE   *+12                                                             
         OI    FILTFLAG,FFCPPYQ                                                 
         B     VFTR500             NEXT FILTER                                  
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   *+12                                                             
         OI    FILTFLAG,FFCPPNQ                                                 
         B     VFTR500             NEXT FILTER                                  
*                                                                               
         B     ERRFNF                                                           
*                                                                               
VFTR20   DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),=C'THTR'                                             
         BNE   VFTR30                                                           
*                                                                               
         TM    FILTFLAG,FFTHTRYQ+FFTHTRNQ FILTER VALUE ALREADY SET?             
         BNZ   ERRDUP               YES - DISPLAY ERROR                         
         CLI   SC2NDLEN,1           DOES L'RIGHT OPERAND =1?                    
         BNE   ERRLEN2              NO...ERROR                                  
*                                                                               
         CLI   SC2NDFLD,C'Y'                                                    
         BNE   *+12                                                             
         OI    FILTFLAG,FFTHTRYQ                                                
         B     VFTR500             NEXT FILTER                                  
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   *+12                                                             
         OI    FILTFLAG,FFTHTRNQ                                                
         B     VFTR500             NEXT FILTER                                  
*                                                                               
         B     ERRFNF                                                           
*                                                                               
VFTR30   DS    0H                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),=C'THNT'                                             
         BNE   ERRFNF                                                           
*                                                                               
         TM    FILTFLAG,FFTHNTYQ+FFTHNTNQ FILTER VALUE ALREADY SET?             
         BNZ   ERRDUP               YES - DISPLAY ERROR                         
         CLI   SC2NDLEN,1           DOES L'RIGHT OPERAND =1?                    
         BNE   ERRLEN2              NO...ERROR                                  
*                                                                               
         CLI   SC2NDFLD,C'Y'                                                    
         BNE   *+12                                                             
         OI    FILTFLAG,FFTHNTYQ                                                
         B     VFTR500             NEXT FILTER                                  
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   *+12                                                             
         OI    FILTFLAG,FFTHNTNQ                                                
         B     VFTR500             NEXT FILTER                                  
*                                                                               
         B     ERRFNF                                                           
*                                                                               
VFTR500  DS    0H                                                               
         LA    R3,SCBLKLQ(R3)                                                   
         BCT   R4,VFTR10                                                        
*                                                                               
VFTRX    DS    0H                                                               
         B     XIT                                                              
         DROP  R3                                                               
*                                                                               
***********************************************************************         
*        MEDIA TABLE                                                  *         
***********************************************************************         
*                                                                               
MEDTAB   DC   CL1'T',XL1'01'                                                    
MEDTABLQ EQU  *-MEDTAB                                                          
         DC   CL1'R',XL1'02'                                                    
         DC   CL1'N',XL1'03'                                                    
         DC   CL1'X',XL1'04'                                                    
         DC   CL1'C',XL1'08'                                                    
         DC   X'FF'                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        FILTER TABLE                                                 *         
***********************************************************************         
CPPRSTAB DC   X'05',CL5'CPPRS'                                                  
         DC   X'FF'                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        PFKEY TABLE                                                  *         
***********************************************************************         
*                                                                               
PFTABLE  DS   0H                                                                
*        PRODUCT MAINTENANCE DISPLAY                                            
         DC   AL1(MPF02X-*,02,PFTCPROG,(MPF02X-MPF02)/KEYLNQ,0)                 
         DC   CL3'PM '                 MAINT                                    
         DC   CL8'PRD'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF02    DC   AL1(KEYTYTWA,L'PLSMEDK-1),AL2(PLSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PLSCLIK-1),AL2(PLSCLIK-T217FFD)                    
         DC   AL1(KEYTYCUR,L'LSPRDC-1),AL2(LSPRDC-LSPRDC)                       
MPF02X   EQU  *                                                                 
*                                                                               
*        CLIENT MAINT DISPLAY                                                   
         DC   AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                 
         DC   CL3'CM '                 MAINT                                    
         DC   CL8'CLT'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF04    DC   AL1(KEYTYTWA,L'PLSMEDK-1),AL2(PLSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PLSCLIK-1),AL2(PLSCLIK-T217FFD)                    
MPF04X   EQU  *                                                                 
*                                                                               
*        CLIENT2 MAINT DISPLAY                                                  
         DC   AL1(MPF05X-*,05,PFTCPROG,(MPF05X-MPF05)/KEYLNQ,0)                 
         DC   CL3'CM2'                 MAINT                                    
         DC   CL8'CL2'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF05    DC   AL1(KEYTYTWA,L'PLSMEDK-1),AL2(PLSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PLSCLIK-1),AL2(PLSCLIK-T217FFD)                    
MPF05X   EQU  *                                                                 
*                                                                               
*        CLIENT LIST DISPLAY                                                    
         DC   AL1(LPF06X-*,06,PFTCPROG,(LPF06X-LPF06)/KEYLNQ,0)                 
         DC   CL3'CL '                 LIST                                     
         DC   CL8'CLT'                 RECORD                                   
         DC   CL8'LIST'                ACTION                                   
LPF06    DC   AL1(KEYTYTWA,L'PLSMEDK-1),AL2(PLSMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PLSCLIK-1),AL2(PLSCLIK-T217FFD)                    
LPF06X   EQU  *                                                                 
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT4,NODELLST                                                
         OI    GENSTAT3,OKVALSEL                                                
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'      MODIFY SERVICE REQUEST                     
         OI    CONSERVH+6,X'80'      TRANSMIT TO GET CONTROL                    
*                                                                               
         OI    PLSREH+1,X'0C'        HIDE PF12=RETURN FIELD                     
         CLI   CALLSP,0                                                         
         BE    *+8                                                              
         NI    PLSREH+1,X'FF'-X'04' LIGHT UP PF12 FIELD                         
         OI    PLSREH+6,X'80'                                                   
*                                                                               
SETUP10  GOTO1 INITPFKY,DMCB,PFTABLE                                            
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        HEADLINE SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'PRODUCT LIST'                                            
         SSPEC H2,30,C'------------'                                            
         SPACE 1                                                                
         SSPEC H3,1,C'MEDIA:'                                                   
         SSPEC H3,41,C'CLIENT:'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'-------'                                                  
         SSPEC H5,11,C'PRODUCT NAME'                                            
         SSPEC H6,11,C'------------'                                            
         SSPEC H5,41,C'CLT/PRD CODE'                                            
         SSPEC H6,41,C'------------'                                            
         SSPEC H5,58,C'CLASS'                                                   
         SSPEC H6,58,C'-----'                                                   
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        HEADLINE SPECS                                                         
***********************************************************************         
HEDSPEC2 DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'PRODUCT LIST'                                            
         SSPEC H2,30,C'------------'                                            
         SPACE 1                                                                
         SSPEC H3,1,C'MEDIA:'                                                   
         SSPEC H3,41,C'CLIENT:'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'-------'                                                  
         SSPEC H5,11,C'PRODUCT NAME'                                            
         SSPEC H6,11,C'------------'                                            
         SSPEC H5,41,C'CLT/PRD CODE'                                            
         SSPEC H6,41,C'------------'                                            
         SSPEC H5,58,C'CLASS'                                                   
         SSPEC H6,58,C'-----'                                                   
         SSPEC H5,65,C'GST CODE'                                                
         SSPEC H6,65,C'--------'                                                
         SSPEC H5,76,C'PST CODE'                                                
         SSPEC H6,76,C'--------'                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        HEADHOOKS                                                    *         
***********************************************************************         
HDHOOK   NTR1                                                                   
         MVC   H3+7(L'QMED),QMED                                                
         MVC   H3+10(L'MEDNM),MEDNM                                             
         MVC   H3+48(L'QCLT),QCLT                                               
         MVC   H3+57(L'CLTNM),CLTNM                                             
         XIT1                                                                   
***********************************************************************         
*        CONSTANTS                                                    *         
***********************************************************************         
PFERR    EQU   559                  INVALID PFKEY                               
ERRFNF   MVC   ERRNUM,=AL2(851)     FILTER NOT FOUND IN TABLE                   
         B     SPERREX                                                          
ERRLEN2  MVC   ERRNUM,=AL2(852)     MUST BE Y/N NOT LONGER                      
         B     SPERREX                                                          
ERROP2   MVC   ERRNUM,=AL2(853)     MUST BE Y/N                                 
         B     SPERREX                                                          
ERRDUP   MVC   ERRNUM,=AL2(854)     DUPLICATE FILTER                            
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
       ++INCLUDE SPGENPRD          PRODUCT RECORD                               
         EJECT                                                                  
       ++INCLUDE SPGENCLT          CLIENT RECORD                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM70D          MAINTENACE SCREEN                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM71D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDPSTBLK          PROVINCIAL TAX VALIDATION                    
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAPY          AUTOPAY RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
*                                                                               
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
PROKEY   DS    CL13                                                             
CLT3C    DS    CL3                                                              
PRO3C    DS    CL3                                                              
ERRNUM   DS    XL2                                                              
SBLOCK   DS   2CL32                BLOCK FOR SCANNER                            
*                                                                               
FILTFLAG DS    X                                                                
FFCPPYQ  EQU   X'01'               CPPRS PRODUCTS ONLY                          
FFCPPNQ  EQU   X'02'               NON-CPPRS PRODUCTS ONLY                      
FFTHTRYQ EQU   X'04'               THEATRICAL ONLY                              
FFTHTRNQ EQU   X'08'               NON-THEATRICAL ONLY                          
FFTHNTYQ EQU   X'10'               THNT ONLY                                    
FFTHNTNQ EQU   X'20'               NON-THNT ONLY                                
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                LABELS FOR LISTMON                         
LSPRDC   DS    CL3                   PRODUCT CODE                               
         DS    CL5                                                              
LSPRDN   DS    CL20                  PRODUCT NAME                               
         DS    CL6                                                              
LSPRCD   DS    CL5                   CLT/PRD CODE                               
         DS    CL7                                                              
LSPRCLS  DS    CL2                   PRODUCT CLASS                              
         DS    CL6                                                              
LSPRGST  DS    CL1                   GST CODE                                   
         DS    CL5                                                              
LSPRPST  DS    CL3                   PST CODE                                   
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PPRD     DS    CL3                   PRODUCT CODE                               
         DS    CL7                                                              
PPRDNAME DS    CL20                  PRODUCT NAME                               
         DS    CL10                                                             
PCPCODE  DS    CL5                   CLT/PRD CODE                               
         DS    CL12                                                             
PRCLASS  DS    CL2                   PRODUCT CLASS                              
         DS    CL5                                                              
PRGST    DS    CL1                   GST CODE                                   
         DS    CL10                                                             
PRPST    DS    CL3                   PST CODE                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPSFM51   03/08/10'                                      
         END                                                                    
