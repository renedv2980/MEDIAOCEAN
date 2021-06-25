*          DATA SET SPSFM55    AT LEVEL 011 AS OF 06/18/10                      
*PHASE T21755A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21755  -- ESTIMATE DOLLARS                          *         
*                                                                     *         
*  COMMENTS:     MAINTAINS ESTIMATE DOLLAR RECORDS                    *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21900), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM75 (MAINT)                               *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- RECORD                                         *         
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
         TITLE 'T21755 - ESTD MAINTENANCE'                                      
T21755   CSECT                                                                  
         PRINT GEN                                                              
         NMOD1 0,T21755                                                         
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
*                                                                               
         USING ESTHDR,R4                                                        
VK       LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   EKEYTYPE,X'00'                                                   
*                                                                               
         MVC   ESTMEDN,SPACES        CLEAR MEDIA NAME AND CLIENT NAME           
         OI    ESTMEDNH+6,X'80'      AND PRODUCT NAME                           
         MVC   ESTCLIN,SPACES                                                   
         OI    ESTCLINH+6,X'80'                                                 
         MVC   ESTPRDN,SPACES                                                   
         OI    ESTPRDNH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTMEDKH           MEDIA                                      
         GOTO1 VALIMED               VALIDATE MEDIA CODE AND TRANSMIT           
         MVC   ESTMEDN,MEDNM         MEDIA NAME                                 
         OI    ESTMEDNH+6,X'80'                                                 
         MVC   EKEYAM,BAGYMD         COPY MEDIA INTO KEY                        
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTCLIKH           CLIENT                                     
         GOTO1 VALICLT               VALIDATE CLIENT CODE AND TRANSMIT          
         MVC   ESTCLIN,CLTNM         CLIENT NAME                                
         OI    ESTCLINH+6,X'80'                                                 
*                                                                               
         L     RE,AIO                SAVE CLIENT RECORD INFORMATION             
         USING CLTHDR,RE                                                        
         MVC   SVCLEX,CEXTRA                                                    
         DROP  RE                                                               
*                                                                               
         MVC   EKEYCLT,BCLT          COPY CLIENT INTO KEY                       
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTPRDKH           PRODUCT                                    
         MVI   AAAOK,C'Y'                                                       
         GOTO1 VALIPRD               VALIDATE PRODUCT CODE AND TRANSMIT         
         MVI   AAAOK,C'N'            PRODUCT NAME                               
         MVC   ESTPRDN,PRDNM                                                    
         OI    ESTPRDNH+6,X'80'                                                 
*                                                                               
         MVC   EKEYPRD,QPRD          COPY PRODUCT INTO KEY                      
         OC    EKEYPRD,SPACES                                                   
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTESTKH           ESTIMATE                                   
         CLI   ESTESTKH+5,0                                                     
         BE    ERRMIS                                                           
         BAS   RE,SPTOZER                                                       
*                                                                               
         MVI   ESTESTKH+5,3          LENGTH                                     
         CLI   ESTESTK+2,C' '                                                   
         BH    VK15                                                             
         MVI   ESTESTKH+5,2                                                     
         CLI   ESTESTK+1,C' '                                                   
         BH    VK15                                                             
         MVI   ESTESTKH+5,1                                                     
*                                                                               
VK15     GOTO1 VALIEST               VALIDATE ESTIMATE CODE AND                 
         MVC   ESTESTN,ESTNM         TRANSMIT ESTIMATE DESCRIPTION              
         OI    ESTESTNH+6,X'80'                                                 
         MVC   EKEYEST,BEST          SAVE ESTIMATE CODE INTO KEY                
         MVC   ESTKEY,KEY            SAVE ESTIMATE KEY                          
*                                                                               
         GOTO1 HIGH                  EST RECORD MUST EXIST                      
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SPERREX                                                          
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DISPLAY RECORD                                               *         
***********************************************************************         
*                                                                               
DR       L     R3,AIO               ESTIMATE RECORD                             
         USING ESTHDR,R3                                                        
         ZAP   ATOTAL,=PL6'0'                                                   
         ZAP   OTOTAL,=PL6'0'                                                   
         ZAP   PTOTAL,=PL6'0'                                                   
*                                                                               
*                                   COPY ESTIMATE START AND END DATE            
*                                   TO SCREEN                                   
         GOTO1 DATCON,DMCB,(0,ESTART),(5,ESTSTRD)                               
         OI    ESTSTRDH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(0,EEND),(5,ESTENDD)                                 
         OI    ESTENDDH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTAJANH           FIRST AUTHORIZATION FIELD                  
         SR    R5,R5                 COUNTER                                    
*                                                                               
DR10     LA    R4,EAUTH              AUTHORIZATIONS                             
         MVI   BYTE,C'A'                                                        
         BAS   RE,PUTAMT                                                        
*                                                                               
         LA    R4,EORD               ORDERED                                    
         MVI   BYTE,C'O'                                                        
         BAS   RE,PUTAMT                                                        
*                                                                               
         LA    R4,EPAID              PAID                                       
         MVI   BYTE,C'P'                                                        
         BAS   RE,PUTAMT                                                        
*                                                                               
         BAS   RE,NEXTFLD            BUMP PAST MONTH LABEL                      
         LA    R5,1(R5)              NEXT MONTH                                 
         CHI   R5,12                                                            
         BL    DR10                                                             
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTATOTH           TRANSMIT AUTHORIZATION, ORDERED            
         LA    R4,ATOTAL             AND PAID TOTALS TO SCREEN                  
         LA    R5,3                  FOR BCTR                                   
*                                                                               
DR20     ZAP   TEMPAMT,0(6,R4)                                                  
         SRP   TEMPAMT,62,0          GET RID OF PENNIES WITHOUT DYING           
*        DP    TEMPAMT,=P'100'       GET RID OF PENNIES                         
         EDIT  (P6,TEMPAMT),(10,8(R2)),0                                        
         OI    6(R2),X'80'                                                      
         LA    R4,6(R4)              BUMP TOTAL                                 
         BAS   RE,NEXTFLD            FIND NEXT FIELD                            
         BCT   R5,DR20                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
**********************************************************************          
*        OUTPUT AMOUNT                                               *          
**********************************************************************          
PUTAMT   NTR1                                                                   
         XC    8(VALLEN,R2),8(R2)                                               
         MHI   R5,6                EACH ACCUM IS PL6                            
         AR    R4,R5               TAB INTO BLOCK THAT STARTS AT R4             
         ZAP   TEMPAMT,0(6,R4)     SAVE NUMBER TO DISPLAY                       
*                                                                               
         LA    R1,ATOTAL                                                        
         CLI   BYTE,C'A'                                                        
         BE    PUT20                                                            
*                                                                               
PUT10    LA    R1,OTOTAL           ORDER TOTAL                                  
         CLI   BYTE,C'O'                                                        
         BE    PUT20                                                            
         LA    R1,PTOTAL           PAID TOTAL                                   
*                                                                               
PUT20    AP    0(6,R1),TEMPAMT     ADD TO TOTAL                                 
*                                                                               
PUT40    SRP   TEMPAMT,62,0        GET RID OF PENNIES WITHOUT DYING             
         EDIT  (P6,TEMPAMT),(10,8(R2)),0                                        
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTFLD          SAVE R2 (TO POINT AT NEXT FIELD)             
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
**********************************************************************          
*        VALIDATE RECORD                                             *          
**********************************************************************          
                                                                                
VR       L     R3,AIO                                                           
         L     R4,AIO3               SAVE OLD AUTH $'S INTO AIO3                
         XC    0(100,R4),0(R4)                                                  
         MVC   0(BUCKLEN,R4),EAUTH                                              
*                                                                               
         LA    R5,EAUTH              CLEAR RECORD'S AUTHORIZATION               
         LA    R1,13                                                            
         ZAP   0(6,R5),=PL6'0'                                                  
         LA    R5,6(R5)                                                         
         BCT   R1,*-10                                                          
         LA    R5,EAUTH              RESET R5 TO BEGINNING                      
*                                                                               
**********************************************************************          
*                                                                               
         LA    R2,ESTAJANH           FIRST AUTHORIZATION FIELD                  
         SR    R6,R6                 COUNTER                                    
*                                                                               
VR10     MVC   ERRNUM,=AL2(AUTHDERR)                                            
         CLI   5(R2),0               IF $'S WERE PREVIOUSLY IN                  
         BNE   VR20                  AUTHORIZATION FIELD ... MUST USE           
         CP    0(6,R4),=PL6'0'       0 TO DELETE                                
         BE    VR30                                                             
         B     SPERREX                                                          
*                                                                               
VR20     ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(C'0',8(R2)),(R0)                                   
         CLI   0(R1),0                                                          
         BNE   ERRINV                                                           
         ZAP   TEMPAMT(6),4(8,R1)    IF $'S ENTERED ...                         
         CP    TEMPAMT,=P'9999999'   AUTH CANT BE GREATER THEN 9999999          
         BH    ERRINV                                                           
         CP    TEMPAMT,=PL6'0'       AUTHORIZATION CANNOT BE NEGATIVE           
         BL    ERRINV                                                           
         BE    *+8                   IF $'S POSITIVE ...                        
         BAS   RE,VALPER             VALIDATE AUTH WITHIN EST DATES             
         MP    TEMPAMT,=P'100'       IF $'S POSITIVE OR ZERO ...                
         ZAP   0(6,R5),TEMPAMT       ADD CENTS                                  
*                                                                               
VR30     LA    R4,6(R4)                                                         
         LA    R5,6(R5)                                                         
         BAS   RE,FNDNXUF1           NEXT UNPROTECTED FIELD                     
         LA    R6,1(R6)                                                         
         CHI   R6,12                                                            
         BL    VR10                                                             
*                                                                               
         BAS   RE,PTREC            WRITE BACK RECORD                            
         B     DR                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        ENSURE AUTHORIZATION INPUT IS WITHIN ESTIMATE DATES          *         
***********************************************************************         
*                                                                               
VALPER   NTR1                                                                   
         XC    ELEM,ELEM                                                        
         XC    BLOCK(255),BLOCK                                                 
         XC    BLOCK+255(225),BLOCK+255                                         
         LA    R4,ELEM                                                          
         USING DBLOCK,R4             SET UP CALL TO GTBRD                       
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '        SET DBFILE = NAD FOR NETWORK               
         CLI   OVSYS,3               SET DBFILE = TP  OTHERWISE                 
         BNE   *+10                                                             
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'        SET DBSELMED = C IF CANADIAN               
         BNE   VALP10                AGENCY USING US DEMOS                      
         CLI   SVCLEX,C'U'           SET DBSELMED = R OTHERWISE                 
         BE    VALP10                                                           
         MVI   DBSELMED,C'C'                                                    
VALP10   MVC   DMCB+4(4),=X'D9000A1D'                                           
         GOTO1 CALLOV,DMCB           CALL GETBRD                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,ESTART),WORK,GETDAY,ADDAY                           
*                                 WORK+6(6)=BRD START DT FOR EST START          
         GOTO1 (RF),(R1),(1,EEND),WORK+12,GETDAY,ADDAY                          
*                                 WORK+18(6)=BRD END DATE FOR EST END           
         DROP  R4                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,SYR)                                     
         GOTO1 DATCON,DMCB,(0,WORK+18),(3,EYR)                                  
*                                                                               
         LA    R6,1(R6)            R6 = REAL MONTH NUMBER                       
         CLC   SYR,EYR             SAME YEAR                                    
         BE    VP20                                                             
         ZIC   R1,SYR                                                           
         ZIC   R4,EYR                                                           
         SR    R4,R1               R4 = NUM YEARS                               
         CH    R4,=H'1'            IF MORE THAN 1 YEAR                          
         BH    XIT                 ALL MONTHS ARE OK                            
*                                                                               
         MVC   ERRNUM,=AL2(MONTHERR)                                            
         ZIC   R1,EMN              IF MONTH IS LOWER THAN END MONTH             
         CR    R6,R1                                                            
         BE    XIT                                                              
         BH    VP15                                                             
         ZIC   R1,SMN              IT MUST BE HIGHER (=) TO START MONTH         
         LA    R4,12(R6)                                                        
         CR    R4,R1                                                            
         BL    SPERREX                                                          
         B     XIT                                                              
*                                                                               
VP15     ZIC   R1,SMN              MONTH IS HIGHER THAN END MONTH               
         CR    R6,R1               MUST BE HIGHER (=) TO START MONTH            
         BL    SPERREX                                                          
         B     XIT                                                              
*                                                                               
VP20     ZIC   R1,SMN              START MONTH                                  
         CR    R6,R1                                                            
         BL    SPERREX                                                          
         ZIC   R1,EMN              END MONTH                                    
         CR    R6,R1                                                            
         BH    SPERREX                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
**********************************************************************          
*        SPACES TO ZEROS                                                        
**********************************************************************          
SPTOZER  NTR1                                                                   
SPTOZ10  CLI   ESTESTK,C' '          CHANGE LEADING SPACES TO ZEROS             
         BH    SPTOZX                                                           
         MVI   ESTESTK,X'F0'                                                    
         CLI   ESTESTK+1,C' '                                                   
         BH    SPTOZ20                                                          
         MVI   ESTESTK+1,X'F0'                                                  
*                                                                               
SPTOZ20  MVC   ERRNUM,=AL2(ESTERR1)  EST CODE MUST BE NUMERIC                   
         LA    R3,3                                                             
         LA    R4,ESTESTK                                                       
SPTOZ25  CLI   0(R4),X'F9'                                                      
         BH    SPERREX                                                          
         CLI   0(R4),X'F0'                                                      
         BL    SPERREX                                                          
         LA    R4,1(R4)                                                         
         BCT   R3,SPTOZ25                                                       
         OI    ESTESTKH+4,X'08'      SET FOR VALID NUMERIC                      
SPTOZX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         MVI   USEIO,C'N'                                                       
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    GENSTAT4,CONFDEL                                                 
         OI    CONSERVH+1,X'01'      MODIFY SERVICE REQUEST                     
         OI    CONSERVH+6,X'80'      TRANSMIT TO GET CONTROL                    
         MVI   IOOPT,C'Y'                                                       
*                                                                               
         OI    ESTREH+1,X'0C'        HIDE PF12=RETURN FIELD                     
         CLI   CALLSP,0                                                         
         BE    *+8                                                              
         NI    ESTREH+1,X'FF'-X'04'  LIGHT UP PF12 FIELD                        
         OI    ESTREH+6,X'80'                                                   
*                                                                               
SETUP10  GOTO1 INITPFKY,DMCB,PFTABLE PF TABLE                                   
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,0(R1)              SET MEDIA IN USE                           
         USING FACTSD,R1                                                        
         MVC   OVSYS,FAOVSYS         2=SPOT,3=NET                               
         MVC   LINID,FALINE                                                     
         MVC   LINADDR,FAADDR                                                   
         MVC   OVSYS,FAOVSYS         2=SPOT,3=NET                               
         DROP  R1,RF                                                            
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        FIND UNPROTECTED FIELD                                       *         
***********************************************************************         
*                                                                               
FNDUF1   TM    1(R2),X'20'         FIND UNPROTECTED FIELD                       
         BZR   RE                                                               
*                                                                               
FNDNXUF1 ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF1                                                           
         DC    H'0'                END OF SCREEN                                
*                                                                               
***********************************************************************         
*                                                                               
NEXTFLD  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNER  RE                                                               
         DC    H'0'                END OF SCREEN                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                       PUT RECORD                                    *         
***********************************************************************         
*                                                                               
PTREC    NTR1                                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        ERROR MESSAGES                                               *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
ESTERR1  EQU   563         ESTIMATE CODE MUST BE NUMERIC                        
AUTHDERR EQU   777         MUST USE 0 TO DELETE $'S                             
MONTHERR EQU   778         MUST BE >= TO START MONTH AND <= END MONTH           
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                                                                               
VALLEN   EQU   10                  LENGTH OF FIELD                              
BUCKLEN  EQU   78                  LENGTH OF BUCKETS                            
*                                                                               
**********************************************************************          
*        PFKEY TABLES                                                *          
**********************************************************************          
*                                                                               
PFTABLE  DS   0H                                                                
*        CLIENT MAINT DISPLAY                                                   
         DC   AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                 
         DC   CL3'CM '                 MAINT                                    
         DC   CL8'CLT'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF04    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
MPF04X   EQU  *                                                                 
*                                                                               
*        CLIENT2 MAINT DISPLAY                                                  
         DC   AL1(MPF05X-*,05,PFTCPROG,(MPF05X-MPF05)/KEYLNQ,0)                 
         DC   CL3'CM2'                 MAINT                                    
         DC   CL8'CL2'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF05    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
MPF05X   EQU  *                                                                 
*                                                                               
*        PRODUCT MAINT DISPLAY                                                  
         DC   AL1(MPF02X-*,02,PFTCPROG,(MPF02X-MPF02)/KEYLNQ,0)                 
         DC   CL3'PM '                 DISPLAY                                  
         DC   CL8'PRD'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF02    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTPRDK-1),AL2(ESTPRDK-T217FFD)                    
MPF02X   EQU  *                                                                 
*                                                                               
*        ESTIMATE MAINT DISPLAY                                                 
         DC   AL1(MPF07X-*,07,PFTCPROG,(MPF07X-MPF07)/KEYLNQ,0)                 
         DC   CL3'EM '                 MAINT                                    
         DC   CL8'EST'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF07    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTPRDK-1),AL2(ESTPRDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTESTK-1),AL2(ESTESTK-T217FFD)                    
MPF07X   EQU  *                                                                 
*                                                                               
*        ESTIMATE COPY                                                          
         DC   AL1(MPF08X-*,08,PFTCPROG,(MPF08X-MPF08)/KEYLNQ,0)                 
         DC   CL3'EC '                 MAINT                                    
         DC   CL8'EST'                 RECORD                                   
         DC   CL8'COPY'                ACTION                                   
MPF08    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTPRDK-1),AL2(ESTPRDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTESTK-1),AL2(ESTESTK-T217FFD)                    
MPF08X   EQU  *                                                                 
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG CONTAGH                                                            
       ++INCLUDE SCSFM75D                                                       
         EJECT                                                                  
         ORG CONTAGH                                                            
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE FAGETTXTD           ERROR MESSAGES                             
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
         ORG   SYSSPARE                                                         
ESTKEY   DS    CL13                                                             
ERRNUM   DS    XL2                                                              
OVSYS    DS    CL1                                                              
SVCLEX   DS    CL15                                                             
LINID    DS    CL4                                                              
LINADDR  DS    CL4                                                              
*                                                                               
SYR      DS    XL1                                                              
SMN      DS    XL1                                                              
SDY      DS    XL1                                                              
EYR      DS    XL1                                                              
EMN      DS    XL1                                                              
EDY      DS    XL1                                                              
*                                                                               
ATOTAL   DS    PL6                 AUTHORIZATION TOTAL                          
OTOTAL   DS    PL6                 ORDERED TOTAL                                
PTOTAL   DS    PL6                 PAID TOTAL                                   
TEMPAMT  DS    PL6                 TEMP HOLDING                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPSFM55   06/18/10'                                      
         END                                                                    
