*          DATA SET SPSFM07    AT LEVEL 031 AS OF 11/17/20                      
*PHASE T21707A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE:        T21707  -- CLIENT RECORD LIST                        *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM76 (MAINT) & SCSFM77 (LIST)              *         
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
         TITLE 'T21707 - CLIENT RECORD LIST'                                    
T21707   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1707**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         USING OFFICED,OFCBLK                                                   
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY (FOR LIST)                      
         BE    VK                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                  LIST RECORDS                                 
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BNE   XIT                                                              
         LA    R5,HDHOOK           FOR SPOOL CALLS                              
         ST    R5,HEADHOOK                                                      
*                                                                               
         SELECT  CLI,DISFLAG,EQ                                                 
           WHEN  (0)                                                            
             LAY  R5,HSPEC                                                      
           WHEN  (C'0')                                                         
             LAY  R5,HSPEC0                                                     
           WHEN  (C'1')                                                         
             LAY  R5,HSPEC1                                                     
           WHEN  (C'2')                                                         
             LAY  R5,HSPEC2                                                     
           WHEN  (C'4')                                                         
             LAY  R5,HSPEC4                                                     
           WHEN  (C'3')                                                         
             LAY  R5,HSPEC                                                      
             IF  (CLI,CLTFRZ,EQ,C'Y')                                           
               LAY  R5,HSPEC3                                                   
             ENDIF ,                                                            
           OTHRWISE                                                             
             J   XIT                                                            
         ENDSEL ,                                                               
         ST    R5,SPECS                                                         
         B     LR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0X                                                               
         LA    R2,LSTMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   8(R2),C'T'          DEFAULT TO MEDIA T                           
         MVI   5(R2),X'01'                                                      
*                                                                               
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   LSTMEDN,MEDNM       TRANSMIT MEDIA NAME                          
         OI    LSTMEDNH+6,X'80'                                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLTRECD,R4                                                       
         MVC   CKEYAM,BAGYMD       AGENCY/MEDIA CODE                            
*                                                                               
         MVC   CLT3C,=C'AAA'       INITIALIZE TO THE LOWEST CLT CODE            
         ZICM  R1,LSTCLTH+5,1      INPUT LENGTH OF CLT FIELD                    
         BZ    VK10                NO CLT INPUT                                 
*                                                                               
         LA    RE,2                                                             
         CR    R1,RE               CLT INPUT LENGTH IS 2?                       
         BNE   *+8                                                              
         MVI   CLT3C+2,C' '        BLANK OUT THE LAST BYTE OF CLT CODE          
*                                                                               
         BCTR  R1,0                EX MOVE OF CLT CODE (START AT)               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CLT3C(0),LSTCLT                                                  
*                                                                               
VK10     GOTO1 CLPACK,DMCB,CLT3C,CKEYCLT                                        
         MVC   SVCLTKEY,KEY        SAVE A COPY OF THE KEY                       
*                                                                               
         OI    LSTMEDH+6,X'80'     TRANSMIT ALL KEY FIELDS                      
         OI    LSTCLTH+6,X'80'     TO ENFORCE ALL CAPS DISPLAY                  
         OI    LSTFILH+6,X'80'                                                  
*                                                                               
         MVI   DISFLAG,0           NO, RESET DISPLAY FORMAT FLAG                
         MVI   FCNTR,0             RESET FILTER COUNTER                         
         CLI   LSTFILH+5,0         ANY FILTER INPUT?                            
         BE    VKX                 YES,EXIT                                     
         LA    R2,LSTFILH                                                       
         BAS   RE,FILVAL                                                        
*                                                                               
VKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       DS    0X                                                               
         MVI   OKPFKEY,1           ALLOW SETUP TO INITPFKY                      
         NI    LSTFKEYH+1,X'FF'-X'04' LIGHT UP THE PFKEY DISPLAY                
         OI    LSTFKEYH+6,X'80'                                                 
         CLI   SELFLAG,0                                                        
         BE    LR05                                                             
         GOTO1 =A(UNSEL),RR=RELO   UNPROTECT ALL SEL FLDS                       
*                                  AND CLEAR ALL FLDS AFTER KEY FLDS            
*                                                                               
LR05     GOTO1 =A(PRTHEAD),RR=RELO PRINT THE HEADING                            
         MVI   NLISTS,14                                                        
         LA    R4,KEY                                                           
         USING CLTRECD,R4                                                       
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
         MVC   KEY(L'SVCLTKEY),SVCLTKEY                                         
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R4,KEY                                                           
         GOTO1 SEQ                 NEXT RECORD                                  
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY(L'CKEYTYPE+L'CKEYAM),SVCLTKEY  SAME TYPE OF RECORD?          
         BNE   LRX                 NO MORE RECORD                               
*                                                                               
         CLI   CKEYCLT+2,0         CLIENT RECORD?                               
         BE    *+14                YES                                          
         MVC   CKEYCLT+2(9),HIKEY  NO, SO MOVE HI TO REST OF KEY                
         B     LR10                SKIP OVER OTHER RECORDS                      
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
*                                                                               
*        CLI   LSTFILH+5,0         ANY FILTER?                                  
         CLI   FCNTR,0             ANY FILTER?                                  
         BE    LR40                NO, PRINT THE RECORD                         
         GOTO1 =A(FILREC),RR=RELO  YES, FILTER THE RECORD                       
         BNE   LR20                NOT MATCH, NEXT RECORD                       
*                                                                               
LR40     CLI   DISFLAG,C'3'        DISPLAYING CLIENT FREEZE DATE?               
         BNE   LR50                NO                                           
         CLI   CLTFRZ,C'Y'         ONLY WANT CLIENTS W/FREEZE DATE?             
         BNE   LR45                                                             
         OC    CLOCKYM,CLOCKYM     YES - HAVE CLIENT FREEZE DATE?               
         BZ    LR20                NO - FILTER THIS RECORD OUT!                 
         B     LR55                                                             
*                                                                               
LR45     OC    CLOCKYM,CLOCKYM                                                  
         BNZ   LR20                                                             
         J     LR55                                                             
*                                                                               
LR50     CLI   DISFLAG,C'4'                                                     
         JNE   LR55                                                             
         CLI   CLFRZFLG,C'Y'       ONLY WANT CLIENTS THAT ARE CLFRZ?            
         JNE   LR52                                                             
         TM    COPT2,COP2FRZ       YES, IS THIS CLIENT CLFRZ?                   
         JZ    LR20                NO, FILTER THIS RECORD OUT!                  
         J     LR55                                                             
*                                                                               
LR52     CLI   CLFRZFLG,C'N'       CLIENTS THAT ARE NOT CLFRZ?                  
         JNE   LR55                NO, WE WANT THEM ALL                         
         TM    COPT2,COP2FRZ       IS THIS CLIENT CLFRZ?                        
         JNZ   LR20                YES, FILTER THIS RECORD OUT!                 
*                                                                               
LR55     GOTO1 =A(PRTLINE),RR=RELO                                              
         MVC   LSTXCLT,LSCLTC      SAVE LAST CLT CODE, SO IT WILL BE            
*                                  USED WHEN PFKEY PRESSED ON LAST ROW          
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR60                                                             
         MVC   P(80),LISTAR                                                     
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR20                NEXT RECORD                                  
*                                                                               
LR60     GOTO1 LISTMON             PRINT LIST LINE                              
         B     LR20                NEXT RECORD                                  
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                          FILTER VALIDATION ROUTINE                  *         
***********************************************************************         
FILVAL   NTR1                                                                   
         GOTO1 SCANNER,DMCB,(R2),(15,SCANBLK)                                   
         ZICM  R0,DMCB+4,1         # OF SCANNER LINES                           
         JZ    ERRINVFS            INVALID FILTER INPUT SYNTAX                  
*                                                                               
         CLI   8(R2),C'?'                                                       
         JNE   FV00                                                             
         GOTO1 =A(DISFTAB),RR=RELO DISPLAY FILTER SYNTAX, EXIT                  
*                                                                               
FV00     MVI   CLFRZFLG,0          CLEAR CFR FLAG                               
*                                                                               
         CLI   5(R2),5             ASSUME NOT D0,D1,D2                          
         BH    FV07                                                             
         CLI   11(R2),C'='         THE 4TH CHAR IN FILTER SCR. FLD              
         BE    FV07                                                             
*                                                                               
         CLI   8(R2),C'D'          FILER = D0,D1,D2,DIS,DLY                     
         BNE   *+14                                                             
         CLC   =C'DLY',8(R2)       DON'T CHK D'S EXCEPT DLY                     
         BNE   FV07                                                             
*                                                                               
         USING FILVTABD,R3                                                      
         L     R3,=A(FILVTAB)                                                   
         A     R3,RELO                                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   FCNTR,0                                                          
*                                                                               
FV04     CLC   FILVTNAM,8(R2)                                                   
         BE    FVX                                                              
*                                                                               
         CLC   FILVTNAM,=C'RAT'    RAT is the last of DISFLAG C'1'              
         BNE   *+8                                                              
         MVI   DISFLAG,C'2'                                                     
*                                                                               
         CLC   FILVTNAM,=C'ADD'    ADD is the last of DISFLAG,C'2'              
         BNE   *+8                                                              
         MVI   DISFLAG,C'4'                                                     
*                                                                               
         CLC   FILVTNAM,=C'CFR'    CFR is the last of DISFLAG,C'4'              
         BNE   *+8                                                              
         MVI   DISFLAG,0                                                        
*                                                                               
         XC    HALF,HALF                                                        
         LLC   RF,FILVTNVF         # OF FIL VALUE IN LIST                       
         MVC   HALF+1(1),FILVTFVL  EACH FILTER VALUE LENGTH                     
         MH    RF,HALF             LIST LENGTH IN BYTES                         
         LA    R3,FILVTOVQ(RF,R3)  OVERHEAD+LIST LENGTH, NEXT ENTRY,FVT         
         CLI   0(R3),X'00'         END OF FVT                                   
         BNE   FV04                                                             
         DROP  R3                                                               
*                                                                               
FV07     CLI   8(R2),C','                                                       
         BE    ERRINVFS            INVALID FILTER INPUT SYNTAX                  
         CLI   8(R2),C'='                                                       
         BE    ERRINVFS                                                         
         CLI   8(R2),C' '                                                       
         BE    ERRINVFS                                                         
*                                                                               
         STC   R0,FCNTR            SAVE INPUTED FILTER COUNTER                  
         LA    R1,SCANBLK          SCANNER TABLE (SRT)                          
         USING SCANBLKD,R1                                                      
         L     R3,=A(FILVTAB)      FILTER VALIDATION TABLE (FVT)                
         A     R3,RELO                                                          
         USING FILVTABD,R3                                                      
         LA    R5,FILTER           REC FILTER                                   
         USING FILTERD,R5                                                       
         ST    R5,NEXTAVRF         RESET NEXT AVAILABLE REC FIL ENTRY           
         LA    RE,FILSPACE         RESET FILSPACE                               
         ST    RE,NEXTAVSP                                                      
         MVI   SDFLAG,1            ALLOW CHANGE OF DISPLAY FORMAT               
*                                                                               
FV10     CLI   SC1STLEN,3          FILTER NAME LENGTH MUST BE 3 CHAR            
         BNL   FV12                EXCEPT D0/D1/D2                              
         CLI   SC1STLEN,2                                                       
         BNE   ERRINVFN            INVALID FILTER NAME ERROR                    
         CLI   SC1STFLD,C'D'                                                    
         BNE   ERRINVFN            INVALID FILTER NAME ERROR                    
*                                                                               
FV12     CLC   FILVTNAM,SC1STFLD   FILTER NAME MATCHES?                         
         BE    FV20                YES, VALIDATE FILTER VALUE                   
*                                                                               
         XC    HALF,HALF                                                        
         LLC   RF,FILVTNVF         # OF FIL VALUE IN LIST                       
         MVC   HALF+1(1),FILVTFVL  EACH FILTER VALUE LENGTH                     
         MH    RF,HALF             LIST LENGTH IN BYTES                         
         LA    R3,FILVTOVQ(RF,R3)  OVERHEAD+LIST LENGTH, NEXT ENTRY,FVT         
         CLI   0(R3),X'00'         END OF FVT                                   
         BNE   FV12                                                             
         B     ERRINVFN            INVALID FILTER NAME ERROR                    
*                                                                               
FV20     LA    R4,FILVTLIS         VALID FILTER VALUE LIST                      
         ZICM  RF,FILVTNVF,1       # OF FIL VALUE IN LIST                       
         BZ    FV40                SKIP FIL VALUE CHECK                         
         LLC   RE,FILVTFVL         EACH FILTER VALUE LENGTH                     
         LR    R6,RE                                                            
         BCTR  R6,0                                                             
*                                                                               
FV30     EX    R6,*+8              EX CLC FOR FILTER VALUE                      
         B     *+10                                                             
         CLC   0(0,R4),SC2NDFLD    FILTER VALUE MATCHES?                        
         BNE   FV37                                                             
         CLC   SC2NDLEN,FILVTFVL   AND HAVE SAME LENGTH?(EXCEPT U35=U5)         
         BNH   FV40                YES, CALL TRANSLATION ROUTINE                
         B     ERRINVFV            INVALID FILTER VALUE ERROR                   
*                                                                               
FV37     LA    R4,0(RE,R4)         BUMP FILTER VALUE LIST                       
         BCT   RF,FV30                                                          
         B     ERRINVFV            INVALID FILTER VALUE ERROR                   
*                                                                               
FV40     ICM   RF,15,FILVTTRT                                                   
         A     RF,RELO             A(TRANSLATION ROUTINE)                       
         BASR  RE,RF                                                            
         L     R5,NEXTAVRF         A(NEXT AVAILABLE REC FILTER ENTRY)           
*                                                                               
         LA    R1,SCBLKLQ(R1)      BUMP SCANNER TABLE                           
         L     R3,=A(FILVTAB)      RESET FVT                                    
         A     R3,RELO                                                          
         BCT   R0,FV10             NEXT FILTER INPUT,SRT                        
*                                                                               
FVX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                          FILTER TRANSLATION ROUTINES                *         
***********************************************************************         
*                          INPUT:  R1 = A(SRT ENTRY TO BE TRANSLATED)           
*                          INPUT:  R2 = A(FILTER FIELD HEADER)                  
*                          INPUT:  R3 = A(MATCHED ENTRY IN FVT)                 
*                          INPUT:  R5 = A(NEXT AVAILABLE REC FILTER)            
*                          OUTPUT: ADD A NEW REC FILTER ENTRY                   
*                          OUTPUT: UPDATE NEXTAVSP                              
*                          OUTPUT: UPDATE DISPLAY FORMAT                        
TRTYP    NTR1                      BRAND/POL BUYING TYPE                        
         LA    RE,CPROF+0-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLC   =C'BPOL',SC2NDFLD                                                
         BNE   TRTYP10                                                          
*                                                                               
         MVI   FILTNF,2            TPY=BPOL                                     
         MVC   0(2,RF),=C'12'                                                   
         LA    RF,2(RF)                                                         
         B     TRTYPX                                                           
*                                                                               
TRTYP10  MVI   FILTNF,1            TYP=BRD/TPOL                                 
         MVI   0(RF),C'0'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
         LA    R5,FILTERQ(R5)      SET ANOTHER FILTER                           
         LA    RE,CPOLONLY-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         ST    RF,FILTADD                                                       
         MVI   FILTFL,1                                                         
*                                                                               
         CLC   =C'BRD ',SC2NDFLD                                                
         BNE   TRTYP20                                                          
         MVI   FILTNF,2            TYP=BRD  # ALTERNATIVE FILTER VALUES         
         MVI   0(RF),X'00'                                                      
         MVI   1(RF),C'N'                                                       
         LA    RF,2(RF)            BUMP A(NEXT AVAILABLE FILSPACE)              
         B     TRTYP30                                                          
*                                                                               
TRTYP20  MVI   FILTNF,1            TYP=TPOL                                     
         MVI   0(RF),C'Y'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRTYP30  LLC   RE,FCNTR            INCREMENT REC FILTER COUNTER BY 1            
         LA    RE,1(RE)                                                         
         STC   RE,FCNTR                                                         
         B     TRTYPX                                                           
*                                                                               
TRTYPX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRLBX    NTR1                      LOCK BOX NUM                                 
         LA    RE,CPROF+1-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),SC2NDFLD                                                 
         LA    RF,1(RF)                                                         
*                                                                               
TRLBXX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRU35    NTR1                      MKT/STA TURNAROUND                           
         LA    RE,CPROF+2-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLC   =C'U5',SC2NDFLD                                                  
         BNE   TRU3510                                                          
         MVI   FILTNF,2            U35=U5                                       
         MVC   0(2,RF),=C'01'                                                   
         LA    RF,2(RF)                                                         
         B     TRU35X                                                           
*                                                                               
TRU3510  CLC   =C'U3M',SC2NDFLD                                                 
         BNE   TRU3520                                                          
         MVI   FILTNF,1            U35=U3M                                      
         MVI   0(RF),C'2'                                                       
         LA    RF,1(RF)                                                         
         B     TRU35X                                                           
*                                                                               
TRU3520  MVI   FILTNF,1            U35=U3S                                      
         MVI   0(RF),C'3'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRU35X   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRRSV    NTR1                      RATING SERVICE                               
         LA    RE,CPROF+3-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLC   =C'CSI',SC2NDFLD                                                 
         BE    *+18                                                             
         CLC   =C'NSI',SC2NDFLD                                                 
         BE    *+8                                                              
         B     TRRSV10                                                          
         MVI   FILTNF,1            RSV=CSI OR NSI                               
         MVI   0(RF),C'0'                                                       
         LA    RF,1(RF)                                                         
         B     TRRSV20                                                          
*                                                                               
TRRSV10  MVI   FILTNF,1            RSV=ARB OR BBM                               
         MVI   0(RF),C'1'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRRSV20  CLI   SVAPROF+7,C'C'      CANADIAN AGY?                                
         BNE   TRRSV30                                                          
         CLC   =C'CSI',SC2NDFLD    CANADIAN AGY                                 
         BE    TRRSVX                                                           
         CLC   =C'BBM',SC2NDFLD                                                 
         BE    TRRSVX                                                           
*                                  NSI OR ARB FOR CAN AGY, CDM=U                
         LA    R5,FILTERQ(R5)      SET A FILTER OF CDM=U IN REC FIL             
         LA    RE,CEXTRA+0-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         ST    RF,FILTADD                                                       
         MVI   FILTFL,1                                                         
         MVI   FILTNF,1                                                         
         MVI   0(RF),C'U'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
         LLC   RE,FCNTR            INCREMENT REC FILTER COUNTER BY 1            
         LA    RE,1(RE)                                                         
         STC   RE,FCNTR                                                         
         B     TRRSVX                                                           
*                                                                               
TRRSV30  CLC   =C'NSI',SC2NDFLD    US AGY                                       
         BE    TRRSVX                                                           
         CLC   =C'ARB',SC2NDFLD                                                 
         BE    TRRSVX                                                           
         B     ERRINVFV            INVALID FILTER VALUE ERROR                   
*                                                                               
TRRSVX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRBFC    NTR1                      BILL FORMULA CNTRL                           
         LA    RE,CPROF+4-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),SC2NDFLD                                                 
         LA    RF,1(RF)                                                         
*                                                                               
TRBFCX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRUBC    NTR1                      UCOMM BILL CONTROL                           
         LA    RE,COPT4-CLTRECD                                                 
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),SC2NDFLD                                                 
         MVI   1(RF),COP4UCOM                                                   
         LA    RF,2(RF)                                                         
*                                                                               
         ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRBPS    NTR1                      BILL PERCENTAGE SPLIT                        
         LA    RE,COPT4-CLTRECD                                                 
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),SC2NDFLD                                                 
         MVI   1(RF),COP4BPCT                                                   
         LA    RF,2(RF)                                                         
*                                                                               
         ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRBEC    NTR1                      BILL ESTIMATE CNTRL                          
         LA    RE,CPROF+5-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),SC2NDFLD                                                 
         LA    RF,1(RF)                                                         
*                                                                               
TRBECX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRAAN    NTR1                      PRINT CLT CODE=AAN                           
         LA    RE,CPROF+6-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRAAN10                                                          
         MVI   FILTNF,2            AAN=N                                        
         MVC   0(2,RF),=C'0N'                                                   
         LA    RF,2(RF)                                                         
         B     TRAANX                                                           
*                                                                               
TRAAN10  MVI   FILTNF,2            AAN=Y                                        
         MVC   0(2,RF),=C'1Y'                                                   
         LA    RF,2(RF)                                                         
*                                                                               
TRAANX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRPES    NTR1                      PRINT EST SERIES NM                          
         LA    RE,CPROF+7-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),SC2NDFLD                                                 
         LA    RF,1(RF)                                                         
*                                                                               
TRPESX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRCPP    NTR1                      GOALS CPP OVERRIDE                           
         CLC   =C'CPPRS',SC1STFLD                                               
         BE    TRCPPRS                                                          
                                                                                
         LA    RE,CPROF+8-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRCPP10                                                          
         MVI   FILTNF,1            CPP=N                                        
         MVI   0(RF),C'0'                                                       
         LA    RF,1(RF)                                                         
         B     TRCPPX                                                           
*                                                                               
TRCPP10  MVI   FILTNF,1            CPP=Y                                        
         MVI   0(RF),C'1'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRCPPX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRCPPRS  LA    RE,CCPPRS-CLTRECD   CPPRS FILTER                                 
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRCPPRS1                                                         
         MVI   FILTNF,1            CPP=N                                        
         MVI   0(RF),C'N'                                                       
         LA    RF,1(RF)                                                         
         B     TRCPPRSX                                                         
*                                                                               
TRCPPRS1 MVI   FILTNF,2            CPP=Y                                        
         MVI   0(RF),C'Y'                                                       
         MVI   1(RF),0                                                          
         LA    RF,2(RF)                                                         
*                                                                               
TRCPPRSX ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRPAJ    NTR1                      PROGRAM ADJ. CONTROL                         
         LA    RE,CPROF+9-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'0'                                                    
         BE    TRPAJ10                                                          
         CLI   SC2NDFLD,C'A'                                                    
         BE    TRPAJ20                                                          
         B     TRPAJ30                                                          
*                                                                               
TRPAJ10  MVI   FILTNF,1            PAJ=0                                        
         MVI   0(RF),C'0'                                                       
         LA    RF,1(RF)                                                         
         B     TRPAJX                                                           
*                                                                               
TRPAJ20  MVI   FILTNF,1            PAJ=A                                        
         MVI   0(RF),C'1'                                                       
         LA    RF,1(RF)                                                         
         B     TRPAJX                                                           
*                                                                               
TRPAJ30  MVI   FILTNF,1            PAJ=N                                        
         MVI   0(RF),C'2'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRPAJX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRPDM    NTR1                      POL TIMESHEET DEMOS                          
         LA    RE,CPROF+10-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),SC2NDFLD                                                 
         LA    RF,1(RF)                                                         
*                                                                               
TRPDMX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRESR    NTR1                      FORCE EST SERIES REQ                         
         LA    RE,CPROF+11-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRESR10                                                          
         MVI   FILTNF,2            ESR=N                                        
         MVC   0(2,RF),=C'0N'                                                   
         LA    RF,2(RF)                                                         
         B     TRESRX                                                           
*                                                                               
TRESR10  MVI   FILTNF,2            ESR=Y                                        
         MVC   0(2,RF),=C'1Y'                                                   
         LA    RF,2(RF)                                                         
*                                                                               
TRESRX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRPRP    NTR1                      POL T/A REPORTS BY PRD                       
         LA    RE,CPROF+12-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRPRP10                                                          
         MVI   FILTNF,2            PRP=N                                        
         MVC   0(2,RF),=C'0N'                                                   
         LA    RF,2(RF)                                                         
         B     TRPRPX                                                           
*                                                                               
TRPRP10  MVI   FILTNF,1            PRP=Y                                        
         MVI   0(RF),C'Y'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRPRPX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TREXC    NTR1                      EXCL GROUP CODE                              
         LA    RE,CPROF+13-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),SC2NDFLD                                                 
         LA    RF,1(RF)                                                         
*                                                                               
TREXCX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRRAT    NTR1                      RATE CONTROL                                 
         LA    RE,CPROF+14-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),SC2NDFLD                                                 
         LA    RF,1(RF)                                                         
*                                                                               
TRRATX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'1'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRCDM    NTR1                       CANADIAN DEMO OPTION                        
         LA    RE,CEXTRA+0-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),SC2NDFLD                                                 
         LA    RF,1(RF)                                                         
*                                                                               
TRCDMX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRCTX    NTR1                       CANADIAN NETWORK TAX                        
         LA    RE,CEXTRA+1-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),SC2NDFLD                                                 
         LA    RF,1(RF)                                                         
*                                                                               
TRCTXX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRBID    NTR1                       BUY ID REQUIRED                             
         LA    RE,CEXTRA+2-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRBID10                                                          
         MVI   FILTNF,2            BID=N                                        
         MVC   0(2,RF),=C'0N'                                                   
         LA    RF,2(RF)                                                         
         B     TRBIDX                                                           
*                                                                               
TRBID10  MVI   FILTNF,1            BID!=N                                       
         MVC   0(1,RF),SC2NDFLD                                                 
         LA    RF,1(RF)                                                         
*                                                                               
TRBIDX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRFLT    NTR1                       ESTIMATE FILTERS REQ                        
         LA    RE,CEXTRA+3-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRFLT10                                                          
         MVI   FILTNF,2            FLT=N                                        
         MVC   0(2,RF),=C'0N'                                                   
         LA    RF,2(RF)                                                         
         B     TRFLTX                                                           
*                                                                               
TRFLT10  MVI   FILTNF,1            FLT=Y                                        
         MVI   0(RF),C'1'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRFLTX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRCAM    NTR1                       CAMPAIGNS                                   
         LA    RE,CEXTRA+4-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),SC2NDFLD                                                 
         LA    RF,1(RF)                                                         
*                                                                               
TRCAMX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRUSP    NTR1                       US SPILL                                    
         LA    RE,CEXTRA+5-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRUSP10                                                          
         MVI   FILTNF,2            USP=N                                        
         MVC   0(2,RF),=C'0N'                                                   
         LA    RF,2(RF)                                                         
         B     TRUSPX                                                           
*                                                                               
TRUSP10  MVI   FILTNF,1            USP=Y                                        
         MVI   0(RF),C'Y'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRUSPX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRENO    NTR1                       'EST=NO' EST NAME                           
         LA    RE,CEXTRA+6-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRENO10                                                          
         MVI   FILTNF,2            ENO=N                                        
         MVC   0(2,RF),=C'0N'                                                   
         LA    RF,2(RF)                                                         
         B     TRENOX                                                           
*                                                                               
TRENO10  MVI   FILTNF,1            ENO=Y                                        
         MVI   0(RF),C'Y'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRENOX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRMKG    NTR1                       MKGDS IN MISSED MTH                         
         LA    RE,CEXTRA+7-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRMKG10                                                          
         MVI   FILTNF,2            MKG=N                                        
         MVC   0(2,RF),=C'0N'                                                   
         LA    RF,2(RF)                                                         
         B     TRMKGX                                                           
*                                                                               
TRMKG10  MVI   FILTNF,1            MKG=Y                                        
         MVI   0(RF),C'Y'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRMKGX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRGOL    NTR1                       GOAL REQD FOR BUY                           
         LA    RE,CEXTRA+8-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRGOL10                                                          
         MVI   FILTNF,2            GOL=N                                        
         MVC   0(2,RF),=C'0N'                                                   
         LA    RF,2(RF)                                                         
         B     TRGOLX                                                           
*                                                                               
TRGOL10  MVI   FILTNF,1            GOL=Y                                        
         MVI   0(RF),C'Y'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRGOLX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRCTY    NTR1                       COUNTRY                                     
         LA    RE,CEXTRA+9-CLTRECD                                              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLC   =C'CAN',SC2NDFLD                                                 
         BNE   TRCTY10                                                          
         MVI   FILTNF,1            CTY=CAN                                      
         MVI   0(RF),C'C'                                                       
         LA    RF,1(RF)                                                         
         CLI   SVAPROF+7,C'C'      CANADIAN AGY?                                
         BE    TRCTY20             ->FILTNF=3,FILT=0,N,C                        
         B     TRCTYX                                                           
*                                                                               
TRCTY10  MVI   FILTNF,1            CTY=USA                                      
         MVI   0(RF),C'U'                                                       
         LA    RF,1(RF)                                                         
         CLI   SVAPROF+7,C'C'      CANADIAN AGY?                                
         BNE   TRCTY20             ->FILTNF=3,FILT=0,N,U                        
         B     TRCTYX                                                           
*                                                                               
TRCTY20  MVI   FILTNF,3            CTY=CTY(AGY)                                 
         MVC   0(2,RF),=C'0N'                                                   
         LA    RF,2(RF)                                                         
*                                                                               
TRCTYX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TROOW    NTR1                       OUT-OF-WEEK CLIENT                          
         LA    RE,CEXTRA+10-CLTRECD                                             
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TROOW10                                                          
         MVI   FILTNF,2            OOW=N                                        
         MVC   0(2,RF),=C'0N'                                                   
         LA    RF,2(RF)                                                         
         B     TROOWX                                                           
*                                                                               
TROOW10  MVI   FILTNF,1            OOW=Y                                        
         MVI   0(RF),C'Y'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TROOWX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRGST    NTR1                       GST CODE                                    
         LA    RE,CEXTRA+11-CLTRECD                                             
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'S'                                                    
         BNE   TRGST10                                                          
         MVI   FILTNF,2            GST=S                                        
         MVC   0(2,RF),=C'0S'                                                   
         LA    RF,2(RF)                                                         
         B     TRGSTX                                                           
*                                                                               
TRGST10  MVI   FILTNF,1            GST=U,X,Z                                    
         MVC   0(1,RF),SC2NDFLD                                                 
         LA    RF,1(RF)                                                         
*                                                                               
TRGSTX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRFRZ    NTR1                       CLIENT FREEZE DATE                          
*                                                                               
         CLI   SC2NDFLD,C'Y'                                                    
         BE    *+12                                                             
         CLI   SC2NDFLD,C'N'                                                    
         BNE   ERRINVFV            INVALID FILTER VALUE ERROR                   
*                                                                               
         MVC   CLTFRZ,SC2NDFLD                                                  
         LLC   RE,FCNTR            DECREMENT REC FILTER COUNTER BY 1            
         BCTR  RE,0                                                             
         STC   RE,FCNTR                                                         
*                                                                               
         MVI   DISFLAG,C'3'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRCFR    NTR1                       CLIENT FREEZE FLAG                          
*                                                                               
         CLI   SC2NDFLD,C'Y'                                                    
         BE    *+12                                                             
         CLI   SC2NDFLD,C'N'                                                    
         BNE   ERRINVFV            INVALID FILTER VALUE ERROR                   
*                                                                               
         MVC   CLFRZFLG,SC2NDFLD                                                
         LLC   RE,FCNTR            DECREMENT REC FILTER COUNTER BY 1            
         BCTR  RE,0                                                             
         STC   RE,FCNTR                                                         
*                                                                               
         MVI   DISFLAG,C'4'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRSDA    NTR1                       SPECIAL DEMO ADJ.                           
         LA    RE,CEXTRA+12-CLTRECD                                             
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRSDA10                                                          
         MVI   FILTNF,2            SDA=N                                        
         MVC   0(2,RF),=C'0N'                                                   
         LA    RF,2(RF)                                                         
         B     TRSDAX                                                           
*                                                                               
TRSDA10  MVI   FILTNF,1            SDA=Y                                        
         MVI   0(RF),C'Y'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRSDAX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRADD    NTR1                       ADDS XFR FOR PRD                            
         LA    RE,CEXTRA+13-CLTRECD                                             
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRADD10                                                          
         MVI   FILTNF,2            ADD=N                                        
         MVC   0(2,RF),=C'0N'                                                   
         LA    RF,2(RF)                                                         
         B     TRADDX                                                           
*                                                                               
TRADD10  MVI   FILTNF,1            ADD=Y                                        
         MVI   0(RF),C'Y'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRADDX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,C'2'                                                     
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRDLY    NTR1                      DAILY ESTIMATE                               
         LA    RE,CDAILY-CLTRECD                                                
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         CLI   SC2NDFLD,C'N'                                                    
         BNE   TRDLY10                                                          
         MVI   FILTNF,1            DLY=N                                        
         MVI   0(RF),X'00'                                                      
         LA    RF,1(RF)                                                         
         B     TRDLYX                                                           
*                                                                               
TRDLY10  MVI   FILTNF,1            DLY=Y                                        
         MVI   0(RF),C'Y'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
TRDLYX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,0                                                        
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TROFC    NTR1                      OFFICE NUMBER                                
         CLI   SC2NDLEN,1          HAVE 1 CHAR?                                 
         BE    *+8                 YES                                          
         CLI   SC2NDLEN,2          HAVE 2 CHAR?                                 
         BNE   ERRINVFV            NO, INVALID FILTER VALUE ERROR               
*                                                                               
         CLI   SC2NDFLD,C' '       CAN'T BE BLANK                               
         BE    ERRINVFV            INVALID FILTER VALUE ERROR                   
*                                                                               
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T217FFD+6                                                
         MVC   OFCLMT,T217FFD+6                                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,SC2NDFLD                                                 
         LR    R2,R1               R1 IS ACTIVE USING ON SCANNER                
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         LR    R1,R2               RESTORE R1                                   
         TM    OFCINDS,OFCIOINV    INVALID OFFICE?                              
         BNZ   ERRINVFV             YES                                         
         CLI   DMCB,0                                                           
         BNE   ERRINVFV                                                         
*                                                                               
         LA    RE,COFFICE-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         MVI   FILTFL,1                                                         
*                                                                               
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),OFCOFC                                                   
         LA    RF,1(RF)                                                         
*                                                                               
TROFCX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,0                                                        
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRAOF    NTR1                      ACC OFFICE CODE & ACC AGY OVERRIDE           
         CLI   SC2NDLEN,5          MUST BE <= 5 CHAR                            
         BH    ERRINVFV            INVALID FILTER VALUE ERROR                   
*                                                                               
         CLI   SC2NDFLD,C' '       CAN'T BE BLANK                               
         BE    ERRINVFV            INVALID FILTER VALUE ERROR                   
*                                                                               
         LA    RE,CACCOFC-CLTRECD  SET A FILTER OF ACC OFFICE CODE              
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
*                                                                               
         CLI   SC2NDLEN,2                                                       
         BH    TRAOF10                                                          
         MVI   FILTFL,2            LEN<=2                                       
         MVI   FILTNF,1                                                         
         MVC   0(2,RF),SC2NDFLD                                                 
         LA    RF,2(RF)                                                         
         B     TRAOFX                                                           
*                                                                               
TRAOF10  CLI   SC2NDLEN,3          LEN=3                                        
         BNE   TRAOF12                                                          
         CLI   SC2NDFLD,C'/'       ONLY ENTERING ACC AGENCY CODE?               
         BNE   ERRINVFV                                                         
         LA    RE,CACCAGY-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         ST    RF,FILTADD                                                       
         MVI   FILTFL,2                                                         
         MVI   FILTNF,1                                                         
         MVC   0(2,RF),SC2NDFLD+1                                               
         LA    RF,2(RF)                                                         
         B     TRAOFX                                                           
*                                                                               
TRAOF12  CLI   SC2NDLEN,4                                                       
         BH    TRAOF20                                                          
         CLI   SC2NDFLD+1,C'/'     LEN=4                                        
         BNE   ERRINVFV            INVALID FILTER VALUE ERROR                   
*                                                                               
         MVI   FILTFL,1                                                         
         MVI   FILTNF,1                                                         
         MVC   0(1,RF),SC2NDFLD                                                 
         LA    RF,1(RF)                                                         
*                                                                               
         LA    R5,FILTERQ(R5)      SET A FILTER OF ACC AGY OVERRIDE             
         LA    RE,CACCAGY-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         ST    RF,FILTADD                                                       
         MVI   FILTFL,2                                                         
         MVI   FILTNF,1                                                         
         MVC   0(2,RF),SC2NDFLD+2                                               
         LA    RF,2(RF)                                                         
*                                                                               
         LLC   RE,FCNTR            INCREMENT REC FILTER COUNTER BY 1            
         LA    RE,1(RE)                                                         
         STC   RE,FCNTR                                                         
         B     TRAOFX                                                           
*                                                                               
TRAOF20  CLI   SC2NDFLD+2,C'/'     LEN=5                                        
         BNE   ERRINVFV            INVALID FILTER VALUE ERROR                   
*                                                                               
         MVI   FILTFL,2                                                         
         MVI   FILTNF,1                                                         
         MVC   0(2,RF),SC2NDFLD                                                 
         LA    RF,2(RF)                                                         
*                                                                               
         LA    R5,FILTERQ(R5)      SET A FILTER OF ACC AGY OVERRIDE             
         LA    RE,CACCAGY-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         ST    RF,FILTADD                                                       
         MVI   FILTFL,2                                                         
         MVI   FILTNF,1                                                         
         MVC   0(2,RF),SC2NDFLD+3                                               
         LA    RF,2(RF)                                                         
*                                                                               
         LLC   RE,FCNTR            INCREMENT REC FILTER COUNTER BY 1            
         LA    RE,1(RE)                                                         
         STC   RE,FCNTR                                                         
         B     TRAOFX                                                           
*                                                                               
TRAOFX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,0                                                        
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRIFC    NTR1                      OFFICE NUMBER                                
         CLI   SC2NDLEN,8          MUST BE <= 8 CHAR                            
         BH    ERRINVFV            INVALID FILTER VALUE ERROR                   
*                                                                               
         CLI   SC2NDFLD,C' '       CAN'T BE BLANK                               
         BE    ERRINVFV            INVALID FILTER VALUE ERROR                   
*                                                                               
         LA    RE,CCLTIFC-CLTRECD                                               
         STH   RE,FILTDIS                                                       
         MVC   FILTADD,NEXTAVSP                                                 
         L     RF,NEXTAVSP                                                      
         LLC   RE,SC2NDLEN                                                      
         STC   RE,FILTFL                                                        
         MVI   FILTNF,1                                                         
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SC2NDFLD                                                 
         LA    RF,1(RE,RF)         RF+SC2NDLEN                                  
*                                                                               
TRIFCX   ST    RF,NEXTAVSP                                                      
         LA    R5,FILTERQ(R5)                                                   
         ST    R5,NEXTAVRF                                                      
*                                                                               
         CLI   SDFLAG,0            DISPLAY FORMAT SET ALREADY?                  
         BE    *+12                YES                                          
         MVI   DISFLAG,0                                                        
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         B     XIT                                                              
*---------------------------------------------------------------------*         
TRDIS    NTR1                      DISPLAY OPTION                               
         MVC   DISFLAG,SC2NDFLD    SET DISPLAY OPTION FLAG                      
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         LLC   RE,FCNTR            DECREMENT REC FILTER COUNTER BY 1            
         BCTR  RE,0                                                             
         STC   RE,FCNTR                                                         
TRDISX   B     XIT                                                              
*---------------------------------------------------------------------*         
TRDN     NTR1                      DISPLAY SHORTCUT OPTION                      
         CLI   SC2NDLEN,0          NO FILTER VALUE FOR D0,D1,D2                 
         BNE   ERRINVFV                                                         
*                                  SET DISPLAY OPTION FLAG                      
         MVC   DISFLAG,1(R3)       W/ 2ND CHAR OF FILTER NAME  (0/1/2)          
         MVI   SDFLAG,0            BLOCK CHANGE OF DISPLAY FORMAT               
*                                                                               
         LLC   RE,FCNTR            DECREMENT REC FILTER COUNTER BY 1            
         BCTR  RE,0                                                             
         STC   RE,FCNTR                                                         
TRDNX    B     XIT                                                              
*---------------------------------------------------------------------*         
         DROP  R1                                                               
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*                          MY ERROR MESSAGES                          *         
***********************************************************************         
*                          INPUT:  R1 = A(INV FILTER ENTRY IN SCANBLK)          
*                          INPUT:  R2 = A(FILTER SCREEN FIELD HEADER)           
*                          INPUT:  R3 = A(FILTER NAME MATCHED IN FVT)           
*                          OUTPUT: CONSTRUCT THE ERROR MESSAGE AND EXIT         
         USING SCANBLKD,R1                                                      
ERRINVFN XC    CONHEAD,CONHEAD                                                  
         LA    R5,CONHEAD                                                       
         USING INVFNAMD,R5                                                      
*                                                                               
         MVI   INVFN#,C'#'                                                      
         MVC   INVFNMSG,=C'Invalid filter name :  '                             
         MVC   INVFNIN,SC1STFLD    INVALID FILTER NAME                          
*                                                                               
         LR    RF,R1                                                            
         LA    RE,SCANBLK                                                       
         SR    RF,RE                                                            
         SR    RE,RE               PREPARE FOR DIVISION                         
         LA    R0,32                                                            
         DR    RE,R0                                                            
         LA    RF,1(RF)            RF = FILTER # ON THE LINE                    
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  INVFNFN,DUB                                                      
*                                                                               
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
         DROP  R5                                                               
*---------------------------------------------------------------------*         
ERRINVFV XC    CONHEAD,CONHEAD                                                  
         LA    R5,CONHEAD                                                       
         USING INVFVALD,R5                                                      
*                                                                               
         MVI   INVFV#,C'#'                                                      
         MVC   INVFVMS1,=C'Invalid filter value for '                           
         MVC   INVFVMS2,=C' :  '                                                
         CLI   SC2NDFLD,C' '                                                    
         BNE   *+10                                                             
         MVC   INVFVMS1(7),=C'Missing'                                          
*                                                                               
         MVC   INVFVIN,SC2NDFLD    INV FILTER VALUE                             
         MVC   INVFVFNA,0(R3)      FILTER NAME                                  
*                                                                               
         LR    RF,R1                                                            
         LA    RE,SCANBLK                                                       
         SR    RF,RE                                                            
         SR    RE,RE               PREPARE FOR DIVISION                         
         LA    R0,32                                                            
         DR    RE,R0                                                            
         LA    RF,1(RF)            RF = FILTER # ON THE LINE                    
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  INVFVFN,DUB                                                      
*                                                                               
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
         DROP  R5                                                               
*                                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINVFS MVC   CONHEAD(46),=C'Invalid filter syntax - type ? for more d+        
               etail'                                                           
         B     MYEXIT                                                           
*                                                                               
MSGDFT1  MVC   CONHEAD(55),=C'Filter description table displayed - hit +        
               enter for next'                                                  
         B     MYEXIT                                                           
*                                                                               
MSGDFT2  MVC   CONHEAD(58),=C'Filter description table ended - hit ente+        
               r for first again'                                               
MYEXIT   OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
ERRPFKY  MVC   ERRNUM,=AL2(PFKYINV)                                             
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
*                                  SHORT DESP OF ERROR MSGS                     
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
PFKYINV  EQU   1002                INVALID PF KEY                               
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT4,NODELLST                                                
         OI    GENSTAT3,OKVALSEL                                                
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
         OI    LSTREH+1,X'0C'      HIDE PF12=RETURN FIELD                       
         OI    LSTREH+6,X'80'      TRANSMIT THE RESULT                          
         CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    SETUP05             NO                                           
*                                                                               
*        CLI   CALLSTCK,X'76'/X'78'                                             
         CLI   0(RE),X'76'                                                      
         BE    *+12                                                             
         CLI   0(RE),X'78'                                                      
         BNE   *+12                                                             
         MVI   1(RE),0                                                          
         BE    SETUP05                                                          
         NI    LSTREH+1,X'FF'-X'04' LIGHT UP PF12=RETURN FIELD                  
*                                                                               
SETUP05  CLI   16(RA),0            FIRST TIME RUN                               
         BNE   *+16                                                             
         MVI   16(RA),1            FLIP THE 1ST TIME FLAG                       
         MVI   DFTFLAG,0           RESET DISPLAY FILTER TABLE FLAG              
         MVI   OKPFKEY,1           RESET INITPKEY CTRL FLAG                     
*                                                                               
         CLI   OKPFKEY,0           FILTER DESCRIPTION TABLE IS ON               
         BNE   SETUP10             NO                                           
*                                                                               
         OC    PFKEY,PFKEY         ANY PFKEYS ENTERED                           
         BZ    SETUPX              NO - EXIT                                    
         B     ERRPFKY             ELSE - CAN'T HAVE PFKEYS WHEN DFT ON         
*                                                                               
SETUP10  CLI   PFKEY,4                                                          
         BE    ERRPFKY             PF04 IS NOT ALLOWED                          
         CLI   PFKEY,5                                                          
         BE    ERRPFKY             PF05 IS NOT ALLOWED                          
*        TM    LSTREH+1,X'0C'      PF12 IS ALLOWED?                             
*        BNO   *+12                YES                                          
*        CLI   PFKEY,12                                                         
*        BE    ERRPFKY             PF12 IS NOT ALLOWED                          
*                                                                               
         L     R3,=A(PFTABLE)                                                   
         A     R3,RELO                                                          
         GOTO1 INITPFKY,DMCB,0(R3) MAINT PF TABLE                               
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        HEADHOOK                                                     *         
***********************************************************************         
HDHOOK   NTR1                                                                   
         MVC   H3+7(L'QMED),QMED                                                
         MVC   H3+10(L'MEDNM),MEDNM                                             
         MVC   H3+48(L'QCLT),LSTCLT                                             
         XIT1                                                                   
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
***********************************************************************         
*        CONSTANT                                                     *         
***********************************************************************         
HIKEY    DC    9X'FF'              HIGH KEY FOR CLT RECORD                      
HD       DS    0CL79                                                            
         DC    CL30'Sel  Clt Code/Name           '                              
         DC    CL49'   OFC#   AOF/AGY   TYPE   DLY   INTERFACE CODE'            
ULN      DS    0CL79                                                            
         DC    CL30'---  -------------           '                              
         DC    CL49'   ----   -------   ----   ---   --------------'            
HD0      DS    0CL79                                                            
         DC    CL33'SEL  CLT CODE/NAME OFC AOF/A P D '                          
         DC    CL46'PRIMARY PROFILE          SECONDARY PROFILE'                 
ULN0     DS    0CL79                                                            
         DC    CL33'---  ------------- --- ----- - - '                          
         DC    CL46'---------------          -----------------  '               
HD1      DS    0CL79                                                            
         DC    CL44'Sel  Clt Code/Name  TYP LBX U35 RSV BFC BEC '               
         DC    CL35'AAN PES CPP PAJ PDM ESR PRP EXC RAT'                        
ULN1     DS    0CL79                                                            
         DC    CL44'---  -------------  --- --- --- --- --- --- '               
         DC    CL35'--- --- --- --- --- --- --- --- ---'                        
HD2      DS    0CL79                                                            
         DC    CL43'Sel  Clt Code/Name     CDM CTX BID FLT CAM '                
         DC    CL36'USP ENO MKG GOL CTY OOW GST SDA ADD'                        
ULN2     DS    0CL79                                                            
         DC    CL43'---  -------------     --- --- --- --- --- '                
         DC    CL36'--- --- --- --- --- --- --- --- ---'                        
HD3      DS    0CL79                                                            
         DC    CL30'Sel  Clt Code/Name           '                              
         DC    CL50'   OFC#   AOF/AGY   TYPE   DLY   CLT FREEZE DATE'           
ULN3     DS    0CL79                                                            
         DC    CL30'---  -------------           '                              
         DC    CL50'   ----   -------   ----   ---   ---------------'           
HD4      DS    0CL79                                                            
         DC    CL43'Sel  Clt Code/Name             Frozen      '                
         DC    CL36'                                   '                        
ULN4     DS    0CL79                                                            
         DC    CL43'---  -------------             ------      '                
         DC    CL36'                                   '                        
         EJECT                                                                  
***********************************************************************         
*        EXTRA/PROFILE CONVERSION TABLE (XPCT/PCT)                              
***********************************************************************         
*              LENGTH OF ENTIRE ENTRY, DISPLAY LENGTH,                          
*              # VALUE IN LIST, DISPLAY VALUE LIST                              
PFCTTAB  DS    0C                       PROFILE CONVERSION TABLE                
PCT1     DC    AL1(PCT1X-PCT1),AL1(01)  CPROF+0  BRAND/POL TRNDS                
         DC    AL1(03),C'0N1Y2Y'                                                
PCT1X    EQU   *                                                                
PCT2     DC    AL1(PCT2X-PCT2),AL1(01)  CPROF+1  LOCK BOX NUM                   
         DC    AL1(00)        NO LIST                                           
PCT2X    EQU   *                                                                
PCT3     DC    AL1(PCT3X-PCT3),AL1(03)  CPROF+2  MKT/STA TURNAROUND             
         DC    AL1(04),C'0U5 1U5 2U3M3U3S'                                      
PCT3X    EQU   *                                                                
PCT4     DC    AL1(PCT4X-PCT4),AL1(03)  CPROF+3  RATING SERVICE                 
         DC    AL1(02),C'0NSI1ARB'  NEED TO CHK SVAPROF+7!='C'                  
PCT4X    EQU   *                                                                
PCT5     DC    AL1(PCT5X-PCT5),AL1(01)  CPROF+4  BILL FORMULA CNTRL             
         DC    AL1(00)        NO LIST                                           
PCT5X    EQU   *                                                                
PCT6     DC    AL1(PCT6X-PCT6),AL1(01)  CPROF+5  BILL ESTIMATE CNTRL            
         DC    AL1(00)        NO LIST                                           
PCT6X    EQU   *                                                                
PCT7     DC    AL1(PCT7X-PCT7),AL1(01)  CPROF+6  PRINT CLT CODE=AAN             
         DC    AL1(04),C'0NNN1YYY'                                              
PCT7X    EQU   *                                                                
PCT8     DC    AL1(PCT8X-PCT8),AL1(01)  CPROF+7  PRINT EST SERIES NM            
         DC    AL1(00)        NO LIST                                           
PCT8X    EQU   *                                                                
PCT9     DC    AL1(PCT9X-PCT9),AL1(01)  CPROF+8  GOALS CPP OVERRIDE             
         DC    AL1(02),C'0N1Y'                                                  
PCT9X    EQU   *                                                                
PCTA     DC    AL1(PCTAX-PCTA),AL1(01)  CPROF+9  PROGRAM ADJ. CONTROL           
         DC    AL1(03),C'001A2N'                                                
PCTAX    EQU   *                                                                
PCTB     DC    AL1(PCTBX-PCTB),AL1(01)  CPROF+A  POL TIMESHEET DEMOS            
         DC    AL1(00)        NO LIST                                           
PCTBX    EQU   *                                                                
PCTC     DC    AL1(PCTCX-PCTC),AL1(01)  CPROF+B  FORCE EST SERIES REQ           
         DC    AL1(04),C'0NNN1YYY'                                              
PCTCX    EQU   *                                                                
PCTD     DC    AL1(PCTDX-PCTD),AL1(01)  CPROF+C  PRD REQ FOR TRUE POL           
         DC    AL1(03),C'0NNNYY'                                                
PCTDX    EQU   *                                                                
PCTE     DC    AL1(PCTEX-PCTE),AL1(01)  CPROF+D  EXCL GROUP CODE                
         DC    AL1(00)        NO LIST                                           
PCTEX    EQU   *                                                                
PCTF     DC    AL1(PCTFX-PCTF),AL1(01)  CPROF+E  RATE CONTROL                   
         DC    AL1(00)        NO LIST                                           
PCTFX    EQU   *                                                                
*                                                                               
XPCTTAB  DS    0C                       EXTRA PROFILE CONVERSION TABLE          
XPCT1    DC    AL1(XPCT1X-XPCT1),AL1(01) CEXTRA+0 CANADIAN DEMO OPTION          
         DC    AL1(00)        NO LIST                                           
XPCT1X   EQU   *                                                                
XPCT2    DC    AL1(XPCT2X-XPCT2),AL1(01) CEXTRA+1 CANADIAN NETWORK TAX          
         DC    AL1(00)        NO LIST                                           
XPCT2X   EQU   *                                                                
XPCT3    DC    AL1(XPCT3X-XPCT3),AL1(01) CEXTRA+2 BUY ID REQUIRED               
         DC    AL1(00)  NO LIST, BUT DISPLAY N FOR 0                            
XPCT3X   EQU   *                                                                
XPCT4    DC    AL1(XPCT4X-XPCT4),AL1(01) CEXTRA+3 ESTIMATE FILTERS REQ          
         DC    AL1(03),C'0NNN1Y'                                                
XPCT4X   EQU   *                                                                
XPCT5    DC    AL1(XPCT5X-XPCT5),AL1(01) CEXTRA+4 CAMPAIGNS                     
         DC    AL1(00)        NO LIST                                           
XPCT5X   EQU   *                                                                
XPCT6    DC    AL1(XPCT6X-XPCT6),AL1(01) CEXTRA+5 U.S. SPILL                    
         DC    AL1(03),C'0NNNYY'                                                
XPCT6X   EQU   *                                                                
XPCT7    DC    AL1(XPCT7X-XPCT7),AL1(01) CEXTRA+6 'EST=NO' EST NAME             
         DC    AL1(03),C'0NNNYY'                                                
XPCT7X   EQU   *                                                                
XPCT8    DC    AL1(XPCT8X-XPCT8),AL1(01) CEXTRA+7 MKGDS IN MISSED MTH           
         DC    AL1(03),C'0NNNYY'                                                
XPCT8X   EQU   *                                                                
XPCT9    DC    AL1(XPCT9X-XPCT9),AL1(01) CEXTRA+8 GOAL REQD FOR BUY             
         DC    AL1(03),C'0NNNYY'                                                
XPCT9X   EQU   *                                                                
XPCTA    DC    AL1(XPCTAX-XPCTA),AL1(03) CEXTRA+9 COUNTRY                       
         DC    AL1(04),C'0USANUSAUUSACCAN'  NEED TO CHK SVAPROF+7!='C'          
XPCTAX   EQU   *                                                                
XPCTB    DC    AL1(XPCTBX-XPCTB),AL1(01) CEXTRA+A OUT-OF-WEEK CLIENT            
         DC    AL1(03),C'0NNNYY'                                                
XPCTBX   EQU   *                                                                
XPCTC    DC    AL1(XPCTCX-XPCTC),AL1(01) CEXTRA+B GST CODE                      
         DC    AL1(05),C'0SSSUUXXZZ'                                            
XPCTCX   EQU   *                                                                
XPCTD    DC    AL1(XPCTDX-XPCTD),AL1(01) CEXTRA+C SPECIAL DEMO ADJ.             
         DC    AL1(03),C'0NNNYY'                                                
XPCTDX   EQU   *                                                                
XPCTE    DC    AL1(XPCTEX-XPCTE),AL1(01) CEXTRA+D ADDS XFR FOR PRD              
         DC    AL1(03),C'0NNNYY'                                                
XPCTEX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*        FILTER VALIDATION TABLE (FVT)                                *         
***********************************************************************         
*              FILTER NAME,# VALID VALUES,LENGTH OF EACH VALID VALUE            
*              TRANSLATE ROUTINE,VALID VALUES                                   
FILVTAB  DS    0C                                                               
         DC    CL3'DIS',AL1(03),AL1(01)          DISPLAY OPTION                 
         DC    AL4(TRDIS),C'012'                                                
         DC    CL3'D0 ',AL1(00),AL1(00)          SHORTCUT FOR DIS=0             
         DC    AL4(TRDN)    NO FILTER VALUES                                    
         DC    CL3'D1 ',AL1(00),AL1(00)          SHORTCUT FOR DIS=1             
         DC    AL4(TRDN)    NO FILTER VALUES                                    
         DC    CL3'D2 ',AL1(00),AL1(00)          SHORTCUT FOR DIS=2             
         DC    AL4(TRDN)    NO FILTER VALUES                                    
*                                       CPROF FILTERS                           
         DC    CL3'TYP',AL1(03),AL1(04) CPROF+0  BRAND/POL TRNDS                
         DC    AL4(TRTYP),C'BPOLTPOLBRD '                                       
         DC    CL3'LBX',AL1(36),AL1(01) CPROF+1  LOCK BOX NUM                   
         DC    AL4(TRLBX),C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'               
         DC    CL3'U35',AL1(03),AL1(03) CPROF+2  MKT/STA TURNAROUND             
         DC    AL4(TRU35),C'U5 U3MU3S'                                          
         DC    CL3'RSV',AL1(04),AL1(03) CPROF+3  RATING SERVICE                 
         DC    AL4(TRRSV),C'NSICSIBBMARB'                                       
         DC    CL3'BFC',AL1(07),AL1(01) CPROF+4  BILL FORMULA CNTRL             
         DC    AL4(TRBFC),C'0123459'                                            
         DC    CL3'BEC',AL1(03),AL1(01) CPROF+5  BILL ESTIMATE CNTRL            
         DC    AL4(TRBEC),C'012'                                                
         DC    CL3'AAN',AL1(02),AL1(01) CPROF+6  PRINT CLT CODE=AAN             
         DC    AL4(TRAAN),C'NY'                                                 
         DC    CL3'PES',AL1(11),AL1(01) CPROF+7  PRINT EST SERIES NM            
         DC    AL4(TRPES),C'0123456789*'                                        
*option cpp also validates option cpprs                                         
         DC    CL3'CPP',AL1(02),AL1(01) CPROF+8  GOALS CPP OVERRIDE             
         DC    AL4(TRCPP),C'NY'                                                 
         DC    CL3'PAJ',AL1(03),AL1(01) CPROF+9  PROGRAM ADJ. CONTROL           
         DC    AL4(TRPAJ),C'0AN'                                                
         DC    CL3'PDM',AL1(03),AL1(01) CPROF+A  POL TIMESHEET DEMOS            
         DC    AL4(TRPDM),C'012'                                                
         DC    CL3'ESR',AL1(02),AL1(01) CPROF+B  FORCE EST SERIES REQ           
         DC    AL4(TRESR),C'NY'                                                 
         DC    CL3'PRP',AL1(02),AL1(01) CPROF+C  POL T/A REPORTS BY PRD         
         DC    AL4(TRPRP),C'NY'                                                 
         DC    CL3'EXC',AL1(36),AL1(01) CPROF+D  EXCL GROUP CODE                
         DC    AL4(TREXC),C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'               
         DC    CL3'RAT',AL1(10),AL1(01) CPROF+E  RATE CONTROL                   
         DC    AL4(TRRAT),C'012345678*'                                         
*                                       CEXTRA FILTERS                          
         DC    CL3'CDM',AL1(03),AL1(01) CEXTRA+0 CANADIAN DEMO OPTION           
         DC    AL4(TRCDM),C'0UC'                                                
         DC    CL3'CTX',AL1(02),AL1(01) CEXTRA+1 CANADIAN NETWORK TAX           
         DC    AL4(TRCTX),C'01'                                                 
         DC    CL3'BID',AL1(27),AL1(01) CEXTRA+2 BUY ID REQUIRED                
         DC    AL4(TRBID),C'ABCDEFGHIJKLMNOPQRSTUVWXYZ*'                        
         DC    CL3'FLT',AL1(02),AL1(01) CEXTRA+3 ESTIMATE FILTERS REQ           
         DC    AL4(TRFLT),C'NY'                                                 
         DC    CL3'CAM',AL1(02),AL1(01) CEXTRA+4 CAMPAIGNS                      
         DC    AL4(TRCAM),C'0E'         ???????                                 
         DC    CL3'USP',AL1(02),AL1(01) CEXTRA+5 US SPILL                       
         DC    AL4(TRUSP),C'NY'                                                 
         DC    CL3'ENO',AL1(02),AL1(01) CEXTRA+6 'EST=NO' EST NAME              
         DC    AL4(TRENO),C'NY'                                                 
         DC    CL3'MKG',AL1(02),AL1(01) CEXTRA+7 MKGDS IN MISSED MTH            
         DC    AL4(TRMKG),C'NY'                                                 
         DC    CL3'GOL',AL1(02),AL1(01) CEXTRA+8 GOALS REQD FOR BUY             
         DC    AL4(TRGOL),C'NY'                                                 
         DC    CL3'CTY',AL1(02),AL1(03) CEXTRA+9 COUNTRY                        
         DC    AL4(TRCTY),C'USACAN'                                             
         DC    CL3'OOW',AL1(02),AL1(01) CEXTRA+A OUT-OF-WEEK CLIENT             
         DC    AL4(TROOW),C'NY'                                                 
         DC    CL3'GST',AL1(04),AL1(01) CEXTRA+B GST CODE                       
         DC    AL4(TRGST),C'SUXZ'                                               
         DC    CL3'SDA',AL1(02),AL1(01) CEXTRA+C SPECIAL DEMO ADJ.              
         DC    AL4(TRSDA),C'NY'                                                 
         DC    CL3'ADD',AL1(02),AL1(01) CEXTRA+D ADDS XFR FOR PRD               
         DC    AL4(TRADD),C'NY'                                                 
*                                                                               
         DC    CL3'CFR',AL1(02),AL1(01)          CLIENT FREEZE FLAG             
         DC    AL4(TRCFR),C'NY'                                                 
*                                                                               
         DC    CL3'DLY',AL1(02),AL1(01)          DAILY ESTIMATE                 
         DC    AL4(TRDLY),C'NY'                                                 
         DC    CL3'OFC',AL1(00),AL1(00)          OFFICE NUMBER                  
         DC    AL4(TROFC)  ANY CHAR, MUST BE 1 CHAR                             
         DC    CL3'AOF',AL1(00),AL1(00)          ACC OFFICE CODE                
         DC    AL4(TRAOF)  ANY CHAR, MUST BE <= 5 CHARS                         
         DC    CL3'IFC',AL1(00),AL1(00)          INTERFACE CODE                 
         DC    AL4(TRIFC)  ANY CHAR, MUST BE <= 8 CHARS                         
         DC    CL3'FRZ',AL1(00),AL1(00)          CLIENT FREEZE MONTH            
         DC    AL4(TRFRZ)                                                       
         DC    CL3'UBC',AL1(02),AL1(01) COPT4    UCOMM BILL CONTROL             
         DC    AL4(TRUBC),C'NY'                                                 
         DC    CL3'BPS',AL1(02),AL1(01) COPT4    BILL % SPLIT                   
         DC    AL4(TRBPS),C'NY'                                                 
*                                                                               
         DC    XL1'00'                  END OF FILVTAB                          
         EJECT                                                                  
***********************************************************************         
*        FILTER DESCRIPTION TABLE                                     *         
***********************************************************************         
*              FILTER NAME, FILTER DESCRIPTION, FILTER VALUE                    
FIDETAB  DS    0C                                                               
         DC    CL3'AAN',CL30'Print Client Code as AAN'                          
         DC    CL25'N,Y'                                                        
         DC    CL3'ADD',CL30'ADDS Transfer by Product'                          
         DC    CL25'N,Y'                                                        
         DC    CL3'AOF',CL30'Acc Office Code(1 or 2)/Agy(2)'                    
         DC    CL25'ex: 01/SJ,1/*B'                                             
         DC    CL3'BEC',CL30'Bill Estimate Control'                             
         DC    CL25'0,1,2'                                                      
         DC    CL3'BFC',CL30'Bill Formula Control'                              
         DC    CL25'0-5,9'                                                      
         DC    CL3'BID',CL30'Buy ID Required'                                   
         DC    CL25'N,Y,A-Z,*'                                                  
         DC    CL3'CAM',CL30'Campaigns'                                         
         DC    CL25'0,E'                                                        
         DC    CL3'CDM',CL30'Canadian Demo Option'                              
         DC    CL25'0,U,C'                                                      
         DC    CL3'CTX',CL30'Canadian Network Tax'                              
         DC    CL25'0,1'                                                        
         DC    CL3'CPP',CL30'Goals CPP Override'                                
         DC    CL25'N,Y'                                                        
         DC    CL3'CTY',CL30'Country'                                           
         DC    CL25'USA,CAN'                                                    
         DC    CL3'D0 ',CL30'Shortcut for DIS=0'                                
         DC    CL25'No FilterValue!'                                            
         DC    CL3'D1 ',CL30'Shortcut for DIS=1'                                
         DC    CL25'No FilterValue!'                                            
         DC    CL3'D2 ',CL30'Shortcut for DIS=2'                                
         DC    CL25'No FilterValue!'                                            
         DC    CL3'DIS',CL30'List display format'                               
         DC    CL25'0,1,2'                                                      
         DC    CL3'DLY',CL30'Daily Estimate'                                    
         DC    CL25'N,Y'                                                        
         DC    CL3'FLT',CL30'Estimate Filter Required'                          
         DC    CL25'N,Y'                                                        
         DC    CL3'EXC',CL30'Exclusion Group Code'                              
         DC    CL25'0-9,A-Z'                                                    
         DC    CL3'ENO',CL30'''EST=NO'' Estimate Name'                          
         DC    CL25'N,Y'                                                        
         DC    CL3'ESR',CL30'Force Estimate Series Requests'                    
         DC    CL25'N,Y'                                                        
         DC    CL3'GOL',CL30'Goals Required for Buy'                            
         DC    CL25'N,Y'                                                        
         DC    CL3'GST',CL30'GST Code'                                          
         DC    CL25'S,U,X,Z'                                                    
         DC    CL3'IFC',CL30'Interface Code (max 8)'                            
         DC    CL25'any characters, but blank'                                  
         DC    CL3'LBX',CL30'Lock Box Number'                                   
         DC    CL25'0-9,A-Z'                                                    
         DC    CL3'MKG',CL30'MKGDS in Missed Month'                             
         DC    CL25'N,Y'                                                        
         DC    CL3'U35',CL30'Buy T/A Report'                                    
         DC    CL25'U5,U3M,U3S'                                                 
         DC    CL3'OFC',CL30'Office Number (1)'                                 
         DC    CL25'any character, but blank'                                   
         DC    CL3'OOW',CL30'Out-Of-Week Client'                                
         DC    CL25'N,Y'                                                        
         DC    CL3'PAJ',CL30'Program Adjacency Control'                         
         DC    CL25'0,A,N'                                                      
         DC    CL3'PES',CL30'Print Estimate Series Number'                      
         DC    CL25'0-9,*'                                                      
         DC    CL3'PDM',CL30'Pol Timesheet Demos'                               
         DC    CL25'0,1,2'                                                      
         DC    CL3'PRP',CL30'Pol T/A Reports by Product'                        
         DC    CL25'N,Y'                                                        
         DC    CL3'RAT',CL30'Rate Control'                                      
         DC    CL25'0-8,*'                                                      
         DC    CL3'RSV',CL30'Rating Service'                                    
         DC    CL25'NSI,ARB,CSI,BBM'                                            
         DC    CL3'SDA',CL30'Special Demo Adjustment'                           
         DC    CL25'N,Y'                                                        
         DC    CL3'TYP',CL30'Buying Type'                                       
         DC    CL25'BPOL,TPOL,BRD '                                             
         DC    CL3'USP',CL30'U.S. Spill'                                        
         DC    CL25'N,Y'                                                        
         DC    CL3'UBC',CL30'UComm Bill Control'                                
         DC    CL25'N,Y'                                                        
         DC    CL3'BPS',CL30'Bill % Split'                                      
         DC    CL25'N,Y'                                                        
         DC    CL3'FRZ',CL30'Client Freeze Date'                                
         DC    CL25'N,Y'                                                        
         DC    CL3'CFR',CL30'CLFRZ Client'                                      
         DC    CL25'N,Y'                                                        
         DC    X'00'               END OF FIDETAB                               
         EJECT                                                                  
***********************************************************************         
*        PFKEYS TABLES                                                *         
***********************************************************************         
PFTABLE DS   0H                                                                 
*        PRODUCT MAINT DISPLAY                                                  
         DC   AL1(MPF02X-*,02,PFTCPROG,(MPF02X-MPF02)/KEYLNQ,0)                 
         DC   CL3'PM '                 MAINT                                    
         DC   CL8'PRD'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF02    DC   AL1(KEYTYTWA,L'LSTMED-1),AL2(LSTMED-T217FFD)                      
         DC   AL1(KEYTYCUR,L'LSCLTC-1),AL2(LSCLTC-LSCLTC)                       
MPF02X   EQU  *                                                                 
*                                                                               
*        PRODUCT LIST                                                           
         DC   AL1(MPF03X-*,03,PFTCPROG,(MPF03X-MPF03)/KEYLNQ,0)                 
         DC   CL3'PL '                 MAINT                                    
         DC   CL8'PRD'                 RECORD                                   
         DC   CL8'LIST'                ACTION                                   
MPF03    DC   AL1(KEYTYTWA,L'LSTMED-1),AL2(LSTMED-T217FFD)                      
         DC   AL1(KEYTYCUR,L'LSCLTC-1),AL2(LSCLTC-LSCLTC)                       
MPF03X   EQU  *                                                                 
*                                                                               
*        CLIENT2 MAINT DISPLAY   **NOTE: ONLY SPECIAL SEL CODE IS USED          
*                                        PF04 IS NOT USED AT ALL                
         DC   AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                 
         DC   CL3'S2 '                 MAINT                                    
         DC   CL8'CL2'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF04    DC   AL1(KEYTYTWA,L'LSTMED-1),AL2(LSTMED-T217FFD)                      
         DC   AL1(KEYTYCUR,L'LSCLTC-1),AL2(LSCLTC-LSCLTC)                       
MPF04X   EQU  *                                                                 
*                                                                               
*        CLIENT2 MAINT CHANGE    **NOTE: ONLY SPECIAL SEL CODE IS USED          
*                                        PF05 IS NOT USED AT ALL                
         DC   AL1(MPF05X-*,05,PFTCPROG,(MPF05X-MPF05)/KEYLNQ,0)                 
         DC   CL3'C2 '                 MAINT                                    
         DC   CL8'CL2'                 RECORD                                   
         DC   CL8'CHANGE'              ACTION                                   
MPF05    DC   AL1(KEYTYTWA,L'LSTMED-1),AL2(LSTMED-T217FFD)                      
         DC   AL1(KEYTYCUR,L'LSCLTC-1),AL2(LSCLTC-LSCLTC)                       
MPF05X   EQU  *                                                                 
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(MPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
MPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        SUBROUTINE SECTION! (WHEN WE RUN OUT OF ADDRESSABILITY)      *         
*                                                                               
***********************************************************************         
*                          FILREC - FILTER RECORD ROUTINE             *         
***********************************************************************         
*                          INPUT:  AIO HAS A(REC)                               
*                          OUTPUT: CC=EQ IF REC PASSES THE FILTER               
*                                  OTHERWISE, CC=NE                             
FILREC   NTR1  BASE=*,LABEL=*                                                   
         LA    R5,FILTER                                                        
         USING FILTERD,R5                                                       
         ZICM  R0,FCNTR,1          REC FILTER COUNTER                           
         BZ    FRX                                                              
*                                                                               
FR10     L     R6,AIO                                                           
         AH    R6,FILTDIS                                                       
         L     R4,FILTADD          A(ALTERNATIVE FILTER VALUE LIST)             
         LLC   R1,FILTNF           # ALTERNATIVE REC FILTER VALUE               
         LLC   R3,FILTFL           EACH REC FILTER VALUE LENGTH                 
*                                                                               
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
***                                                                             
* THE FOLLOWING CODE IS TO TEST THE BITS IN COPT4. FILTADD CONTAINS             
* Y/N FOLLOWED BY THE BIT VALUE TO TEST AT COPT4.  THE REST OF THIS             
* CODE WOULD JUST TEST PROFILE VALUES THAT WERE EITHER Y OR N                   
***                                                                             
         L     R2,AIO              A(CLIENT RECORD)                             
         AHI   R2,(COPT4-CLTRECD)  ADD DISPLACEMENT OF FIELD                    
         CR    R2,R6               POINTING TO COPT4 MASK?                      
         BNE   FR20                NOPE                                         
         MVC   BYTE,0(R6)          SAVE COPT4                                   
         NC    BYTE,1(R4)          SAVE BIT IF SET IN COPT4                     
         CLI   0(R4),C'Y'          TESTING FOR BIT TO BE ON?                    
         BNE   FR15                NO - WANT TO TEST IF THE BIT IS OFF          
         CLC   BYTE,1(R4)          IS BIT SET?                                  
         BNE   FRNEQ               NO, EXIT CC=NEQ                              
         B     FR30                                                             
*                                                                               
FR15     CLC   BYTE,1(R4)          IS BIT SET?                                  
         BE    FRNEQ               YES                                          
*                                                                               
FREQU    CR    RB,RB                                                            
         B     FRX                 EXIT CC=EQU                                  
*                                                                               
FRNEQ    LTR   RB,RB                                                            
         B     FRX                 EXIT CC=NEQ                                  
*                                                                               
FR20     EX    RE,*+8              EX CLC FOR FILTER VALUE                      
         B     *+10                                                             
         CLC   0(0,R6),0(R4)       REC==FIL VALUE?                              
         BE    FR30                YES, NEXT FILTER                             
         LA    R4,0(R3,R4)         NO, NEXT VALUE IN THE LIST                   
         BCT   R1,FR20                                                          
         B     FRX                 NOT MATCH W/FILTER, CC=NE, EXIT              
*                                                                               
FR30     LA    R5,FILTERQ(R5)      NEXT FILTER                                  
         BCT   R0,FR10                                                          
FRX      XIT1                      MATCH W/FILTER, CC=EQ, EXIT                  
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*                          PRTHEAD - PRINT HEADING FOR DIF DIS FORMAT *         
***********************************************************************         
*                          INPUT:  DISFLAG IS SET. (0,1,2)                      
*                                  (FILVAL->TRDIS->SET DPFLAG)                  
PRTHEAD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   R1,HD0                                                           
         LA    R2,L'HD0-1                                                       
         LAY   R3,ULN0                                                          
         LA    R4,L'ULN0-1                                                      
         CLI   DISFLAG,C'0'                                                     
         BE    PH20                                                             
*                                                                               
         LAY   R1,HD1                                                           
         LA    R2,L'HD1-1                                                       
         LAY   R3,ULN1                                                          
         LA    R4,L'ULN1-1                                                      
         CLI   DISFLAG,C'1'                                                     
         BE    PH20                                                             
*                                                                               
         LAY   R1,HD2                                                           
         LA    R2,L'HD2-1                                                       
         LAY   R3,ULN2                                                          
         LA    R4,L'ULN2-1                                                      
         CLI   DISFLAG,C'2'                                                     
         BE    PH20                                                             
*                                                                               
         LAY   R1,HD4                                                           
         LA    R2,L'HD4-1                                                       
         LAY   R3,ULN4                                                          
         LA    R4,L'ULN4-1                                                      
         CLI   DISFLAG,C'4'                                                     
         BE    PH20                                                             
*                                                                               
         LAY   R1,HD3                                                           
         LA    R2,L'HD3-1                                                       
         LAY   R3,ULN3                                                          
         LA    R4,L'ULN3-1                                                      
         CLI   DISFLAG,C'3'                                                     
         JNE   PH10                                                             
         CLI   CLTFRZ,C'Y'                                                      
         JE    PH20                                                             
*                                                                               
PH10     LAY   R1,HD                                                            
         LA    R2,L'HD-1                                                        
         LAY   R3,ULN                                                           
         LA    R4,L'ULN-1                                                       
*                                  DEFLAUT DISPLAY FORMAT                       
PH20     EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   LSTHEAD(0),0(R1)                                                 
         OI    LSTHEADH+6,X'80'    TRANSMIT A NEW HEADING                       
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   LSTULN(0),0(R3)                                                  
         OI    LSTULNH+6,X'80'                                                  
*                                                                               
PHX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                          UNSEL - UNPROTECT ALL SEL FLDS             *         
***********************************************************************         
*                          UNPROTECT ALL SEL FLDS                               
*                          AND CLEAR ALL FLDS AFTER KEY FLDS                    
UNSEL    NTR1  BASE=*,LABEL=*                                                   
         LA    RE,LSTHEADH         1ST FLD AFTER KEY FIELDS                     
         LA    RF,LSTENDH          LAST FLD ON SCREEN                           
*                                                                               
UNS10    OI    6(RE),X'80'         TRANSMIT THE CHANGES                         
         CLI   0(RE),11            SEL FLD HEADER?                              
         BNE   *+8                                                              
         NI    1(RE),X'FF'-X'20'   SET PROTECT BIT OFF                          
         LLC   R1,0(RE)            FIELD LENGTH (HDR+DATA+EXT)                  
         SHI   R1,9                EX LEN = FLD LEN - HDR(8) - 1                
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,RE),8(RE)       CLEAR ALL DATA IN THE FLD                    
*                                                                               
         LA    RE,9(R1,RE)         BUMP TO NEXT FLD HEADER                      
         CR    RF,RE                                                            
         BNL   UNS10                                                            
*                                                                               
UNSX     MVI   SELFLAG,0           SET SELFLAG OFF                              
         MVI   DFTFLAG,0           RESET DISPLAY FILTER TABLE FLAG              
         XC    KEY,KEY             RESTART THE LISTING                          
         LA    RE,LSTSELH                                                       
         OI    6(RE),X'80'+X'40'   POSITION CURSOR TO 1ST SEL FLD               
         LLC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         ST    RE,ATHISLST         SET A(FIRST LIST LINE)                       
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                          DISFTAB - DISPLAY FILTER DESCRIPTION TABLE *         
***********************************************************************         
DISFTAB  NTR1  BASE=*,LABEL=*                                                   
         CLI   DFTFLAG,0                                                        
         BNE   DFT20                                                            
*                                                                               
         MVI   OKPFKEY,0           NOT ALLOW SETUP TO INITPFKY                  
         OI    LSTFKEYH+1,X'0C'    HIDE THE PFKEY DISPLAY                       
         OI    LSTFKEYH+6,X'80'                                                 
         MVI   SELFLAG,1           TELL LR TO CALL UNSEL                        
         LA    RE,LSTHEADH         1ST FLD AFTER KEY FIELDS                     
         LA    RF,LSTENDH          LAST FLD ON SCREEN                           
*                                                                               
*                           CLEAR & PROTECT ALL FIELDS AFTER KEY FIELDS         
DFT10    OI    6(RE),X'80'         TRANSMIT THE CHANGES                         
         OI    1(RE),X'20'         SET PROTECT BIT ON                           
         LLC   R1,0(RE)            FIELD LENGTH (HDR+DATA+EXT)                  
         SHI   R1,9                EX LEN = FLD LEN - HDR(8) - 1                
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,RE),8(RE)       CLEAR ALL DATA IN THE FLD                    
*                                                                               
         LA    RE,9(R1,RE)         BUMP TO NEXT FLD HEADER                      
         CR    RF,RE                                                            
         BNL   DFT10                                                            
*                                                                               
         MVC   LSTHEAD(72),=C'Filter Syntax:  FilterName=FilterValue   +        
                Note: No blanks in this string'                                 
         MVC   LSTULN(37),=C'      Example:  DIS=1,RSV=NSI,CTY=USA'             
*                                                                               
         LA    R3,LSTFDH1          SCREEN POSITION WHERE FDT HEADINGS           
         USING FDTLND,R3                                                        
         MVC   FDTLNST(10),=CL10'FilterName'                                    
         MVC   FDTLNDES(11),=CL11'Description'                                  
         MVC   FDTLNFV(11),=CL11'FilterValue'                                   
*                                                                               
         LA    R3,LSTFDH2                                                       
         MVC   FDTLNST(10),=CL11'----------'                                    
         MVC   FDTLNDES(11),=CL11'-----------'                                  
         MVC   FDTLNFV(11),=CL11'-----------'                                   
*                                                                               
DFT20    L     R2,=A(FIDETAB)      FILTER DES TABLE TO BE DISPLAYED             
         A     R2,RELO                                                          
         USING FIDETABD,R2                                                      
*                                  INDEX TO FDT TO BE DISPLAYED                 
         LLC   RE,DFTFLAG          DFT PAGE INDEX                               
         MH    RE,=Y(11)           X #LINES TO DISPLAY AT ONE TIME              
         MH    RE,=Y(FIDELENQ)     X LENGTH OF EACH FDT ENTRY                   
         AR    R2,RE                                                            
*                                                                               
         LA    R3,LSTFDTLH         LINE HDR                                     
         LA    R0,11               # LINES TO DISPLAY                           
*                                                                               
DFT30    OI    6(R3),X'80'         TRANSMIT THE LINE                            
         LA    R3,8(R3)            DATA OF THE LINE                             
         MVC   FDTLNFN,FIDEFN      FILTER NAME                                  
         MVC   FDTLNDES,FIDEDES    FILTER DESCRIPTION                           
         MVC   FDTLNFV,FIDEFV      FILTER VALUE                                 
*                            NEXT LINE HDR ON SCREEN (PASS A SEL FLD)           
         LA    R3,85(R3)           LN DATA(74) + SEL HDR+DATA(11)               
         LA    R2,FIDELENQ(R2)     NEXT ENTRY IN FDT                            
*                                                                               
         CLI   0(R2),0             END OF FDT?                                  
         BE    DFT40                                                            
         BCT   R0,DFT30                                                         
*                                                                               
         IC    RE,DFTFLAG          INCREMENT DFTFLAG BY 1                       
         LA    RE,1(RE)                                                         
         STC   RE,DFTFLAG                                                       
         B     DFTX1                                                            
*                                                                               
DFT40    MVI   DFTFLAG,0           RESTART DFT FROM BEGINNING                   
*                                                                               
         BCTR  R0,0                CLEAR REST OF THE SCREEN                     
DFT50    OI    6(R3),X'80'         TRANSMIT THE LINE                            
         LA    R3,8(R3)            DATA OF THE LINE                             
         XC    0(74,R3),0(R3)      CLEAR LN DATA                                
         LA    R3,85(R3)           LN DATA(74) + SEL HDR+DATA(11)               
         BCT   R0,DFT50                                                         
         B     DFTX2                                                            
*                                                                               
DFTX1    LA    R2,LSTFILH                                                       
         B     MSGDFT1                                                          
DFTX2    LA    R2,LSTFILH                                                       
         B     MSGDFT2                                                          
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
         LTORG                     LITERAL POOL FOR DISFTAB ROUTINE             
         EJECT                                                                  
***********************************************************************         
*                          PRTLINE - PRINT A LIST LINE                *         
***********************************************************************         
*                          INPUT:  R4 == A(REC)                                 
*                          INPUT:  DISFLAG IS SET. (0,1,2)                      
*                                  (FILVAL->TRDIS->SET DPFLAG)                  
*                          OUTPUT: MOVE INFO INTO LISTAR READY TO PRINT         
PRTLINE  NTR1  BASE=*,LABEL=*                                                   
         USING CLTRECD,R4                                                       
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         CLI   DISFLAG,C'0'                                                     
         BE    PL18                                                             
         CLI   DISFLAG,C'1'                                                     
         BE    PL20                                                             
         CLI   DISFLAG,C'2'                                                     
         BE    PL30                                                             
         CLI   DISFLAG,C'4'                                                     
         BE    PL40                                                             
*                                  DEFLAUT DISPLAY FORMAT                       
*                                  CHECK CLT CODE = AAN AND UNPK IT             
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),LSCLTC                             
         MVI   LSSEP1,C'/'                                                      
         MVC   LSCLTN,CNAME        CLIENT NAME                                  
*                                                                               
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(X'01',ACOMFACS)                     
         CLI   0(R1),0                                                          
         BNE   *+10                                                             
         MVC   LSOFFN,OFCOFC2                                                   
*                                                                               
         MVC   LSAOFC,CACCOFC      ACC OFFICE CODE                              
         OC    CACCAGY,CACCAGY     ACC AGENCY                                   
         BZ    *+14                                                             
         MVI   LSSEP2,C'/'                                                      
         MVC   LSACCA,CACCAGY      ACC AGENCY                                   
*                                                                               
         CLI   DISFLAG,C'3'        HERE, IT CAN'T BE 0, 1, NOR 2                
         BNE   PL08                                                             
*                                                                               
         CLI   CLTFRZ,C'Y'         ONLY WANT CLIENTS W/FREEZE DATE?             
         BNE   PL08                                                             
         LA    RE,CLOCKYM-CLTRECD                                               
         MVC   WORK(2),CLOCKYM                                                  
         MVI   WORK+2,X'01'                                                     
         NI    WORK+1,X'FF'-X'80'-X'40'                                         
         GOTO1 DATCON,DMCB,(3,WORK),(6,LSIFC)                                   
         TM    CLOCKMON,X'80'      LOCK EVERYTHING PRIOR TO DATE                
         BZ    *+8                                                              
         MVI   LSIFC+6,C'-'                                                     
         TM    CLOCKMON,X'40'      LOCK EVERYTHING AFTER DATE                   
         BZ    *+8                                                              
         MVI   LSIFC+6,C'+'                                                     
         B     PL10                                                             
*                                                                               
PL08     MVC   LSIFC,CCLTIFC       INTERFACE CODE                               
*                                                                               
PL10     MVC   LSTYP,=C'BPOL'                                                   
         CLI   CPROF,C'0'                                                       
         BNE   PL14                                                             
*                                                                               
         MVC   LSTYP,=C'TPOL'                                                   
         CLI   CPOLONLY,C'Y'                                                    
         BE    PL14                                                             
*                                                                               
         MVC   LSTYP,=C'BRD '                                                   
*                                                                               
PL14     MVC   LSTYP+2(1),CPROF+3   RATING SERVICE                              
*                                                                               
         MVC   LSDLY,CDAILY        DAILY EST                                    
         OC    CDAILY,CDAILY                                                    
         BNZ   *+8                                                              
         MVI   LSDLY,C'N'                                                       
         B     PLX                                                              
*                                                                               
PL18     DS    0H                  DISFLAG=0, PACKED FORMAT DISPLAY             
*                                  CHECK CLT CODE = AAN AND UNPK IT             
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),LS0CLTC                            
         MVI   LS0SEP1,C'/'                                                     
         MVC   LS0CLTN,CNAME       CLIENT NAME                                  
*                                                                               
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(X'01',ACOMFACS)                     
         CLI   0(R1),0                                                          
         BNE   *+10                                                             
         MVC   LS0OFFN,OFCOFC2                                                  
*                                                                               
         MVC   LS0POL,CPOLONLY     POL BUY ONLY                                 
         OC    CPOLONLY,CPOLONLY                                                
         BNZ   *+8                                                              
         MVI   LS0POL,C'N'                                                      
         MVC   LS0DLY,CDAILY       DAILY EST                                    
         OC    CDAILY,CDAILY                                                    
         BNZ   *+8                                                              
         MVI   LS0DLY,C'N'                                                      
*                                                                               
         MVC   LS0AOFC,CACCOFC     ACC OFFICE CODE                              
         OC    CACCAGY,CACCAGY     ACC AGENCY                                   
         BZ    *+14                                                             
         MVI   LS0SEP2,C'/'                                                     
         MVC   LS0ACCA,CACCAGY                                                  
*                                                                               
         BAS   RE,DISPROF                                                       
         MVC   LS0PROF(5),TEMPWORK                                              
         MVC   LS0PROF+6(5),TEMPWORK+5                                          
         MVC   LS0PROF+12(3),TEMPWORK+10                                        
         MVC   LS0PROF+16(3),TEMPWORK+13                                        
         MVC   LS0PROF+20(3),TEMPWORK+16                                        
*                                                                               
         BAS   RE,DISEXTRA                                                      
         MVC   LS0EXTRA(3),TEMPWORK                                             
         MVC   LS0EXTRA+4(3),TEMPWORK+3                                         
         MVC   LS0EXTRA+8(3),TEMPWORK+6                                         
         MVC   LS0EXTRA+12(5),TEMPWORK+9                                        
         MVC   LS0EXTRA+18(2),TEMPWORK+14                                       
         B     PLX                                                              
*                                                                               
PL20     DS    0H                  DISFLAG=1, ONLY 1ST PROFILE DISPLAY          
*                                  CHECK CLT CODE = AAN AND UNPK IT             
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),LS0CLTC                            
         MVI   LS1SEP1,C'/'                                                     
         MVC   LS1CLTN,CNAME       CLIENT NAME                                  
*                                                                               
         BAS   RE,DISPROF                                                       
         MVC   LS1PF1,TEMPWORK+19                                               
         MVC   LS1PF2,TEMPWORK+1                                                
         MVC   LS1PF3,TEMPWORK+2                                                
         MVC   LS1PF4,TEMPWORK+5                                                
         MVC   LS1PF5,TEMPWORK+8                                                
         MVC   LS1PF6,TEMPWORK+9                                                
         MVC   LS1PF7,TEMPWORK+10                                               
         MVC   LS1PF8,TEMPWORK+11                                               
         MVC   LS1PF9,TEMPWORK+12                                               
         MVC   LS1PFA,TEMPWORK+13                                               
         MVC   LS1PFB,TEMPWORK+14                                               
         MVC   LS1PFC,TEMPWORK+15                                               
         MVC   LS1PFD,TEMPWORK+16                                               
         MVC   LS1PFE,TEMPWORK+17                                               
         MVC   LS1PFF,TEMPWORK+18                                               
         B     PLX                                                              
*                                                                               
PL30     DS    0H                  DISFLAG=2, ONLY 2ND PROFILE DISPLAY          
*                                  CHECK CLT CODE = AAN AND UNPK IT             
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),LS0CLTC                            
         MVI   LS2SEP1,C'/'                                                     
         MVC   LS2CLTN,CNAME       CLIENT NAME                                  
*                                                                               
         BAS   RE,DISEXTRA                                                      
         MVC   LS2EX1,TEMPWORK                                                  
         MVC   LS2EX2,TEMPWORK+1                                                
         MVC   LS2EX3,TEMPWORK+2                                                
         MVC   LS2EX4,TEMPWORK+3                                                
         MVC   LS2EX5,TEMPWORK+4                                                
         MVC   LS2EX6,TEMPWORK+5                                                
         MVC   LS2EX7,TEMPWORK+6                                                
         MVC   LS2EX8,TEMPWORK+7                                                
         MVC   LS2EX9,TEMPWORK+8                                                
         MVC   LS2EXA,TEMPWORK+9                                                
         MVC   LS2EXB,TEMPWORK+12                                               
         MVC   LS2EXC,TEMPWORK+13                                               
         MVC   LS2EXD,TEMPWORK+14                                               
         MVC   LS2EXE,TEMPWORK+15                                               
         B     PLX                                                              
*                                                                               
PL40     DS    0H                  DISFLAG=4, PACKED FORMAT DISPLAY             
*                                  CHECK CLT CODE = AAN AND UNPK IT             
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),LS4CLTC                            
         MVI   LS4SEP1,C'/'                                                     
         MVC   LS4CLTN,CNAME       CLIENT NAME                                  
*                                                                               
         TM    COPT2,COP2FRZ       IS THIS CLIENT CLFRZ?                        
         JZ    *+12                                                             
         MVI   LS4CFR,C'Y'                                                      
         J     *+8                                                              
         MVI   LS4CFR,C'N'                                                      
*                                                                               
PLX      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                          DISPROF - CONVERT CPROF TO DISPLAY FORMAT  *         
*                          DISEXTRA - CONVERT CEXTRA TO DISPLAY FORMAT*         
***********************************************************************         
*                          INPUT:  R4 = A(REC)                                  
*                          OUTPUT: TEMPWORK = PROFILE IN DISPLAY FORMAT         
DISPROF  NTR1                                                                   
         USING CLTRECD,R4                                                       
         LA    R4,CPROF                                                         
         DROP  R4                                                               
*                                                                               
         XC    TEMPWORK,TEMPWORK                                                
         LA    R1,TEMPWORK                                                      
         L     RF,=A(PFCTTAB)      PROFILE CONVERSION TABLE                     
         A     RF,RELO                                                          
         USING PCTTABD,RF                                                       
         LA    R0,15               BCT LOOP COUNTER                             
         BAS   RE,DIS10                                                         
*                                                                               
         USING CLTRECD,R3                                                       
         L     R3,AIO                                                           
*                                  THE BUYING TYPE DISPLAY                      
         CLI   DISFLAG,C'1'                                                     
         BNE   DP10                                                             
*                                                                               
         MVC   TEMPWORK+19(4),=C'BPOL'                                          
         CLI   CPROF,C'0'                                                       
         BNE   DP10                                                             
*                                                                               
         MVC   TEMPWORK+19(4),=C'TPOL'                                          
         CLI   CPOLONLY,C'Y'                                                    
         BE    DP10                                                             
         MVC   TEMPWORK+19(4),=C'BRD '                                          
*                                                                               
DP10     CLI   SVAPROF+7,C'C'      CANADIAN AGY?                                
         BNE   DPX                                                              
         CLI   CEXTRA,C'U'         WHEN CDM=U, US RATING IS DISPLAYED           
         BE    DPX                                                              
         CLI   CPROF+3,C'0'        CORRECT RATING SERVICE DISPLAY               
         BE    *+14                                                             
         MVC   TEMPWORK+5(3),=C'BBM'                                            
         B     DPX                                                              
         MVC   TEMPWORK+5(3),=C'CSI'                                            
*                                                                               
DPX      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  R3                                                               
*---------------------------------------------------------------------*         
DISEXTRA NTR1                                                                   
         USING CLTRECD,R4                                                       
         LA    R4,CEXTRA                                                        
         DROP  R4                                                               
*                                                                               
         XC    TEMPWORK,TEMPWORK                                                
         LA    R1,TEMPWORK                                                      
         L     RF,=A(XPCTTAB)      EXTRA PROFILE CONVERSION TABLE               
         A     RF,RELO                                                          
         USING PCTTABD,RF                                                       
         LA    R0,14               BCT LOOP COUNTER                             
         BAS   RE,DIS10                                                         
*                                                                               
         USING CLTRECD,R3                                                       
         L     R3,AIO                                                           
         CLI   CEXTRA+2,C'0'      CORRECT BUY ID REQ DISPLAY                    
         BNE   *+8                                                              
         MVI   TEMPWORK+2,C'N'                                                  
*                                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGY?                                
         BNE   DEX                                                              
         CLI   CEXTRA+9,C'U'       CORRECT COUNTRY DISPLAY                      
         BE    DEX                                                              
         CLI   CEXTRA+9,C'C'                                                    
         BE    DEX                                                              
         MVC   TEMPWORK+9(3),=C'CAN'                                            
DEX      XIT1                                                                   
         DROP  R3                                                               
*---------------------------------------------------------------------*         
DIS10    ZICM  R3,PCTTNV,1        # OF VALUES IN THE LIST                       
         BNZ   *+18                                                             
         MVC   0(1,R1),0(R4)      NO LIST, NO CONVERSION FOR DISPLAY            
         LA    R1,1(R1)                                                         
         B     DIS40              NEXT BYTE IN CPROF                            
*                                                                               
         LLC   R2,PCTTDLEN        DISPLAY LENGTH                                
         LA    R5,PCTTLIS         LIST                                          
*                                                                               
DIS20    CLC   0(1,R4),0(R5)      REC == LIST?                                  
         BE    DIS30              MATCH                                         
         LA    R5,1(R2,R5)        NO, BUMP THE LIST (1+DISPLAY LEN)             
         BCT   R3,DIS20                                                         
*                                 NO MATCH, IMPOSSIBLE, BAD REC                 
         LA    R5,PCTTLIS         USE DEFAULT VALUE (1ST DISPLAY VALUE          
*                                 ON THE LIST)                                  
DIS30    BCTR  R2,0               MOVE DISPLAY VALUE INTO TEMPWORK              
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),1(R5)                                                    
         LA    R1,1(R2,R1)                                                      
*                                                                               
DIS40    LA    R4,1(R4)           NEXT BYTE IN CPROF                            
         LLC   R2,PCTTALEN        OVERALL LENGTH OF THE ENTRY                   
         LA    RF,0(R2,RF)        NEXT TABLE ENTRY                              
         BCT   R0,DIS10                                                         
*                                                                               
DISX     BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        HEADSPEC - DEFAULT                                                     
***********************************************************************         
HSPEC    DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'CLIENT LIST'                                             
         SSPEC H2,30,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H3,1,C'MEDIA:'                                                   
         SSPEC H3,41,C'CLIENT:'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'CLT CODE/NAME'                                            
         SSPEC H6,1,C'-------------'                                            
         SSPEC H5,29,C'OFC#'                                                    
         SSPEC H6,29,C'----'                                                    
         SSPEC H5,36,C'AOF/AGY'                                                 
         SSPEC H6,36,C'-------'                                                 
         SSPEC H5,46,C'TYPE'                                                    
         SSPEC H6,46,C'----'                                                    
         SSPEC H5,53,C'DLY'                                                     
         SSPEC H6,53,C'---'                                                     
         SSPEC H5,59,C'INTERFACE CODE'                                          
         SSPEC H6,59,C'--------------'                                          
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*        HEADSPEC - DISPLAY TYPE 0                                              
***********************************************************************         
HSPEC0   DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'CLIENT LIST'                                             
         SSPEC H2,30,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H3,1,C'MEDIA:'                                                   
         SSPEC H3,41,C'CLIENT:'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'CLT CODE/NAME'                                            
         SSPEC H6,1,C'-------------'                                            
         SSPEC H5,15,C'OFC'                                                     
         SSPEC H6,15,C'---'                                                     
         SSPEC H5,19,C'AOF/A'                                                   
         SSPEC H6,19,C'-----'                                                   
         SSPEC H5,25,C'P'                                                       
         SSPEC H6,25,C'-'                                                       
         SSPEC H5,27,C'D'                                                       
         SSPEC H6,27,C'-'                                                       
         SSPEC H5,29,C'PRIMARY PROFILE'                                         
         SSPEC H6,29,C'---------------'                                         
         SSPEC H5,54,C'SECONDARY PROFILE'                                       
         SSPEC H6,54,C'-----------------'                                       
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        HEADSPEC - DISPLAY TYPE 1                                              
***********************************************************************         
HSPEC1   DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'CLIENT LIST'                                             
         SSPEC H2,30,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H3,1,C'MEDIA:'                                                   
         SSPEC H3,41,C'CLIENT:'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'CLT CODE/NAME'                                            
         SSPEC H6,1,C'-------------'                                            
         SSPEC H5,16,C'TYP'                                                     
         SSPEC H6,16,C'---'                                                     
         SSPEC H5,20,C'LBX'                                                     
         SSPEC H6,20,C'---'                                                     
         SSPEC H5,24,C'U35'                                                     
         SSPEC H6,24,C'---'                                                     
         SSPEC H5,28,C'RSV'                                                     
         SSPEC H6,28,C'---'                                                     
         SSPEC H5,32,C'BFC'                                                     
         SSPEC H6,32,C'---'                                                     
         SSPEC H5,36,C'BEC'                                                     
         SSPEC H6,36,C'---'                                                     
         SSPEC H5,40,C'AAN'                                                     
         SSPEC H6,40,C'---'                                                     
         SSPEC H5,44,C'PES'                                                     
         SSPEC H6,44,C'---'                                                     
         SSPEC H5,48,C'CPP'                                                     
         SSPEC H6,48,C'---'                                                     
         SSPEC H5,52,C'PAJ'                                                     
         SSPEC H6,52,C'---'                                                     
         SSPEC H5,56,C'PDM'                                                     
         SSPEC H6,56,C'---'                                                     
         SSPEC H5,60,C'ESR'                                                     
         SSPEC H6,60,C'---'                                                     
         SSPEC H5,64,C'PRP'                                                     
         SSPEC H6,64,C'---'                                                     
         SSPEC H5,68,C'EXC'                                                     
         SSPEC H6,68,C'---'                                                     
         SSPEC H5,72,C'RAT'                                                     
         SSPEC H6,72,C'---'                                                     
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        HEADSPEC - DISPLAY TYPE 2                                              
***********************************************************************         
HSPEC2   DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'CLIENT LIST'                                             
         SSPEC H2,30,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H3,1,C'MEDIA:'                                                   
         SSPEC H3,41,C'CLIENT:'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'CLT CODE/NAME'                                            
         SSPEC H6,1,C'-------------'                                            
         SSPEC H5,19,C'CDM'                                                     
         SSPEC H6,19,C'---'                                                     
         SSPEC H5,23,C'CTX'                                                     
         SSPEC H6,23,C'---'                                                     
         SSPEC H5,27,C'BID'                                                     
         SSPEC H6,27,C'---'                                                     
         SSPEC H5,31,C'FLT'                                                     
         SSPEC H6,31,C'---'                                                     
         SSPEC H5,35,C'CAM'                                                     
         SSPEC H6,35,C'---'                                                     
         SSPEC H5,39,C'USP'                                                     
         SSPEC H6,39,C'---'                                                     
         SSPEC H5,43,C'ENO'                                                     
         SSPEC H6,43,C'---'                                                     
         SSPEC H5,47,C'MKG'                                                     
         SSPEC H6,47,C'---'                                                     
         SSPEC H5,51,C'GOL'                                                     
         SSPEC H6,51,C'---'                                                     
         SSPEC H5,55,C'CTY'                                                     
         SSPEC H6,55,C'---'                                                     
         SSPEC H5,59,C'OOW'                                                     
         SSPEC H6,59,C'---'                                                     
         SSPEC H5,63,C'GST'                                                     
         SSPEC H6,63,C'---'                                                     
         SSPEC H5,67,C'SDA'                                                     
         SSPEC H6,67,C'---'                                                     
         SSPEC H5,71,C'ADD'                                                     
         SSPEC H6,71,C'---'                                                     
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        HEADSPEC - CLIENT FREEZE DATE                                          
***********************************************************************         
HSPEC3   DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'CLIENT LIST'                                             
         SSPEC H2,30,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H3,1,C'MEDIA:'                                                   
         SSPEC H3,41,C'CLIENT:'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'CLT CODE/NAME'                                            
         SSPEC H6,1,C'-------------'                                            
         SSPEC H5,29,C'OFC#'                                                    
         SSPEC H6,29,C'----'                                                    
         SSPEC H5,36,C'AOF/AGY'                                                 
         SSPEC H6,36,C'-------'                                                 
         SSPEC H5,46,C'TYPE'                                                    
         SSPEC H6,46,C'----'                                                    
         SSPEC H5,53,C'DLY'                                                     
         SSPEC H6,53,C'---'                                                     
         SSPEC H5,59,C'CLT FREEZE DATE'                                         
         SSPEC H6,59,C'---------------'                                         
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*        HEADSPEC - DISPLAY TYPE 4                                              
***********************************************************************         
HSPEC4   DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'CLIENT LIST'                                             
         SSPEC H2,30,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H3,1,C'MEDIA:'                                                   
         SSPEC H3,41,C'CLIENT:'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'CLT CODE/NAME'                                            
         SSPEC H6,1,C'-------------'                                            
         SSPEC H5,29,C'FROZEN'                                                  
         SSPEC H6,29,C'------'                                                  
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*INCLUDE SPSFMWORKD                                                             
         PRINT  OFF                                                             
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
FAKEFLD  DS    XL11                                                             
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
*                                                                               
SVCLTKEY DS    CL13                SAVED CLT KEY                                
*                                                                               
CLT3C    DS    CL3                 TEMPERATORY STORAGE FOR CLT CODE             
TEMPWORK DS    CL30                TEMPERATORY WORKING STORAGE                  
OKPFKEY  DS    X                   CTRL FLAG FOR CALL INITPFKY IN SETUP         
SELFLAG  DS    X                   SEL FLDS UNPROTECT CALL FLAG                 
DFTFLAG  DS    X                   DISPLAY FILTER TABLE FLAG                    
SDFLAG   DS    X                   1:ALLOW CHANGE OF DISP FORMAT 0:NO           
DISFLAG  DS    X                   DISPLAY FORMAT FLAG                          
SCANBLK  DS    15CL32              SCANNER BLANK                                
NEXTAVSP DS    F                   A(NEXT AVAILABE SPACE IN FILSPACE)           
NEXTAVRF DS    F                   A(NEXT AVAILABE REC FILTER ENTRY)            
FILTER   DS    30CL6               REC FILTER TABLE                             
FCNTR    DS    X                   INPUTED FILTER COUNTER                       
FILSPACE DS    CL200               DYNAMIC REC FILTER VALUE LIST                
OFCBLK   DS    XL(OFCLENQ)                                                      
CLTFRZ   DS    CL1                                                              
CLFRZFLG DS    CL1                 lock from CLFRZ/REPORT                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        TABLE DSECTS                                                 *         
***********************************************************************         
FILTERD  DSECT                     REC FILTER DESECT                            
FILTDIS  DS    H                   DISPLACEMENT FROM CLTREC                     
FILTNF   DS    X                   # OF ALTERNATIVE FILTER VALUE                
FILTFL   DS    X                   LENGTH OF EACH FILTER VALUE                  
FILTADD  DS    A                   A(FILTER VALUE LIST)                         
FILTERQ  EQU   *-FILTERD                                                        
*                                                                               
FILVTABD DSECT                     FILTER VALIDATION TABLE DESECT               
FILVTNAM DS    CL3                 FILTER NAME                                  
FILVTNVF DS    X                   # OF VALID FILTER VALUE                      
FILVTFVL DS    X                   LENGTH OF EACH FILTER VALUE                  
FILVTTRT DS    AL4                 TRANSLATE ROUTINE ADDRESS                    
FILVTOVQ EQU   *-FILVTABD          OVERHEAD LENGTH                              
FILVTLIS DS    0C                  BEGIN VALID FILTER VALUE LIST                
*                                                                               
PCTTABD  DSECT                     EXTRA/PROFILE CONVERTION TABLE DSECT         
PCTTALEN DS    X                   OVERALL LENGTH OF THE ENTRY                  
PCTTDLEN DS    X                   DISPLAY LENGTH                               
PCTTNV   DS    X                   # VALUES IN THE LIST                         
PCTTOVQ  EQU   *-FILVTABD          OVERHEAD LENGTH                              
PCTTLIS  DS    0C                  LIST BEGINS                                  
*                                                                               
INVFNAMD DSECT                     INVALID FILTER NAME MESSAGE DSECT            
INVFN#   DC    C'#'                                                             
INVFNFN  DS    CL2                 FILTER # ON THE LINE                         
         DS    CL2                                                              
INVFNMSG DC    CL23'Invalid filter name :  '                                    
INVFNIN  DS    CL10                USER INPUTED FILTER NAME                     
INVFVALD DSECT                     INVALID FILTER VALUE MESSAGE ESECT           
INVFV#   DC    C'#'                                                             
INVFVFN  DS    CL2                 FILTER # ON THE LINE                         
         DS    CL2                                                              
INVFVMS1 DC    CL25'INVALID FILTER VALUE FOR '                                  
INVFVFNA DS    CL3                 FILTER NAME                                  
INVFVMS2 DC    CL4' :  '                                                        
INVFVIN  DS    CL10                USER INPUTED FILTER NAME                     
*                                                                               
FIDETABD DSECT                     FILTER DESCRIPTION TABLE (FDT) DSECT         
FIDEFN   DS    CL3                 FILTER NAME                                  
FIDEDES  DS    CL30                FILTER DESCRIPTION                           
FIDEFV   DS    CL25                FILTER VALUES                                
FIDELENQ EQU   *-FIDETABD          ENTRY LENGTH                                 
*                                                                               
FDTLND   DSECT                     FDT LIST LINE DSECT                          
FDTLNST  DS    0C                                                               
         DS    CL3                                                              
FDTLNFN  DS    CL3                 FILTER NAME                                  
         DS    CL8                                                              
FDTLNDES DS    CL30                FILTER DESCRIPTION                           
         DS    CL5                                                              
FDTLNFV  DS    CL25                FILTER VALUES                                
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
*SPGENCLT                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*SPSFMFFD                                                                       
*SCSFM76D                                                                       
*SCSFM77D                                                                       
*DDGENTWA                                                                       
*SPGENAPY                                                                       
*DDFLDIND                                                                       
*DDSCANBLKD                                                                     
*FAGETTXTD                                                                      
         PRINT OFF                                                              
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM76D          MAINTENACE SCREEN                            
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM77D          LIST SCREEN                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPGENAPY          AUTOPAY RECORD DSECT                         
       ++INCLUDE DDFLDIND          FOR FOUT                                     
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
       ++INCLUDE DDOFFICED         FOR OFFICED                                  
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
LSCLTC   DS    CL3                 CLIENT CODE                                  
         DS    CL1                                                              
LSSEP1   DC    C'/'                                                             
LSCLTN   DS    CL20                CLIENT NAME                                  
         DS    CL5                                                              
LSOFFN   DS    CL2                 OFFICE NUMBER                                
         DS    CL4                                                              
LSAOFC   DS    CL2                 ACC OFFICE CODE                              
LSSEP2   DC    C'/'                                                             
LSACCA   DS    CL2                 ACC AGENCY                                   
         DS    CL4                                                              
LSTYP    DS    CL4                 BUYING TYPE                                  
         DS    CL4                                                              
LSDLY    DS    CL1                 DAILY ESTIMATE                               
         DS    CL7                                                              
LSIFC    DS    CL8                 INTERFACE CODE                               
*                                                                               
         ORG   LISTAR              DISFLAG=0, PACKED FORMAT                     
LS0CLTC  DS    CL3                 CLIENT CODE                                  
LS0SEP1  DC    C'/'                                                             
LS0CLTN  DS    CL9                 CLIENT NAME                                  
         DS    CL1                                                              
LS0OFFN  DS    CL2                 OFFICE NUMBER                                
         DS    CL2                                                              
LS0AOFC  DS    CL2                 ACC OFFICE CODE                              
LS0SEP2  DC    C'/'                                                             
LS0ACCA  DS    CL2                 ACC AGENCY                                   
         DS    CL1                                                              
LS0POL   DS    CL1                 POL BUY ONLY                                 
         DS    CL1                                                              
LS0DLY   DS    CL1                 DAILY EST                                    
         DS    CL1                                                              
LS0PROF  DS    CL23                PROFILE                                      
         DS    CL2                                                              
LS0EXTRA DS    CL21                EXTRA PROFILE                                
*                                                                               
         ORG   LISTAR              DISFLAG=0, PACKED FORMAT                     
LS4CLTC  DS    CL3                 CLIENT CODE                                  
         DS    CL1                                                              
LS4SEP1  DC    C'/'                                                             
LS4CLTN  DS    CL20                CLIENT NAME                                  
         DS    CL3                                                              
LS4CFR   DS    CL1                 OFFICE NUMBER                                
*                                                                               
         ORG   LISTAR              LABELS FOR LISTMON                           
LS1CLTC  DS    CL3                 CLIENT CODE                                  
LS1SEP1  DC    C'/'                                                             
LS1CLTN  DS    CL9                 CLIENT NAME                                  
         DS    CL2                                                              
LS1PF1   DS    CL4                 PROFILE 1 TYP                                
         DS    CL1                                                              
LS1PF2   DS    CL1                 PROFILE 2 LBX                                
         DS    CL2                                                              
LS1PF3   DS    CL3                 PROFILE 3 U35                                
         DS    CL1                                                              
LS1PF4   DS    CL3                 PROFILE 4 RSV                                
         DS    CL2                                                              
LS1PF5   DS    CL1                 PROFILE 5 BFC                                
         DS    CL3                                                              
LS1PF6   DS    CL1                 PROFILE 6 BEC                                
         DS    CL3                                                              
LS1PF7   DS    CL1                 PROFILE 7 AAN                                
         DS    CL3                                                              
LS1PF8   DS    CL1                 PROFILE 8 PES                                
         DS    CL3                                                              
LS1PF9   DS    CL1                 PROFILE 9 CPP                                
         DS    CL3                                                              
LS1PFA   DS    CL1                 PROFILE A PAJ                                
         DS    CL3                                                              
LS1PFB   DS    CL1                 PROFILE B PDM                                
         DS    CL3                                                              
LS1PFC   DS    CL1                 PROFILE C ESR                                
         DS    CL3                                                              
LS1PFD   DS    CL1                 PROFILE D PRP                                
         DS    CL3                                                              
LS1PFE   DS    CL1                 PROFILE E EXC                                
         DS    CL3                                                              
LS1PFF   DS    CL1                 PROFILE F RAT                                
*                                                                               
         ORG   LISTAR              LABELS FOR LISTMON                           
LS2CLTC  DS    CL3                 CLIENT CODE                                  
LS2SEP1  DC    C'/'                                                             
LS2CLTN  DS    CL12                CLIENT NAME                                  
         DS    CL3                                                              
LS2EX1   DS    CL1                 EXTRA PROFILE 1 CDM                          
         DS    CL3                                                              
LS2EX2   DS    CL1                 EXTRA PROFILE 2 CTX                          
         DS    CL3                                                              
LS2EX3   DS    CL1                 EXTRA PROFILE 3 BID                          
         DS    CL3                                                              
LS2EX4   DS    CL1                 EXTRA PROFILE 4 FLT                          
         DS    CL3                                                              
LS2EX5   DS    CL1                 EXTRA PROFILE 5 CAM                          
         DS    CL3                                                              
LS2EX6   DS    CL1                 EXTRA PROFILE 6 USP                          
         DS    CL3                                                              
LS2EX7   DS    CL1                 EXTRA PROFILE 7 ENO                          
         DS    CL3                                                              
LS2EX8   DS    CL1                 EXTRA PROFILE 8 MKG                          
         DS    CL3                                                              
LS2EX9   DS    CL1                 EXTRA PROFILE 9 GOL                          
         DS    CL2                                                              
LS2EXA   DS    CL3                 EXTRA PROFILE A CTY                          
         DS    CL2                                                              
LS2EXB   DS    CL1                 EXTRA PROFILE B OOW                          
         DS    CL3                                                              
LS2EXC   DS    CL1                 EXTRA PROFILE C GST                          
         DS    CL3                                                              
LS2EXD   DS    CL1                 EXTRA PROFILE D SDA                          
         DS    CL3                                                              
LS2EXE   DS    CL1                 EXTRA PROFILE E ADD                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031SPSFM07   11/17/20'                                      
         END                                                                    
