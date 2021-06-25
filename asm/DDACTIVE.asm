*          DATA SET DDACTIVE   AT LEVEL 024 AS OF 09/09/19                      
*PHASE ACTIVEA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE CUREDIT                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE FAGETTXT                                                               
*INCLUDE FATABOFF                                                               
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DMPRTQB                                                                
*INCLUDE DMPRTQB2                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE PRTREC                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE SORTER                                                                 
         EJECT                                                                  
         TITLE 'COMBINED DDS DAILY ACTIVITY REPORT'                             
         PRINT NOGEN                                                            
ACTIVE   START                                                                  
         ENTRY SSB                                                              
         NBASE WORKL,*ACTIVE*,VREGSVE,CLEAR=YES                                 
         USING WORKD,RC            LOCAL W/S                                    
         L     RA,ACOMMON                                                       
         USING COMMON,RA                                                        
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         USING PLINED,P                                                         
         ST    RD,SAVERD                                                        
         BRAS  RE,INIT             INITIALISE                                   
*                                                                               
         BRAS  RE,READFILE         READ INPUT FILE/FILTER/ADD TO SORT           
         BNE   MAIN02                                                           
         BRAS  RE,PROCRECS         PROCESS OUTPUT FROM SORT                     
*                                                                               
MAIN02   GOTO1 VSORTER,PLIST,END   END SORT                                     
         B     XBASE               THAT'S ALL FOLKS                             
*                                                                               
VREGSVE  DC    V(REGSAVE)                                                       
ACOMMON  DC    A(COMMON)                                                        
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         MVI   GOTERR,C'N'         NO CARD ERRORS                               
         MVI   FORMAT,C'H'         DEFAULT FORMAT IS HEADER INFO                
*                                                                               
         LHI   RF,SCANTAB-WORKD    RELOCATE SCANTAB                             
         AR    RF,RC                                                            
         ST    RF,ASCANTAB                                                      
*                                                                               
         BRAS  RE,PRTDATE          PRINT OUT DATE= CARD                         
         BRAS  RE,GETCARD          GET ALL INPUT CARDS INTO A BUFFER            
         BNE   XBASE                                                            
         BRAS  RE,VALCARD          VALIDATE INPUT CARDS FROM BUFFER             
         BNE   XBASE                                                            
         BRAS  RE,OPENSYS          OPEN CONTROL FILE (IF REQUIRED)              
*                                                                               
         GOTO1 VSORTER,PLIST,SORTCARD,RECCARD,0                                 
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=CL8'FILTAB'),0                                    
         MVC   AFILTAB,DMCB+4                                                   
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ IN ALL INPUT CARDS                                             *         
* FORMAT INTO A SCANNER BLOCK AND SAVE FOR VALIDATION                 *         
***********************************************************************         
GETCARD  NTR1  BASE=*,LABEL=*                                                   
         L     R3,ASCANTAB         SAVE IN SCANTAB FOR PROCESSING               
         USING SCANBLKD,R3                                                      
*                                                                               
GCD02    GOTO1 VCARDS,PLIST,CARDIO,=C'RE00'                                     
         CLI   CARDIO,C'*'         IGNORE COMMENTS                              
         BE    GCD02                                                            
         CLC   =C'/*',CARDIO       END OF CARDS?                                
         BE    EXITOK              YES                                          
*                                                                               
*DON'T VALIDATE ARC CARD, just save it, YYUN, 9/30                              
         CLC   =C'ARC',CARDIO                                                   
         BNE   GCD03                                                            
         MVC   ARCCARD,CARDIO                                                   
         B     GCD02                                                            
*                                                                               
GCD03    GOTO1 VSCANNER,PLIST,(C'C',CARDIO),((R3))                              
         XR    RF,RF                                                            
         ICM   RF,1,4(R1)          RF=NUMBER OF INPUT PARAMETERS                
         BZ    GCD04                                                            
*                                                                               
         XR    R0,R0               INCREMENT NUMBER OF PARAMETERS               
         IC    R0,PARMCNT                                                       
         AR    R0,RF                                                            
         CHI   R0,PARMCNTQ                                                      
         BH    GCD04                                                            
         STC   R0,PARMCNT                                                       
*                                                                               
         MHI   RF,SCBLKLQ          GO TO NEXT FREE SLOT IN SCANTAB              
         AR    R3,RF                                                            
         B     GCD02                                                            
*                                                                               
GCD04    MVI   FERN,8              INVALID PARAMETER CARD                       
         BRAS  RE,ERRMSG                                                        
         B     EXITL                                                            
         EJECT                                                                  
**********************************************************************          
* PRINT OUT DATE=MM/DD/YY CARD FOR TODAY                             *          
**********************************************************************          
PRTDATE  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VDATCON,DMCB,(5,0),(10,P+5)      TODAY'S DATE                    
         MVC   P(5),=C'DATE='                                                   
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS IN SCANNER BLOCK (FROM GETCARD)            *         
***********************************************************************         
VALCARD  NTR1  BASE=*,LABEL=*                                                   
         MVC   TITLE,VCTITLE                                                    
         XR    R2,R2                                                            
         ICM   R2,1,PARMCNT                                                     
         BNZ   VCD02                                                            
         MVI   OUTTYPE,DEFOUT      SET DEFAULT OUTPUT                           
         B     EXITOK                                                           
*                                                                               
VCD02    L     R3,ASCANTAB                                                      
         USING SCANBLKD,R3                                                      
*                                                                               
VCD04    XR    RF,RF               RECONSTRUCT INPUT PARAMETER                  
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),SC1STFLD                                                    
         LA    RE,P+1(RF)                                                       
*                                                                               
         CLI   SC2NDLEN,0          ANY SECOND HALF TO PARAMETER                 
         BE    VCD06                                                            
         MVI   0(RE),C'='                                                       
         MVC   1(L'SC2NDFLD,RE),SC2NDFLD                                        
*                                                                               
VCD06    GOTO1 VPRINTER            PRINT RECONSTRUCTED PARAMETER                
*                                                                               
         L     R4,ACARDTAB         TABLE OF SUPPORTED PARAMETERS                
         USING CARDTABD,R4                                                      
         XR    RF,RF                                                            
         IC    RF,SC1STLEN         LENGTH OF PARAMETER                          
         BCTR  RF,0                                                             
*                                                                               
VCD08    CLI   0(R4),255           END OF TABLE?                                
         BNE   *+16                                                             
         MVI   FERN,13             FLAG UNKNOWN PARAMETER CARD                  
         BRAS  RE,ERRMSG                                                        
         B     VCD14                                                            
*                                                                               
         CLM   RF,1,CMIN           CHECK LENGTHS ARE OK                         
         BL    VCD10                                                            
         CLM   RF,1,CMAX                                                        
         BH    VCD10                                                            
         EX    RF,*+8              MATCH TEXT STRING                            
         BE    VCD12                                                            
         CLC   SC1STFLD(0),CARDTXT                                              
*                                                                               
VCD10    AHI   R4,CARDTABL         TRY NEXT ENTRY                               
         B     VCD08                                                            
*                                                                               
VCD12    ICM   RF,15,CARDVAL       GO AND VALIDATE THIS INPUT                   
         BZ    VCD14                                                            
         BASR  RE,RF                                                            
         BE    VCD14                                                            
         BRAS  RE,ERRMSG                                                        
*                                                                               
VCD14    AHI   R3,SCBLKLQ          NEXT LINE IN SCANTAB                         
         BCT   R2,VCD04            ANY MORE PARMS INPUT?                        
*                                                                               
         MVC   P(L'ARCCARD),ARCCARD    PRINT ARC AS THE LAST CARD               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         TM    OUTTYPE,ADDOUT+CHNGOUT+DELOUT+RESTOUT                            
         BNZ   *+8                                                              
         OI    OUTTYPE,DEFOUT      SET DEFAULT OUTPUT IF REQUIRED               
         ZAP   LINE,P99            FORCE PAGE THROW                             
*                                                                               
         CLI   GOTERR,C'Y'         SET CC IN CASE OF ERROR                      
         BE    EXITL                                                            
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE TICKET=XXXX PARAMETER                                     *          
* NTRY: R3     = A(SCANBLK)                                          *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
VALTIK   NTR1  BASE=*,LABEL=*                                                   
         MVC   V_LUID,SC2NDFLD                                                  
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE DDSIO=                                                    *          
* NTRY: R3     = A(SCANBLK)                                          *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
VALDDSIO NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,VDDSIO                                                     
         MVC   0(8,RF),SC2NDFLD                                                 
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE FORMAT=                                                   *          
* NTRY: R3     = A(SCANBLK)                                          *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
VALFORM  NTR1  BASE=*,LABEL=*                                                   
         MVC   FORMAT,SC2NDFLD                                                  
         CLI   FORMAT,C'H'         HEADER                                       
         BE    EXITOK                                                           
         CLI   FORMAT,C'E'         ELEMENT                                      
         BE    EXITOK                                                           
         CLI   FORMAT,C'D'         DUMP                                         
         BE    EXITOK                                                           
         MVI   FERN,19                                                          
         B     EXITL                                                            
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE DSPACE=                                                   *          
* NTRY: R3     = A(SCANBLK)                                          *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
VALDSPAC NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,ASSB                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),SC2NDFLD                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE USERNAME=XXXXXXXXXX PARAMETER                             *          
* NTRY: R3     = A(SCANBLK)                                          *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
VALNID   NTR1  BASE=*,LABEL=*                                                   
         OC    V_USRID,V_USRID                                                  
         BZ    *+12                                                             
         MVI   FERN,14             PARAMETER ALREADY SET                        
         B     EXITL                                                            
*                                                                               
         BRAS  RE,OPENSYS                                                       
*                                                                               
         L     R2,AIOTEMP                                                       
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SC2NDFLD                                                  
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,AIOTEMP,AIOTEMP                         
         CLI   8(R1),0             TEST IF RECORD FOUND                         
         BE    *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         GOTO1 VHELLO,PLIST,(C'G',CTFILE),(2,AIOTEMP),0,0,0,0                   
         CLI   12(R1),0            ELEMENT FOUND?                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,12(R1)                                                        
         MVC   V_USRID,2(RF)                                                    
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE USERID = XXXX PARAMETER                                   *          
* NTRY: R3     = A(SCANBLK)                                          *          
**********************************************************************          
         USING SCANBLKD,R3                                                      
VALID    NTR1  BASE=*,LABEL=*                                                   
         OC    V_USRID,V_USRID                                                  
         BZ    *+12                                                             
         MVI   FERN,14             PARAMETER ALREADY SET                        
         B     EXITL                                                            
*                                                                               
         TM    SC2NDVAL,SCNUMQ     NUMERIC?                                     
         BNZ   *+12                                                             
         MVI   FERN,15             PARAMETER MUST BE A NUMBER                   
         B     EXITL                                                            
*                                                                               
         ICM   R1,15,SC2NDNUM                                                   
         C     R1,=X'0000FFFF'                                                  
         BL    *+12                                                             
         MVI   FERN,16             NUMBER ENTERED IS TOO BIG                    
         B     EXITL                                                            
*                                                                               
         BRAS  RE,OPENSYS                                                       
*                                                                               
         L     R2,AIOTEMP                                                       
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         STCM  R1,3,CTIKNUM                                                     
         STCM  R1,3,V_USRID                                                     
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,AIOTEMP,AIOTEMP                         
         CLI   8(R1),0                                                          
         BE    EXITOK                                                           
         MVI   FERN,3              SET ERROR MESSAGE                            
         B     EXITL                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE AGYBIN = XX PARAMETER                                      *         
* NTRY: R3     = A(SCANBLK)                                           *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VALAGY   NTR1  BASE=*,LABEL=*                                                   
         OC    V_AGYID,V_AGYID                                                  
         BZ    *+12                                                             
         MVI   FERN,14             PARAMETER ALREADY SET                        
         B     EXITL                                                            
*                                                                               
         TM    SC2NDVAL,SCNUMQ     NUMERIC?                                     
         BNZ   *+12                                                             
         MVI   FERN,18             PARAMETER MUST BE A NUMBER                   
         B     EXITL                                                            
*                                                                               
         ICM   R1,15,SC2NDNUM                                                   
         CHI   R1,255                                                           
         BNH   *+12                                                             
         MVI   FERN,16             NUMBER ENTERED IS TOO BIG                    
         B     EXITL                                                            
*                                                                               
         STC   R1,V_AGYID                                                       
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROGNAME=XXXXXXX PARAMETER                                 *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VALPRG   NTR1  BASE=*,LABEL=*                                                   
         CLI   FLTSYS,0            YOU MUST HAVE A SYSTEM FILTER                
         BNE   *+12                                                             
         MVI   FERN,21                                                          
         B     EXITL                                                            
*                                                                               
         L     R5,VSELIST                                                       
         LH    RE,0(R5)                                                         
         L     RF,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
VPRG02   CLI   FLTSYSF,FLTSYSFO    FLAG GENERIC SYSTEM TYPE                     
         BNE   *+14                                                             
         CLC   SEOVSYS,FLTSYS                                                   
         BE    VPRG04                                                           
         CLI   FLTSYSF,FLTSYSFN    FLAG SPECIFIC SYSTEM TYPE                    
         BNE   *+14                                                             
         CLC   SESYS,FLTSYS                                                     
         BE    VPRG04                                                           
         BXLE  R5,RE,VPRG02                                                     
         DC    H'0'                CONTROL SYSTEM NOT FOUND                     
*                                                                               
VPRG04   ICM   R5,15,SEPGMS                                                     
         LH    RE,0(R5)                                                         
         L     RF,2(R5)                                                         
         AHI   R5,6                                                             
         USING PGMLSTD,R5                                                       
         XR    R1,R1                                                            
         ICM   R1,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,VPRGCLC          MATCH PROGRAM                                
         BE    VPRG06                                                           
         BXLE  R5,RE,*-8                                                        
         MVI   FERN,4              INVALID PROGRAM                              
         B     EXIT                                                             
*                                                                               
VPRG06   MVC   V_PRNUM,PGMNUM      SET NUMBER TO MATCH ON                       
         MVC   PROGRAM,PGMNAME     SAVE NAME                                    
         B     EXITOK                                                           
*                                                                               
VPRGCLC  CLC   PGMNAME(0),SC2NDFLD MATCH?                                       
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATES RECNUM=XXXX PARAMETER                                     *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VALRCN   NTR1  BASE=*,LABEL=*                                                   
         TM    SC2NDVAL,SCNUMQ                                                  
         BO    *+12                                                             
         MVI   FERN,12                                                          
         B     EXITL                                                            
*                                                                               
         L     R1,SC2NDNUM                                                      
         STC   R1,V_RTYPE                                                       
         CHI   R1,256              MUST BE <255                                 
         BL    EXITOK                                                           
         MVI   FERN,12                                                          
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SYSTEM                                                     *         
***********************************************************************         
         USING SCANBLKD,R3                                                      
VALSYS   NTR1  BASE=*,LABEL=*                                                   
         XR    R1,R1                                                            
         ICM   R1,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  R1,0                                                             
*                                                                               
         L     R2,VSYSLST          FIRST TRY GENERIC SYSTEM (SPOT/ACC)          
         LH    RE,0(R2)                                                         
         L     RF,2(R2)                                                         
         AHI   R2,6                                                             
         USING SYSLSTD,R2                                                       
         EX    R1,VSYSCLC1                                                      
         BE    *+12                                                             
         BXLE  R2,RE,*-8                                                        
         B     VSYS02                                                           
*                                                                               
         MVC   FLTSYS,SYSLNUM                                                   
         MVI   FLTSYSF,FLTSYSFO    FLAG GENERIC SYSTEM TYPE                     
         B     EXITOK                                                           
*                                                                               
VSYSCLC1 CLC   SC2NDFLD(0),SYSLNAME                                             
*                                                                               
VSYS02   L     R2,VSELIST          NOW TRY SPECIFIC SYSTEM                      
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         AHI   R2,6                                                             
         USING SELISTD,R2                                                       
         EX    R1,VSYSCLC2                                                      
         BE    VSYS04                                                           
         BXLE  R1,RE,*-8                                                        
         MVI   FERN,20                                                          
         B     EXITL                                                            
*                                                                               
VSYS04   MVC   FLTSYS,SESYS                                                     
         MVI   FLTSYSF,FLTSYSFN    FLAG SPECIFIC SYSTEM                         
         B     EXITOK                                                           
*                                                                               
VSYSCLC2 CLC   SC2NDFLD(0),SENAME                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATES SHOW ADD RECORD COMMAND                                   *         
***********************************************************************         
VALADD   NTR1  BASE=*,LABEL=*                                                   
         NI    OUTTYPE,255-ADDOUT                                               
         BRAS  RE,VALTRUE                                                       
         BNE   EXITL                                                            
         OI    OUTTYPE,ADDOUT                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SHOW CHANGE RECORD COMMAND                                 *         
***********************************************************************         
VALCHNG  NTR1  BASE=*,LABEL=*                                                   
         NI    OUTTYPE,255-CHNGOUT                                              
         BRAS  RE,VALTRUE                                                       
         BNE   EXITL                                                            
         OI    OUTTYPE,CHNGOUT                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SHOW DELETE RECORD COMMAND                                 *         
***********************************************************************         
VALDEL   NTR1  BASE=*,LABEL=*                                                   
         NI    OUTTYPE,255-DELOUT                                               
         BRAS  RE,VALTRUE                                                       
         BNE   EXITL                                                            
         OI    OUTTYPE,DELOUT                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SHOW RESTORE RECORD COMMAND                                *         
***********************************************************************         
VALREST  NTR1  BASE=*,LABEL=*                                                   
         NI    OUTTYPE,255-RESTOUT                                              
         BRAS  RE,VALTRUE                                                       
         BNE   EXITL                                                            
         OI    OUTTYPE,RESTOUT                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ DUMP OF RECOVERY FILE FILTER AND PASS TO SORT                  *         
***********************************************************************         
READFILE NTR1  BASE=*,LABEL=*                                                   
         OPEN  (TAPEIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RFIL02   EQU   *                                                                
         LAY   RE,RECBUFF          CLEAR RECBUFF                                
         LAY   RF,RECBUFFX                                                      
         SR    RF,RE                                                            
         XCEFL                                                                  
*                                                                               
         LAY   R7,RECBUFF                                                       
         GET   TAPEIN,(R7)         GET NEXT RECORD                              
         USING RECDS,R7                                                         
*                                                                               
RFIL03   TM    RRECTY,X'80'        TEST POINTER COPY OR CHG                     
         BO    RFIL02                                                           
         CLI   RSIN,RBACKOUT       Backout transaciton                          
         BE    RFIL02                                                           
*                                                                               
         XR    R3,R3               MAKE SURE FILE IS IN TABLE PROPERLY          
         IC    R3,RFILTY                                                        
         MHI   R3,DMFLLEN                                                       
         A     R3,AFILTAB                                                       
         USING FILTABD,R3                                                       
         CLC   RFILTY,DMFLNUM                                                   
         BE    *+6                                                              
         DC    H'0'                FIX DMFILTAB AND RELINK                      
*                                                                               
         ST    R3,AFILE                                                         
         TM    DMFLSTYP,DMFLREQ    REQUEST FILE                                 
         BO    RFIL02                                                           
         TM    DMFLSTYP,DMFLRCV    RECOVERY FILE                                
         BO    RFIL02                                                           
         TM    DMFLFLG1,DMFLISDA   DIRECTORY RECORD                             
         BO    RFIL02                                                           
*                                                                               
RFIL04   BRAS  RE,FILTER           FILTER ON PARAMETERS                         
         BNE   RFIL02              FAIL                                         
*                                                                               
         L     R2,ASRTBLK          NOW SET SORT BLOCK                           
         USING SRTBLKD,R2                                                       
         XC    SRTBLKD(SRTBLKLQ),SRTBLKD                                        
*                                                                               
         MVC   SRTUSER,RUSER       SET SORT PARAMETERS                          
         MVC   SRTPERS,RPERSON                                                  
         MVC   SRTDATE,RDATE                                                    
*                                                                               
         MVC   SRTTIME,RTIME                                                    
         TM    SRTTIME,X'80'       NEW STYLE TIME?                              
         BZ    RFIL06              NO                                           
         NI    SRTTIME,X'3F'       SET NEW STYLE TO OLD STYLE FOR SORT          
         MVI   SRTTIME+3,X'C0'                                                  
         ICM   R1,15,SRTTIME                                                    
         SRL   R1,4                                                             
         STCM  R1,15,SRTTIME                                                    
*                                                                               
RFIL06   MVC   SRTSYS,RSYS         SET SYSTEM PROGRAM AND SIN                   
         MVC   SRTPRG,RPRG                                                      
         MVC   SRTSIN,RSIN                                                      
         MVI   RSIN,0              Clear out HOB                                
*                                                                               
         XR    RF,RF                                                            
         IC    RF,DMFLKEYL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRTKEY(0),RCVFRST                                                
*                                                                               
         MVI   SRTTRL,C'N'                                                      
         TM    RTIME,X'40'         TRAILER RECORD ON THIS RECORD?               
         BZ    RFIL08              NO                                           
         MVI   SRTTRL,C'Y'                                                      
*                                                                               
         LH    RF,RECLN            POINT TO TRAILER RECORD                      
         LA    RF,RECDS(RF)                                                     
         BCTR  RF,0                                                             
         XR    R0,R0                                                            
         IC    R0,0(RF)                                                         
         AHI   RF,1                                                             
         SR    RF,R0                                                            
*                                                                               
         USING RECVEXT,RF                                                       
         MVC   SRTLUID,RLUID       SET SORT INFO FROM TRAILER                   
         MVC   SRTGRP,RAGYSEC                                                   
         MVI   OFFLINE,NO                                                       
         CLI   RSYSIX,0            If zero then offline                         
         BNE   *+8                                                              
         MVI   OFFLINE,YES                                                      
         DROP  RF                                                               
*                                                                               
RFIL08   LH    RF,RECLN                                                         
         AHI   RF,SRTBLKLQ                                                      
         STH   RF,SRTLEN           SET LENGTH OF SORT RECORD                    
*                                                                               
         LA    R0,SRTRECH          MOVE IN RECORD TO BLOCK                      
         LH    R1,RECLN                                                         
         LA    RE,RECDS                                                         
         LH    RF,RECLN                                                         
         MVCL  R0,RE                                                            
*                                                                               
         CLI   RRECTY,X'03'        RECORD WAS AN ADD?                           
         BE    RFIL12              YES                                          
         CLI   RRECTY,X'02'        RECORD WAS A COPY WITH NO CHANGE             
         BE    RFIL12              YES                                          
*                                                                               
         CLI   RRECTY,1            IF NOT IT BETTER BE A COPY                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LAY   RE,RECBUFF          CLEAR RECBUFF                                
         LAY   RF,RECBUFFX                                                      
         SR    RF,RE                                                            
         XCEFL                                                                  
         GET   TAPEIN,(R7)         GET NEXT RECORD                              
*                                                                               
         CLI   RRECTY,2            AND IT HAD BETTER BE A CHANGE                
         BE    *+8                                                              
         B     RFIL03              IGNORE LAST REC, PROCESS THIS AS NEW         
*                                  REASON: LIKELY, CHG REC IS TOO BIG!          
         XR    RF,RF                                                            
         IC    RF,DMFLKEYL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BE    RFIL10                                                           
         CLC   SRTKEY(0),RCVFRST   YOU BETTER HAVE MATCHING KEYS TOO            
**NOP**  DC    H'0'                ACTUALLY, AIN'T NO BIG THING                 
*                                                                               
RFIL10   LA    R0,SRTBLKD          ADD CHANGE AFTER COPY ON ONE RECORD          
         AH    R0,SRTLEN                                                        
         LH    R1,RECLN                                                         
         LA    RE,RECDS                                                         
         LH    RF,RECLN                                                         
         MVCL  R0,RE                                                            
*                                                                               
         LH    R0,SRTLEN           UPDATE LENGTH TO SHOW BOTH RECORDS           
         AH    R0,RECLN                                                         
         STH   R0,SRTLEN                                                        
*                                                                               
RFIL12   GOTO1 VSORTER,PLIST,PUT,SRTBLKD                                        
         B     RFIL02                                                           
*                                                                               
TAPEEND  CLOSE (TAPEIN)            THAT'S ALL SHE WROTE                         
         B     EXITOK                                                           
         DROP  R2,R3,R7                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETERS BEFORE PASSING RECORD TO SORT                   *         
* NTRY: R7 = A(RECOVERY RECORD)                                       *         
***********************************************************************         
         USING RECDS,R7                                                         
FILTER   NTR1  BASE=*,LABEL=*                                                   
         TM    RTIME,X'40'         TRAILER RECORD ON THIS RECORD?               
         BNZ   VFLT02              YES                                          
*                                                                               
         OC    V_TCODE,V_TCODE     TEST FOR TERMINAL #                          
         BZ    VFLT04                                                           
         CLC   RTRM,V_TCODE                                                     
         BNE   EXITL                                                            
*                                                                               
VFLT02   OC    V_LUID,V_LUID       TEST FOR LUID INSTEAD OF TERMINAL #          
         BZ    VFLT04                                                           
         LH    RF,RECLN                                                         
         LA    RF,RECDS(RF)                                                     
         BCTR  RF,0                                                             
         XR    R0,R0                                                            
         IC    R0,0(RF)                                                         
         AHI   RF,1                                                             
         SR    RF,R0                                                            
         USING RECVEXT,RF                                                       
         CLC   V_LUID,RLUID                                                     
         BNE   EXITL                                                            
         DROP  RF                                                               
*                                                                               
VFLT04   OC    V_USRID,V_USRID     TEST FOR USER ID #                           
         BZ    VFLT06                                                           
         CLC   RUSER,V_USRID                                                    
         BNE   EXITL                                                            
*                                                                               
VFLT06   OC    V_AGYID,V_AGYID     TEST FOR AGENCY ID                           
         BZ    VFLT08                                                           
         CLC   RAG,V_AGYID                                                      
         BNE   EXITL                                                            
*                                                                               
VFLT08   OC    FLTSYS,FLTSYS       SYSTEM FILTER                                
         BZ    VFLT12              NO                                           
*                                                                               
         CLI   FLTSYSF,FLTSYSFN    FILTERING FOR A SPECIFIC SYSTEM?             
         BNE   VFLT10              NO                                           
         CLC   RSYS,FLTSYS                                                      
         BNE   EXITL                                                            
         B     VFLT12              MATCH - CONTINUE                             
*                                                                               
VFLT10   L     R1,VSELIST          FILTERING FOR A SYSTEM TYPE                  
         LH    RE,0(R1)            (ALL SPOT SYSTEMS SAY)                       
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SESYS,RSYS          GET THE SYSTEM IN THE RECOVERY FILE          
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
*                                                                               
         CLC   SEOVSYS,FLTSYS      MATCH SYSTEM TYPE                            
         BNE   EXITL                                                            
         DROP  R1                                                               
*                                                                               
VFLT12   OC    V_PRNUM,V_PRNUM     PROGRAM NUMBER TO CHECK?                     
         BZ    VFLT14                                                           
         CLC   V_PRNUM,RPRG                                                     
         BNE   EXITL                                                            
*                                                                               
VFLT14   B     EXITOK                                                           
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORDS - AFTER YOU FINISH WITH THE INPUT FILE              *         
***********************************************************************         
PROCRECS NTR1  BASE=*,LABEL=*                                                   
         OPEN  (EXTRACT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZAP   PAGE,PONE                                                        
         MVC   TITLE(L'PRTOT),PRTOT                                             
*                                                                               
PRCR02   BRAS  RE,GETFRSRT         GET NEXT RECORD FROM SORT                    
         BNE   PRCRX               FINISHED                                     
*                                                                               
         L     R2,AIOSORT          GET A(CHANGE/ADD RECORD)                     
         USING RECDS,R2                                                         
SVE      USING SRTBLKD,SRTSAVE                                                  
*                                                                               
         TM    RTIME,X'80'         NEW STYLE TIME?                              
         BZ    PRCR04              NO                                           
         NI    RTIME,X'3F'         SETS NEW STYLE TIME TO OLD                   
         MVI   RTIME+3,X'C0'       STYLE TIME FOR OUTPUT                        
*                                                                               
         ICM   R1,15,RTIME                                                      
         SRL   R1,4                                                             
         STCM  R1,15,RTIME                                                      
*                                                                               
PRCR04   XC    SVETRL,SVETRL                                                    
         CLI   SVE.SRTTRL,C'Y'     TRAILER RECORD?                              
         BNE   PRCR06              NO                                           
*                                                                               
         LH    RF,RECLN                                                         
         LA    RF,RECDS(RF)                                                     
         BCTR  RF,0                                                             
         XR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         AHI   RF,1                                                             
         SR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVETRL(0),0(RF)     SAVE OFF TRAILER RECORD                      
TRL      USING RECVEXT,SVETRL                                                   
*                                                                               
         OC    LSTAGY,LSTAGY                                                    
         BZ    *+14                                                             
         CLC   LSTAGY,TRL.RAGYSEC  SAME AGENCY ALPHA                            
         BE    PRCR06              YES                                          
*                                                                               
         MVC   LSTAGY,TRL.RAGYSEC                                               
         MVC   HALF,TRL.RAGYSEC    SET NEW AGY 2 CHAR                           
         BRAS  RE,GETAGYID                                                      
         MVC   LSTAGYN,WORK+2      SET NEW PRINCIPLE ID                         
         DROP  TRL                                                              
*                                                                               
PRCR06   CLC   LSTNUM,RUSER        IS USERID SAME AS LAST TIME?                 
         BE    PRCR08                                                           
         MVC   HALF,RUSER          GET AND SAVE USERID                          
         BRAS  RE,GETUSRID                                                      
         MVC   LSTCTFRD,WORK                                                    
*                                                                               
PRCR08   BRAS  RE,GETTRM           GET TERMINAL DETAILS                         
         BRAS  RE,RECOUT           OUTPUT RECORD INFORMATION                    
         BRAS  RE,XTROUT           OUTPUT RECORD INFORMATION                    
         B     PRCR02                                                           
*                                                                               
PRCRX    CLOSE (EXTRACT)                                                        
         B     EXITOK                                                           
         DROP  R2,SVE                                                           
         EJECT                                                                  
***********************************************************************         
* GET A RECORD FROM THE SORT                                          *         
***********************************************************************         
GETFRSRT NTR1  BASE=*,LABEL=*                                                   
         L     R3,AFILE                                                         
         USING FILTABD,R3                                                       
*                                                                               
GFS02    GOTO1 VSORTER,PLIST,GET,0                                              
         ICM   R2,15,PLIST+4       END OF FILE?                                 
         BZ    EXITL                                                            
         USING SRTBLKD,R2                                                       
*                                                                               
         L     R0,AIOSORT          CLEAR SORT AREA                              
         LHI   R1,IOL                                                           
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AIOCOPY          AND CLEAR COPY AREA                          
         LHI   R1,IOL                                                           
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   SRTSAVE,SRTBLKD     SAVE A COPY OF THIS SORT RECORD HDR          
         USING RECDS,SRTRECH                                                    
*                                                                               
         CLI   RRECTY,3            WAS RECORD AN ADD?                           
         BNE   GFS04               NO                                           
         MVI   MODE,MODEADD        SET MODE=ADD                                 
         TM    OUTTYPE,ADDOUT      OUTPUT ADD RECORDS?                          
         BZ    GFS02               NO                                           
*                                                                               
         L     R0,AIOSORT          MOVE IT INTO MAIN I/O AREA                   
         LHI   R1,IOL                                                           
         LA    RE,RECDS                                                         
         LH    RF,RECLN                                                         
         MVCL  R0,RE                                                            
         B     EXITOK                                                           
*                                                                               
GFS04    TM    OUTTYPE,CHNGOUT+DELOUT+RESTOUT                                   
         BZ    GFS02               ONLY ADDS REQUIRED                           
*                                                                               
         L     R0,AIOCOPY          MOVE COPY RECORD INTO COPY AREA              
         CLI   RRECTY,X'02'                                                     
         BNE   *+8                                                              
         L     R0,AIOSORT          MOVE CHANGE WITHOUT COPY                     
*                                                                               
         LHI   R1,IOL                                                           
         LA    RE,RECDS                                                         
         LH    RF,RECLN                                                         
         MVCL  R0,RE                                                            
*                                                                               
         CLI   RRECTY,X'02'        IS THIS A 'NO COPY RECORD' CHANGE            
         BE    GFS06               YES                                          
*                                                                               
         L     R0,AIOSORT          MOVE CHANGE INTO CHANGE/ADD AREA             
         LHI   R1,IOL                                                           
         LA    RE,RECDS            CHANGE FOLLOWS COPY REMEMBER                 
         AH    RE,RECLN                                                         
         LH    RF,0(RE)                                                         
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,AIOSORT          MAKE SURE THIS PAIR REALLY CHANGED           
         LH    R5,0(R4)                                                         
         AHI   R5,-(RCVFRST-RECDS)                                              
*                                                                               
         L     RE,AIOCOPY                                                       
         LH    RF,0(RE)                                                         
         AHI   RF,-(RCVFRST-RECDS)                                              
*                                                                               
         CR    R5,RF               MAKE SURE LENGTHS ARE SAME                   
         BNE   GFS06                                                            
*                                                                               
         AHI   R4,RCVFRST-RECDS    GO TO FIRST ACTUAL DATA                      
         AHI   RE,RCVFRST-RECDS                                                 
*                                                                               
         CLCL  R4,RE               TEST RECORDS ARE IDENTICAL                   
         BE    GFS02               YES - IGNORE THEM                            
*                                                                               
GFS06    XR    RF,RF               GET DISP TO STATUS                           
         IC    RF,DMFLLEND                                                      
         AHI   RF,2                ( = DISP TO RECORD LEN + 2 )                 
*                                                                               
GFS08    MVI   MODE,MODECHG        SET DEFAULT MODE=CHANGE                      
         AHI   RF,RCVFRST-RECDS                                                 
         STH   RF,HALF             SET DISP TO STATUS FROM START                
*                                                                               
         LH    R1,HALF                                                          
         A     R1,AIOSORT                                                       
         MVC   BYTE,0(R1)          SAVE STATUS BYTE OF CHANGE                   
         LH    R1,HALF                                                          
         A     R1,AIOCOPY                                                       
         MVC   BYTE1,0(R1)         SAVE STATUS BYTE OF COPY                     
*                                                                               
         TM    BYTE1,X'80'         COPY RECORD DELETED?                         
         BO    GFS10               YES                                          
         TM    BYTE,X'80'          CHANGE RECORD DELETED?                       
         BZ    GFS12               NO                                           
*                                                                               
         MVI   MODE,MODEDEL        SET MODE=DELETED                             
         TM    OUTTYPE,DELOUT      REQUIRE TO OUTPUT DELETED RECORDS?           
         BZ    GFS02               NO                                           
         B     EXITOK                                                           
*                                                                               
GFS10    TM    BYTE,X'80'          CHANGE RECORD DELETED ALSO                   
         BO    GFS12               YES                                          
*                                                                               
         MVI   MODE,MODERES        SET MODE=RESTORED                            
         TM    OUTTYPE,RESTOUT     REQUIRED TO OUTPUT RESTORED RECORDS?         
         BZ    GFS02               NO                                           
         B     EXITOK                                                           
*                                                                               
GFS12    TM    OUTTYPE,CHNGOUT     OUTPUT COPY/CHANGE PAIRS?                    
         BZ    GFS02                                                            
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* EXTRACT OUTPUT ROUTINE                                              *         
***********************************************************************         
XTROUT   NTR1  BASE=*,LABEL=*                                                   
         LA    R0,XTRREC                                                        
         LHI   R1,L'XTRREC                                                      
         LA    RF,C' '                                                          
         SLL   RF,32-8                                                          
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AIOSORT          GET A(ADD OR CHANGE RECORD)                  
         USING RECDS,R2                                                         
SVE      USING SRTBLKD,SRTSAVE                                                  
         XR    R3,R3                                                            
         IC    R3,RFILTY                                                        
         MHI   R3,DMFLLEN                                                       
         A     R3,AFILTAB                                                       
         USING FILTABD,R3                                                       
         CLC   RFILTY,DMFLNUM      MAKE SURE FILE DETAILS THERE                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R3,AFILE            FILENAME                                     
         USING FILTABD,R3                                                       
         LA    R4,XTRREC                                                        
         MVC   0(5,R4),=CL5'00100' SET UP DUMMY ESS HEADER INFO                 
         MVI   5(R4),DELIM                                                      
         MVI   6(R4),C'A'                                                       
         MVI   7(R4),DELIM                                                      
         AHI   R4,8                                                             
*                                                                               
         GOTO1 VDATCON,DMCB,(3,RDATE),(20,0(R4))                                
         NI    RTIME,X'3F'         TURN OFF EXTRA BITS                          
         ZAP   DUB,RTIME                                                        
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  TIME,DUB                                                         
         MVC   09(2,R4),TIME+0                                                  
         MVI   11(R4),C':'                                                      
         MVC   12(2,R4),TIME+2                                                  
         MVI   14(R4),C':'                                                      
         MVC   15(2,R4),TIME+4                                                  
         MVI   17(R4),DELIM                                                     
         AHI   R4,18                                                            
*                                                                               
         MVC   HALF,RUSER                                                       
         BRAS  RE,GETAGY           OUTPUT AGENCY                                
         MVC   0(2,R4),WORK+2                                                   
         AHI   R4,2                                                             
         BRAS  RE,STRIPPER                                                      
*                                                                               
         MVC   HALF,RUSER                                                       
         BRAS  RE,GETUSRID         OUTPUT CONNECTED USER ID                     
         MVC   0(10,R4),WORK+2                                                  
         AHI   R4,10                                                            
         BRAS  RE,STRIPPER                                                      
*                                                                               
         BRAS  RE,GETSYPR          OUTPUT SYSTEM AND PROGRAM                    
         MVC   0(8,R4),WORK                                                     
         AHI   R4,8                                                             
         BRAS  RE,STRIPPER                                                      
         MVC   0(8,R4),WORK+8                                                   
         AHI   R4,8                                                             
         BRAS  RE,STRIPPER                                                      
*                                                                               
         MVC   0(L'DMFLNAME,R4),DMFLNAME                                        
         AHI   R4,L'DMFLNAME       OUTPUT FILENAME                              
         BRAS  RE,STRIPPER                                                      
*                                                                               
         XR    R1,R1               OUTPUT ACTION PERFORMED                      
         IC    R1,MODE                                                          
         BCTR  R1,0                                                             
         MHI   R1,L'MODTAB2                                                     
         LA    R1,MODTAB2(R1)                                                   
         MVC   0(L'MODTAB2,R4),0(R1)                                            
         MVI   L'MODTAB2(R4),DELIM                                              
         AHI   R4,L'MODTAB2+1                                                   
*                                                                               
         OC    RPERSON,RPERSON     ONLINE HAS EITHER 0000 OR PERSON ID          
         BNZ   *+14                                                             
         MVC   0(10,R4),=CL10'UNKNOWN'                                          
         B     XTR02                                                            
*                                                                               
         CLC   =X'0001',RPERSON    OFFLINE HAS 0001 IN RPERSON                  
         BNE   *+14                                                             
         MVC   0(10,R4),=CL10'OFFLINE'                                          
         B     XTR02                                                            
*                                                                               
         MVC   FULL+0(2),LSTAGY                                                 
         MVC   FULL+2(2),RPERSON                                                
         BRAS  RE,GETPID                                                        
         MVC   0(10,R4),WORK+2                                                  
*                                                                               
XTR02    AHI   R4,10                                                            
         BRAS  RE,STRIPPER                                                      
*                                                                               
TRL      USING RECVEXT,SVETRL                                                   
         TM    TRL.RCTSTAT,RCTSTTKT                                             
         BZ    XTR04                                                            
*                                                                               
         MVC   0(L'LSTTNAM,R4),LSTTNAM                                          
         AHI   R4,L'LSTTNAM                                                     
         BRAS  RE,STRIPPER                                                      
         AHI   R4,1                                                             
         MVI   0(R4),DELIM                                                      
         AHI   R4,1                                                             
         B     XTR06                                                            
*                                                                               
XTR04    AHI   R4,1                                                             
         MVI   0(R4),DELIM                                                      
         AHI   R4,1                                                             
         MVC   0(L'LSTTNAM,R4),LSTTNAM                                          
         AHI   R4,L'LSTTNAM                                                     
         BRAS  RE,STRIPPER                                                      
*                                                                               
XTR06    MVI   0(R4),C'N'          DDS TERMINAL OVERRIDE                        
         TM    TRL.RCTSTAT,RCTSTDTE                                             
         BZ    *+8                                                              
         MVI   0(R4),C'Y'                                                       
         MVI   1(R4),DELIM                                                      
         AHI   R4,2                                                             
*                                                                               
         MVI   0(R4),C'N'          DDS PERSON                                   
         TM    TRL.RCTSTAT,RCTSTDPE                                             
         BZ    *+8                                                              
         MVI   0(R4),C'Y'                                                       
         MVI   1(R4),DELIM                                                      
         AHI   R4,2                                                             
*                                                                               
         MVI   0(R4),C'N'          DDS PERSON OVERRIDE                          
         TM    TRL.RCTSTAT,RCTSTDPO                                             
         BZ    *+8                                                              
         MVI   0(R4),C'Y'                                                       
         MVI   1(R4),DELIM                                                      
         AHI   R4,2                                                             
         DROP  TRL                                                              
*                                                                               
         GOTO1 VDATCON,DMCB,(3,RDATE),(20,0(R4))                                
         NI    RTIME,X'3F'         TURN OFF EXTRA BITS                          
         ZAP   DUB,RTIME                                                        
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  TIME,DUB                                                         
         MVC   09(2,R4),TIME+0                                                  
         MVI   11(R4),C':'                                                      
         MVC   12(2,R4),TIME+2                                                  
         MVI   14(R4),C':'                                                      
         MVC   15(2,R4),TIME+4                                                  
         MVI   17(R4),DELIM                                                     
         AHI   R4,18                                                            
*                                                                               
         XR    R1,R1               OUTPUT RECOVERY FILE KEY                     
         IC    R1,DMFLKEYL         GET KEY LENGTH                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),RCVFRST                                                  
*                                                                               
         L     RF,=A(TROUTBND)     TRANSLATE NON-PRINTABLE INTO SPACES          
         EX    R1,*+8                                                           
         B     *+10                                                             
         TR    0(0,R4),0(RF)                                                    
*                                                                               
         AHI   R1,1                                                             
         AR    R4,R1                                                            
         MVI   0(R4),DELIM                                                      
         AHI   R4,1                                                             
*                                                                               
         GOTO1 VHEXOUT,PLIST,RECVHDR,0(R4),L'RECVHDR,0                          
         AHI   R4,L'RECVHDR*2                                                   
         MVI   0(R4),DELIM                                                      
         AHI   R4,1                                                             
*                                                                               
         AHI   R2,RCVFRST-RECDS                                                 
         XR    R0,R0               OUTPUT RECOVERY FILE KEY                     
         IC    R0,DMFLKEYL         GET KEY LENGTH                               
         GOTO1 VHEXOUT,PLIST,(R2),WORK,(R0),=C'SEP'                             
*                                  MOVE IN THE ZONE                             
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),WORK                                                     
*                                                                               
         AR    R4,R1                                                            
         AHI   R4,1                                                             
         MVI   0(R4),DELIM                                                      
         AHI   R4,1                                                             
*                                  MOVE IN THE NUMERICS                         
         LA    RE,WORK+1(R1)                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(RE)                                                    
*                                                                               
         AR    R4,R1                                                            
         AHI   R4,1                                                             
         MVI   0(R4),DELIM                                                      
         AHI   R4,1                                                             
*                                                                               
         LA    R0,XTRRECH          SET LENGTH                                   
         SR    R4,R0                                                            
         STH   R4,XTRRECH                                                       
         CHI   R4,L'XTRREC+L'XTRRECH                                            
         JH    *+2                 Increase record size                         
*                                                                               
         PUT   EXTRACT,XTRRECH                                                  
         B     EXITOK                                                           
*                                                                               
MODTAB2  DS    0CL3                                                             
         DC    CL3'ADD'                                                         
         DC    CL3'CHA'                                                         
         DC    CL3'DEL'                                                         
         DC    CL3'RES'                                                         
         DC    CL3'???'                                                         
         DROP  SVE                                                              
         EJECT                                                                  
***********************************************************************         
* RECORD OUTPUT ROUTINE                                               *         
***********************************************************************         
RECOUT   NTR1  BASE=*,LABEL=*                                                   
         ZAP   LINE,P99                                                         
*                                                                               
         L     R2,AIOSORT          GET A(ADD OR CHANGE RECORD)                  
         USING RECDS,R2                                                         
SVE      USING SRTBLKD,SRTSAVE                                                  
         XR    R3,R3                                                            
         IC    R3,RFILTY                                                        
         MHI   R3,DMFLLEN                                                       
         A     R3,AFILTAB                                                       
         USING FILTABD,R3                                                       
         CLC   RFILTY,DMFLNUM      MAKE SURE FILE DETAILS THERE                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R3,AFILE            FILENAME                                     
         USING FILTABD,R3                                                       
         MVC   PLTEXT,=CL15'Action'                                             
*                                                                               
         XR    R1,R1               GET ACTION PERFORMED                         
         IC    R1,MODE                                                          
         BCTR  R1,0                                                             
         MHI   R1,L'MODTAB                                                      
         LA    R1,MODTAB(R1)                                                    
         MVC   PLDATA(L'MODTAB),0(R1)                                           
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   PLTEXT,=CL15'Agency'                                             
         MVC   HALF,RUSER                                                       
         BRAS  RE,GETAGY           GET AGENCY                                   
         MVC   PLDATA(2),WORK+2                                                 
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   PLTEXT,=CL15'User ID'                                            
         MVC   HALF,RUSER                                                       
         BRAS  RE,GETUSRID         GET CONNECTED USER ID                        
         MVC   PLDATA(10),WORK+2                                                
         GOTO1 VPRINTER                                                         
*                                                                               
         BRAS  RE,DEFHDOUT         OUTPUT HEADER DETAIL                         
         GOTO1 VPRINTER                                                         
         BRAS  RE,DEFKYOUT         OUTPUT KEY DETAIL                            
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   PLTEXT,=CL15'System/Program'                                     
         BRAS  RE,GETSYPR                                                       
         MVC   PLDATA(8),WORK      SET SYSTEM PROGRAM                           
         LA    RF,PLDATA+8                                                      
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'/'                                                       
         MVC   2(8,RF),WORK+8                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   PLTEXT,=CL15'File'                                               
         MVC   PLDATA(L'DMFLNAME),DMFLNAME                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   PLTEXT,=CL15'Person'                                             
         OC    RPERSON,RPERSON     ONLINE HAS EITHER 0000 OR PERSON ID          
         BNZ   *+14                                                             
         MVC   PLDATA(10),=CL10'Unknown'                                        
         B     RECO02                                                           
*                                                                               
         CLC   =X'0001',RPERSON    OFFLINE HAS 0001 IN RPERSON                  
         BNE   *+14                                                             
         MVC   PLDATA(10),=CL10'Offline'                                        
         B     RECO02                                                           
*                                                                               
         MVC   FULL+0(2),LSTAGY                                                 
         MVC   FULL+2(2),RPERSON                                                
         BRAS  RE,GETPID                                                        
         MVC   PLDATA(10),WORK+2                                                
*                                                                               
RECO02   GOTO1 VPRINTER                                                         
*                                                                               
         MVC   PLTEXT,=CL15'At terminal'                                        
         MVC   PLDATA(8),LSTTNAM                                                
*                                                                               
         CLI   SVE.SRTTRL,C'Y'     TRAILER RECORD?                              
         BNE   RECO04              NO                                           
TRL      USING RECVEXT,SVETRL                                                   
         TM    TRL.RCTSTAT,RCTSTTKT                                             
         BZ    RECO04                                                           
         MVC   PLDATA(20),=CL20'Not Defined'                                    
*                                                                               
RECO04   GOTO1 VPRINTER                                                         
*                                                                               
         MVC   PLTEXT,=CL15'Ticket number'                                      
         MVC   PLDATA(20),=CL20'Not Defined'                                    
         CLI   SVE.SRTTRL,C'Y'     TRAILER RECORD?                              
         BNE   RECO06              NO                                           
TRL      USING RECVEXT,SVETRL                                                   
         TM    TRL.RCTSTAT,RCTSTTKT                                             
         BZ    RECO06                                                           
         MVC   PLDATA(20),SPACES                                                
         MVC   PLDATA(L'LSTTNAM),LSTTNAM                                        
*                                                                               
RECO06   GOTO1 VPRINTER                                                         
         MVC   PLTEXT,=CL15'DTE/DPE/DPO'                                        
         MVC   PLDATA(3),=CL3'NNN'                                              
         TM    TRL.RCTSTAT,RCTSTDTE                                             
         BZ    *+8                                                              
         MVI   PLDATA+0,C'Y'                                                    
         TM    TRL.RCTSTAT,RCTSTDPE                                             
         BZ    *+8                                                              
         MVI   PLDATA+1,C'Y'                                                    
         TM    TRL.RCTSTAT,RCTSTDPO                                             
         BZ    *+8                                                              
         MVI   PLDATA+2,C'Y'                                                    
         GOTO1 VPRINTER            PRINT OUT CONNECT DATA                       
         DROP  TRL                                                              
*                                                                               
         MVC   PLTEXT,=CL15'Date/Time'                                          
         GOTO1 VDATCON,DMCB,(3,RDATE),(20,PLDATA)                               
         NI    RTIME,X'3F'         TURN OFF EXTRA BITS                          
         ZAP   DUB,RTIME                                                        
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  TIME,DUB                                                         
         LA    R4,PLDATA                                                        
         MVC   09(2,R4),TIME+0                                                  
         MVI   11(R4),C':'                                                      
         MVC   12(2,R4),TIME+2                                                  
         MVI   14(R4),C':'                                                      
         MVC   15(2,R4),TIME+4                                                  
         GOTO1 VPRINTER                                                         
*                                                                               
         BRAS  RE,SYSINFO                                                       
*                                                                               
         CLI   FORMAT,C'H'         HEADER INFO ONLY?                            
         BE    EXITOK              YES                                          
*                                                                               
RECO08   XC    DUB,DUB                                                          
         LA    R3,RCVFRST          R2 = A(START OF RECORD)                      
*                                                                               
         CLI   MODE,MODEDEL        DELETE - OUTPUT ALL ELEMENTS REMOVED         
         BNE   RECO10                                                           
         ST    R3,DUB                                                           
         B     RECO18                                                           
*                                                                               
RECO10   CLI   MODE,MODERES        RESTORE - OUTPUT ALL ELEMENTS ADDED          
         BNE   RECO12                                                           
         ST    R3,DUB                                                           
         B     RECO18                                                           
*                                                                               
RECO12   CLI   MODE,MODEADD        ADD - OUTPUT ALL ELEMENTS ADDED              
         BNE   RECO14                                                           
         ST    R3,DUB                                                           
         B     RECO18                                                           
*                                                                               
RECO14   CLI   MODE,MODECPY        SPECIAL - OUTPUT COPIES ONLY                 
         BNE   RECO16                                                           
         L     R2,AIOCOPY                                                       
         LA    R3,RCVFRST          FIRST DATA OF COPY RECORD                    
         ST    R3,DUB              THEN OUTPUT AS ADDS                          
         B     RECO18                                                           
*                                                                               
RECO16   CLI   MODE,MODECHG        COPY/CHANGE PAIRS                            
         BNE   EXITOK                                                           
*                                                                               
         ST    R3,DUB                                                           
         L     R2,AIOCOPY                                                       
         LA    R3,RCVFRST                                                       
         ST    R3,DUB+4                                                         
*                                                                               
RECO18   LA    R1,DUB                                                           
         CLI   FORMAT,C'E'         ELEMENT DUMP (DEFAULT)                       
         BNE   *+8                                                              
         BRAS  RE,DEFPAIRE                                                      
         CLI   FORMAT,C'D'         HEX DUMP                                     
         BNE   *+8                                                              
         BRAS  RE,DEFPAIR                                                       
*                                                                               
         GOTO1 VPRINTER            PRINT SPACE LINE UNDERNEATH                  
         B     EXITOK                                                           
*                                                                               
MODTAB   DS    0CL8                                                             
         DC    CL8'Add'                                                         
         DC    CL8'Change'                                                      
         DC    CL8'Delete'                                                      
         DC    CL8'Restore'                                                     
         DC    CL8'????????'                                                    
         DROP  SVE                                                              
         EJECT                                                                  
***********************************************************************         
* DEFAULT KEY OUTPUT ROUTINE                                          *         
* NTRY:  R2    = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
         USING RECDS,R2                                                         
DEFKYOUT NTR1  BASE=*,LABEL=*                                                   
         MVC   PLTEXT,=CL15'Record Key'                                         
         L     R3,AFILE                                                         
         USING FILTABD,R3                                                       
         XR    R0,R0                                                            
         IC    R0,DMFLKEYL         GET KEY LENGTH                               
         GOTO1 VHEXOUT,PLIST,RCVFRST,PLDATA,(R0),0                              
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DEFAULT HEADER OUTPUT ROUTINE                                       *         
* NTRY:  R2    = A(RECOVERY FILE RECORD)                              *         
***********************************************************************         
         USING RECDS,R2                                                         
DEFHDOUT NTR1  BASE=*,LABEL=*                                                   
         MVC   PLTEXT,=CL15'Recovery Header'                                    
         GOTO1 VHEXOUT,PLIST,RECVHDR,PLDATA,L'RECVHDR,0                         
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DEFAULT RECORD PAIR HANDLER                                         *         
* NTRY: P1        = A(COPY RECORD OR 0)                               *         
*       P2        = A(CHANGE RECORD OR 0)                             *         
*       AELMNTRY  = A(ELEMENT TABLE ENTRY FOR THIS ELEMENT)           *         
***********************************************************************         
DEFPAIRE NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
         L     R4,AFILE                                                         
         USING FILTABD,R4                                                       
*                                                                               
         LTR   R2,R2               CHANGE RECORD?                               
         BZ    DFPR02              NO                                           
*                                                                               
         XC    DMCB(24),DMCB                                                    
         ST    R2,DMCB                                                          
         MVI   DMCB+00,C'E'                                                     
*                                                                               
         MVC   DMCB+04(1),DMFLELD                                               
         MVC   DMCB+07(1),DMFLLEND                                              
         MVC   DMCB+08(4),VPRINT                                                
         MVC   DMCB+08(4),=A(RECPRT)                                            
         MVC   DMCB+12(4),VHEXOUT                                               
         MVC   DMCB+16(4),=C'DOME'                                              
*                                                                               
         CLI   MODE,MODECHG        COPY/CHANGE PAIRS                            
         BNE   *+8                                                              
         LA    R0,TCHANGE                                                       
         CLI   MODE,MODERES        RESTORED                                     
         BNE   *+8                                                              
         LA    R0,TRESTORE                                                      
         CLI   MODE,MODECPY        SPECIAL - OUTPUT COPIES ONLY                 
         BNE   *+8                                                              
         LA    R0,TCOPYO                                                        
         CLI   MODE,MODEADD        SPECIAL - OUTPUT COPIES ONLY                 
         BNE   *+8                                                              
         LA    R0,TADD                                                          
         CLI   MODE,MODEDEL        DELETED RECORDS                              
         BNE   *+8                                                              
         LA    R0,TDELETE                                                       
*                                                                               
         ST    R0,DMCB+20          SET HEADER TEXT                              
         MVI   DMCB+20,16                                                       
         GOTO1 VPRTREC,DMCB                                                     
*                                                                               
DFPR02   LTR   R3,R3               COPY RECORD?                                 
         BZ    EXITOK              NO                                           
         OC    0(64,R3),0(R3)                                                   
         BZ    EXITOK                                                           
*                                                                               
         XC    DMCB(24),DMCB                                                    
         ST    R3,DMCB                                                          
         MVI   DMCB+00,C'E'                                                     
*                                                                               
         MVC   DMCB+04(1),DMFLELD                                               
         MVC   DMCB+07(1),DMFLLEND                                              
         MVC   DMCB+08(4),VPRINT                                                
         MVC   DMCB+08(4),=A(RECPRT)                                            
         MVC   DMCB+12(4),VHEXOUT                                               
         MVC   DMCB+16(4),=C'DOME'                                              
         LA    R0,TCOPY                                                         
         ST    R0,DMCB+20          SET HEADER TEXT                              
         MVI   DMCB+20,16                                                       
         GOTO1 VPRTREC,DMCB                                                     
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DEFAULT RECORD PAIR HANDLER                                         *         
* NTRY: P1        = A(COPY RECORD OR 0)                               *         
*       P2        = A(CHANGE RECORD OR 0)                             *         
*       AELMNTRY  = A(ELEMENT TABLE ENTRY FOR THIS ELEMENT)           *         
***********************************************************************         
DEFPAIR  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
         L     R4,AFILE                                                         
         USING FILTABD,R4                                                       
*                                                                               
         LTR   R2,R2               CHANGE RECORD?                               
         BZ    DEFP02              NO                                           
*                                                                               
         XC    DMCB(24),DMCB                                                    
         CLI   MODE,MODECHG        COPY/CHANGE PAIRS                            
         BNE   *+8                                                              
         LA    R0,TCHANGE                                                       
         CLI   MODE,MODERES        RESTORED                                     
         BNE   *+8                                                              
         LA    R0,TRESTORE                                                      
         CLI   MODE,MODECPY        SPECIAL - OUTPUT COPIES ONLY                 
         BNE   *+8                                                              
         LA    R0,TCOPYO                                                        
         CLI   MODE,MODEADD        SPECIAL - OUTPUT COPIES ONLY                 
         BNE   *+8                                                              
         LA    R0,TADD                                                          
         CLI   MODE,MODEDEL        DELETED RECORDS                              
         BNE   *+8                                                              
         LA    R0,TDELETE                                                       
*                                                                               
         ST    R0,DMCB             SET HEADER TEXT                              
         MVI   DMCB,16                                                          
*                                                                               
         XR    RF,RF                                                            
         IC    RF,DMFLLEND                                                      
         AR    RF,R2                                                            
         XR    R0,R0                                                            
         ICM   R0,3,0(RF)          PICK UP RECORD LENGTH                        
*                                                                               
         GOTO1 VPRNTBL,DMCB,,(R2),C'DUMP',(R0),=C'1D'                           
*                                                                               
DEFP02   LTR   R3,R3               COPY RECORD?                                 
         BZ    EXITOK              NO                                           
         OC    0(64,R3),0(R3)                                                   
         BZ    EXITOK                                                           
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LA    R0,TCOPY                                                         
         ST    R0,DMCB             SET HEADER TEXT                              
         MVI   DMCB,16                                                          
*                                                                               
         XR    RF,RF                                                            
         IC    RF,DMFLLEND                                                      
         AR    RF,R3                                                            
         XR    R0,R0                                                            
         ICM   R0,3,0(RF)          PICK UP RECORD LENGTH                        
*                                                                               
         GOTO1 VPRNTBL,DMCB,,(R3),C'DUMP',(R0),=C'1D'                           
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SYSINFO - OUTPUT SYSTEM SPECIFIC DETAILS FOR THE RECORD             *         
***********************************************************************         
         USING RECDS,R2                                                         
SYSINFO  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,ARSOFF                                                        
*                                                                               
         L     R1,VSELIST          FILTERING FOR A SYSTEM TYPE                  
         LH    RE,0(R1)            (ALL SPOT SYSTEMS SAY)                       
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SESYS,RSYS          GET THE SYSTEM IN THE RECOVERY FILE          
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,SEOVSYS                                                     
         BCTR  RF,0                                                             
         MHI   RF,L'SYSTAB                                                      
         LA    R3,SYSTAB(RF)                                                    
         CLC   EFFS(4),8(R3)       CHECK PRIOR (FAILED) LOAD                    
         BE    EXITOK                                                           
*                                                                               
         ICM   RF,15,8(R3)                                                      
         BNZ   SYSI02                                                           
*                                                                               
         LR    R0,R3                                                            
         LOAD  EPLOC=(0),ERRET=NOSYS                                            
*                                                                               
         STCM  R0,15,8(R3)                                                      
         LR    RF,R0                                                            
*                                                                               
SYSI02   L     R0,VPRINTER                                                      
         GOTO1 (RF),DMCB,RCVFRST,(R9),(R0)                                      
         B     EXITOK                                                           
*                                                                               
NOSYS    MVC   8(4,R3),EFFS                                                     
         B     EXITOK                                                           
*                                                                               
         DS    0D                                                               
SYSTAB   DS    0XL16                                                            
SYS01    DC    C'ACTVSRV ',A(0,0)                                               
SYS02    DC    C'ACTVSPT ',A(0,0)                                               
SYS03    DC    C'ACTVNET ',A(0,0)                                               
SYS04    DC    C'ACTVPRT ',A(0,0)                                               
SYS05    DC    C'ACTVPPV ',A(0,0)                                               
SYS06    DC    C'ACTVMPL ',A(0,0)                                               
SYS07    DC    C'ACTVACC ',A(0,0)                                               
SYS08    DC    C'ACTVTAL ',A(0,0)                                               
SYS09    DC    C'ACTVREP ',A(0,0)                                               
SYS0A    DC    C'ACTVMBA ',A(0,0)                                               
SYS0B    DC    C'ACTVCTL ',A(0,0)                                               
SYS0C    DC    C'ACTVGAM ',A(0,0)                                               
SYS0D    DC    C'ACTVCPP ',A(0,0)                                               
         EJECT                                                                  
***********************************************************************         
* READ CTFILE                                                         *         
* NTRY: AIOTEMP HOLDS KEY                                             *         
* EXIT: AIOTEMP HOLDS RECORD CC SET                                   *         
***********************************************************************         
CTREAD   NTR1  BASE=*,LABEL=*                                                   
         L     R0,AIOTEMP                                                       
         GOTO1 VDMGR,DMCB,(X'08',DMREAD),CTFILE,(R0),(R0),0                     
         CLI   8(R1),0                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ CTFILE FOR AGENCY                                              *         
* NTRY: HALF    = USERID TO READ                                      *         
* EXIT: WORK    = AGENCY(2)                                           *         
***********************************************************************         
GETAGY   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(4),WORK        CLEAR USERID AND READ NAME                   
         MVC   WORK(2),HALF                                                     
*                                                                               
         OC    HALF,HALF                                                        
         BNZ   *+14                                                             
         MVC   WORK+2(2),SPACES                                                 
         B     EXITOK                                                           
*                                                                               
         L     R3,AIOTEMP                                                       
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ    SET UP KEY FOR USERID                        
         MVC   CTIKNUM,HALF                                                     
         BRAS  RE,CTREAD                                                        
         BNE   GAGYBAD                                                          
*                                                                               
         GOTO1 VHELLO,PLIST,(C'G',CTFILE),(X'06',CTIREC),0,0,0,0                
         CLI   12(R1),0            ELEMENT FOUND?                               
         BNE   GAGYBAD                                                          
*                                                                               
         L     RF,12(R1)                                                        
         MVC   WORK+2(2),2(RF)                                                  
         B     EXITOK                                                           
*                                                                               
GAGYBAD  MVC   WORK+2(2),SPACES                                                 
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* READ CTFILE FOR A USERID                                            *         
* NTRY: HALF    = USERID TO READ                                      *         
* EXIT: WORK    = USERID(2)/USERNAME(10)                              *         
***********************************************************************         
GETUSRID NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(12),WORK       CLEAR USERID AND READ NAME                   
         MVC   WORK(2),HALF                                                     
*                                                                               
         OC    HALF,HALF                                                        
         BNZ   *+14                                                             
         MVC   WORK+2(10),=CL10'OFFLINE'                                        
         B     EXITOK                                                           
*                                                                               
         L     R3,AIOTEMP                                                       
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ    SET UP KEY FOR USERID                        
         MVC   CTIKNUM,HALF                                                     
         BRAS  RE,CTREAD                                                        
         BNE   GUSRBAD                                                          
*                                                                               
         GOTO1 VHELLO,PLIST,(C'G',CTFILE),(2,CTIREC),0,0,0,0                    
         CLI   12(R1),0            ELEMENT FOUND?                               
         BNE   GUSRBAD                                                          
*                                                                               
         ICM   RF,15,12(R1)                                                     
         CLI   1(RF),4             LENGTH MUST BE >4                            
         BNH   GUSRBAD                                                          
         MVC   WORK+2(10),2(RF)                                                 
         B     EXITOK                                                           
*                                                                               
GUSRBAD  MVC   WORK+2(10),SPACES                                                
         MVI   WORK+2,C'#'                                                      
         CURED HALF,(6,WORK+3),0,ALIGN=LEFT,ZERO=NOBLANK                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* READ CTFILE FOR AN AGENCY ID                                        *         
* NTRY: HALF    = AGYID TO READ                                       *         
* EXIT: WORK    = AGYID(2)/AGY NAME(10)                               *         
***********************************************************************         
GETAGYID NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(12),WORK       CLEAR USERID AND NAME READ                   
         MVC   WORK(2),HALF                                                     
         OC    HALF,HALF                                                        
         BNZ   *+14                                                             
         MVC   WORK+2(10),=CL10'Undefined'                                      
         B     EXITOK                                                           
*                                                                               
         L     R3,AIOTEMP                                                       
         USING CT5REC,R3                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    SET UP KEY FOR USERID                        
         MVC   CT5KALPH,HALF                                                    
         BRAS  RE,CTREAD                                                        
         BNE   GALFBAD                                                          
*                                                                               
         GOTO1 VHELLO,PLIST,(C'G',CTFILE),(X'02',(R3)),0,0,0,0                  
         CLI   12(R1),0            ELEMENT FOUND?                               
         BNE   GALFBAD                                                          
*                                                                               
         L     RF,12(R1)                                                        
         MVC   HALF,2(RF)                                                       
         BRAS  RE,GETUSRID                                                      
         B     EXITOK                                                           
*                                                                               
GALFBAD  MVC   WORK+2(08),=CL08'Unknown-'                                       
         MVC   WORK+10(2),HALF                                                  
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* READ CTFILE FOR A PERSON ID                                         *         
* NTRY: FULL    = AGY ALPHA (2) PERSON PASSWORD (2)                   *         
* EXIT: WORK    = PERSON ID(2)/PERSON NAME(8)                         *         
***********************************************************************         
GETPID   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK(12),WORK       CLEAR OUTPUT AREA                            
         MVC   WORK(2),FULL+2                                                   
*                                                                               
         CLC   FULL+2(2),=X'1000'  DDS PID?                                     
         BNL   *+10                                                             
*&&UK*&& MVC   FULL(2),=C'#E'      USE DDS SECURITY FILE - UK                   
*&&US*&& MVC   FULL(2),=C'#N'      USE DDS SECURITY FILE - US                   
*                                                                               
         L     R3,AIOTEMP                                                       
         USING SA0REC,R3                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ    SET UP KEY FOR USERID                        
         MVC   SA0KAGY,FULL                                                     
         MVC   SA0KNUM,FULL+2                                                   
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,(R3),(R3)                               
         CLI   8(R1),0                                                          
         BNE   GPIDBAD                                                          
*                                                                               
         GOTO1 VHELLO,PLIST,(C'G',CTFILE),(X'C3',SA0REC),0,0,0,0                
         CLI   12(R1),0            ELEMENT FOUND?                               
         BNE   GPIDBAD                                                          
*                                                                               
         L     RF,12(R1)                                                        
         MVC   WORK+2(10),SPACES                                                
         MVC   WORK+2(8),2(RF)                                                  
         B     EXITOK                                                           
*                                                                               
GPIDBAD  MVC   WORK+2(10),=CL10'Unknown'                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* READ CTFILE FOR A TERMINAL ID                                       *         
***********************************************************************         
GETTRM   NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIOSORT                                                       
         USING RECDS,R2                                                         
SVE      USING SRTBLKD,SRTSAVE                                                  
         XC    LSTCTFRT,LSTCTFRT   CLEAR LAST READ                              
         MVC   LSTTNUM,RTRM                                                     
         MVC   LSTTNAM,QUERY                                                    
*                                                                               
         CLI   SVE.SRTTRL,C'Y'     TRAILER RECORDS?                             
         BNE   GTRM02              NO                                           
TRL      USING RECVEXT,SVETRL                                                   
         MVC   LSTTNAM(8),TRL.RLUID                                             
         CLI   TRL.RXLEN,X'2D'     NEW LENGTH FOR 16-CHAR TICKET                
         BL    *+10                                                             
         MVC   LSTTNAM+8(8),TRL.RTICKETX                                        
         OC    LSTTNAM,SPACES                                                   
*                                                                               
         CLC   LSTTNAM,SPACES      CHECK DEFINED                                
         BH    EXITOK                                                           
         MVC   LSTTNAM,=CL16'Offline'                                           
         B     EXITOK                                                           
         DROP  TRL                                                              
*                                                                               
GTRM02   OC    RTRM,RTRM           ANOTHER CHECK FOR NO TRAILER                 
         BNZ   *+14                                                             
         MVC   LSTTNAM,=CL16'Offline'                                           
         B     EXITOK                                                           
*                                                                               
         L     R3,AIOTEMP                                                       
         USING CTTREC,R3                                                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   23(2,R3),RTRM                                                    
         GOTO1 VDMGR,DMCB,DMREAD,CTFILE,(R3),(R3)                               
         CLI   8(R1),0                                                          
         BE    GTRM04                                                           
         MVC   LSTTNAM,SPACES                                                   
         CURED RTRM,(6,LSTTNAM),0,ALIGN=LEFT,ZERO=NOBLANK                       
         B     EXITOK                                                           
*                                                                               
GTRM04   GOTO1 VHELLO,PLIST,(C'G',CTFILE),(3,(R3)),0,0,0,0                      
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,12(R1)                                                        
         MVC   LSTTNAM,2(RF)                                                    
         CLC   LSTNAM(2),=C'AA'    CHARACTERS?                                  
         BNL   EXITOK                                                           
         MVC   LSTNAM,=CL8'Automate'                                            
         B     EXITOK                                                           
         DROP  R2,R3,SVE                                                        
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT OUT SYSTEM/PROGRAM                                 *         
* NTRY:  R2         = A(RECOVERY FILE RECORD                          *         
* EXIT:  WORK+00(8) = SYSTEM LEFT ALIGNED SPACE FILLED                *         
* EXIT:  WORK+08(8) = PROGAM LEFT ALIGNED SPACE FILLED                *         
***********************************************************************         
         USING RECDS,R2                                                         
GETSYPR  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(16),SPACES                                                  
*                                                                               
         L     R1,VSELIST          CHECK REAL SYSTEM NUMBER                     
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         AHI   R1,6                                                             
         USING SELISTD,R1                                                       
         CLC   SESYS,RSYS                                                       
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     GSYPR01                                                          
*                                                                               
         MVC   WORK(L'SENAME),SENAME                                            
         B     GSYPR04                                                          
*                                                                               
GSYPR01  L     R1,VSYSLST          CHECK OV SYSTEM NUMBER                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         AHI   R1,6                                                             
         USING SYSLSTD,R1                                                       
         CLC   SYSLNUM,RSYS                                                     
         BE    GSYPR02                                                          
         BXLE  R1,RE,*-10                                                       
         GOTO1 VHEXOUT,PLIST,RSYS,WORK,L'RSYS                                   
         GOTO1 (RF),(R1),RPRG,WORK+8,L'RPRG                                     
         B     EXITOK                                                           
*                                                                               
GSYPR02  MVC   WORK(L'SYSLNAME),SYSLNAME                                        
*                                                                               
         L     R1,VSELIST                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         AHI   R1,6                                                             
         USING SELISTD,R1                                                       
         CLC   SEOVSYS,RSYS                                                     
         BE    GSYPR04                                                          
         BXLE  R1,RE,*-10                                                       
*                                                                               
GSYPR04  L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
         CLC   PGMNUM,RPRG                                                      
         BE    GSYPR06                                                          
         BXLE  R1,RE,*-10                                                       
*                                                                               
         CLI   RPRG,0                                                           
         BNE   *+14                                                             
         MVC   WORK+8(8),=CL8'Offline'                                          
         B     EXITOK                                                           
*                                                                               
         GOTO1 VHEXOUT,PLIST,RPRG,WORK+8,L'RPRG                                 
         B     EXITOK                                                           
*                                                                               
GSYPR06  MVC   WORK+8(L'PGMNAME),PGMNAME                                        
         B     EXITOK                                                           
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT ERROR MESSAGE                                      *         
***********************************************************************         
ERRMSG   NTR1  BASE=*,LABEL=*                                                   
         CLI   FERN,0                                                           
         BE    EXITOK                                                           
         MVI   GOTERR,C'Y'         SET GOT AN ERROR                             
*                                                                               
         XR    RF,RF               INDEX INTO ERROR TABLE                       
         IC    RF,FERN                                                          
         BCTR  RF,0                                                             
         MHI   RF,EMSGL                                                         
         LA    RF,ERRTAB(RF)       RF = A(ERROR MESSAGE)                        
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(L'ERRHDR),ERRHDR                                               
         MVC   P+L'ERRHDR(EMSGL),0(RF)                                          
         GOTO1 VPRINTER                                                         
         MVI   FERN,0              RESET ERROR NUMBER AND EXIT                  
         B     EXITOK                                                           
*                                                                               
ERRHDR   DC    C'*** ERROR - '                                                  
*                                                                               
EMSGL    EQU   50                  ERROR MESSAGE LENGTH                         
ERRTAB   DS    0X                                                               
ERR01    DC    CL(EMSGL)'Terminal Number Does Not Exist'                        
ERR02    DC    CL(EMSGL)'LUID Does Not Exist'                                   
ERR03    DC    CL(EMSGL)'User ID Does Not Exist'                                
ERR04    DC    CL(EMSGL)'Program Does Not Exist'                                
ERR05    DC    CL(EMSGL)'Invalid Record Type'                                   
ERR06    DC    CL(EMSGL)'Invalid Agency ID'                                     
ERR07    DC    CL(EMSGL)'Invalid Record Parameter'                              
ERR08    DC    CL(EMSGL)'Invalid Parameter Card'                                
ERR09    DC    CL(EMSGL)'XXXXXXXXXXXXXXXXX'                                     
ERR10    DC    CL(EMSGL)'Parameter Card Not Found'                              
ERR11    DC    CL(EMSGL)'User Name Not Found'                                   
ERR12    DC    CL(EMSGL)'Program Number is Not Valid'                           
ERR13    DC    CL(EMSGL)'Unknown Parameter Card'                                
ERR14    DC    CL(EMSGL)'Parameter has already been set'                        
ERR15    DC    CL(EMSGL)'User-Id must be a number'                              
ERR16    DC    CL(EMSGL)'Number entered is too big'                             
ERR17    DC    CL(EMSGL)'Terminal Number must be numeric'                       
ERR18    DC    CL(EMSGL)'Agency binary value must be numeric'                   
ERR19    DC    CL(EMSGL)'Format is E (Element) or D (Hex dump)'                 
ERR20    DC    CL(EMSGL)'Invalid system'                                        
ERR21    DC    CL(EMSGL)'You have to specify a system first'                    
         EJECT                                                                  
**********************************************************************          
* VALIDATE INPUT CARD IN SC2NDFLD FOR WORD 'TRUE'                    *          
* NTRY:  R3    = A(SCANBLK ENTRY)                                    *          
* EXIT:  CC    SET                                                   *          
**********************************************************************          
         USING SCANBLKD,R3         CHECK XXXXX=TRUE INPUT CARD                  
VALTRUE  NTR1  BASE=*,LABEL=*                                                   
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,8                                                           
         B     EXITL                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,VTRUECLC                                                      
         B     EXIT                                                             
*                                                                               
VTRUECLC CLC   TRUE(0),SC2NDFLD                                                 
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* OPEN FILES                                                          *         
***********************************************************************         
OPENSYS  NTR1  BASE=*,LABEL=*                                                   
         CLI   SYSOP,C'Y'          SYSOP FLAGS PRIOR OPEN                       
         BE    EXITOK                                                           
*                                                                               
         MVI   SYSOP,C'Y'                                                       
         GOTO1 VDMGR,DMCB,DMOPEN,DMSYS,DMFLIST                                  
         B     EXITOK                                                           
*                                                                               
DMFLIST  DC    C'NCTFILE NGENDIR NGENFIL X'                                     
         EJECT                                                                  
**********************************************************************          
* COMMONLY ADDRESSIBLE ROUTINES - ADDRESSED OFF R?                   *          
**********************************************************************          
         ORG   ACTIVE+(((*-ACTIVE)/4096)+1)*4096                                
COMMON   DC    CL8'*COMMON*'                                                    
*                                                                               
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,=16F'0'                                                  
         BR    RE                                                               
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    L     RD,SAVERD                                                        
         XBASE                                                                  
*                                                                               
STRIPPER CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),DELIM                                                      
         AHI   R4,2                                                             
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* CONSTANTS AND EQUATES                                              *          
**********************************************************************          
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
DELIM    EQU   C';'                                                             
QUERY    DC    16C'?'                                                           
EFFS     DC    16X'FF'                                                          
*                                                                               
KB       EQU   1024                                                             
FF       EQU   255                                                              
IOL      EQU   8*KB                LENGTH OF AN IO AREA                         
*                                                                               
ACARDTAB DC    A(CARDTAB)          A(CARDTAB)                                   
* AFILTAB  DC    A(FILTAB)           A(FILTAB)                                  
AFILTAB  DC    A(0)                A(FILTAB)                                    
AIOSORT  DC    A(IO1)                                                           
AIOCOPY  DC    A(IO2)                                                           
AIOTEMP  DC    A(IO3)                                                           
ASRTBLK  DC    A(SRTBLK)                                                        
*                                                                               
VCARDS   DC    V(CARDS)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VDMGR    DC    V(DATAMGR)                                                       
VPRNTBL  DC    V(PRNTBL)                                                        
VPRTREC  DC    V(PRTREC)                                                        
VDATCON  DC    V(DATCON)                                                        
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VPRINTER DC    V(PRINTER)                                                       
VPRINT   DC    V(PRINT)                                                         
VSCANNER DC    V(SCANNER)                                                       
VSELIST  DC    V(SELIST)                                                        
VSORTER  DC    V(SORTER)                                                        
CUREDIT  DC    V(CUREDIT)                                                       
VSYSLST  DC    A(SYSLST)                                                        
ASSB     DC    V(SSB)                                                           
VDDSIO   DC    V(DDSIO)                                                         
*                                                                               
DMSYS    DC    CL8'CONTROL '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
GENDIR   DC    CL8'GENDIR  '                                                    
GENFIL   DC    CL8'GENFIL  '                                                    
TRUE     DC    CL8'TRUE    '                                                    
TAPE     DC    CL4'TAPE'                                                        
DISK     DC    CL4'DISK'                                                        
DMOPEN   DC    CL4'OPEN'                                                        
PUT      DC    CL4'PUT '                                                        
GET      DC    CL4'GET '                                                        
END      DC    CL4'END '                                                        
*                                                                               
TCHANGE  DC    CL16'Changed to'                                                 
TCOPY    DC    CL16'Changed from'                                               
TCOPYO   DC    CL16'Copy record'                                                
TADD     DC    CL16'Added record'                                               
TDELETE  DC    CL16'Deleted record'                                             
TRESTORE DC    CL16'Restored record'                                            
*                                                                               
SYSOP    DC    C'N'                                                             
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
P99      DC    P'99'                                                            
*                                                                               
VCTITLE  DC    CL(L'TITLE)'INPUT CARDS TO DDS ACTIVITY REPORT'                  
PRTOT    DC    CL(L'TITLE)'DDS ACTIVITY REPORT'                                 
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,76,A),FORMAT=BI,WORK=1'                      
*                                                                               
* Note: if we go to 8K records this will need to change (12288)      *          
*                                                                               
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(12288,,,,) '                             
*                                                                               
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),RECFM=VB,             *        
               EODAD=TAPEEND                                                    
EXTRACT  DCB   DDNAME=EXTRACT,DSORG=PS,MACRF=(PM),RECFM=VB,BLKSIZE=8120*        
               ,LRECL=2048                                                      
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* SYSLST                                                             *          
**********************************************************************          
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
         EJECT                                                                  
**********************************************************************          
* INPUT CARD TABLE                                                   *          
**********************************************************************          
CARDTAB  DC    CL8'ADDS    ',AL1(2,3,0,0),AL4(VALADD)                           
         DC    CL8'CHANGES ',AL1(2,6,0,0),AL4(VALCHNG)                          
         DC    CL8'DELETES ',AL1(2,6,0,0),AL4(VALDEL)                           
         DC    CL8'RESTORES',AL1(2,7,0,0),AL4(VALREST)                          
*                                                                               
         DC    CL8'AGYBIN  ',AL1(2,6,0,0),AL4(VALAGY)                           
         DC    CL8'DDSIO   ',AL1(2,5,0,0),AL4(VALDDSIO)                         
         DC    CL8'DSPACE  ',AL1(2,5,0,0),AL4(VALDSPAC)                         
         DC    CL8'FORMAT  ',AL1(2,5,0,0),AL4(VALFORM)                          
         DC    CL8'PROGNAME',AL1(5,7,0,0),AL4(VALPRG)                           
         DC    CL8'RECNUM  ',AL1(3,5,0,0),AL4(VALRCN)                           
         DC    CL8'SYSTEM  ',AL1(2,5,0,0),AL4(VALSYS)                           
         DC    CL8'TICKET  ',AL1(3,5,0,0),AL4(VALTIK)                           
         DC    CL8'USERID  ',AL1(4,5,0,0),AL4(VALID)                            
         DC    CL8'USERNAME',AL1(4,7,0,0),AL4(VALNID)                           
         DC    X'FF'                                                            
*                                                                               
CARDTABD DSECT                     INPUT CARD PARAMETERS                        
CARDTXT  DS    CL8                 TEXT                                         
CMIN     DS    X                   MIN LEN FOR EXECUTE (-1)                     
CMAX     DS    X                   MAX LEN FOR EXECUTE (-1)                     
         DS    XL2                                                              
CARDVAL  DS    AL4                 A(VALIDATION ROUTINE)                        
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
ACTIVE   CSECT                                                                  
RECBUFF  DS    64000X                                                           
RECBUFFX EQU   *                                                                
*                                                                               
         EJECT                                                                  
**********************************************************************          
* FILE DEFINITIONS                                                   *          
**********************************************************************          
*      ++INCLUDE DMFILTAB                                                       
         EJECT                                                                  
**********************************************************************          
* NON-ADDRESSIBLE STORAGE AREAS                                      *          
**********************************************************************          
         DS    0D                                                               
         DC    CL8'**IO1**'                                                     
IO1      DS    (IOL)X                                                           
*                                                                               
         DC    CL8'**IO2**'                                                     
IO2      DS    (IOL)X                                                           
*                                                                               
         DC    CL8'**IO3**'                                                     
SRTBLK   DS    CL(SRTBLKLQ)                                                     
IO3      DS    (IOL*2)X                                                         
*                                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT ROUTINE FOR PRTREC TO ENSURE LINE COUNT HONOURED             *          
**********************************************************************          
RECPRT   NTR1  BASE=*,LABEL=*                                                   
         L     RA,=A(COMMON)                                                    
         USING COMMON,RA                                                        
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         L     RF,0(R1)                                                         
         MVC   P,1(RF)                                                          
         GOTO1 VPRINTER                                                         
         XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* W/S                                                                *          
**********************************************************************          
REGSAVE  CSECT                                                                  
         DS    200000C                                                          
         EJECT                                                                  
**********************************************************************          
* DSECTS                                                             *          
**********************************************************************          
WORKD    DSECT                                                                  
DUB      DS    D                                                                
SAVERD   DS    A                                                                
APGMLST  DS    A                                                                
ASCANTAB DS    A                                                                
AFILE    DS    A                                                                
*                                                                               
FULL     DS    F                                                                
DMCB     DS    XL24                                                             
DMWORK   DS    XL72                                                             
PLIST    DS    XL24                GENERAL PARAMETER LIST                       
KEY      DS    CL64                                                             
KEYSAVE  DS    CL64                                                             
*                                                                               
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
GOTERR   DS    C                                                                
FORMAT   DS    C                                                                
TIME     DS    XL6                                                              
*                                                                               
PARMCNT  DS    X                   NO OF I/P PARAMETERS                         
PARMCNTQ EQU   20                  MAX NUMBER OF I/P PARAMETERS                 
*                                                                               
LSTCTFRD DS    0XL12               COVERS LAST USERID READ (CTFILE)             
LSTNUM   DS    XL2                 USERID NO.                                   
LSTNAM   DS    XL10                ASSOCIATED NAME                              
*                                                                               
LSTCTFRT DS    0XL18               COVERS LAST TERMINAL NO. READ (CTF)          
LSTTNUM  DS    XL2                 TERMINAL NO.                                 
LSTTNAM  DS    XL16                LUID                                         
*                                                                               
LSTAGY   DS    CL2                 AGY 2 CHARACTER                              
LSTAGYN  DS    CL10                PRINCIPAL ID NAME                            
*                                                                               
LSTPROG  DS    0XL8                COVERS LAST PROGRAM READ (FATABOFF)          
LSTPNUM  DS    X                   PROGRAM NUMBER                               
LSTPNAM  DS    XL7                 PROGRAM NAME                                 
*                                                                               
LSTSRT   DS    X                                                                
LSTGRP   DS    CL2                                                              
LSTUSR   DS    CL2                                                              
*                                                                               
OUTTYPE  DS    X                   OUTPUT TYPES TO BE DISPLAYED                 
ADDOUT   EQU   X'80'               SHOW ADDS                                    
CHNGOUT  EQU   X'20'               CHANGE RECORDS SHOWN                         
DELOUT   EQU   X'10'               DELETE RECORDS SHOWN                         
RESTOUT  EQU   X'08'               RESTORE RECORDS SHOWN                        
HARDOUT  EQU   X'02'               HARDCOPY ONLY                                
DETLOUT  EQU   X'01'               DETAIL INFORMATION ON RECORDS                
*                                                                               
DEFOUT   EQU   ADDOUT+CHNGOUT+DELOUT+RESTOUT                                    
*                                                                               
FERN     DS    X                   CONTAINS CURRENT ERROR NO. OR 0              
*                                                                               
OFFLINE  DS    X                   Y/N                                          
*                                                                               
THISELM  DS    X                                                                
THISELMH DS    X                                                                
*                                                                               
V_TCODE  DS    H                   TERMINAL CODE VALUE IF SET                   
V_USRID  DS    H                   USER ID VALUE IF SET                         
V_RTYPE  DS    X                   RECORD TYPE IF SET                           
V_AGYID  DS    X                   AGENCY ID IF SET                             
V_PRNUM  DS    X                   PROGRAM NO. IF SET                           
V_LUID   DS    CL16                LUID - FOR TRAILING LUIDS                    
*                                                                               
FLTSYS   DS    X                                                                
FLTSYSF  DS    X                                                                
FLTSYSFO EQU   C'O'                                                             
FLTSYSFN EQU   C'N'                                                             
*                                                                               
PROGRAM  DS    CL7                 PROGRAM NAME IF VALID                        
*                                                                               
MODE     DS    X                   ADD/DELETE/RESTORE/CHANGE TYPE               
MODEADD  EQU   1                   RECORD ADDED                                 
MODECHG  EQU   2                   RECORD CHANGED                               
MODEDEL  EQU   3                   RECORD DELETED                               
MODERES  EQU   4                   RECORD RESTORED                              
MODECPY  EQU   5                   SPECIAL                                      
MODENOC  EQU   6                   OFFLINE CHANGE RECORD (NO COPY)              
MODEERR  EQU   255                 ERROR                                        
*                                                                               
WORK     DS    CL255                                                            
CARDIO   DS    CL80                                                             
ARCCARD  DS    CL80                                                             
*                                                                               
*                                                                               
FLAG     DS    X                                                                
XTRRECH  DS    F                                                                
XTRREC   DS    CL500                                                            
*                                                                               
SRTSAVE  DS    XL(SRTBLKLQ)                                                     
SVETRL   DS    XL64                                                             
SCANTAB  DS    (PARMCNTQ)CL(SCBLKLQ)                                            
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* DMRCVRHDR                                                           *         
***********************************************************************         
RECDS    DSECT                                                                  
RECLN    DS    XL4                                                              
       ++INCLUDE DMRCVRHDR                                                      
RCVFRST  DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* SORT BLOCK DSECT                                                    *         
***********************************************************************         
SRTBLKD  DSECT                                                                  
SRTLEN   DS    H                 LENGTH OF SORT RECORD WITH HEADER              
         DS    H                                                                
SRTSYS   DS    X                                                                
SRTPRG   DS    X                                                                
SRTGRP   DS    XL2               AGY ALPHA                                      
SRTUSER  DS    XL2               USER ID                                        
SRTLUID  DS    XL8               TERMINAL LUID                                  
SRTPERS  DS    XL2               PERSON ID (WAS RTRM)                           
SRTDATE  DS    XL3               DATE                                           
SRTTIME  DS    XL3               TIME                                           
SRTKEY   DS    XL64              RECORD KEY (ZERO FILLED, LEFT ALIGNED)         
SRTSIN   DS    XL4               SYSTEM INPUT NUMBER                            
SRTTRL   DS    X                 TRAILER                                        
SRTRECH  DS    0X                                                               
SRTBLKLQ EQU   *-SRTBLKD                                                        
         EJECT                                                                  
***********************************************************************         
* SSB                                                                 *         
***********************************************************************         
SSB      CSECT                                                                  
         DC    X'0000FF',1021X'00'                                              
         EJECT                                                                  
TROUTBND DS    0XL256                                                           
*                0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                               
         DC    X'40404040404040404040404040404040' 00-0F                        
         DC    X'40404040404040404040404040404040' 10-1F                        
         DC    X'40404040404040404040404040404040' 20-2F                        
         DC    X'40404040404040404040404040404040' 30-3F                        
         DC    X'404040404040404040404A4B4C4D4E4F' 40-4F                        
         DC    X'504040404040404040405A5B5C5D5E5F' 50-5F                        
         DC    X'606140404040404040406A6B6C6D6E6F' 60-6F                        
         DC    X'404040404040404040797A7B7C7D7E7F' 70-7F                        
         DC    X'40818283848586878889408B8C8D8E8F' 80-8F                        
         DC    X'40919293949596979899409B9C9D9E9F' 90-9F                        
         DC    X'A0A1A2A3A4A5A6A7A8A940ABACADAEAF' A0-AF                        
         DC    X'B0B1B2B3B4B5B6B7B8B940BBBCBDBEBF' B0-BF                        
         DC    X'C0C1C2C3C4C5C6C7C8C9CA4040404040' C0-CF                        
         DC    X'D0D1D2D3D4D5D6D7D8D9404040404040' D0-DF                        
         DC    X'E0E1E2E3E4E5E6E7E8E9404040404040' E0-EF                        
         DC    X'F0F1F2F3F4F5F6F7F8F9404040404040' F0-FF                        
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PRINT LINE DSECT                                                    *         
***********************************************************************         
PLINED   DSECT                                                                  
PLTEXT   DS    CL15                                                             
         DS    X                                                                
PLDATA   DS    CL116                                                            
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* FAPGMLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAPGMLST                                                       
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* DMRCVREXT                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMRCVREXT                                                      
         PRINT ON                                                               
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
* DMFILTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMFILTABD                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024DDACTIVE  09/09/19'                                      
         END                                                                    
