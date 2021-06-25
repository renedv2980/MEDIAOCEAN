*          DATA SET GEBDEMAKE  AT LEVEL 002 AS OF 09/30/02                      
*PHASE BDEMAKEA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE SORTER                                                                 
         TITLE 'USE INPUT FILE TO MAKE EMAIL RECORDS ON GENFILE'                
         PRINT NOGEN                                                            
         ENTRY WORKAREA                                                         
         ENTRY UTL                                                              
         ENTRY SSB                                                              
BDEMAKE  START                                                                  
         NBASE 0,BDEMAKE*,RA,WORK=VWRKAREA                                      
         L     R9,ACOMMON                                                       
         USING COMMON,R9           R9 = A(COMMONLY ADDRESSIBLE STORAGE)         
*                                                                               
         LR    RC,RD               GET SOME W/S FOR THIS MODULE                 
         USING WORKD,RC                                                         
         A     RD,=A((WORKL+7)/8*8)                                             
         XC    0(12,RD),0(RD)                                                   
         MVC   0(4,RD),=CL4'AATK'                                               
         MVC   4(4,RD),4(RC)                                                    
*                                                                               
         LR    R0,RC                                                            
         LR    R1,RD                                                            
         SR    R1,RC                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR W/S                                    
*                                                                               
         ST    R9,SAVER9           SAVE THESE FOR LATER                         
         ST    RD,SAVERD                                                        
*                                                                               
         BRAS  RE,INIT             INITIALISE JOB                               
         BL    MAINX                                                            
*                                                                               
         BRAS  RE,PUTSORT          READ RECORDS AND PUT TO SORTER               
         BRAS  RE,MERGEM           MERGE RECORDS                                
*                                                                               
         GOTO1 VSORTER,DMCB,END                                                 
*                                                                               
MAINX    CLOSE SYSPRINT            CLOSE PRINT OUTPUT                           
         GOTO1 VDMGR,DMCB,DMCLSE,DMSYS,DMFLIST                                  
         B     XBASE                                                            
*                                                                               
ACOMMON  DC    A(COMMON)                                                        
VWRKAREA DC    V(WORKAREA)                                                      
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         LHI   RF,IO1-WORKD                                                     
         AR    RF,RC                                                            
         ST    RF,AIO1                                                          
         LHI   RF,IO2-WORKD                                                     
         AR    RF,RC                                                            
         ST    RF,AIO2                                                          
*                                                                               
         OPEN  (SYSPRINT,OUTPUT)   INITIALISE PRINT OUTPUT                      
         BRAS  RE,CARDIN                                                        
         BL    EXITL                                                            
*                                                                               
         LHI   RF,SRTRECL          SET SOFT SORT LENGTH                         
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RECLEN4(4),DUB                                                   
*                                                                               
         GOTO1 VDMGR,DMCB,DMOPEN,DMSYS,DMFLIST                                  
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD,0                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ RECORDS AND ADD TO SORTER                                      *         
***********************************************************************         
         SPACE 1                                                                
PUTSORT  NTR1  ,                                                                
         OPEN  (TAPEIN,INPUT)                                                   
*                                                                               
PS02     GET   TAPEIN                                                           
         LR    R2,R1                                                            
         USING INFILED,R2                                                       
         CLI   INSHOW,C'1'                                                      
         BL    PS02                                                             
         CLI   INSHOW,C'9'                                                      
         BH    PS02                                                             
*                                                                               
X        USING SRTRECD,SRTBLD                                                   
         MVC   X.SRTORG,INORG                                                   
         CLI   INSHOW,C'1'         STATION ONLY                                 
         BNE   PS03                                                             
         MVC   X.SRTORG+5(2),=C'  ' CLEAR OUT THE V IF TV                       
         OC    X.SRTORG,SPACES      CAPS THE STATION CALL LETTERS               
*                                                                               
PS03     MVC   X.SRTNAME,INNAME                                                 
         MVC   X.SRTFNAME,INNAME                                                
         MVC   X.SRTEMAIL,INEMAIL                                               
         GOTO1 VSORTER,DMCB,PUT,SRTBLD                                          
         B     PS02                                                             
*                                                                               
PUTSORTX CLOSE TAPEIN                                                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SORT MERGE INPUT FILE WITH DISK FILE                                *         
***********************************************************************         
         SPACE 1                                                                
MERGEM   NTR1  ,                                                                
         XC    ASORT,ASORT                                                      
         XC    LASTSORT,LASTSORT                                                
*                                                                               
K        USING GBDED,KEY                                                        
         MVI   K.GBDEID,GBDEIDQ      SET KEY DETAIL                             
         BRAS  RE,HIDIR                                                         
*                                                                               
MM02     BRAS  RE,NXTSORT          GET NEXT FROM SORT                           
         BNE   EXITOK              FINISHED                                     
         BRAS  RE,BLDBDE           BUILD BDE RECORD INTO AIO1                   
*                                                                               
         L     R2,AIO1                                                          
         USING GBDED,R2                                                         
         L     R3,ASORT                                                         
         USING SRTRECD,R3                                                       
*                                                                               
MM04     CLI   EOFFLAG,255         ANY MORE BDE RECORDS TO PROCESS?             
         BE    ADDEM               NO - JUST DO ADDS                            
*                                                                               
         CLC   K.GBDEKEY,GBDEKEY   MATCH DIR KEY W/SORT RECORD                  
         BL    MM06                                                             
         BE    MM08                                                             
         BH    MM10                                                             
*                                                                               
MM06     BRAS  RE,DELDIR           DIR KEY IS LOW - NEED TO DELETE IT           
         BRAS  RE,SEQDIR           GET NEXT DIRECTORY RECORD                    
         B     MM04                TRY COMPARISON AGAIN                         
*                                                                               
MM08     BRAS  RE,UPDREC           MATCH - UPDATE RECORD ON FILE                
         BRAS  RE,SEQDIR           GET NEXT DIRECTORY RECORD                    
         B     MM02                NEXT SORT RECORD                             
*                                                                               
MM10     B     ADDEM               DIR KEY > SORT KEY - ADD SORT                
*                                                                               
ADDEM    GOTO1 VDMGR,DMCB,(0,ADDREC),GENFIL,K.GBDDA,AIO1,DMWORK                 
         BRAS  RE,HIDIR                                                         
         B     MM02                                                             
         DROP  K,R2,R3                                                          
         EJECT                                                                  
***********************************************************************         
* GET NEXT RECORD FROM SORT                                           *         
* ADDITIONALLY PRINT OUT THE INFORMATION FROM THAT RECORD             *         
* EXIT: CC EQ  A(SORT RECORD IN ASORT)                                *         
*       CC NE  END OF SORT RECORDS                                    *         
***********************************************************************         
         SPACE 1                                                                
NXTSORT  NTR1  ,                                                                
         ICM   RF,15,ASORT                                                      
         BZ    *+10                                                             
         MVC   LASTSORT,ASORT                                                   
*                                                                               
         GOTO1 VSORTER,DMCB,GET,0                                               
         ICM   R3,15,DMCB+4        END OF SORT FILE?                            
         BZ    EXITL               YES                                          
         USING SRTRECD,R3                                                       
         ST    R3,ASORT                                                         
*                                                                               
         MVC   PLINE(L'SRTFNAME),SRTFNAME                                       
         MVC   PLINE+L'SRTFNAME+2(L'SRTEMAIL),SRTEMAIL                          
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD BDE RECORD INTO AIO1 FROM SORT RECORD                         *         
* NTRY: ASORT  = A(SORT RECORD)                                       *         
***********************************************************************         
         SPACE 1                                                                
LST      USING SRTRECD,LASTSORT                                                 
BLDBDE   NTR1  ,                                                                
         L     R0,AIO1             CLEAR AIO1 BUILD AREA                        
         LHI   R1,IO1L                                                          
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AIO1                                                          
         USING GBDED,R2                                                         
         L     R3,ASORT                                                         
         USING SRTRECD,R3                                                       
*                                                                               
         MVI   GBDEID,GBDEIDQ      SET KEY DETAIL                               
         MVC   GBDEORG,SRTORG                                                   
         MVC   GBDENAME,SRTNAME                                                 
         LH    R0,SEQNUM           INCREMENT SEQUENCE NUMBER                    
         AHI   R0,1                                                             
         STH   R0,SEQNUM                                                        
*                                                                               
         CLC   SRTNAME,LST.SRTNAME CHECK IF KEY IS THE SAME                     
         BE    *+10                YES                                          
         XC    SEQNUM,SEQNUM       CLEAR SEQNUM IF DIFFERENT KEY                
         MVC   GBDESEQ,SEQNUM                                                   
*                                                                               
         LHI   RF,(GBDRFST+GBNLNQ+GBELNQ+1)                                     
         STCM  RF,3,GBDRFLEN                                                    
*                                                                               
         LA    R4,GBDRFST(R2)                                                   
         USING GBNELD,R4           FULL NAME ELEMENT                            
         MVI   GBNID,GBNIDQ                                                     
         MVI   GBNLN,GBNLNQ                                                     
         MVC   GBNORG,SRTORG                                                    
         MVC   GBNFNM,SRTFNAME                                                  
*                                                                               
         AHI   R4,GBNLNQ                                                        
         USING GBEELD,R4           EMAIL ELEMENT                                
         MVI   GBEID,GBEIDQ                                                     
         MVI   GBELN,GBELNQ                                                     
         MVC   GBEEMAIL,SRTEMAIL                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ HIGH ON GENDIR FOR UPDATE                                      *         
***********************************************************************         
         SPACE 1                                                                
HIDIR    NTR1  ,                                                                
         MVC   KEYSAVE(L'GBDEKEY),KEY                                           
         GOTO1 VDMGR,DMCB,(X'88',DMRDHI),GENDIR,KEY,KEY                         
         CLI   KEY+(GBDEID-GBDED),GBDEIDQ                                       
         BE    EXITOK                                                           
         MVI   EOFFLAG,255                                                      
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* READ SEQUENTIAL GENDIR FOR UPDATE                                   *         
***********************************************************************         
         SPACE 1                                                                
SEQDIR   NTR1  ,                                                                
         MVC   KEYSAVE(L'GBDEKEY),KEY                                           
         GOTO1 VDMGR,DMCB,(X'88',DMRSEQ),GENDIR,KEY,KEY                         
         CLI   KEY+(GBDEID-GBDED),GBDEIDQ                                       
         BE    EXITOK                                                           
         MVI   EOFFLAG,255                                                      
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE DIRECTORY RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
K        USING GBDED,KEY                                                        
DELDIR   NTR1  ,                                                                
         MVC   KEYSAVE(L'GBDEKEY),KEY                                           
         OI    K.GBDSTAT,GBDDEL    SET KEY DELETED AND WRITE BACK               
         GOTO1 VDMGR,DMCB,(X'88',DMWRT),GENDIR,KEY,KEY                          
         CLI   8(R1),0                                                          
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  K                                                                
         EJECT                                                                  
***********************************************************************         
* UPDATE RECORD ON FILE THAT ALREADY EXISTS                           *         
* NTRY: AIO1   = NEW COPY OF RECORD                                   *         
*       KEY    = DIRECTORY RECORD                                     *         
***********************************************************************         
         SPACE 1                                                                
K        USING GBDED,KEY                                                        
UPDREC   NTR1  ,                                                                
         GOTO1 VDMGR,DMCB,(X'88',GETREC),GENFIL,K.GBDDA,AIO2,DMWORK             
         CLI   8(R1),X'00'                                                      
         BE    UPR02                                                            
         CLI   8(R1),X'02'                                                      
         BE    UPR02                                                            
         DC    H'0'                                                             
*                                  WRITE BACK NEW COPY OF RECORD                
UPR02    GOTO1 (RF),(R1),(0,PUTREC),GENFIL,K.GBDDA,AIO1,DMWORK                  
*                                                                               
         TM    K.GBDSTAT,X'80'     DIRECTORY WAS DELETED?                       
         BZ    EXITOK              NO                                           
         NI    K.GBDSTAT,255-X'80' UNDELETE AND WRITE BACK                      
         GOTO1 (RF),(R1),(0,DMWRT),GENDIR,KEY,KEY                               
         B     EXITOK                                                           
         DROP  K                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT LINE                                                          *         
***********************************************************************         
         SPACE 1                                                                
PRINTL   NTR1  ,                                                                
         PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ AND VALIDATE INPUT CARDS                                       *         
***********************************************************************         
         SPACE 1                                                                
CARDIN   NTR1  ,                                                                
CRD02    GOTO1 VCARDS,DMCB,PLINE+1,=C'RE00'                                     
         MVC   CARD,PLINE+1                                                     
         CLC   =C'/*',CARD         END OF CARDS?                                
         BE    EXITOK              YES - RETURN NORMALLY                        
*                                                                               
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
         BRAS  RE,VALCARD          VALIDATE PARAMETER CARD                      
         B     CRD02                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT CARD                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALCARD  NTR1  ,                                                                
         CLI   CARD,C'*'           COMMENT?                                     
         BE    EXITOK              YES                                          
*                                                                               
         GOTO1 VSCANNER,DMCB,(C'C',CARD),(1,SCNBLK)                             
         CLI   4(R1),0                                                          
         BE    CEINVLIN            INVALID LINE                                 
*                                                                               
         LA    R2,SCNBLK                                                        
         USING SCANBLKD,R2                                                      
         L     R3,ACARDTAB                                                      
         USING CARDTABD,R3                                                      
         XR    RF,RF                                                            
*                                                                               
VCRD02   CLI   CNAME,CARDEOT       EOT?                                         
         BE    CEINVKEY            INVALID KEYWORD                              
         ICM   RF,1,CXLEN                                                       
         EX    RF,CARDCMP                                                       
         BE    VCRD04                                                           
         AHI   R3,CARDTABL                                                      
         B     VCRD02                                                           
*                                                                               
CARDCMP  CLC   SC1STFLD(0),CNAME                                                
*                                                                               
VCRD04   CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BNE   VCRD06              NO                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         BNO   CENOTNUM                                                         
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BL    CETOOLOW                                                         
         CLC   SC2NDNUM,CMAX                                                    
         BH    CETOOBIG                                                         
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         B     EXITOK                                                           
*                                                                               
VCRD06   CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         BNE   VCRD08              NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BZ    CENOINP                                                          
         C     RF,CMIN             SCOPE FOR LENGTH                             
         BL    CETOOSHT                                                         
         C     RF,CMAX                                                          
         BH    CETOOLNG                                                         
         ICM   RE,15,COUT          MOVE IN FIELD                                
         ICM   RF,1,CLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),SC2NDFLD                                                 
         B     EXITOK                                                           
*                                                                               
VCRD08   DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CEINVLIN LA    R1,CINVLIN          INVALID LINE                                 
         B     CERR                                                             
CEINVKEY LA    R1,CINVKEY          INVALID KEYWORD                              
         B     CERR                                                             
CENOTNUM LA    R1,CNOTNUM          NOT A NUMBER                                 
         B     CERR                                                             
CENOTCHR LA    R1,CNOTCHR          NOT CHARACTER                                
         B     CERR                                                             
CETOOSHT LA    R1,CTOOSHT          TOO SHORT                                    
         B     CERR                                                             
CETOOLNG LA    R1,CTOOLNG          TOO LONG                                     
         B     CERR                                                             
CETOOLOW LA    R1,CTOOLOW          TOO SMALL                                    
         B     CERR                                                             
CETOOBIG LA    R1,CTOOBIG          TOO BIG                                      
         B     CERR                                                             
CENOINP  LA    R1,CNOINP           NO INPUT                                     
         B     CERR                                                             
*                                                                               
CERR     MVC   PLINE,SPACES                                                     
         MVC   PLINE(CERRHDRL),CERRHDR                                          
         MVC   PLINE+CERRHDRL(CERRMSGL),0(R1)                                   
         BRAS  RE,PRINTL                                                        
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* COMMON ROUTINES AND LITERALS                                        *         
***********************************************************************         
         SPACE 1                                                                
         DROP  RA,RB                                                            
COMMON   DS    0D                                                               
*                                                                               
ON31     O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=X'7FFFFFFF'                                                  
         BSM   0,RE                                                             
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         ORG   *-2                                                              
         BSM   0,RE                                                             
*                                                                               
XBASE    L     RD,SAVERD                                                        
         XBASE ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
SPACES   DC    CL166' '                                                         
*                                                                               
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
MAXLINE  DC    P'60'                                                            
*                                                                               
DMREAD   DC    CL8'DMREAD  '                                                    
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
DMWRT    DC    CL8'DMWRT   '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
GENDIR   DC    CL8'GENDIR  '                                                    
GENFIL   DC    CL8'GENFIL  '                                                    
*                                                                               
DMOPEN   DC    CL8'DMOPEN  '                                                    
DMCLSE   DC    CL8'DMCLSE  '                                                    
DMSYS    DC    CL8'CONTROL '                                                    
DMFLIST  DC    C'UGENDIR UGENFIL X '                                            
PUT      DC    CL4'PUT '                                                        
GET      DC    CL4'GET '                                                        
END      DC    CL4'END '                                                        
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,7,A,8,22,A),FORMAT=BI,WORK=1'                
*                                                                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=('                                        
RECLEN4  DC    C'0000,,,,) '                                                    
         EJECT                                                                  
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GL),RECFM=VB,BLKSIZE=32760*        
               ,LRECL=255,EODAD=PUTSORTX                                        
*                                                                               
VCARDS   DC    V(CARDS)                                                         
ACARDTAB DC    A(CARDTAB)                                                       
VDMGR    DC    V(DATAMGR)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VSCANNER DC    V(SCANNER)                                                       
VSORTER  DC    V(SORTER)                                                        
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
CARDTAB  DS    0D                                                               
         DC    CL8'DSPACE  ',F'001',F'001'                                      
         DC    X'05',AL1(CTCHR),AL1(L'DSPACE),AL1(0),AL4(DSPACE)                
         DC    CL8'DDSIO   ',F'004',F'008'                                      
         DC    X'04',AL1(CTCHR),AL1(8),AL1(0),V(DDSIO)                          
         DC    X'FFFFFFFF'                                                      
         EJECT                                                                  
***********************************************************************         
* INPUT CARD STORAGE AREAS                                            *         
***********************************************************************         
         SPACE 1                                                                
DSPACE   DC    CL1'T'              NAME OF DATASPACE                            
         SPACE 2                                                                
***********************************************************************         
* ERROR MESSAGES                                                      *         
***********************************************************************         
         SPACE 1                                                                
CERRMSGL EQU   40                                                               
CERRHDRL EQU   20                                                               
*                                                                               
CERRHDR  DC    CL20' *** CARD ERROR *** '                                       
CINVLIN  DC    CL40'Invalid Line Format'                                        
CINVKEY  DC    CL40'Invalid Keyword'                                            
CNOTNUM  DC    CL40'Value not a valid number'                                   
CNOTCHR  DC    CL40'Value not a valid character string'                         
CTOOSHT  DC    CL40'Length of input string too short'                           
CTOOLNG  DC    CL40'Length of input string too long'                            
CTOOLOW  DC    CL40'Numeric value too small'                                    
CTOOBIG  DC    CL40'Numeric value too large'                                    
CNOINP   DC    CL40'Invalid/missing value'                                      
         EJECT                                                                  
***********************************************************************         
* UTL AND SSB ENTRIES                                                 *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL16'UTL*UTL*UTL*UTL*'                                           
UTL      DC    F'0'                                                             
         DC    X'0A'                                                            
         DC    XL251'00'                                                        
*                                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      DC    H'0'                                                             
         DC    X'FF'                                                            
         DC    X'02'               SUPPRESS RECOVERY                            
         DC    1020X'00'                                                        
         EJECT                                                                  
***********************************************************************         
* VARIABLE SCAN MODULE FOR CARD VALIDATION                            *         
***********************************************************************         
         SPACE 1                                                                
SCANNER  CSECT                                                                  
         NMOD1 SWORKL,**SCAN**                                                  
         USING SWORKD,RC                                                        
         LR    R9,R1               R9=A(PARAMETER LIST)                         
         LM    R2,R3,0(R9)         R2=A(DATA STRING) R3=A(BLOCK)                
         MVC   MAXLINES,4(R9)                                                   
         XC    SDISP,SDISP                                                      
         XR    R4,R4                                                            
         IC    R4,5(R2)            L'DATA IF SCREEN FIELD                       
         LA    R2,8(R2)                                                         
         MVC   LROW,=H'42'         PRESET DEFAULT LENGTHS                       
         MVC   LRIGHT,=H'20'                                                    
         MVC   LBOTH,=H'30'                                                     
         CLI   0(R9),C'C'                                                       
         BE    SCAN02                                                           
*                                                                               
SCAN02   SH    R2,=H'8'                                                         
         LA    R4,80                                                            
         CLC   0(80,R2),SSPACES                                                 
         BE    SCERR2                                                           
         LA    R5,79(R2)                                                        
*                                                                               
SCAN04   CLI   0(R5),C' '                                                       
         BNE   SCAN06                                                           
         BCTR  R5,0                                                             
         BCT   R4,SCAN04                                                        
*                                                                               
SCAN06   LA    R5,0(R2,R4)         L'DATA IN R4                                 
         MVC   BORROW,0(R5)        SAVE THE NEXT CHARACTER                      
         MVC   0(1,R5),COMMA       AND POP IN A COMMA TO SIMPLIFY               
         XR    R6,R6               R6=NUMBER OF LINES USED                      
         EJECT                                                                  
***********************************************************************         
* HANDLE LINES OF DATA                                                *         
***********************************************************************         
         SPACE 1                                                                
SCAN08   XC    0(12,R3),0(R3)      PRESET A LINE                                
         LH    RF,LBOTH                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   12(0,R3),SSPACES                                                 
         MVC   2(2,R3),=X'E0E0'                                                 
         BRAS  RE,GETL                                                          
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVI   2(R3),0                                                          
         CLI   1(R3),0                                                          
         BNE   *+8                                                              
         MVI   3(R3),0                                                          
         CLC   0(1,R3),LBOTH+1                                                  
         BH    SCERR                                                            
         CLC   1(1,R3),LRIGHT+1                                                 
         BH    SCERR                                                            
         CLI   1(R3),0                                                          
         BE    SCAN10                                                           
         CLI   0(R3),10                                                         
         BH    SCERR                                                            
*                                                                               
SCAN10   XR    R7,R7                                                            
         ICM   R7,1,0(R3)                                                       
         BZ    SCAN16                                                           
         BCTR  R7,0                                                             
         EX    R7,*+4                                                           
         MVC   12(0,R3),0(R2)                                                   
         TM    2(R3),X'80'                                                      
         BZ    SCAN12                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN12                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,5(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING DISPS.               
         BO    SCAN12                                                           
         ST    R8,4(R3)                                                         
*                                                                               
SCAN12   LA    R2,2(R2,R7)                                                      
         ICM   R7,1,1(R3)                                                       
         BZ    SCAN18                                                           
         BCTR  R7,0                                                             
         EX    R7,*+4                                                           
         MVC   22(0,R3),0(R2)                                                   
         TM    3(R3),X'80'                                                      
         BZ    SCAN14                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN14                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,9(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING SDISPS.              
         BO    SCAN14                                                           
         ST    R8,8(R3)                                                         
*                                                                               
SCAN14   LA    R2,2(R2,R7)                                                      
         B     SCAN18                                                           
*                                                                               
VARPAK   PACK  SDUB,0(0,R2)                                                     
*                                                                               
SCAN16   LA    R2,1(R2)                                                         
         CLI   1(R3),0                                                          
         BNE   SCERR                                                            
*                                                                               
SCAN18   LA    R6,1(R6)            BUMP N'LINES                                 
         AH    R3,LROW             BUMP TO NEXT LINE IN BLOCK                   
         CR    R2,R5               ARE WE NOW PAST LAST 'COMMA'                 
         BH    SCANOK                                                           
         ICM   R7,1,MAXLINES                                                    
         BZ    SCAN08                                                           
         CR    R6,R7               HAVE WE REACHED MAX N'LINES                  
         BNE   SCAN08                                                           
*                                                                               
SCANOK   MVC   0(1,R5),BORROW      RETURN THE BYTE                              
         STC   R6,4(R9)            SET NUMBER OF LINES USED                     
         B     SCANX                                                            
*                                                                               
SCERR    MVI   4(R9),0                                                          
         MVC   0(1,R5),BORROW                                                   
         MVC   2(2,R3),=X'FFFF'                                                 
         B     SCANX                                                            
*                                                                               
SCERR2   MVI   4(R9),0                                                          
         MVC   2(2,R3),=X'FFFF'                                                 
*                                                                               
SCANX    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND GET LENGTHS OF FIELDS                                  *         
***********************************************************************         
         SPACE 1                                                                
GETL     NTR1  ,                                                                
         LR    R4,R3                                                            
         SR    R5,R5                                                            
         TM    4(R9),X'80'                                                      
         BZ    GETL02                                                           
         MVC   4(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
*                                                                               
GETL02   CLC   0(1,R2),COMMA       TEST FIELD SEPERATOR                         
         BE    GETL14                                                           
         CLC   0(1,R2),EQUAL                                                    
         BE    GETL16                                                           
*                                                                               
GETL04   LA    R5,1(R5)                                                         
         CLI   0(R2),C'9'                                                       
         BNH   *+8                                                              
         MVI   2(R4),0             (ALL INVALID)                                
         CLI   0(R2),C'0'                                                       
         BL    GETL06                                                           
         NI    2(R4),X'BF'         (INVALID ALPHA)                              
         B     GETL12                                                           
*                                                                               
GETL06   NI    2(R4),X'7F'         (INVALID NUM)                                
         CLI   0(R2),C'Z'                                                       
         BNH   GETL08                                                           
         MVI   2(R4),0             Z-0 = ALL INVALID                            
         B     GETL12                                                           
*                                                                               
GETL08   CLI   0(R2),C'A'          LESS THAN A = ALL INVALID                    
         BNL   GETL10                                                           
         MVI   2(R4),0                                                          
         B     GETL12                                                           
*                                                                               
GETL10   CLI   0(R2),C'F'          OK FOR ALPHA                                 
         BNH   GETL12                                                           
         NI    2(R4),X'DF'         G-Z = INVALID HEX                            
*                                                                               
GETL12   LA    R2,1(R2)                                                         
         B     GETL02                                                           
*                                                                               
GETL14   STC   R5,0(R4)            COMMA FOUND                                  
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         B     GETLX                                                            
*                                                                               
GETL16   CR    R4,R3               EQUAL FOUND - IS THIS THE FIRST ONE?         
         BNE   GETL04              TREAT AS NORMAL CHARACTER IF NOT             
         STC   R5,0(R4)            NOW STORE L1                                 
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         TM    4(R9),X'80'                                                      
         BZ    GETL18                                                           
         MVC   8(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
*                                                                               
GETL18   LA    R4,1(R4)            POINT TO FIELD2 DATA                         
         SR    R5,R5               CLEAR L2                                     
         LA    R2,1(R2)            POINT PAST EQUAL SIGN                        
         B     GETL02                                                           
*                                                                               
GETLX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SCANNER LITERALS AND CONSTANTS                                      *         
***********************************************************************         
         SPACE 1                                                                
COMMA    DC    C','                                                             
EQUAL    DC    C'='                                                             
SSPACES  DC    CL80' '                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SCANNER WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
SWORKD   DSECT                                                                  
SDUB     DS    D                                                                
SWORK    DS    CL32                                                             
LASTSTOP DS    F                                                                
BORROW   DS    CL1                                                              
MAXLINES DS    CL1                                                              
LROW     DS    H                                                                
LRIGHT   DS    H                                                                
LBOTH    DS    H                                                                
SDISP    DS    H                                                                
*                                                                               
SWORKL   EQU   *-SWORKD                                                         
         EJECT                                                                  
***********************************************************************         
* WORK AREA                                                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
WORKAREA CSECT                                                                  
         DC    200000X'00'                                                      
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVER9   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
ASORT    DS    A                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
*                                                                               
PLIST    DS    6F                                                               
DMCB     DS    6F                                                               
DMWORK   DS    12D                                                              
AIO1     DS    A                                                                
AIO2     DS    A                                                                
*                                                                               
SEQNUM   DS    H                                                                
WORK     DS    CL64                                                             
CARD     DS    CL80                                                             
PLINE    DS    CL166                                                            
*                                                                               
SCNBLK   DS    3CL(SCBLKLQ)                                                     
KEY      DS    CL64                                                             
KEYSAVE  DS    CL64                                                             
SRTBLD   DS    CL(SRTRECL)                                                      
LASTSORT DS    CL(SRTRECL)                                                      
EOFFLAG  DS    X                                                                
*                                                                               
IO1      DS    XL1024                                                           
IO1L     EQU   *-IO1                                                            
IO2      DS    XL4000                                                           
IO2L     EQU   *-IO2                                                            
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* SORT RECORD DSECT                                                   *         
***********************************************************************         
         SPACE 1                                                                
SRTRECD  DSECT                                                                  
SRTORG   DS    CL7                                                              
SRTNAME  DS    CL22                                                             
SRTFNAME DS    CL40                                                             
SRTEMAIL DS    CL64                                                             
SRTRECL  EQU   *-SRTRECD                                                        
         EJECT                                                                  
***********************************************************************         
* INPUT FILE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
INFILED  DSECT                                                                  
INLEN    DS    H                                                                
         DS    H                                                                
         DS    X                   SPACER                                       
INNAME   DS    CL40                NAME - LAST(20) FIRST(20)                    
         DS    X                   SPACER                                       
INSHOW   DS    C                   IGNORE UNLESS C'1' - C'9'                    
INORG    DS    CL7                 ORGANISATION                                 
         DS    XL7                 CRAP                                         
         DS    X                   SPACER                                       
INEMAIL  DS    CL64                EMAIL ADDRESS                                
         EJECT                                                                  
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    XL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CLEN     DS    X                   OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    XL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 2                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
* GEGENBDE                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENBDE                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002GEBDEMAKE 09/30/02'                                      
         END                                                                    
