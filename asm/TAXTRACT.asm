*          DATA SET TAXTRACT   AT LEVEL 032 AS OF 07/31/14                      
*PHASE TAXTRCTA                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE TAXROUTS                 XTRACT RECORD CREATION MODULE                 
*INCLUDE TAXCNVX                  CONVERSION ROUTINES FOR ALL ABOVE             
*INCLUDE TAXHDRS                  COLUMN DEF RECORDS                            
*INCLUDE TINVCON                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*                                                                               
         TITLE 'SPXTRACT - EXTRACT SPOT SYSTEM FILE SQL DATA'                   
***********************************************************************         
*  SPOT SQL SUB SYSTEM EXTRACT CONTROL MODULE                        *          
*                                                                     *         
* CONTROL IS PASSED FROM DXTRACT WITH PARAMETERS:                     *         
* R1=A(EXTRACT CONTROL DATA BLOCK - SEE DSECT DXBLOCKD)               *         
*                                                                               
* MODULE ENTERED WITH ONE OF FOLLOWING MODES IN DXMODE:               *         
*                                                                     *         
*   DXOPENQ  - OPEN SYSTEM FILES                                      *         
*   DXCLOSEQ - CLOSE SYSTEM FILES                                     *         
*   DXLOADQ  - EXTRACT FROM FILES IN LOAD MODE                        *         
*   DXUPDTQ  - EXTRACT FROM FILES IN UPDATE MODE                      *         
*                                                                     *         
* FOR DXLOADQ AND DXUPDTQ MODES,                                      *         
* DXBLOCKD CONTAINS DXSTPTR WHICH IS:                                 *         
* A(CURRENT ENTRY IN SUB SYSTEM EXTRACT DRIVER TABLE - SEE DSECT      *         
*                                                      SYSTABD)       *         
*                                                                     *         
* RETURN CC .NE. IF ERROR CONDITION ELSE CC .EQ. FOR OK               *         
***********************************************************************         
*                                                                               
TAXTRACT CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY ADDRESS                                                          
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 WORKL,**TAXT**,CLEAR=YES                                         
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
*                                                                               
         L     RA,=A(ADDRESS)                                                   
         USING ADDRESSD,RA                                                      
         ST    RA,AADDRESS                                                      
*                                                                               
         L     R7,0(R1)            R7=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R7                                                      
         ST    R7,ADXBLOCK                                                      
*                                                                               
         L     R6,DXSTPTR          R6=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R6                                                       
*                                                                               
**NOP**  MVC   ACOMFACS,4(R1) <=== THIS DOESN'T HAPPEN YET!                     
         L     RE,=A(COMFACS)                                                   
         ST    RE,ACOMFACS                                                      
*                                                                               
         USING COMFACSD,RE                                                      
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VDATCON,CDATCON                                                  
         MVC   VHEXOUT,CHEXOUT                                                  
         DROP  RE                                                               
*                                                                               
         MVC   VUTL,=V(UTL)                                                     
*                                                                               
         LHI   RE,IOAREA2-IOAREA1  GET IOA LENGTH                               
         ST    RE,IOLEN                                                         
         LA    RE,IOAREA1                                                       
         ST    RE,AIO1                                                          
         ST    RE,AIO                                                           
         A     RE,IOLEN                POINT TO IOA2                            
         ST    RE,AIO2                                                          
*                                                                               
          MVI  MYSPACES,C' '                                                    
          MVC  MYSPACES+1(79),MYSPACES                                          
*                                                                               
          LA   RE,TLRCELEM-TLRCD                                                
          STH  RE,DATADISP                                                      
*                                                                               
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),DXDSPAC                           
*                                                                               
         L     R6,DXSTPTR          R6=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R6                                                       
         MVC   PLATFORM,SXDTPLFM                                                
         MVC   VERSION,SXDTVER                                                  
         OC    VERSION,VERSION                                                  
         BNZ   MAIN                                                             
         MVI   VERSION,1                                                        
         B     MAIN                                                             
*                                                                               
LOW      SR    RC,RC                                                            
         CHI   RC,256                                                           
         J     EXIT                                                             
*                                                                               
HI       CHI   RC,0                                                             
         J     EXIT                                                             
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
EXIT     XIT1  ,                                                                
*                                                                               
*                                                                               
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
MOPEN    CLI   DXMODE,DXOPENQ                                                   
         BNE   MCLOSE                                                           
         BRAS  RE,PROCOPEN         OPEN SYSTEM FILES                            
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         B     MXIT                                                             
*                                                                               
MCLOSE   CLI   DXMODE,DXCLOSEQ                                                  
         BNE   MLOAD                                                            
         BRAS  RE,PROCCLOS         CLOSE SYSTEM FILES                           
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MLOAD    CLI   DXMODE,DXLOADQ                                                   
         BNE   MUPDT                                                            
         BAS   RE,PROCLOAD         EXTRACT FROM FILES IN LOAD MODE              
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
                                                                                
MUPDT    CLI   DXMODE,DXUPDTQ                                                   
         BNE   MERR                                                             
         BAS   RE,PROCUPDT         EXTRACT FROM FILES IN UPDATE MODE            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MERR     DC    H'0'                                                             
         B     MXIT                                                             
*                                                                               
MXIT     XIT1                                                                   
*                                                                               
***********************************************************************         
* PROCESS DATA IN LOAD MODE                                           *         
***********************************************************************         
*                                                                               
PROCLOAD NTR1  ,                                                                
*                                                                               
         LA    R4,DXUSER                                                        
         USING DXUSERD,R4                                                       
*                                                                               
         CLI   DXUSDATE,C' '       TEST DATE SPECIFIED                          
         BNE   PROCL10             YES                                          
                                                                                
* NO DATES - CALCULATE DATES OF PREVIOUS WEEK                                   
                                                                                
         GOTO1 VDATCON,DMCB,(5,0),WORK                                          
         GOTO1 =V(GETDAY),DMCB,WORK,WORK+6                                      
         LLC   R0,0(R1)            GET DAY OF WEEK                              
         CHI   R0,7                TEST SUNDAY                                  
         BNE   PROCL2                                                           
         MVC   WORK+6(6),WORK      MOVE SUNDAY DATE                             
         B     PROCL4                                                           
*                                                                               
PROCL2   LNR   R0,R0               BACK UP TO PREVIOUS SUNDAY                   
         GOTO1 =V(ADDAY),DMCB,WORK,WORK+6,(R0)                                  
*                                                                               
PROCL4   GOTO1 VDATCON,DMCB,WORK+6,(X'20',DXUEDATE) NO DDS STYLE DATE           
*                                                                               
         LHI   R0,-6               NOW BACK UP TO MONDAY                        
         GOTO1 =V(ADDAY),DMCB,WORK+6,WORK+12,(R0)                               
         GOTO1 VDATCON,DMCB,WORK+12,(X'20',DXUSDATE) NO DDS STYLE DATE          
*                                                                               
PROCL10  GOTO1 VDATCON,DMCB,DXUSDATE,(1,FLTSDATE)                               
         GOTO1 (RF),(R1),DXUEDATE,(1,FLTEDATE)                                  
*                                                                               
         MVC   OUTMAX,=X'7FFFFFFF' SET TO A VERY BIG NUMBER                     
*                                                                               
PROCL12  MVC   TYPECODE,SXDTTYP                                                 
         BRAS  RE,GETTYPE          SET UP RECORD TYPE TABLE DATA                
*                                                                               
         CLC   =C'FLIST=',DXULSTYP                                              
         BNE   PROCL14                                                          
         MVC   DUB,MYSPACES                                                     
         MVC   DUB(6),DXUGROUP                                                  
         BRAS  RE,BLDFLST                                                       
         B     PROCL16                                                          
*                                                                               
PROCL14  CLC   =C'GROUP=',DXULSTYP                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   THISCGR,DXUGROUP                                                 
         BRAS  RE,BLDCGR                                                        
*                                                                               
PROCL16  L     RE,=A(CGRTAB)       POINT TO TOP OF TABLE                        
         LA    RE,L'CGRTAB(RE)                                                  
         CLI   0(RE),0             FIND EOL                                     
         BNE   *-8                 NOT YET!                                     
         MVC   0(6,RE),=6X'FF'                                                  
*                                                                               
         L     RE,=A(CGRTAB)       POINT TO FIRST ENTRY                         
         B     PROCL24             SAVE ENTRY ADDRESS                           
*                                                                               
         USING CGRTABD,RE                                                       
PROCL18  MVC   THISADV,CGRCGR        SET CURRENT VALUES!                        
*                                                                               
         CLC   THISADV(4),=C'NEST'    *** SORRY ABOUT THIS! ***                 
         BNE   *+10                                                             
         MVC   THISADV(4),=C'NES '                                              
*                                                                               
         MVC   THISAGY,CGRAGY                                                   
         MVC   THISCLT,CGRCLT                                                   
*-->                                                                            
**NOP    CLC   THISAGY(4),=C'6329'  TO SPEED UP TESTING                         
**NOP    BNE   PROCL22                                                          
*-->                                                                            
         DROP  R4,RE                                                            
*                                                                               
         TIME  DEC            R0=HHMMSSHH                                       
         ST    R0,FULL                                                          
         GOTO1 =V(HEXOUT),DMCB,FULL,DUB,4,=C'TOG'                               
*                                                                               
         L     RE,=V(CPRINT)                                                    
         USING DPRINT,RE                                                        
*                                                                               
         MVC   P(2),DUB           MOVE HH                                       
         MVI   P+2,C'.'                                                         
         MVC   P+3(2),DUB+2       MOVE MM                                       
         MVI   P+5,C'.'                                                         
         MVC   P+6(2),DUB+4       MOVE SS                                       
*                                                                               
         MVC   P+10(10),=C'PROCESSING'                                          
         MVC   P+22(6),THISADV                                                  
         MVC   P+30(6),THISAGY                                                  
         MVC   P+38(6),THISCLT                                                  
*                                                                               
         MVC   P+56(6),=C'COUNT='                                               
         L     R0,OUTCOUNT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+62(7),DUB                                                      
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         DROP  RE                                                               
*                                                                               
PROCL20  L     RF,TYPEALOD         CALL EXTRACT LOAD ROUTINE                    
         BASR  RE,RF                                                            
         JNE   NO                  ERROR EXIT                                   
*                                                                               
         CLC   TYPENAME,=C'REF'    TEST DOING REF DATA                          
         JE    YES                 YES - PROCESS ONCE ONLY                      
*                                                                               
         L     RE,NEXTCGR          GET A(CURRENT TABLE ENTRY)                   
         USING CGRTABD,RE                                                       
*                                                                               
PROCL22  LA    RE,CGRNEXT                                                       
*                                                                               
PROCL24  ST    RE,NEXTCGR                                                       
         CLI   0(RE),X'FF'                                                      
         BNE   PROCL18                                                          
         DROP  RE                                                               
*                                                                               
PROCL30  BRAS  RE,LOADPRF          OUTPUT PERFORMER REF DATA                    
         BRAS  RE,LOADOVR          OUTPUT OVERSCALE REF DATA                    
         BRAS  RE,LOADPRD          OUTPUT PRODUCT REF DATA                      
         J     YES                 EXIT OK                                      
*                                                                               
***********************************************************************         
* PROCESS FILE DATA IN UPDATE MODE - READ RECOVERY FILES              *         
***********************************************************************         
                                                                                
PROCUPDT NTR1  ,                                                                
         L     R1,=A(MAXIOSW)                                                   
         MVC   MAXIOS,0(R1)                                                     
*                                                                               
         L     R5,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R5                                                         
*                                                                               
         MVC   TYPECODE,SXDTTYP                                                 
         BRAS  RE,GETTYPE          SET TYPE TABLE DATA                          
*                                                                               
         CLI   RFILTY,TALFILQ      TEST TALENT FILE                             
         JNE   YES                 ELSE IGNORE RECORD                           
*                                                                               
PROCUP20 DS    0H                                                               
         BAS   RE,PROCKEY          PROCESS RECORD KEY VALUES                    
         JNE   YES                 EITHER IGNORE RECORD                         
         L     RF,TYPEAUPD         ELSE CALL UPDATE PROCESS ROUTINE             
         GOTO1 (RF),DMCB,(RC)                                                   
         JNE   PROCUPNX            EXIT ERROR                                   
         J     PROCUPQX            EXIT OK                                      
*                                                                               
PROCUPQX DS    0H                                                               
         L     R1,=A(MAXIOSW)                                                   
         MVC   0(4,R1),MAXIOS                                                   
         J     YES                                                              
*                                                                               
PROCUPNX DS    0H                                                               
         L     R1,=A(MAXIOSW)                                                   
         MVC   0(4,R1),MAXIOS                                                   
         J     NO                                                               
         DROP  R5                                                               
*                                                                               
***********************************************************************         
* PROCESS KEY VALUES IN RECOVERY RECORD                               *         
* NTRY: R5 = A(RECOVERY RECORD)                                       *         
***********************************************************************         
*                                                                               
         USING RECDS,R5                                                         
PROCKEY  NTR1  ,                                                                
         BRAS  RE,RECCMP           COMPARE COPY/CHANGE RECORDS                  
         JNE   NO                  NO CHANGES                                   
*                                                                               
         CLI   DXACTION,C'C'                                                    
         JNE   NO                                                               
         CLI   RRECTY,X'02'        IS THIS A CHANGE RECORD                      
         JNE   NO                  NO                                           
*                                                                               
         TM    RECVHDR+24+TLCOSTAT-TLCOD,X'80'  TEST RECORD IS DELETED          
         JNZ   NO                               AVOID DELETED CHANGES           
         MVI   DXACTION,C'D'                                                    
         J     YES                                                              
         DROP  R5                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* COMMON ADDRESSES FOR ALL ROUTINES - COVERED BY ADDRESSD DSECT       *         
* ADDRESS IS AT AADDRESS IN W/S                                       *         
***********************************************************************         
                                                                                
UPDTREF  DC    A(0)                                                             
UPDTPRF  DC    A(0)                                                             
UPDTOVR  DC    A(0)                                                             
UPDTPRD  DC    A(0)                                                             
UPDTFNL  DC    A(0)                                                             
UPDTUSE  DC    A(0)                                                             
FILTOVR  DC    A(0)                                                             
FILTPRD  DC    A(0)                                                             
         DS    0D                                                               
ADDRESS  DC    CL8'ADDRESS*'                                                    
         DC    A(COMFACS)                                                       
         DC    V(DATAMGR)                                                       
         DC    V(DATCON)                                                        
         DC    V(HEXOUT)                                                        
         DC    V(BINSRCH)                                                       
         DC    V(TAXCNVX)                                                       
         DC    V(UTL)                                                           
         DC    A(0)                VT00A88 SET DYNAMICALLY                      
         DC    A(0)                TGAUSES SET DYNAMICALLY                      
         DC    A(CGRTAB)                                                        
*                                                                               
         DC    A(RDHI)                                                          
         DC    A(RDSEQ)                                                         
         DC    A(GETREC)                                                        
*                                                                               
         DC    V(TALCMLC)         COMMERCIAL CAST                               
         DC    V(TALRESC)         RESIDUALS                                     
         DC    V(TALSESC)         SESSION/HOLDING                               
         DC    V(TALFNLC)         FINAL CAST                                    
         DC    V(TALUSEC)         USE AUTH                                      
         DC    V(TALPRFC)         PERFORMER REF                                 
         DC    V(TALOVRC)         OVERSCALE REF                                 
         DC    V(TALPRDC)         PRODUCT REF                                   
         DC    V(TALREFC)         REFERENCE                                     
*                                                                               
         DC    A(LOADALL)                                                       
         DC    A(LOADCML)                                                       
         DC    A(LOADRES)                                                       
         DC    A(LOADSES)                                                       
         DC    A(LOADFNL)                                                       
         DC    A(LOADUSE)                                                       
         DC    A(LOADPRF)         PERFORMER REF                                 
         DC    A(LOADOVR)         OVERSCALE REF                                 
         DC    A(LOADPRD)         PRODUCT REF                                   
         DC    A(LOADREF)         REFERENCE                                     
*                                                                               
         DC    A(UPDTCML)                                                       
         DC    A(UPDTRES)                                                       
         DC    A(UPDTSES)                                                       
         DC    A(UPDTFNL)                                                       
         DC    A(UPDTUSE)                                                       
         DC    A(UPDTPRF)         PERFORMER REF                                 
         DC    A(UPDTOVR)         OVERSCALE REF                                 
         DC    A(UPDTPRD)         PRODUCT REF                                   
         DC    A(UPDTREF)         REFERENCE                                     
*                                                                               
         DC    A(FILTCML)                                                       
         DC    A(FILTRES)                                                       
         DC    A(FILTSES)                                                       
         DC    A(FILTFNL)                                                       
         DC    A(FILTUSE)                                                       
         DC    A(FILTPRF)         PERFORMER REF                                 
         DC    A(FILTOVR)         OVERSCALE REF                                 
         DC    A(FILTPRD)         PRODUCT REF                                   
         DC    A(FILTREF)         REFERENCE                                     
*                                                                               
         DC    A(INITALL)                                                       
*                                                                               
         DC    A(TYPETAB)                                                       
         DC    A(DECIOC)                                                        
*                                                                               
TALFILQ  EQU   X'72'                                                            
FF       EQU   X'FF'                                                            
*                                                                               
***********************************************************************         
* TYPETAB DEFINES PROCESS RECORD TYPES & IS COVERED BY TYPETABD       *         
*                                                                     *         
* CL3    TYPE NAME                                                    *         
* AL1    N/D                                                                    
* AL1    TYPE FLAGS                                                   *         
* AL3    N/D                                                          *         
* AL4    LOAD ROUTINE ADDRESS                                         *         
* AL4    UPDATE ROUTINE ADDRESS                                       *         
***********************************************************************         
*                                                                               
*                                                                               
TYPETAB  DS    0F                                                               
         DC    CL3'ALL',AL1(00,00,00,00,00),AL4(LOADALL,UPDTALL)                
*                                                                               
         DC    CL3'CML',AL1(00,00,00,00,00),AL4(LOADCML,UPDTCML)                
         DC    CL3'RES',AL1(00,00,00,00,00),AL4(LOADRES,UPDTRES)                
         DC    CL3'SES',AL1(00,00,00,00,00),AL4(LOADSES,UPDTSES)                
         DC    CL3'FNL',AL1(00,00,00,00,00),AL4(LOADFNL,UPDTFNL)                
         DC    CL3'USE',AL1(00,00,00,00,00),AL4(LOADUSE,UPDTUSE)                
         DC    CL3'PRF',AL1(00,00,00,00,00),AL4(LOADPRF,UPDTPRF)                
         DC    CL3'OVR',AL1(00,00,00,00,00),AL4(LOADOVR,UPDTOVR)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'REF',AL1(00,00,00,00,00),AL4(LOADREF,UPDTREF)                
*                                                                               
         DC    X'FF'                                                            
         DROP  RB                                                               
*                                                                               
***********************************************************************         
* GET TYPE TABLE ACTION FROM 3 CHARACTER CODE                         *         
***********************************************************************         
                                                                                
GETTYPE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   RF,TYPETAB                                                       
         USING TYPETABD,RF                                                      
GTYP02   CLI   0(RF),FF            END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TYPECODE,TYPNAME    COMPARE NAME                                 
         BE    GTYP04                                                           
         LA    RF,TYPETABLQ(RF)    GET NEXT ENTRY                               
         B     GTYP02                                                           
*                                                                               
GTYP04   MVC   TYPENAME,TYPNAME    MATCH FOUND - GET TABLE INFORMATION          
         MVC   TYPEDEEP,TYPLDEEP                                                
         MVC   TYPEFLAG,TYPFLAG                                                 
         MVC   TYPEALOD,TYPLOAD                                                 
         MVC   TYPEAUPD,TYPUPDT                                                 
         J     YES                                                              
         LTORG                                                                  
         DROP  RF                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DECREMENT MAXIMUM IO COUNT                                          *         
***********************************************************************         
*                                                                               
*                                                                               
DECIOC   NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,MAXIOS                                                     
         BCTR  RF,0                                                             
         STCM  RF,15,MAXIOS                                                     
         JNZ   YES                                                              
*                                                                               
         LA    R3,DECMSG           OUTPUT IO COUNT EXCEEDED MESSAGE             
         MVC   DECTYPE,TYPENAME                                                 
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     NO                                                               
*                                                                               
DECMSGL  DC    AL2(50)                                                          
DECMSG   DC    CL50' '                                                          
         ORG   DECMSG                                                           
         DC    C'IO COUNT EXCEEDED - TYPECODE = '                               
DECTYPE  DC    CL3' '                                                           
         EJECT                                                                  
*==================================================================             
* READ AN FLIST RECORD THAT CONTAINS A LIST OF ALL THE CLTGRPS                  
* TO BE PROCESSED.                                                              
*==================================================================             
                                                                                
BLDFLST  NTR1  BASE=*,LABEL=*                                                   
         MVI   FILETYPE,C'T'       SET TO READ TALENT FILES                     
         MVC   AIO,AIO2                                                         
                                                                                
K        USING TLGLD,KEY                                                        
         XC    KEY,KEY                                                          
         MVI   K.TLGLCD,TLGLCDQ                                                 
         MVI   K.TLGLTYPE,TLGLTYPF                                              
         MVC   K.TLGLLST,DUB       FLIST GROUP                                  
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                 INVALID LIST NAME                            
         DC    H'0'                                                             
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,TAGLELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TAGLD,R6                                                         
BLDF2    MVC   THISCGR,MYSPACES                                                 
         LLC   RE,1(R6)                                                         
         AHI   RE,-4               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   THISCGR(0),TAGLDATA                                              
*                                                                               
         BRAS  RE,BLDCGR                                                        
*                                                                               
         MVI   ELCODE,TAGLELQ                                                   
         BRAS  RE,NEXTEL                                                        
         BE    BLDF2                                                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* BUILD A LIST OF AGENCIES/CLIENTS IN THISCGR BY                                
* READING THROUGH CLIENT GROUP COMMERCIAL POINTERS                              
*                                                                               
* ON FIRST ENTRY, DXUSER CONTAINS FLIST=XXXXXX                                  
*==================================================================             
                                                                                
BLDCGR   NTR1  BASE=*,LABEL=*                                                   
         MVI   FILETYPE,C'T'       SET TO READ TALENT FILES                     
         MVC   AIO,AIO1                                                         
         MVC   THISADV,THISCGR     MOVE TO NEW RECORD                           
                                                                                
* READ THROUGH COMMERCIALS AND FLAG ACTIVE CLIENT CODES                         
                                                                                
K        USING TLCOPD,KEY                                                       
         XC    KEY,KEY                                                          
         MVI   K.TLCOPCD,TLCOGCDQ                                               
         MVC   K.TLCOGCLG,THISCGR    CLIENT GROUP                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         JNE   EXIT                GROUP HAS NO COMMERCIALS                     
*                                                                               
BLDCGR10 GOTO1 GET                                                              
*                                                                               
         BRAS  RE,FILTCML          FILTER ON FIRST AIR DATE                     
         BNE   BLDCGR14                                                         
*                                                                               
         L     R2,AIO              POINT TO COMMERCIAL RECORD                   
         USING TLCOCD,R2                                                        
*                                                                               
         MVC   THISAGY(12),TLCOAGY    MOVE AGY/CLT CODES                        
*                                                                               
         L     R1,=A(CGRPARMS)                                                  
         GOTO1 VBINSRCH,(R1),(1,THISADV)  ADD ENTRY IF NOT THERE                
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDCGR14 GOTO1 SEQ                                                              
         CLC   KEY(7),KEYSAVE                                                   
         BE    BLDCGR10                                                         
         J     EXIT                                                             
         DROP  R2                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* CALL DMGR TO GET A RECORD                                           *         
* NTRY: AIO          = A(RECORD BUFFER)                               *         
*       DSKADDR      = DISK ADDRESS TO READ                           *         
* EXIT: CC EQUAL     = RECORD READ OK                                 *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
                                                                                
GETREC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   DSKADDR,TLDRDA-TLDRD+KEY                                         
*                                                                               
         LA    RE,=C'TALFIL'                                                    
         CLI   FILETYPE,C'C'                                                    
         BNE   *+8                                                              
         LA    RE,=C'CHKFIL'                                                    
         ST    RE,DMCB+4                                                        
*                                                                               
         L     RF,VDATAMGR                                                      
         GOTO1 (RF),DMCB,(DMINBTS,=C'GETREC'),,DSKADDR,AIO,DMWORK               
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'10'                                                      
         JO    LOW                                                              
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),PARMS,DSKADDR,GETDA,L'DSKADDR,0                             
         GOTO1 (RF),(R1),DMCB+8,GETRC,1                                         
*                                                                               
         LA    R3,GETMSGL          OUTPUT DISK READ ERROR MESSAGE               
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     HI                                                               
*                                                                               
GETMSGL  DC    AL2(50)                                                          
GETMSG   DC    CL50' '                                                          
         ORG   GETMSG                                                           
         DC    C'DMGR GETREC ERROR - D/A = '                                    
GETDA    DC    CL8' '                                                           
         DC    C','                                                             
         DC    C' RC = '                                                        
GETRC    DC    CL2' '                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
*       KEY          = KEY TO READ HIGH FOR                           *         
* EXIT: CC EQUAL     = RECORD READ OK OR EOF SET                      *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
                                                                                
RDHI     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         L     RF,VDATAMGR                                                      
*                                                                               
         LA    RE,=C'TALDIR'                                                    
         CLI   FILETYPE,C'C'                                                    
         BNE   *+8                                                              
         LA    RE,=C'CHKDIR'                                                    
         ST    RE,DMCB+4                                                        
         GOTO1 (RF),DMCB,(DMINBTS,=C'DMRDHI'),,KEYSAVE,KEY                      
*                                                                               
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'80'                                                      
         JO    YES                                                              
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),PARMS,KEYSAVE,RDHKEY,L'KEYSAVE,0                            
*                                                                               
         XR    R0,R0                                                            
         WTO   TEXT=((RDHHL,C),(RDH1L,D),(0,E)),MCSFLAG=HRDCPY                  
         J     NO                                                               
*                                                                               
RDHHL    DC    AL2(40)                                                          
         DC    CL40'DMGR READHI ERROR KEY HEXOUT FOLLOWS'                       
*                                                                               
RDH1L    DC    AL2(90)                                                          
RDH1M    DC    CL90' '                                                          
         ORG   RDH1M                                                            
         DC    C'KEY='                                                          
RDHKEY   DC    CL84' '                                                          
         ORG   RDH1M+L'RDH1M                                                    
         LTORG                                                                  
*                                                                               
***********************************************************************         
*       KEY          = KEY TO READ HIGH FOR                           *         
* EXIT: CC EQUAL     = RECORD READ OK OR EOF SET                      *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
                                                                                
RDSEQ    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,=C'TALDIR'                                                    
         CLI   FILETYPE,C'C'                                                    
         BNE   *+8                                                              
         LA    RE,=C'CHKDIR'                                                    
         ST    RE,DMCB+4                                                        
*                                                                               
         L     RF,VDATAMGR                                                      
         GOTO1 (RF),DMCB,(DMINBTS,=C'DMRSEQ'),,KEYSAVE,KEY                      
*                                                                               
         CLI   8(R1),0                                                          
         JE    YES                                                              
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),PARMS,KEYSAVE,RDSKEY,L'KEYSAVE,0                            
*                                                                               
         XR    R0,R0                                                            
         WTO   TEXT=((RDSHL,C),(RDS1L,D),(0,E)),MCSFLAG=HRDCPY                  
         J     NO                                                               
*                                                                               
RDSHL    DC    AL2(40)                                                          
         DC    CL40'DMGR READSEQ ERROR KEY HEXOUT FOLLOWS'                      
*                                                                               
RDS1L    DC    AL2(90)                                                          
RDS1M    DC    CL90' '                                                          
         ORG   RDS1M                                                            
         DC    C'KEY='                                                          
RDSKEY   DC    CL84' '                                                          
         ORG   RDS1M+L'RDS1M                                                    
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPARE COPY AND CHANGE RECORDS TO SEE IF THEY ARE DIFFERENT        *         
*                                                                     *         
* NTRY:                                                               *         
* EXIT: CC EQ    RECORD TO BE PROCESSED                               *         
*     : CC NE    RECORD NOT TO BE PROCESSED                           *         
***********************************************************************         
                                                                                
RECCMP   NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
*                                                                               
         CLI   RRECTY,3            ADD OF NEW RECORD?                           
         JE    YES                 YES - NO COPY, NOTHING TO COMPARE            
*                                                                               
         L     R2,DXARECB          GET CHANGE RECORD ADDRESS                    
         LA    R2,L'RECVHDR+4(R2)                                               
         L     R6,DXACPYB          GET COPY RECORD ADDRESS                      
         LA    R6,L'RECVHDR+4(R6)                                               
*                                                                               
         CLI   RFILTY,TALFILQ                                                   
         BE    RECCMP00                                                         
         J     NO                                                               
*                                                                               
RECCMP00 DS    0H                                                               
*                                                                               
* GET CHANGE RECORD'S LENGTH INTO R3, COPY - INTO R7                            
*                                                                               
         SR    R3,R3                                                            
         SR    R7,R7                                                            
*                                                                               
         CLI   RFILTY,TALFILQ                                                   
         JNE   NO                                                               
         ICM   R3,3,32(R2)                                                      
         ICM   R7,3,32(R6)                                                      
         B     RECCMP50                                                         
*                                                                               
RECCMP50 DS    0H                                                               
         CR    R3,R7               RECORD LENGTH CHANGED?                       
         JNE   YES                 YES - RECORD MUST HAVE CHANGED TOO           
*                                                                               
         CLCL  R2,R6               COMPARE TWO RECORDS                          
         JNE   YES                                                              
         J     NO                  RECORDS ARE IDENTICAL                        
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* LOAD ALL RECORD DATA                                                *         
***********************************************************************         
*                                                                               
LOADALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LOADTAB                                                       
*                                                                               
LOAD02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOAD04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         BRAS  RE,GETTYPE                                                       
         JNE   NO                                                               
         XC    OUTCOUNT,OUTCOUNT   RESET OUTPUT COUNTER                         
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOAD04   LA    R3,L'LOADTAB(R3)                                                 
         J     LOAD02                                                           
*                                                                               
LOADTAB  DS    0XL8                                                             
         DC    CL3'CML',AL1(0),AL4(LOADCML) COMMERCIAL                          
         DC    CL3'RES',AL1(0),AL4(LOADRES) RESIDUALS                           
         DC    CL3'PRF',AL1(0),AL4(LOADREF) REFERENCE                           
         DC    X'00'                                                            
         LTORG                                                                  
*                                                                               
***********************************************************************         
* UPDATE ALL RECORD DATA                                              *         
***********************************************************************         
*                                                                               
UPDTALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPDTTAB                                                       
*                                                                               
UPDT02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDT04                                                           
         MVC   TYPECODE,0(R3)                                                   
*                                                                               
         BRAS RE,GETTYPE                                                        
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDT04   LA    R3,L'UPDTTAB(R3)                                                 
         J     UPDT02                                                           
                                                                                
UPDTTAB  DS    0XL8                                                             
         DC    CL3'CML',AL1(0),AL4(UPDTCML) COMMERCIALS                         
         DC    CL3'RES',AL1(0),AL4(UPDTRES) RESIDUALS                           
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* INITIALISE ALL EXTRACT RECORDS                                      *         
* NTRY: R0 = ADDRESS OF RECORD AREA                                   *         
*       R1 = RECORD LENGTH                                                      
***********************************************************************         
*                                                                               
INITALL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R3,R0               R3=A(EXTRACT RECORD AREA)                    
         USING DXHDRD,R3                                                        
*                                                                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         LA    RF,DXHDRHL                                                       
         SLL   RF,16                                                            
         ST    RF,DXHDRLEN         SET MINIMUM RECORD LEN IN HDR                
*                                                                               
         MVI   DXHDRRTY-1,EOFCHAR                                               
         MVC   DXHDRRTY,DXACTION                                                
         MVI   DXHDRCDT-1,EOFCHAR                                               
*                                                                               
         CLI   DXMODE,DXLOADQ      LOAD MODE?                                   
         JE    IALL02              YES                                          
*                                                                               
         L     R5,DXARECB          HERE IF UPDATE MODE                          
         USING RECDS,R5                                                         
         GOTO1 VDATCON,DMCB,(3,RDATE),(20,DXHDRCDT)                             
         MVI   DXHDRCTI-1,EOFCHAR                                               
         ICM   RF,15,RTIME          FORMAT DATE & TIME FROM RECOVERY            
         TM    RTIME,X'80'                                                      
         BNO   *+12                                                             
         SLL   RF,1                                                             
         SRL   RF,5                                                             
*                                                                               
         XC    DUB,DUB                                                          
         STCM  RF,15,DUB+4                                                      
         OI    DUB+7,X'0C'                                                      
         UNPK  DXHDRCTI(6),DUB+4(4)                                             
         OI    DXHDRCTI+5,X'F0'                                                 
         J     YES                                                              
*                                  HERE IF LOAD MODE                            
IALL02   MVC   DXHDRCDT,DXDATEN    SET TODAYS DATE                              
         MVI   DXHDRCTI-1,EOFCHAR                                               
         MVC   DXHDRCTI,DXTIMEN                                                 
         CLI   DXDAFORM,C'Y'                                                    
         JNE   YES                                                              
         MVI   DXHDRCDT+00,C''''                                                
         MVC   DXHDRCDT+01(6),DXDATEN+2                                         
         MVC   DXHDRCDT+07(2),=C'  '                                            
         MVC   DXHDRCDT+09(2),DXTIMEN                                           
         MVI   DXHDRCDT+11,C':'                                                 
         MVC   DXHDRCDT+12(2),DXTIMEN+2                                         
         MVI   DXHDRCDT+14,C''''                                                
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R3,R5                                                            
*                                                                               
***********************************************************************         
* COMMERCIAL RECORDS                                                            
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
* LOAD COMMERCIAL RECORDS FOR AN AGENCY/CLIENT                                  
* DXUSER SHOULD SAY GROUP=CLTGRP                                                
*---------------------------------------------------------------------*         
                                                                                
LOADCML  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   FILETYPE,C'T'                                                    
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R2,KEY              SET KEY TO READ FIRST RECORD                 
         USING TLCOD,R2                                                         
*                                                                               
         XC    TLCOKEY,TLCOKEY                                                  
         MVI   TLCOCD,TLCOCDQ      X'40'                                        
         MVI   TLCOVER,0                                                        
*                                                                               
         LA    R1,DXUSER                                                        
         USING DXUSERD,R1                                                       
         MVC   TLCOAGY,THISAGY     MOVE AGENCY                                  
         MVC   TLCOCLI,THISCLT     MOVE CLIENT                                  
         DROP  R1                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
LCML02   CLC   KEY(14),KEYSAVE     TEST SAME TYPE/AGY/CLT                       
         BNE   LCML20              NO                                           
         MVC   SVLOADKY,KEY        SAVE THIS KEY                                
         GOTO1 GET                                                              
*                                                                               
         BRAS  RE,FILTCML                                                       
         BE    LCML04                                                           
         GOTO1 SEQ                                                              
         B     LCML02                                                           
*                                                                               
LCML04   GOTO1 VTALCMLC,PARMS,(RC),AIO,0,(R6)                                   
*                                                                               
LCML05   CLI   8(R1),0             TEST RECORD RETURNED                         
         BNE   LCML10              NO - NEXT COMMERCIAL                         
*                                                                               
         GOTO1 VTAXCNVX,PARMS,(RC)      CONVERT TO SQL FORMAT                   
*                                                                               
         L     RE,OUTCOUNT                                                      
         LA    RE,1(RE)                                                         
         ST    RE,OUTCOUNT                                                      
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)  OUTPUT TO SQL BUFFER                    
                                                                                
* GET NEXT OUTPUT RECORD                                                        
                                                                                
         GOTO1 VTALCMLC,PARMS,(RC),AIO,1,(R6)                                   
         B     LCML05                                                           
*                                                                               
LCML10   CLC   OUTCOUNT,OUTMAX                                                  
         BNL   LCML20                                                           
         MVC   KEY,SVLOADKY        RESTORE MY KEY                               
         GOTO1 HIGH                REREAD PREVIOUS RECORD                       
         GOTO1 SEQ                 GET NEXT RECORD                              
         B     LCML02                                                           
*                                                                               
LCML20   DS    0H                                                               
         J     YES                 AND EXIT                                     
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* RESIDUALS (INVOICE RECORDS)                                                   
***********************************************************************         
*                                                                               
                                                                                
LOADRES  NTR1  BASE=*,LABEL=*                                                   
         MVI   FILETYPE,C'T'                                                    
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R2,KEY              SET KEY TO READ FIRST RECORD                 
         USING TLIND,R2                                                         
*                                                                               
         XC    TLINKEY,TLINKEY                                                  
         MVI   TLINCD,TLINCDQ      X'90'                                        
*                                                                               
         LA    R1,DXUSER                                                        
         USING DXUSERD,R1                                                       
         MVC   TLINAGY,THISAGY     MOVE AGENCY                                  
         DROP  R1                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
LRES02   CLC   KEY(20),KEYSAVE     TEST SAME TYPE/AGY                           
         BNE   LRES20              NO                                           
         MVC   SVLOADKY,KEY        SAVE THIS KEY                                
         BRAS  RE,GETREC                                                        
*                                                                               
         BRAS  RE,FILTRES                                                       
         BE    LRES04                                                           
         GOTO1 SEQ                                                              
         B     LRES02                                                           
*                                                                               
LRES04   GOTO1 VTALRESC,PARMS,(RC),AIO,0,(R6)                                   
*                                                                               
LRES05   CLI   8(R1),0             TEST RECORD RETURNED                         
         BNE   LRES10              NO - NEXT COMMERCIAL                         
*                                                                               
         GOTO1 VTAXCNVX,PARMS,(RC)      CONVERT TO SQL FORMAT                   
*                                                                               
         L     RE,OUTCOUNT                                                      
         LA    RE,1(RE)                                                         
         ST    RE,OUTCOUNT                                                      
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)  OUTPUT TO SQL BUFFER                    
                                                                                
* GET NEXT OUTPUT RECORD                                                        
                                                                                
         GOTO1 VTALRESC,PARMS,(RC),AIO,1,(R6)                                   
         B     LRES05                                                           
*                                                                               
LRES10   CLC   OUTCOUNT,OUTMAX                                                  
         BNL   LRES20                                                           
         MVI   FILETYPE,C'T'       BACK TO TALENT FILE                          
         MVC   AIO,AIO1                                                         
         MVC   KEY,SVLOADKY        RESTORE MY KEY                               
         GOTO1 HIGH                REREAD PREVIOUS RECORD                       
         GOTO1 SEQ                 GET NEXT RECORD                              
         B     LRES02                                                           
*                                                                               
LRES20   DS    0H                  OUTPUT REFERENCE DATA                        
         J     YES                 AND EXIT                                     
*                                                                               
***********************************************************************         
* USE AUTHORIZATIONS                                                            
***********************************************************************         
*                                                                               
*---------------------------------------------------------------------*         
                                                                                
LOADUSE  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   FILETYPE,C'T'                                                    
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R2,KEY              SET KEY TO READ FIRST RECORD                 
         USING TLIND,R2                                                         
*                                                                               
         XC    TLINKEY,TLINKEY                                                  
         MVI   TLINCD,TLINCDQ      X'90'                                        
*                                                                               
         LA    R1,DXUSER                                                        
         USING DXUSERD,R1                                                       
         MVC   TLINAGY,THISAGY     MOVE AGENCY                                  
         DROP  R1                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
LUSE02   CLC   KEY(20),KEYSAVE     TEST SAME TYPE/AGY                           
         BNE   LUSE20              NO                                           
         MVC   SVLOADKY,KEY        SAVE THIS KEY                                
         GOTO1 GET                                                              
*                                                                               
         BRAS  RE,FILTUSE                                                       
         BE    LUSE04                                                           
         GOTO1 SEQ                                                              
         B     LUSE02                                                           
*                                                                               
*                                                                               
LUSE04   GOTO1 VTALUSEC,PARMS,(RC),AIO,0,(R6)                                   
*                                                                               
LUSE05   CLI   8(R1),0             TEST RECORD RETURNED                         
         BNE   LUSE10              NO - NEXT COMMERCIAL                         
*                                                                               
         GOTO1 VTAXCNVX,PARMS,(RC)      CONVERT TO SQL FORMAT                   
*                                                                               
         L     RE,OUTCOUNT                                                      
         LA    RE,1(RE)                                                         
         ST    RE,OUTCOUNT                                                      
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)  OUTPUT TO SQL BUFFER                    
                                                                                
* GET NEXT OUTPUT RECORD                                                        
                                                                                
         GOTO1 VTALUSEC,PARMS,(RC),AIO,1,(R6)                                   
         B     LUSE05                                                           
*                                                                               
LUSE10   CLC   OUTCOUNT,OUTMAX                                                  
         BNL   LUSE20                                                           
         MVC   KEY,SVLOADKY        RESTORE MY KEY                               
         GOTO1 HIGH                REREAD PREVIOUS RECORD                       
         GOTO1 SEQ                 GET NEXT RECORD                              
         B     LUSE02                                                           
*                                                                               
LUSE20   DS    0H                                                               
         J     YES                 AND EXIT                                     
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* SESSION DATA                                                                  
***********************************************************************         
*                                                                               
LOADSES  NTR1  BASE=*,LABEL=*                                                   
         MVI   FILETYPE,C'T'                                                    
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R2,KEY              SET KEY TO READ FIRST RECORD                 
         USING TLIND,R2                                                         
*                                                                               
         XC    TLINKEY,TLINKEY                                                  
         MVI   TLINCD,TLINCDQ      X'90'                                        
*                                                                               
         LA    R1,DXUSER                                                        
         USING DXUSERD,R1                                                       
         MVC   TLINAGY,THISAGY     MOVE AGENCY                                  
         DROP  R1                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
LSES02   CLC   KEY(20),KEYSAVE     TEST SAME TYPE/AGY                           
         BNE   LSES20              NO                                           
         MVC   SVLOADKY,KEY        SAVE THIS KEY                                
         BRAS  RE,GETREC                                                        
*                                                                               
         MVI   SESSTYPE,0          CLEAR SESSION/HOLDING FLAG                   
         BRAS  RE,FILTSES                                                       
         BE    LSES04                                                           
         GOTO1 SEQ                                                              
         B     LSES02                                                           
*                                                                               
LSES04   GOTO1 VTALSESC,PARMS,(RC),AIO,0,(R6)                                   
*                                                                               
LSES05   CLI   8(R1),0             TEST RECORD RETURNED                         
         BNE   LSES10              NO                                           
*                                                                               
         GOTO1 VTAXCNVX,PARMS,(RC)      CONVERT TO SQL FORMAT                   
*                                                                               
         L     RE,OUTCOUNT                                                      
         LA    RE,1(RE)                                                         
         ST    RE,OUTCOUNT                                                      
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)  OUTPUT TO SQL BUFFER                    
                                                                                
* GET NEXT OUTPUT RECORD                                                        
                                                                                
         GOTO1 VTALSESC,PARMS,(RC),AIO,1,(R6)                                   
         B     LSES05                                                           
*                                                                               
LSES10   CLC   OUTCOUNT,OUTMAX                                                  
         BNL   LSES20                                                           
*                                                                               
         MVI   FILETYPE,C'T'       BACK TO TALENT FILES                         
         MVC   AIO,AIO1                                                         
         MVC   KEY,SVLOADKY        RESTORE MY KEY                               
         GOTO1 HIGH                REREAD PREVIOUS RECORD                       
         GOTO1 SEQ                 GET NEXT RECORD                              
         B     LSES02                                                           
*                                                                               
LSES20   DS    0H                  OUTPUT REFERENCE DATA                        
         J     YES                                                              
         LTORG                                                                  
*                                                                               
***********************************************************************         
* FINAL CAST                                                                    
***********************************************************************         
*                                                                               
LOADFNL  NTR1  BASE=*,LABEL=*                                                   
         MVI   FILETYPE,C'T'                                                    
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R2,KEY              SET KEY TO READ FIRST RECORD                 
         USING TLCOD,R2                                                         
*                                                                               
         XC    TLCOKEY,TLCOKEY                                                  
         MVI   TLCOCD,TLCOCDQ      X'40'                                        
         MVI   TLCOVER,0                                                        
*                                                                               
         LA    R1,DXUSER                                                        
         USING DXUSERD,R1                                                       
         MVC   TLCOAGY,THISAGY     MOVE AGENCY                                  
         MVC   TLCOCLI,THISCLT     MOVE CLIENT                                  
         DROP  R1                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
LFNL02   CLC   KEY(14),KEYSAVE     TEST SAME TYPE/AGY/CLT                       
         BNE   LFNL20              NO                                           
         MVC   SVLOADKY,KEY        SAVE THIS KEY                                
         GOTO1 GET                                                              
*                                                                               
         BRAS  RE,FILTFNL                                                       
         BE    LFNL04                                                           
         GOTO1 SEQ                                                              
         B     LFNL02                                                           
*                                                                               
LFNL04   GOTO1 VTALFNLC,PARMS,(RC),AIO,0,(R6)                                   
*                                                                               
LFNL05   CLI   8(R1),0             TEST RECORD RETURNED                         
         BNE   LFNL10              NO                                           
*                                                                               
         GOTO1 VTAXCNVX,PARMS,(RC)      CONVERT TO SQL FORMAT                   
*                                                                               
         L     RE,OUTCOUNT                                                      
         LA    RE,1(RE)                                                         
         ST    RE,OUTCOUNT                                                      
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)  OUTPUT TO SQL BUFFER                    
                                                                                
* GET NEXT OUTPUT RECORD                                                        
                                                                                
         GOTO1 VTALFNLC,PARMS,(RC),AIO,1,(R6)                                   
         B     LFNL05                                                           
*                                                                               
LFNL10   CLC   OUTCOUNT,OUTMAX                                                  
         BNL   LFNL20                                                           
*                                                                               
         MVI   FILETYPE,C'T'       BACK TO TALENT FILES                         
         MVC   AIO,AIO1                                                         
         MVC   KEY,SVLOADKY        RESTORE MY KEY                               
         GOTO1 HIGH                REREAD PREVIOUS RECORD                       
         GOTO1 SEQ                 GET NEXT RECORD                              
         B     LFNL02                                                           
*                                                                               
LFNL20   J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* LOAD PERFORMER REF DATA                                                       
*==============================================================                 
                                                                                
LOADPRF  NTR1  BASE=*,LABEL=*                                                   
                                                                                
LPRF2    GOTO1 VTALPRFC,PARMS,(RC),AIO,0,(R6)                                   
         CLI   8(R1),0             TEST                                         
         BNE   LPRFX                                                            
*                                                                               
         GOTO1 VTAXCNVX,PARMS,(RC)      CONVERT TO SQL FORMAT                   
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)  OUTPUT TO SQL BUFFER                    
         B     LPRF2                                                            
*                                                                               
LPRFX    J     YES                                                              
         LTORG                                                                  
         SPACE 2                                                                
*==============================================================                 
* LOAD OVERSCALE REF DATA                                                       
*==============================================================                 
                                                                                
LOADOVR  NTR1  BASE=*,LABEL=*                                                   
                                                                                
LOVR2    GOTO1 VTALOVRC,PARMS,(RC),AIO,0,(R6)                                   
         CLI   8(R1),0             TEST                                         
         BNE   LOVRX                                                            
*                                                                               
         GOTO1 VTAXCNVX,PARMS,(RC)      CONVERT TO SQL FORMAT                   
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)  OUTPUT TO SQL BUFFER                    
         B     LOVR2                                                            
*                                                                               
LOVRX    J     YES                                                              
         LTORG                                                                  
         SPACE 2                                                                
*==============================================================                 
* LOAD PRODUCT REF DATA                                                         
*==============================================================                 
                                                                                
LOADPRD  NTR1  BASE=*,LABEL=*                                                   
                                                                                
LPRD2    GOTO1 VTALPRDC,PARMS,(RC),AIO,0,(R6)                                   
         CLI   8(R1),0             TEST                                         
         BNE   LPRDX                                                            
*                                                                               
         GOTO1 VTAXCNVX,PARMS,(RC)      CONVERT TO SQL FORMAT                   
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)  OUTPUT TO SQL BUFFER                    
         B     LPRD2                                                            
*                                                                               
LPRDX    J     YES                                                              
         LTORG                                                                  
         SPACE 2                                                                
*==============================================================                 
* LOAD ALL REFERENCE DATA BUT PERFORMER AND OVERSCALE                           
* NOTE THAT ROUTINE WILL OUTPUT RECORDS DIRECTLY                                
*==============================================================                 
                                                                                
LOADREF  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTO1 VTALREFC,PARMS,(RC),AIO,0,(R6)                                   
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE COMMERCIAL RECORD DATA                                                 
*---------------------------------------------------------------------*         
*                                                                               
UPDTCML  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING TLCOD,R2                                                         
*                                                                               
UPDT0002 DS    0H                                                               
         GOTO1 AFILTCML                                                         
         JNE   YES                                                              
         GOTO1 AUPDTCML,DMCB,VTALCMLC,0,0                                       
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*                                                                               
*---------------------------------------------------------------------*         
* FILTER CML RECORD AT R2 FOR PERFORMER SESSION EXTRACT               *         
*---------------------------------------------------------------------*         
*                                                                               
FILTCML  NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO                                                           
         USING TLCOD,R2                                                         
*                                                                               
         CLI   TLCOCD,TLCOCDQ       TEST RECORD TYPE                            
         JNE   NO                                                               
         CLI   TLCOVER,0                                                        
         JNE   NO                                                               
*                                                                               
         CLI   KEY,TLCOGCDQ        TEST BUILDING CLTGRP                         
         BE    FLTCML2                                                          
*                                                                               
         LA    R1,DXUSER                                                        
         USING DXUSERD,R1                                                       
         CLC   TLCOAGY,THISAGY      RIGHT AGENCY                                
         JNE   NO                                                               
         CLC   TLCOCLI,THISCLT      RIGHT CLIENT                                
         JNE   NO                                                               
         DROP  R1                                                               
*                                                                               
FLTCML2  MVI   ELCODE,TACOELQ                                                   
         LR    R6,R2                                                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TACOD,R6                                                         
         OC    TACOAIR,TACOAIR                                                  
         JZ    NO                                                               
*                                                                               
         CLI   TACOMED,C'T'        TEST TELEVISION                              
         BE    *+12                                                             
         CLI   TACOMED,C'C'        TEST CABLE                                   
         JNE   NO                                                               
*                                                                               
         CLI   TACOTYPE,0          TEST STANDARD TV                             
         BE    FLTCML4                                                          
         CLI   TACOTYPE,CTYSEAS    TEST SEASONAL                                
         BE    FLTCML4                                                          
         CLI   TACOTYPE,CTYSEAS2   OR SUPER SEASONAL                            
         BE    FLTCML4                                                          
         CLI   TACOTYPE,CTYSPAN    TEST SPANISH                                 
         BE    FLTCML4                                                          
         CLI   TACOTYPE,CTYASIAN   TEST ASIAN                                   
         BE    FLTCML4                                                          
         J     NO                                                               
*                                                                               
FLTCML4  CLI   TACOCTYP,0          TEST ACTRA TYPE                              
         BE    FLTCML6                                                          
         CLI   TACOCTYP,CCTY04A                                                 
         BE    FLTCML6                                                          
         J     NO                                                               
*                                                                               
FLTCML6  CLI   KEY,TLCOGCDQ        TEST BUILDING CLTGRP                         
         JE    YES                                                              
         BRAS  RE,SEEIFPD                                                       
         JE    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R6                                                            
                                                                                
*=====================================================================*         
* DUMMY FILTER ROUTINE FOR REFERENCE RECORDS                                    
*=====================================================================*         
                                                                                
FILTREF  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
FILTPRF  NTR1  BASE=*,LABEL=*                                                   
         J     YES                                                              
         EJECT                                                                  
*=====================================================================*         
* FILTER INVOICE RECORD AT 0(R2) FOR RESIDUALS                        *         
*=====================================================================*         
                                                                                
FILTRES  NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO                                                           
         USING TLIND,R2                                                         
*                                                                               
         CLI   TLINCD,TLINCDQ       TEST RECORD TYPE                            
         JNE   NO                                                               
*                                                                               
         LA    R1,DXUSER                                                        
         USING DXUSERD,R1                                                       
         CLC   TLINAGY,THISAGY      RIGHT AGENCY                                
         JNE   NO                                                               
*                                                                               
         MVI   ELCODE,TAINELQ                                                   
         LR    R6,R2                                                            
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
*                                                                               
         USING TAIND,R6                                                         
         CLC   TAINBDTE,FLTSDATE    BILL DATE IN EXTRACT PERIOD                 
         JL    NO                                                               
         CLC   TAINBDTE,FLTEDATE                                                
         JH    NO                                                               
         DROP  R6                                                               
*                                                                               
FLTRES2  MVI   ELCODE,TAPDELQ                                                   
         LR    R6,R2                                                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R6                                                         
*                                                                               
         LA    R1,DXUSER                                                        
         USING DXUSERD,R1                                                       
         CLC   TAPDCLI,THISCLT                                                  
         JNE   NO                                                               
         DROP  R1                                                               
*                                                                               
         L     R1,=A(USETAB)                                                    
         LHI   R0,(USETABX-USETAB)/L'USETAB                                     
*                                                                               
FLTRES4  CLC   TAPDUSE,0(R1)                                                    
         BE    FLTRES4X                                                         
         LA    R1,L'USETAB(R1)                                                  
         BCT   R0,FLTRES4                                                       
         J     NO                                                               
         DROP  R6                                                               
*                                                                               
FLTRES4X MVI   ELCODE,TACOELQ                                                   
         LR    R6,R2                                                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TACOD,R6                                                         
         CLI   TACOMED,TACOMEDT    TEST TV                                      
         BE    FLTRES6                                                          
         CLI   TACOMED,TACOMEDC    TEST CABLE                                   
         BE    FLTRES6                                                          
         J     NO                                                               
*                                                                               
FLTRES6  CLI   TACOTYPE,0           TEST STANDARD TV                            
         BNH   FLTRES8                                                          
         CLI   TACOTYPE,CTYSEAS     TEST SEASONAL                               
         BE    FLTRES8                                                          
         CLI   TACOTYPE,CTYSEAS2    TEST SEASONAL                               
         BE    FLTRES8                                                          
         CLI   TACOTYPE,CTYSPAN     TEST SPANISH                                
         BE    FLTRES8                                                          
         CLI   TACOTYPE,CTYASIAN                                                
         BE    FLTRES8                                                          
         J     NO                                                               
*                                                                               
FLTRES8  CLI   TACOCTYP,0         TEST ACTRA TYPE                               
         BE    FLTRES10                                                         
         CLI   TACOCTYP,CCTY04A                                                 
         BE    FLTRES10                                                         
         J     NO                                                               
*                                                                               
FLTRES10 BRAS  RE,CHKUNION         MAKE SURE UNION PERFORMER INCLUDED           
         JE    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R6                                                            
*                                                                               
USETAB   DS    0CL3                                                             
         DC    C'CBL'                                                           
         DC    C'CLA'                                                           
         DC    C'LNA'                                                           
         DC    C'LNC'                                                           
         DC    C'LNN'                                                           
         DC    C'PAX'                                                           
         DC    C'SCB'                                                           
         DC    C'SNT'                                                           
USETABX  EQU   *                                                                
         EJECT                                                                  
*=====================================================================*         
* FILTER INVOICE RECORD AT 0(R2) FOR SESSIONS                         *         
*=====================================================================*         
                                                                                
FILTSES  NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO                                                           
         USING TLIND,R2                                                         
*                                                                               
         CLI   TLINCD,TLINCDQ       TEST RECORD TYPE                            
         JNE   NO                                                               
*                                                                               
         LA    R1,DXUSER                                                        
         USING DXUSERD,R1                                                       
         CLC   TLINAGY,THISAGY      RIGHT AGENCY                                
         JNE   NO                                                               
*                                                                               
         MVI   ELCODE,TAINELQ                                                   
         LR    R6,R2                                                            
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
*                                                                               
         USING TAIND,R6                                                         
         CLC   TAINBDTE,FLTSDATE    BILL DATE IN EXTRACT PERIOD                 
         JL    NO                                                               
         CLC   TAINBDTE,FLTEDATE                                                
         JH    NO                                                               
         DROP  R6                                                               
*                                                                               
FLTSES2  MVI   ELCODE,TAPDELQ                                                   
         LR    R6,R2                                                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R6                                                         
*                                                                               
         LA    R1,DXUSER                                                        
         USING DXUSERD,R1                                                       
         CLC   TAPDCLI,THISCLT                                                  
         JNE   NO                                                               
         DROP  R1                                                               
*                                                                               
         OC    TAPDAPPL,TAPDAPPL   TEST ANY APPLIED CREDITS                     
         JZ    NO                  NO-SKIP RECORD                               
*                                                                               
         MVI   ELCODE,TACOELQ                                                   
         LR    R6,R2                                                            
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
*                                                                               
         USING TACOD,R6                                                         
         CLI   TACOMED,C'T'        TEST TV                                      
         BE    FLTSES4                                                          
         CLI   TACOMED,C'C'        TEST CABLE                                   
         BE    FLTSES4                                                          
         J     NO                                                               
*                                                                               
FLTSES4  CLI   TACOTYPE,0          TEST TYPE STANDARD TV                        
         BE    FLTSES6                                                          
         CLI   TACOTYPE,CTYSEAS    TEST SEASONAL                                
         BE    FLTSES6                                                          
         CLI   TACOTYPE,CTYSEAS2   TEST SEASONAL                                
         BE    FLTSES6                                                          
         CLI   TACOTYPE,CTYSPAN    TEST SPANISH                                 
         BE    FLTSES6                                                          
         CLI   TACOTYPE,CTYASIAN   TEST ASIAN                                   
         BE    FLTSES6                                                          
         J     NO                                                               
*                                                                               
FLTSES6  CLI   TACOCTYP,0          TEST ACTRA TYPE                              
         BE    FLTSES10                                                         
         CLI   TACOCTYP,CCTY04A                                                 
         BE    FLTSES10                                                         
         J     NO                                                               
*                                                                               
FLTSES10 J     YES                                                              
         EJECT                                                                  
*=====================================================================*         
* FILTER INVOICE RECORD AT 0(R2) FOR USE AUTHORIZATIONS               *         
*=====================================================================*         
                                                                                
FILTUSE  NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO                                                           
         USING TLIND,R2                                                         
*                                                                               
         CLI   TLINCD,TLINCDQ       TEST RECORD TYPE                            
         JNE   NO                                                               
*                                                                               
         LA    R1,DXUSER                                                        
         USING DXUSERD,R1                                                       
         CLC   TLINAGY,THISAGY      RIGHT AGENCY                                
         JNE   NO                                                               
*                                                                               
         MVI   ELCODE,TAINELQ                                                   
         LR    R6,R2                                                            
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
*                                                                               
         USING TAIND,R6                                                         
         CLC   TAINBDTE,FLTSDATE    BILL DATE IN EXTRACT PERIOD                 
         JL    NO                                                               
         CLC   TAINBDTE,FLTEDATE                                                
         JH    NO                                                               
         DROP  R6                                                               
*                                                                               
FLTUSE2  MVI   ELCODE,TAPDELQ                                                   
         LR    R6,R2                                                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R6                                                         
*                                                                               
         LA    R1,DXUSER                                                        
         USING DXUSERD,R1                                                       
         CLC   TAPDCLI,THISCLT                                                  
         JNE   NO                                                               
         DROP  R1                                                               
                                                                                
* FILTER ON USE TYPE                                                            
*                                                                               
FLTUSE10 L     R1,=A(USETAB)                                                    
         LHI   R0,(USETABX-USETAB)/L'USETAB                                     
*                                                                               
FLTUSE12 CLC   TAPDUSE,0(R1)                                                    
         BE    FLTUSE14                                                         
         LA    R1,L'USETAB(R1)                                                  
         BCT   R0,FLTUSE12                                                      
         J     NO                                                               
*                                                                               
FLTUSE14 MVI   ELCODE,TACOELQ                                                   
         LR    R6,R2                                                            
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
*                                                                               
         USING TACOD,R6                                                         
         CLI   TACOMED,C'T'        TEST TV                                      
         BE    FLTUSE16                                                         
         CLI   TACOMED,C'C'        TEST CABLE                                   
         BE    FLTUSE16                                                         
         J     NO                                                               
*                                                                               
FLTUSE16 CLI   TACOTYPE,0          TEST TYPE STANDARD TV                        
         BE    FLTUSE18                                                         
         CLI   TACOTYPE,CTYSEAS    TEST SEASONAL                                
         BE    FLTUSE18                                                         
         CLI   TACOTYPE,CTYSEAS2   TEST SEASONAL                                
         BE    FLTUSE18                                                         
         CLI   TACOTYPE,CTYSPAN    TEST SPANISH                                 
         BE    FLTUSE18                                                         
         CLI   TACOTYPE,CTYASIAN   TEST ASIAN                                   
         BE    FLTUSE18                                                         
         J     NO                                                               
*                                                                               
FLTUSE18 CLI   TACOCTYP,0          TEST ACTRA TYPE                              
         BE    FLTUSE20                                                         
         CLI   TACOCTYP,CCTY04A                                                 
         BE    FLTUSE20                                                         
         J     NO                                                               
*                                                                               
FLTUSE20 BRAS  RE,CHKUNION         MAKE SURE UNION PERFORMER INCLUDED           
         JE    YES                                                              
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* FILTER INVOICE RECORD AT 0(R2) FOR FINAL CAST COMPLETION            *         
*=====================================================================*         
                                                                                
FILTFNL  NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO                                                           
         USING TLCOD,R2                                                         
*--->                                                                           
**NOP    CLC   TLCOCID(8),=C'AHAU3395'                                          
**NOP    JNE   NO                                                               
*--->                                                                           
         CLI   TLCOCD,TLCOCDQ       TEST RECORD TYPE                            
         JNE   NO                                                               
         CLI   TLCOVER,0                                                        
         JNE   NO                                                               
*                                                                               
         LA    R1,DXUSER                                                        
         USING DXUSERD,R1                                                       
         CLC   TLCOAGY,THISAGY      RIGHT AGENCY                                
         JNE   NO                                                               
         CLC   TLCOCLI,THISCLT      RIGHT CLIENT                                
         JNE   NO                                                               
*                                                                               
         MVI   ELCODE,TACOELQ                                                   
         LR    R6,R2                                                            
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
*                                                                               
         OC    TACOAIR,TACOAIR                                                  
         JZ    NO                                                               
*                                                                               
         USING TACOD,R6                                                         
         CLI   TACOMED,C'T'        TEST TV                                      
         BE    FLTFNL2                                                          
         CLI   TACOMED,C'C'        TEST CABLE                                   
         BE    FLTFNL2                                                          
         J     NO                                                               
*                                                                               
FLTFNL2  CLI   TACOTYPE,0          TEST TYPE STANDARD TV                        
         BE    FLTFNL4                                                          
         CLI   TACOTYPE,CTYSEAS    TEST SEASONAL                                
         BE    FLTFNL4                                                          
         CLI   TACOTYPE,CTYSEAS2   TEST SEASONAL                                
         BE    FLTFNL4                                                          
         CLI   TACOTYPE,CTYSPAN    TEST SPANISH                                 
         BE    FLTFNL4                                                          
         CLI   TACOTYPE,CTYASIAN   TEST ASIAN                                   
         BE    FLTFNL4                                                          
         J     NO                                                               
*                                                                               
FLTFNL4  CLI   TACOCTYP,0          TEST ACTRA TYPE                              
         BE    FLTFNL6                                                          
         CLI   TACOCTYP,CCTY04A                                                 
         BE    FLTFNL6                                                          
         J     NO                                                               
*                                                                               
FLTFNL6  BRAS  RE,SEEIFPD                                                       
         JE    YES                                                              
         J     NO                                                               
         DROP  R6                                                               
         EJECT                                                                  
*============================================================                   
* FOR CML (PERF SESSION) AND FINAL CAST                                         
* READ THROUGH INVOICES TO SEE IF PAYMENT IN REQUEST PERIOD                     
*============================================================                   
                                                                                
SEEIFPD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVLOADKY,KEY        SAVE CURRENT KEY                             
         MVC   AIO,AIO2            USE I/O AREA 2                               
                                                                                
K        USING TLINPD,KEY                                                       
                                                                                
         XC    KEY,KEY                                                          
         MVI   K.TLINPCD,TLINHCDQ    X'91'                                      
         MVC   K.TLINHCOM,TLCOCOM-TLCOD+SVLOADKY                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         DROP  K                                                                
*                                                                               
SEEIFPD2 CLC   KEY(20),KEYSAVE      SAME CMML                                   
         BNE   SEEIFPDN                                                         
*                                                                               
         GOTO1  GET                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   SEEIFPD6                                                         
*                                                                               
         USING TAIND,R6                                                         
         CLC   TAINBDTE,FLTSDATE                                                
         BL    SEEIFPD6                                                         
         CLC   TAINBDTE,FLTEDATE                                                
         BH    SEEIFPD6                                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   SEEIFPD6                                                         
*                                                                               
         USING TAPDD,R6                                                         
         L     R1,=A(USETAB)                                                    
         LA    R0,(USETABX-USETAB)/L'USETAB                                     
*                                                                               
SEEIFPD4 CLC   TAPDUSE,0(R1)                                                    
         BE    SEEIFPDY                                                         
         LA    R1,L'USETAB(R1)                                                  
         BCT   R0,SEEIFPD4         IF FALL THROUGH, GET NEXT INV                
*                                                                               
SEEIFPD6 GOTO1 SEQ                                                              
         B     SEEIFPD2                                                         
*                                                                               
SEEIFPDN MVI   ELCODE,1            PREPARE TO SET CC NEQ                        
         B     SEEIFPDX                                                         
*                                                                               
SEEIFPDY MVI   ELCODE,0            PREPARE TO SET CC EQ                         
*                                                                               
SEEIFPDX MVC   AIO,AIO1            RESTORE ORIGINAL IO ADDRESS                  
         MVC   KEY,SVLOADKY        RESTORE KEY                                  
         GOTO1 HIGH                                                             
         CLI   ELCODE,0            SET CC FOR EXIT                              
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* FOR USE AUTH AND RESIDUALS, READ THROUGH CHECKS FOR THIS                      
* INVOICE TO MAKE SURE AT LEAST ONE CHECK TO A UNION CAST MEMBER                
*===============================================================                
                                                                                
CHKUNION NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVLOADKY,KEY        SAVE CURRENT KEY                             
         MVC   AIO,AIO2            USE I/O AREA 2                               
         MVI   FILETYPE,C'C'       INDICATE READING CHKDIR/CHKFILE              
                                                                                
* READ CHECK RECORDS FOR THIS INVOICE                                           
                                                                                
K        USING TLCKD,KEY                                                        
         XC    KEY,KEY                                                          
         MVI   K.TLCKCD,TLCKCDQ                                                 
*                                                                               
         L     R6,AIO1                                                          
         USING TLIND,R6                                                         
         MVC   K.TLCKAGY,TLINAGY                                                
         MVC   K.TLCKINV,TLININV                                                
         XC    K.TLCKINV,=6X'FF'   UNCOMPLEMENT                                 
*                                                                               
         GOTO1 HIGH                                                             
         B     CHKUN4                                                           
*                                                                               
CHKUN2   GOTO1 SEQ                                                              
*                                                                               
CHKUN4   CLC   KEY(13),KEYSAVE     SAME AGENCY/INVOICE                          
         JNE   CHKUNNO             NO                                           
*                                                                               
         CLC   K.TLCKSORT+4(2),=X'FFFF' IF NOT A REAL SEQNUM                    
         BE    CHKUN2                   JUST SKIP IT                            
         DROP  K                                                                
*                                                                               
         GOTO1 GET                 GET THE CHECK RECORD                         
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,TACDELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   CHKUN2                                                           
         USING TACDD,R6                                                         
         TM    TACDSTAT,TACDSTRS   SKIP TRUSTEE CHECKS                          
         BO    CHKUN2                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R6                                                         
*                                                                               
         CLC   TACAUN,=C'SAG'                                                   
         BE    CHKUNYES                                                         
         CLC   TACAUN,=C'AFT'                                                   
         BE    CHKUNYES                                                         
         B     CHKUN2                                                           
         DROP  R6                                                               
*                                                                               
CHKUNNO  MVI   ELCODE,1            PREPARE TO SET CC NEQ                        
         B     CHKUNX                                                           
*                                                                               
CHKUNYES MVI   ELCODE,0            PREPARE TO SET CC EQ                         
*                                                                               
CHKUNX   MVC   AIO,AIO1            RESTORE ORIGINAL IO ADDRESS                  
         MVC   KEY,SVLOADKY        RESTORE KEY                                  
         MVI   FILETYPE,C'T'       BACK TO TALDIR/TALFIL                        
         GOTO1 HIGH                                                             
         CLI   ELCODE,0            SET CC FOR EXIT                              
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE RESIDUAL DATA                                                          
*---------------------------------------------------------------------*         
*                                                                               
UPDTRES  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING TLCOD,R2                                                         
*                                                                               
         GOTO1 AFILTRES                                                         
         JNE   YES                                                              
         GOTO1 AUPDTRES,DMCB,VTALRESC,0,0                                       
                                                                                
         J     YES                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE SESSION DATA                                                           
*---------------------------------------------------------------------*         
*                                                                               
UPDTSES  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING TLCOD,R2                                                         
*                                                                               
         GOTO1 AFILTSES                                                         
         JNE   YES                                                              
         GOTO1 AUPDTSES,DMCB,VTALRESC,0,0                                       
                                                                                
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* R2 = A(RECORD BUFFER)                                               *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(3 CHAR MNEMONIC)                                             *         
***********************************************************************         
*                                                                               
ACCUPDT  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)                                                      
*&&DO                                                                           
UPDL01   DS    0H                                                               
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),(R8)   EXTRACT                     
         JNE   YES                                                              
         CLI   DMCB+8,FF           DATA NOT COMPLETE - DO NOT WRITE             
         JE    YES                                                              
         TM    DMCB+8,X'80'                                                     
         JO    NO                                                               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
*                                                                               
         LAY   R0,COPYBUFF         R0=A(EXTRACT RECORD AREA FOR COPY)           
         LHI   R1,L'COPYBUFF                                                    
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         LAY   R0,COPYBUFF                                                      
         GOTO1 (R3),DMCB,(R0),(R2),0,(R6),(R8)    BUILD COPY REC                
*                                                                               
         L     R0,DXAXREC          R0=A(CHANGE SQL RECORD)                      
         LAY   RE,COPYBUFF         RE=A(COPY SQL RECORD)                        
         LH    RF,0(RE)            RF=L'RECORD                                  
         LHI   R1,SPTMDAGY-SPTMDD  DISP TO TALALPHA                             
         AR    R0,R1               BUMP TO TALALPHA CODE ALL RECORDS            
         AR    RE,R1                                                            
         SR    RF,R1               REDUCE LENGTH APPROPRIATELY                  
*                                  DON'T LOOK AT TRAILING BYTES                 
         LR    R1,RF                                                            
         CLCL  R0,RE               COPY, CHANGE RECORDS IDENTICAL?              
         JE    YES                 YES - SKIP THEM                              
*                                                                               
UPDL02   DS    0H                                                               
         GOTO1 VTAXCNVX,DMCB,(RC)                                               
*                                                                               
         L     RF,DXASQLB                                                       
*                                                                               
         CLI   (SPTESACT-SPTESD)(RF),C'C'                                       
         BNE   UPDL04                                                           
*                                                                               
         MVI   (SPTESACT-SPTESD)(RF),C'D'                                       
         B     UPDL04                                                           
*                                                                               
UPDL03   DS    0H                                                               
         L     RF,DXASQLB                                                       
         MVI   (SPTESACT-SPTESD)(RF),C'A'                                       
*                                                                               
UPDL04   GOTO1 DXPUT,DMCB,(RF),(R7),(R8)                                        
         BRAS  RE,DECIOC                                                        
         JNE   NO                                                               
*                                                                               
         L     RF,DXAXREC                                                       
*&&                               CONVERT RECORD TO SQL BUFFER                  
         GOTO1 VTAXCNVX,DMCB,(RC)                                               
         BE    *+6                                                              
         DC    H'0'                DEAL WITH ERROR STATE HERE??                 
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         BRAS  RE,DECIOC                                                        
         JNE   NO                  TOO MANY IOS                                 
         J     YES                                                              
         LTORG                                                                  
**NOP    DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXOPENQ - OPEN TALENT SYSTEM FILES                         *         
***********************************************************************         
                                                                                
PROCOPEN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         L     RF,VDATAMGR                                                      
         GOTO1 (RF),DMCB,=C'DMOPEN',=CL8'TALENT',TALFILES,AIO                   
*                                                                               
         OC    VT00A88,VT00A88     ALREADY LOADED?                              
         JZ    POPEN10                                                          
         OC    VTGAUSES,VTGAUSES                                                
         JNZ   YES                                                              
*                                                                               
POPEN10  GOTO1 =V(LOADER),DMCB,=CL8'T00A88',0                                   
         ICM   RE,15,4(R1)                 GET PHASE ADDRESS                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,VT00A88                                                       
         ICM   R0,15,TGAUSES-TGTABLED(RE)  USES TABLE                           
         AR    RE,R0                       ADD TO PHASE ADDRESS                 
         ST    RE,VTGAUSES                 AND SAVE IT FOREVER                  
         J     YES                                                              
*                                                                               
MAXIOSW  DC    AL4(0)                                                           
*                                                                               
TALFILES DC    CL8'NTALDIR'                                                     
         DC    CL8'NTALFIL'                                                     
         DC    CL8'NCHKDIR'                                                     
         DC    CL8'NCHKFIL'                                                     
         DC    CL10'X       '                                                   
*                                                                               
*                                                                               
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE SYSTEM FILES                              *         
***********************************************************************         
                                                                                
PROCCLOS NTR1  BASE=*,LABEL=*                                                   
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         L     RF,VDATAMGR                                                      
         GOTO1 (RF),DMCB,=C'DMCLSE',=CL8'TALENT',0,AIO1                         
         CLI   8(R1),0                                                          
         JE    YES                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
         LTORG                                                                  
*                                                                               
GENINITX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* AREA USED FOR DETERMINING WHETHER CHANGES NEED TO BE EXTRACTED                
***********************************************************************         
COPYBUFF DS    CL10000                                                          
*                                                                               
***********************************************************************         
* SSB                                                                           
***********************************************************************         
*                                                                               
         DS    0D                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      DS    0D                                                               
         DC    H'0'                                                             
         DC    X'FF'               OFFLINE EXTENDED                             
         DC    XL256'00'                                                        
                                                                                
***********************************************************************         
* LOCAL COMFACS - SHOULD BE CHANGED TO USE COMFACS FROM DXTRACT                 
***********************************************************************         
                                                                                
COMFACS  DS    0D                  COMMON FACILITIES LIST                       
         DC    V(DATAMGR)                                                       
         DC    A(0)                CALLOFF                                      
         DC    A(0)                GETMSG)                                      
         DC    A(0)                GETTXT)                                      
         DC    A(0)                SWITCH)                                      
         DC    A(0)                HELLO)                                       
         DC    A(0)                SCANNER)                                     
         DC    A(0)                UNSCAN)                                      
         DC    A(0)                HEXIN)                                       
         DC    V(HEXOUT)                                                        
         DC    A(0)                CASHVAL)                                     
         DC    A(0)                DATVAL)                                      
         DC    V(DATCON)                                                        
         DC    A(0)                TERMVAL)                                     
         DC    A(0)                SCUNKEY)                                     
         DC    A(0)                ADDAY                                        
         DC    A(0)                GETDAY                                       
         DC    A(0)                GETPROF)                                     
         DC    A(0)                PERVERT                                      
         DC    A(0)                GETFACT)                                     
         DC    A(0)                XSORT)                                       
         DC    A(0)                REQTWA)                                      
         DC    A(0)                GETFLD)                                      
         DC    A(0)                DDISPSRT)                                    
         DC    A(0)                DEMADDR                                      
         DC    A(0)                DEMDISP)                                     
         DC    A(0)                DBOOK)                                       
         DC    A(0)                DSTATION)                                    
         DC    A(0)                DMASTER)                                     
         DC    A(0)                DFORMULA)                                    
         DC    A(0)                DNAME)                                       
         DC    A(0)                DCODE)                                       
         DC    A(0)                DCONTROL)                                    
         DC    A(0)                DADJUST)                                     
         DC    A(0)                DEMOUT)                                      
         DC    A(0)                DEMEL)                                       
         DC    A(0)                DEMAINT)                                     
         DC    A(0)                DEMAND)                                      
         DC    A(0)                DEMOMATH)                                    
         DC    A(0)                DEMOVAL)                                     
         DC    A(0)                GENERAL)                                     
         DC    A(0)                PERVAL                                       
         DC    A(0)                DLFLD)                                       
         DC    A(0)                                                             
         DC    A(0)                GLOBBER)                                     
         DC    A(0)                MINIO)                                       
         DC    A(0)                PARSNIP)                                     
         DC    A(0)                DICTATE)                                     
         DC    A(0)                EDITOR)                                      
         DC    A(0)                GETHELP)                                     
         DC    A(0)                CUREDIT)                                     
         DC    A(0)                GETRET)                                      
         DC    A(0)                REPORT)                                      
         DC    A(0)                BLDCUR)                                      
         DC    A(0)                GETCUR)                                      
         DC    A(0)                GETNARR)                                     
         DC    A(0)                DEJAVU)                                      
         DC    A(0)                SECRET)                                      
         DC    A(0)                BILLIT)                                      
         DC    A(0)                                                             
         DC    A(0)                PQPROF)                                      
         DC    2A(0)                                                            
         DC    A(0)                BINSRCH)                                     
         DC    V(PROTON)           PROTON)                                      
         DC    V(PROTOFF)          PROTOFF)                                     
         DC    A(0)                HELEN)                                       
         DC    A(0)                MQIO)                                        
         DC    A(0)                EUREKA                                       
         DC    V(LOCKUP)                                                        
         DC    V(MASTC)            MASTC                                        
         DC    V(LOCKSPC)          LOCKSPACE                                    
         DC    24A(0)              SPARE                                        
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*CGRTAB*'                                                    
CGRTAB   DS    0XL18                                                            
         DS    1000XL18                                                         
CGRTABX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
CGRPARMS DS    0XL24                                                            
CGRPARM1 DC    A(0)                                                             
CGRPARM2 DC    A(CGRTAB)                                                        
CGRPARM3 DC    F'0'                                                             
CGRPARM4 DC    AL4(L'CGRTAB)                                                    
CGRPARM5 DC    AL1(0),AL3(L'CGRTAB)                                             
CGRPARM6 DC    A((CGRTABX-CGRTAB)/L'CGRTAB)                                     
         EJECT                                                                  
       ++INCLUDE TAXWORKD                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TYPETAB TABLE                                        *         
***********************************************************************         
*                                                                               
TYPETABD DSECT                                                                  
TYPNAME  DS    XL3                 3 CHAR MNEMONIC FOR RECORD TYPE              
TYPLDEEP DS    XL1                 DEPTH INTO LEDGER FOR COMPARE (LOAD)         
TYPFLAG  DS    XL1                 FLAGS                                        
         DS    XL3                 N/D                                          
TYPLOAD  DS    XL4                 A(LOAD ROUTINE)                              
TYPUPDT  DS    XL4                 A(UPDATE ROUTINE)                            
TYPETABLQ EQU  *-TYPETABD                                                       
*                                                                               
*                                                                               
***********************************************************************         
* DSECT TO COVER RECOVERY HEADER                                      *         
***********************************************************************         
*                                                                               
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
***********************************************************************         
* OTHER DSECTS REQUIRED FOR THIS PROGRAM                              *         
***********************************************************************         
*                                                                               
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
* DXDSECTS                                                                      
       ++INCLUDE DXDSECTS                                                       
*                                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
* FACTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
*                                                                               
       ++INCLUDE FASSBOFF                                                       
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
       ++INCLUDE DDDPRINT                                                       
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032TAXTRACT  07/31/14'                                      
         END                                                                    
