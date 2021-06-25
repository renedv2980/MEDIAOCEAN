*          DATA SET NTXTRACT   AT LEVEL 081 AS OF 03/02/21                      
*PHASE NXTRACTB                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE CLUNPK                                                                 
*INCLUDE CLPACK                                                                 
*INCLUDE PPBYOUT                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE PUBEDIT                                                                
*INCLUDE PERVAL                                                                 
*INCLUDE PERVERT                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE NTXROUTS                 XTRACT RECORD CREATION MODULE                 
*INCLUDE NTXCNVX                  CONVERSION ROUTINES FOR ALL ABOVE             
*INCLUDE NETIO                                                                  
*INCLUDE NETVALUE                                                               
*INCLUDE FASWITCH                                                               
*INCLUDE BINSRCH2                                                               
*INCLUDE FAGETTXT                                                               
*INCLUDE NETWEEK                                                                
*INCLUDE CALLOFF                                                                
*INCLUDE CORELIST                                                               
*INCLUDE MASTER                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE SPBVAL                                                                 
*INCLUDE GETPROF                                                                
*INCLUDE NETBLRDR                                                               
*INCLUDE NETNET                                                                 
*INCLUDE SPFMTINO                                                               
*INCLUDE RECUP                                                                  
* INCLUDE LOADER   - RE-LINK CAUSED ERRORS IN NEW BINDCHK                       
* INCLUDE LOCKSPC  - RE-LINK CAUSED ERRORS IN NEW BINDCHK                       
         TITLE 'NTXTRACT - EXTRACT NET SYSTEM FILE SQL DATA'                    
***********************************************************************         
*  NET  SQL SUB SYSTEM EXTRACT CONTROL MODULE                        *          
*                                                                     *         
* CONTROL IS PASSED FROM DXTRACT WITH PARAMETERS:                     *         
* R1=A(EXTRACT CONTROL DATA BLOCK - SEE DSECT DXBLOCKD)               *         
*                                                                     *         
* MODULE ENTERED WITH ONE OF FOLLOWING MODES IN DXMODE:               *         
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
*                                                                     *         
* DXUSER = 32 BYTE INPUT CARD FROM USERPARM, SEE RXUSERD              *         
***********************************************************************         
NTTRACT  CSECT                                                                  
         ENTRY COMFACS                                                          
         ENTRY MASTC                                                            
         ENTRY SSB                                                              
         ENTRY NTXCOMM                                                          
         PRINT NOGEN                                                            
         NMOD1 WORKL,*NTXTR**,R9                                                
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         LA    RA,ADDRESS                                                       
         USING ADDRESSD,RA                                                      
         LA    RF,TYPTAB                                                        
         ST    RF,ATYPTAB                                                       
         LA    RF,MEDTAB                                                        
         ST    RF,AMEDTAB                                                       
         L     R7,0(R1)            R7=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R7                                                      
DXU      USING RXUSERD,DXUSER                                                   
*                                                                               
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),DXDSPAC                           
         L     RF,=V(DDSIO)                                                     
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         MVC   0(8,RF),DXDDSIO                                                  
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
HIGH     CHI   RC,0                                                             
         J     EXIT                                                             
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
EXIT     XIT1  ,                                                                
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
MOPEN    CLI   DXMODE,DXOPENQ                                                   
         BNE   MCLOSE                                                           
         BAS   RE,PROCOPEN         OPEN SYSTEM FILES                            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MCLOSE   CLI   DXMODE,DXCLOSEQ                                                  
         BNE   MLOAD                                                            
*                                                                               
**********************************************************************          
* SPEC-40720                                                                    
*                                                                               
* AFTER ADDING UNIT ACTUAL DEMOS TO THE UNIT SUPPORTING TABLES FOR              
* UPDATE MODE, IT RESULTED IN A DUPLICATE KEY ERROR WITH THE UND                
* NIGHTLY LOAD IN CERTAIN SITUATIONS.  WE UPDATED THE UND NIGHTLY LOAD          
* TO USE SUBSYS=SQL TO ENSURE THE DATASETS PROCESS IN THE CORRECT               
* ORDER.  WE NEED TO TREAT THE UND LIKE AN UPDATE EVEN THOUGH ITS               
* NOT READING THE RECOVERY FILE SO IT IS TREATED SPECIAL AND WILL               
* RUN IN CLOSE MODE SINCE WE DON'T SET UPDATE MODE B/C THERE IS NO              
* RECOVERY FILE TO PROCESS. - SCHT                                              
**********************************************************************          
         CLC   SXDTTYP,=C'UND'     UNIT NIGHTLY DEMOS                           
         JNE   MCLOSEX                                                          
         CLI   DXPONLY,C'Y'        POSTONLY=Y?                                  
         JE    MCLOSEX             SKIP UNIT UPDATE                             
         BAS   RE,INITTAB          INITIALIZE TABLES                            
         BRAS  RE,UPDTUND                                                       
MCLOSEX  DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
         BAS   RE,PROCCLOS         CLOSE SYSTEM FILES                           
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MLOAD    CLI   DXMODE,DXLOADQ                                                   
         BNE   MUPDT                                                            
         BAS   RE,INITTAB          INITIALIZE TABLES                            
         BAS   RE,PROCLOAD         EXTRACT FROM FILES IN LOAD MODE              
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
                                                                                
MUPDT    CLI   DXMODE,DXUPDTQ                                                   
         BNE   MERR                                                             
         BAS   RE,INITTAB          INITIALIZE TABLES                            
         BAS   RE,PROCUPDT         EXTRACT FROM FILES IN UPDATE MODE            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,0                                                        
         B     MXIT                                                             
*                                                                               
MXIT     XMOD1 1                                                                
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
GENINIT  NTR1                                                                   
         MVI   FLAGS,X'0'                                                       
         MVI   FLAGS2,X'0'                                                      
         MVI   ELCOUNT,X'0'                                                     
*                                                                               
         L     R4,=A(VDEMADDR)                                                  
         OC    0(4,R4),0(R4)       IS DEMADDR ALREADY LOADED?                   
         BNZ   GENI20              YES: V(DEMADDR) ALREADY IN COMFACS           
         MVC   DUB,=CL8'T00ADE'    <-- CHANGE THIS TO LOAD TEST DEMADDR         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENI20   L     R4,=A(VDEMAND)                                                   
         OC    0(4,R4),0(R4)       IS DEMAND ALREADY LOADED?                    
         BNZ   GENI30              YES: V(DEMADDR) ALREADY IN COMFACS           
         MVC   DUB,=CL8'T00ADD'    <-- CHANGE THIS TO LOAD TEST DEMADDR         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENI30   L     R4,=A(VDEMDISP)                                                  
         OC    0(4,R4),0(R4)       IS ALREADY LOADED?                           
         BNZ   GENI40              YES: V(DEMADDR) ALREADY IN COMFACS           
         MVC   DUB,=CL8'T00AD0'    <-- CHANGE THIS TO LOAD TEST DEMADDR         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENI40   L     R4,=A(VDEMOUT)                                                   
         OC    0(4,R4),0(R4)       IS ALREADY LOADED?                           
         BNZ   GENI50              YES: V(DEMADDR) ALREADY IN COMFACS           
         MVC   DUB,=CL8'T00ADF'    <-- CHANGE THIS TO LOAD TEST DEMADDR         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENI50   L     R4,=A(VDEMEL)                                                    
         OC    0(4,R4),0(R4)       IS ALREADY LOADED?                           
         BNZ   GENI60              YES: V(DEMADDR) ALREADY IN COMFACS           
         MVC   DUB,=CL8'T00ADB'    <-- CHANGE THIS TO LOAD TEST DEMADDR         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENI60   L     R4,=A(VDEMAINT)                                                  
         OC    0(4,R4),0(R4)       IS ALREADY LOADED?                           
         BNZ   GENI70              YES: V(DEMADDR) ALREADY IN COMFACS           
         MVC   DUB,=CL8'T00ADC'    <-- CHANGE THIS TO LOAD TEST DEMADDR         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENI70   L     R4,=A(VDEMMATH)                                                  
         OC    0(4,R4),0(R4)       IS ALREADY LOADED?                           
         BNZ   GENI80              YES: V(DEMADDR) ALREADY IN COMFACS           
         MVC   DUB,=CL8'T00ADA'    <-- CHANGE THIS TO LOAD TEST DEMADDR         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENI80   L     R4,=A(VDEMOVAL)                                                  
         OC    0(4,R4),0(R4)       IS ALREADY LOADED?                           
         BNZ   GENI90              YES: V(DEMADDR) ALREADY IN COMFACS           
         MVC   DUB,=CL8'T00AD9'    <-- CHANGE THIS TO LOAD TEST DEMADDR         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENI90   LA    R4,VDEMCON                                                       
         OC    0(4,R4),0(R4)       IS ALREADY LOADED?                           
         BNZ   GENI100             YES: V(DEMADDR) ALREADY IN COMFACS           
         MVC   DUB,=CL8'T00AE0'    <-- CHANGE THIS TO LOAD TEST DEMADDR         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENI100  L     R4,=A(VDEMTABS)                                                  
         OC    0(4,R4),0(R4)       IS ALREADY LOADED?                           
         BNZ   GENI110             YES: V(DEMTABS) ALREADY IN COMFACS           
         MVC   DUB,=CL8'T00AD1'    <-- CHANGE THIS TO LOAD TEST DEMTABS         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENI110  DS    0H                                                               
         LARL  R4,NTXCOMM                                                       
         USING NTXCOMMD,R4                                                      
*                                                                               
         OC    ATRPACK,ATRPACK                                                  
         BNZ   GENI120                                                          
         MVC   DUB,=CL8'T00AFE'    TRPACK                                       
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   ATRPACK,DMCB+4                                                   
         DROP  R4                                                               
*                                                                               
GENI120  L     R4,=A(VDAYUNPK)                                                  
         OC    0(4,R4),0(R4)       IS ALREADY LOADED?                           
         BNZ   GENI130                                                          
         MVC   DUB,=CL8'T00A0F'    <-- CHANGE THIS TO LOAD TEST DAYUNPK         
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENI130  L     R4,=A(VUNTIME)                                                   
         OC    0(4,R4),0(R4)       IS ALREADY LOADED?                           
         BNZ   GENINITX                                                         
         MVC   DUB,=CL8'T00A11'                                                 
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         MVC   0(4,R4),DMCB+4                                                   
*                                                                               
GENINITX B     EXIT                                                             
***********************************************************************         
* INITIALIZE TABLES                                                   *         
***********************************************************************         
INITTAB  NTR1                                                                   
         XC    PARM,PARM                                                        
         MVC   PARM+4(4),=X'D9000A38'  <--LOAD LIVE VERSION                     
         GOTO1 =V(CALLOFF),PARM                                                 
         CLI   PARM+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'00'                                                            
         MVC   VOFFICER,PARM                                                    
*                                                                               
         L     R4,=A(VDPTBL)                                                    
         OC    0(4,R4),0(R4)                                                    
         BNZ   INITTABX                                                         
*                                                                               
         LA    R5,5                GETMAIN AREA FOR SORTING ELEMENT             
         L     RE,=F'20000'                                                     
         MR    R4,RE                                                            
         LR    R0,R5                                                            
         GETMAIN  R,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,=A(VDPTBL)                                                    
         ST    R1,0(R4)                                                         
         L     RF,=F'6000'                                                      
         AR    R1,RF                                                            
         L     R4,=A(VCLTREC)      A(CLT REC)                                   
         ST    R1,0(R4)                                                         
         L     RF,=F'2000'                                                      
         AR    R1,RF                                                            
         L     R4,=A(VESTREC)      A(EST REC)                                   
         ST    R1,0(R4)                                                         
         L     RF,=F'2000'                                                      
         AR    R1,RF                                                            
         L     R4,=A(VSTATAB)      A(STATION MASTER TABLE)                      
         ST    R1,0(R4)                                                         
*                                                                               
         CLC   SXDTTYP,=C'ECT'     ASSIGNED COST - THEN SKIP                    
         BE    INITTABX                                                         
         CLC   SXDTTYP,=C'GXL'     GOAL (XSPOT) - THEN SKIP                     
         BE    INITTABX                                                         
*                                                                               
         BRAS  RE,BLDDAYP          BUILD TABLE OF 2CHAR DAYPARTS                
*                                                                               
INITTABX B     EXIT                                                             
***********************************************************************         
* DXMODE = DXOPENQ - OPEN ACCOUNT SYSTEM FILES                        *         
***********************************************************************         
PROCOPEN NTR1  ,                   SET UTL SENUM                                
         L     RE,VUTL                                                          
         MVI   4(RE),X'0A'         CONTROL                                      
         GOTO1 VDATAMGR,DMCB,DMOPEN,CNTRL,CFLIST,IO                             
*                                                                               
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
         GOTO1 VDATAMGR,DMCB,DMOPEN,SPOT,FILELIST,IO                            
         J     YES                                                              
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE ACCOUNT SYSTEM FILES                      *         
***********************************************************************         
PROCCLOS NTR1  ,                                                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,PRINT,0,IO                                  
         CLI   8(R1),0                                                          
         JE    YES                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
*                                                                               
PRINT    DC    CL8'PRINT'                                                       
SPOT     DC    CL8'SPOT'                                                        
CNTRL    DC    CL8'CONTROL'                                                     
*                                                                               
CFLIST   DC    CL8'NCTFILE'                                                     
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
         DC    CL10'X       '                                                   
*                                                                               
FILELIST DC    CL8'NSPTFIL'                                                     
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NXSPFIL'                                                     
         DC    CL8'NXSPDIR'                                                     
         DC    CL8'NSTAFIL'                                                     
         DC    CL8'NUNTDIR'                                                     
         DC    CL8'NUNTFIL'                                                     
         DC    CL8'NNTIDIR'                                                     
         DC    CL8'NL=NTIFL'                                                    
         DC    CL10'X       '                                                   
*                                                                               
VUTL     DC    V(UTL)                                                           
***********************************************************************         
* PROCESS ACCOUNT FILE DATA IN LOAD MODE                              *         
***********************************************************************         
PROCLOAD NTR1  ,                                                                
         MVC   PRIALPHA,SXDTAGY    SET PRIALPHA CODE FROM SYSTEM TABLE          
         MVC   TYPECODE,SXDTTYP                                                 
         CLC   TYPECODE,=C'SUP'    TEST FOR SUPPORT MACRO TYPE                  
         BE    PROCL10             YES                                          
*                                                                               
         GOTO1 AGETTYP             SET UP RECORD TYPE TABLE DATA                
*                                                                               
         L     RF,TYPEALOD         CALL EXTRACT LOAD ROUTINE                    
         BASR  RE,RF                                                            
         JNE   NO                  ERROR EXIT                                   
         J     YES                 EXIT OK                                      
*                                                                               
* FOR TYPE=SUP LOOP THROUGH TYPE TABLE CALLING THE LOAD                         
* ROUTINE FOR EACH SUPPORT RECORD TYPE                                          
*                                                                               
PROCL10  L     R2,ATYPTAB                                                       
         USING TYPTABD,R2                                                       
*                                                                               
PROCL20  CLI   TYPNAME,FF          TEST FOR E-O-T                               
         BE    YES                                                              
*                                                                               
         TM    TYPSTAT,TYPSSUP     TEST FOR SUPPORT RECORD                      
         BNO   PROCL30             NO                                           
*                                                                               
         MVC   TYPECODE,TYPNAME    SET CODE FOR THIS SUPPORT RECORD             
         GOTO1 AGETTYP                                                          
*                                                                               
         L     RF,TYPEALOD         CALL LOAD ROUTINE FOR THIS REOCRD            
         BASR  RE,RF                                                            
         BNE   NO                                                               
*                                                                               
PROCL30  AHI   R2,TYPTABLQ         POINT TO NEXT TABLE ENTRY                    
         B     PROCL20                                                          
         DROP  R2                                                               
***********************************************************************         
* PROCESS ACCOUNT FILE DATA IN UPDATE MODE READ RECOVERY FILES        *         
***********************************************************************         
PROCUPDT NTR1  ,                                                                
         L     R5,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R5                                                         
         MVC   PRIALPHA,SXDTAGY    SET PRIALPHA CODE FROM SYSTEM TABLE          
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             SET TYPE TABLE DATA                          
*                                                                               
         CLI   RFILTY,UNTDIRQ                                                   
         JNE   PROCUP10                                                         
         LA    RF,RECVHDR+L'RECVHDR                                             
         CLC   =X'0D07',0(RF)      DAYPARTS ARE DIRONLY                         
         JE    PROCUP20                                                         
*                                                                               
PROCUP10 CLI   RFILTY,SPTFILQ                                                   
         JE    PROCUP20                                                         
         CLI   RFILTY,STAFILQ                                                   
         JE    PROCUP20                                                         
         CLI   RFILTY,XSPFILQ                                                   
         JE    PROCUP20                                                         
         CLI   RFILTY,UNTFILQ                                                   
         JNE   YES                                                              
*                                                                               
PROCUP20 DS    0H                                                               
         BAS   RE,PROCKEY          PROCESS RECORD KEY VALUES                    
         JNE   YES                 EITHER IGNORE RECORD                         
         L     RF,TYPEAUPD         ELSE CALL UPDATE PROCESS ROUTINE             
         GOTO1 (RF),DMCB,(RC)                                                   
         JNE   NO                  EXIT ERROR                                   
         J     YES                 EXIT OK                                      
         DROP  R5                                                               
***********************************************************************         
* PROCESS KEY VALUES IN RECOVERY RECORD                               *         
* NTRY: R5 = A(RECOVERY RECORD)                                       *         
***********************************************************************         
         USING RECDS,R5                                                         
RX       USING AGYHDR,RECVHDR+L'RECVHDR                                         
PROCKEY  NTR1  ,                                                                
         GOTO1 ARECCMP             COMPARE COPY/CHANGE RECORDS                  
         JNE   NO                  NO CHANGES                                   
*                                                                               
         L     R4,DXACPYB                                                       
*                                                                               
         MVC   CTRL4,L'RECVHDR+4+15(R4)                                         
         MVC   CTRL5,L'RECVHDR+4+15(R5)                                         
*                                                                               
         CLI   RFILTY,STAFILQ                                                   
         BNE   PKEY10                                                           
         MVC   CTRL4,L'RECVHDR+4+17(R4)                                         
         MVC   CTRL5,L'RECVHDR+4+17(R5)                                         
         B     PKEY15                                                           
*                                                                               
PKEY10   CLI   RFILTY,XSPFILQ                                                   
         BNE   PKEY12                                                           
         MVC   CTRL4,L'RECVHDR+4+34(R4)                                         
         MVC   CTRL5,L'RECVHDR+4+34(R5)                                         
         B     PKEY15                                                           
*                                                                               
PKEY12   CLI   RFILTY,UNTFILQ                                                   
         BNE   PKEY14                                                           
         MVC   CTRL4,L'RECVHDR+4+22(R4)                                         
         MVC   CTRL5,L'RECVHDR+4+22(R5)                                         
*                                                                               
PKEY14   CLI   RFILTY,UNTDIRQ                                                   
         BNE   PKEY15                                                           
         CLC   =X'0D07',L'RECVHDR+4(R5)                                         
         BNE   PKEY15                                                           
         MVI   DXACTION,C'A'                                                    
         CLI   RRECTY,3      ADD?                                               
         JE    YES                                                              
         MVI   DXACTION,C'C'                                                    
         J     YES                                                              
*                                                                               
PKEY15   TM    CTRL5,X'80'   IS THIS RECORD DELETED?                            
         BZ    PKEY20              NO                                           
         CLI   DXACTION,C'C'                                                    
         JNE   NO                                                               
         CLI   RRECTY,X'02'        IS THIS A CHANGE RECORD                      
         JNE   NO                  NO                                           
*                                                                               
         TM    CTRL4,X'80'                                                      
         JNZ   NO                  AVOID DELETED 'CHANGED' RECORDS              
         MVI   DXACTION,C'D'                                                    
         J     YES                                                              
*                                                                               
* TEST RESTORED RECORD USING SAVED RECOVERY COPY RECORD                         
*                                                                               
PKEY20   CLI   RRECTY,X'02'        WITH CHANGE RECOVERY RECORD TYPE             
         JNE   YES                                                              
         TM    CTRL4,X'80'                                                      
         JZ    YES                                                              
         MVI   DXACTION,C'A'                                                    
*                                                                               
         CLI   RFILTY,SPTFILQ                                                   
         BNE   PKEY30                                                           
         CLC   L'RECVHDR+4(13,R4),L'RECVHDR+4(R5)  TEST ONLY CHG IN             
         JE    YES                                 RECOVERY FILE                
         J     PKEY70                                                           
*                                                                               
PKEY30   CLI   RFILTY,STAFILQ                                                   
         BNE   PKEY40                                                           
         CLC   L'RECVHDR+4(15,R4),L'RECVHDR+4(R5)  TEST ONLY CHG IN             
         JE    YES                                 RECOVERY FILE                
         J     PKEY70                                                           
*                                                                               
PKEY40   CLI   RFILTY,XSPFILQ                                                   
         BNE   PKEY50                                                           
         CLC   L'RECVHDR+4(32,R4),L'RECVHDR+4(R5)  TEST ONLY CHG IN             
         JE    YES                                 RECOVERY FILE                
         J     PKEY70                                                           
*                                                                               
PKEY50   CLI   RFILTY,UNTFILQ                                                   
         JNE   YES                                                              
         CLC   L'RECVHDR+4(20,R4),L'RECVHDR+4(R5)  TEST ONLY CHG IN             
         JE    YES                                 RECOVERY FILE                
PKEY70   MVI   DXACTION,C'C'                                                    
         J     YES                                                              
         DROP  RX,R5                                                            
***********************************************************************         
* COMMON ADDRESSES FOR ALL ROUTINES - COVERED BY ADDRESSD DSECT       *         
* ADDRESS IS AT AADDRESS IN W/S                                       *         
***********************************************************************         
         DS    0L                                                               
ADDRESS  DC    CL8'EXTERNS'                                                     
         DC    V(DATAMGR)                                                       
         DC    V(DMOD000)                                                       
         DC    V(DADDS)                                                         
         DC    V(LOGIO)                                                         
         DC    V(DATCON)                                                        
         DC    V(NTXCNVX)                                                       
         DC    A(MEDTAB)                                                        
         DC    V(NETIO)                                                         
         DC    V(NETVALUE)                                                      
         DC    V(DEMOUT)                                                        
* EXTRACT ROUTINES                                                              
         DC    V(NTTMDC)          MEDIA/AGENCY                                  
         DC    V(NTTCNC)          CLIENT                                        
         DC    V(NTTPDC)          PRODUCT                                       
         DC    V(NTTPLC)          CLIENT PRODUCT LIST                           
         DC    V(NTTESC)          ESTIMATE                                      
         DC    V(NTTSTC)          STATION                                       
         DC    V(NTTPGM)          PROGRAM                                       
         DC    V(NTTDPT)          DAYPARTS                                      
         DC    V(NTTEDC)          ESTIMATE DEMO LIST                            
         DC    V(NTTPAK)          PACKAGE RECORD                                
         DC    V(NTTUNI)          UNIT RECORD                                   
         DC    V(NTTGOL)          NET GOAL RECORDS                              
         DC    V(NTTPDG)          PRODUCT GROUP RECORDS                         
         DC    V(NTTUAD)          UNIT ACTUAL DEMOS                             
         DC    V(NTTECT)          EARNED COST                                   
         DC    V(NTTSGR)          STATION GROUP                                 
         DC    V(NTTCGR)          CLIENT GROUP                                  
         DC    V(NTTGXL)          NET GOAL RECORDS                              
         DC    V(NTTREP)          REP                                           
         DC    V(NTTFLT)          FLIGHTS                                       
         DC    V(NTTUCM)          UCOM                                          
         DC    V(NTTCML)          COMMERCIAL                                    
         DC    V(NTTMKT)          MARKET                                        
         DC    V(NTTBFM)          BFORM                                         
         DC    V(NTTUNV)          UNIVERSE                                      
         DC    V(NTTPAT)          PATTERN                                       
*                                                                               
         DC    CL8'FOR_ALL'        COMMON ROUTINES USED BY ALL SUBS             
         DC    A(ACCLOAD)                                                       
         DC    A(ACCUPDT)                                                       
         DC    A(DECIOC)                                                        
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(GETTYP)                                                        
         DC    A(GETIT)                                                         
         DC    A(READHI)                                                        
         DC    A(RECCMP)                                                        
*                                                                               
         DC    CL8'LOADING'        ADDRESSES OF LOAD ROUTINES                   
         DC    A(LOADAGY)          MEDIA/AGENCY                                 
         DC    A(LOADCNT)          CLIENT                                       
         DC    A(LOADPRD)          PRODUCT                                      
         DC    A(LOADPDL)          PRODUCT LIST                                 
         DC    A(LOADEST)          ESTIMATE                                     
         DC    A(LOADSTA)          STATION                                      
         DC    A(LOADPGM)          PROGRAM                                      
         DC    A(LOADDPT)          DAYPART                                      
         DC    A(LOADEDL)          ESTIMATE DEMO LIST                           
         DC    A(LOADPAK)          PACKAGE RECORD                               
         DC    A(LOADUNI)          UNIT RECORD                                  
         DC    A(LOADGOL)          NET GOAL RECORDS                             
         DC    A(LOADPDG)          PRODUCT GROUP RECORDS                        
         DC    A(LOADUAD)          UNIT ACTUAL DEMOS                            
         DC    A(LOADECT)          EARNED COST                                  
         DC    A(LOADSGR)          STATION GROUP                                
         DC    A(LOADCGR)          CLIENT  GROUP                                
         DC    A(LOADUND)          UNIT ACTUAL DEMOS                            
         DC    A(LOADGXL)          NET GOAL RECORDS                             
         DC    A(LOADREP)          REP                                          
         DC    A(LOADFLT)          FLIGHTS                                      
         DC    A(LOADUCM)          UCOM                                         
         DC    A(LOADCML)          COMMERCIAL                                   
         DC    A(LOADMKT)          MARKET                                       
         DC    A(LOADBFM)          BFORM                                        
         DC    A(LOADUNV)          UNIVERSE                                     
         DC    A(LOADPAT)          PATTERN                                      
*                                                                               
         DC    CL8'UPDTING'                                                     
         DC    A(UPDTAGY)          MEDIA/AGENCY                                 
         DC    A(UPDTCNT)          CLIENT                                       
         DC    A(UPDTPRD)          PRODUCT                                      
         DC    A(UPDTPDL)          PRODUCT LIST                                 
         DC    A(UPDTEST)          ESTIMATE                                     
         DC    A(UPDTSTA)          STATION                                      
         DC    A(UPDTPGM)          PROGRAM                                      
         DC    A(UPDTDPT)          DAYPART                                      
         DC    A(UPDTEDL)          ESTIMATE DEMO LIST                           
         DC    A(UPDTPAK)          PACKAGE RECORD                               
         DC    A(UPDTUNI)          UNIT RECORD                                  
         DC    A(UPDTGOL)          NET GOAL RECORDS                             
         DC    A(UPDTPDG)          PRODUCT GROUP RECORDS                        
         DC    A(UPDTUAD)          UNIT ACTUAL DEMOS                            
         DC    A(UPDTECT)          EARNED COST                                  
         DC    A(UPDTSGR)          STATION GROUP                                
         DC    A(UPDTCGR)          CLIENT  GROUP                                
         DC    A(UPDTUND)          UNIT ACTUAL DEMOS                            
         DC    A(UPDTGXL)          NET GOAL RECORDS                             
         DC    A(UPDTREP)          REP                                          
         DC    A(UPDTFLT)          FLIGHTS                                      
         DC    A(UPDTUCM)          UCOM                                         
         DC    A(UPDTCML)          COMMERCIAL                                   
         DC    A(UPDTMKT)          MARKET                                       
         DC    A(UPDTBFM)          BFORM                                        
         DC    A(UPDTUNV)          UNIVERSE                                     
         DC    A(UPDTPAT)          PATTERN                                      
*                                                                               
         DC    CL8'FILTERS'                                                     
         DC    A(FILTAGY)          MEDIA/AGENCY                                 
         DC    A(FILTCNT)          CLIENT                                       
         DC    A(FILTPRD)          PRODUCT                                      
         DC    A(FILTPDL)          PRODUCT LIST                                 
         DC    A(FILTEST)          ESTIMATE                                     
         DC    A(FILTSTA)          STATION                                      
         DC    A(FILTPGM)          PROGRAM                                      
         DC    A(FILTDPT)          DAYPART                                      
         DC    A(FILTEDL)          ESTIMATE DEMO LIST                           
         DC    A(FILTPAK)          PACKAGE RECORD                               
         DC    A(FILTUPK)          PACKAGE RECORD - UPDATE                      
         DC    A(FILTUNI)          UNIT RECORD                                  
         DC    A(FILTUUN)          UNIT RECORD - UPDATE                         
         DC    A(FILTGOL)          NET GOAL RECORDS                             
         DC    A(FILTPDG)          PRODUCT GROUP RECORDS                        
         DC    A(FILTUAD)          UNIT ACTUAL DEMOS                            
         DC    A(FILTUUA)          UNIT ACTUAL DEMOS - UPDATE                   
         DC    A(FILTECT)          EARNED COST                                  
         DC    A(FILTSGR)          STATION GROUP                                
         DC    A(FILTCGR)          CLIENT  GROUP                                
         DC    A(FILTUND)          UNIT ACTUAL DEMOS                            
         DC    A(FILTGXL)          NET GOAL RECORDS                             
         DC    A(FILTREP)          REP                                          
         DC    A(FILTFLT)          FLIGHTS                                      
         DC    A(FILTUCM)          UCOM                                         
         DC    A(FILTCML)          COMMERCIAL                                   
         DC    A(FILTMKT)          MARKET                                       
         DC    A(FILTBFM)          BFORM                                        
         DC    A(FILTUNV)          UNIVERSE                                     
         DC    A(FILTPAT)          PATTERN                                      
*                                                                               
         DC    CL8'INITS'                                                       
         DC    A(INITALL)          GENERAL INITIALISATION                       
         DC    A(INITAGY)          MEDIA/AGENCY                                 
         DC    A(INITCNT)          CLIENT                                       
         DC    A(INITPRD)          PRODUCT                                      
         DC    A(INITPDL)          PRODUCT LIST                                 
         DC    A(INITEST)          ESTIMATE                                     
         DC    A(INITSTA)          STATION                                      
         DC    A(INITPGM)          PROGRAM                                      
         DC    A(INITDPT)          DAYPART                                      
         DC    A(INITEDL)          ESTIMATE DEMO LIST                           
         DC    A(INITPAK)          PACKAGE RECORD                               
         DC    A(INITUNI)          UNIT RECORD                                  
         DC    A(INITGOL)          NET GOAL RECORDS                             
         DC    A(INITPDG)          PRODUCT GROUP RECORDS                        
         DC    A(INITUAD)          UNIT ACTUAL DEMOS                            
         DC    A(INITECT)          EARNED COST                                  
         DC    A(INITSGR)          STATION GROUP                                
         DC    A(INITCGR)          CLIENT  GROUP                                
         DC    A(INITUND)          UNIT ACTUAL DEMOS                            
         DC    A(INITGXL)          NET GOAL RECORDS                             
         DC    A(INITREP)          REP                                          
         DC    A(INITFLT)          FLIGHTS                                      
         DC    A(INITUCM)          UCOM                                         
         DC    A(INITCML)          COMMERCIAL                                   
         DC    A(INITMKT)          MARKET                                       
         DC    A(INITBFM)          BFORM                                        
         DC    A(INITUNV)          UNIVERSE                                     
         DC    A(INITPAT)          PATTERN                                      
*                                                                               
         DC    CL7'OPEN'                                                        
         DC    CL7'DMREAD'                                                      
         DC    CL7'DMRSEQ'                                                      
         DC    CL7'DMRDHI'                                                      
         DC    CL7'DMCLSE'                                                      
         DC    CL7'DMFAST'                                                      
         DC    CL7'GETREC'                                                      
         DC    CL7'RECOVER'                                                     
         DC    CL7'CONTROL'                                                     
         DC    CL7'CTFILE'                                                      
         DC    CL7'XSPDIR'                                                      
         DC    CL7'XSPFILE'                                                     
         DC    CL7'SPTDIR'                                                      
         DC    CL7'SPTFIL'                                                      
         DC    CL7'STATION'                                                     
         DC    CL7'UNTDIR'                                                      
         DC    CL7'UNTFIL'                                                      
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    A(BIGWORK)                                                       
         DC    A(COPYBUFF)                                                      
         DC    CL1'Y'                                                           
         DC    76C' '                                                           
*                                                                               
*                                                                               
         LTORG                                                                  
SPTFILQ  EQU   X'21'                                                            
STAFILQ  EQU   X'22'                                                            
UNTDIRQ  EQU   X'27'                                                            
UNTFILQ  EQU   X'2A'                                                            
XSPFILQ  EQU   X'37'                                                            
MXTRTQ   EQU   X'5E'               FIELD SEPARATOR CHR                          
FF       EQU   X'FF'                                                            
***********************************************************************         
* TYPTAB DEFINES PROCESS RECORD TYPES & IS COVERED BY TYPTABD         *         
*                                                                     *         
* CL3    TYPE NAME                                                    *         
* AL1    N/D                                                                    
* AL1    TYPE FLAGS                                                             
* AL1    STATUS BYTE - X'80' = SUPPORT RECORD FLAG                    *         
* AL2    N/D                                                          *         
* AL4    LOAD ROUTINE ADDRESS                                         *         
* AL4    UPDATE ROUTINE ADDRESS                                       *         
***********************************************************************         
TYPTAB   DS    0L                                                               
         DC    CL3'ALL',AL1(00,00,00,00,00),AL4(LOADALL,UPDTALL)                
*                                                                               
         DC    CL3'OPT',AL1(00,00,00,00,00),AL4(LOADOPT,UPDTOPT)                
*                                                                               
         DC    CL3'MDM',AL1(00,00,00,00,00),AL4(LOADMDM,UPDTMDM)                
*                                                                               
         DC    CL3'AGY',AL1(00,00,TYPSSUP,00,00),AL4(LOADAGY,UPDTAGY)           
         DC    CL3'CNT',AL1(00,00,TYPSSUP,00,00),AL4(LOADCNT,UPDTCNT)           
         DC    CL3'CGR',AL1(00,00,TYPSSUP,00,00),AL4(LOADCGR,UPDTCGR)           
         DC    CL3'DPT',AL1(00,00,TYPSSUP,00,00),AL4(LOADDPT,UPDTDPT)           
         DC    CL3'EDL',AL1(00,00,TYPSSUP,00,00),AL4(LOADEDL,UPDTEDL)           
         DC    CL3'EST',AL1(00,00,TYPSSUP,00,00),AL4(LOADEST,UPDTEST)           
         DC    CL3'PAK',AL1(00,00,TYPSSUP,00,00),AL4(LOADPAK,UPDTPAK)           
         DC    CL3'PDG',AL1(00,00,TYPSSUP,00,00),AL4(LOADPDG,UPDTPDG)           
         DC    CL3'PDL',AL1(00,00,TYPSSUP,00,00),AL4(LOADPDL,UPDTPDL)           
         DC    CL3'PGM',AL1(00,00,TYPSSUP,00,00),AL4(LOADPGM,UPDTPGM)           
         DC    CL3'PRD',AL1(00,00,TYPSSUP,00,00),AL4(LOADPRD,UPDTPRD)           
         DC    CL3'SGR',AL1(00,00,TYPSSUP,00,00),AL4(LOADSGR,UPDTSGR)           
         DC    CL3'REP',AL1(00,00,TYPSSUP,00,00),AL4(LOADREP,UPDTREP)           
         DC    CL3'STA',AL1(00,00,TYPSSUP,00,00),AL4(LOADSTA,UPDTSTA)           
         DC    CL3'FLT',AL1(00,00,TYPSSUP,00,00),AL4(LOADFLT,UPDTFLT)           
         DC    CL3'UCM',AL1(00,00,TYPSSUP,00,00),AL4(LOADUCM,UPDTUCM)           
*                                                                               
         DC    CL3'GOL',AL1(00,00,00,00,00),AL4(LOADGOL,UPDTGOL)                
         DC    CL3'GXL',AL1(00,00,00,00,00),AL4(LOADGXL,UPDTGXL)                
*                                                                               
         DC    CL3'ECT',AL1(00,00,00,00,00),AL4(LOADECT,UPDTECT)                
*                                                                               
         DC    CL3'UAD',AL1(00,00,00,00,00),AL4(LOADUAD,UPDTUAD)                
         DC    CL3'UND',AL1(00,00,00,00,00),AL4(LOADUND,UPDTUND)                
         DC    CL3'UNI',AL1(00,00,00,00,00),AL4(LOADUNI,UPDTUNI)                
*                                                                               
         DC    CL3'CML',AL1(00,00,TYPSSUP,00,00),AL4(LOADCML,UPDTCML)           
         DC    CL3'MKT',AL1(00,00,TYPSSUP,00,00),AL4(LOADMKT,UPDTMKT)           
         DC    CL3'BFM',AL1(00,00,TYPSSUP,00,00),AL4(LOADBFM,UPDTBFM)           
         DC    CL3'UNV',AL1(00,00,TYPSSUP,00,00),AL4(LOADUNV,UPDTUNV)           
         DC    CL3'PAT',AL1(00,00,TYPSSUP,00,00),AL4(LOADPAT,UPDTPAT)           
         DC    X'FF'                                                            
***********************************************************************         
* GET TYPE TABLE ACTION FROM 3 CHARACTER CODE                         *         
***********************************************************************         
GETTYP   NTR1  BASE=*,LABEL=*                                                   
         L     RF,ATYPTAB                                                       
         USING TYPTABD,RF                                                       
GTYP10   CLI   0(RF),FF             END OF TABLE                                
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TYPECODE,TYPNAME     COMPARE NAME                                
         BE    *+12                                                             
         LA    RF,TYPTABLQ(RF)      GET NEXT ENTRY                              
         B     GTYP10                                                           
*                                                                               
         MVC   TYPENAME,TYPNAME     MATCH FOUND                                 
         MVC   TYPEDEEP,TYPLDEEP                                                
         MVC   TYPEFLAG,TYPFLAG                                                 
         MVC   TYPEALOD,TYPLOAD                                                 
         MVC   TYPEAUPD,TYPUPDT                                                 
*                                                                               
         MVI   FLAGS,X'00'                                                      
*                                                                               
         CLC   TYPENAME,=C'AGY'     AGENCY?                                     
         BNE   *+12                                                             
         OI    FLAGS,NORDSEQ                                                    
         B     GTYPX                                                            
*                                                                               
         CLC   TYPENAME,=C'DPT'     DAYPARTS?                                   
         BNE   *+12                                                             
         OI    FLAGS,UNTFILEQ+DIRONLY                                           
         B     GTYPX                                                            
*                                                                               
         CLC   TYPENAME,=C'GXL'     GOALS (XSPFILE)?                            
         BE    GTYPXSPF                                                         
         CLC   TYPENAME,=C'FLT'     FLGHTS?                                     
         BE    GTYPXSPF                                                         
         CLC   TYPENAME,=C'PAT'     PATTERN?                                    
         BNE   *+12                                                             
GTYPXSPF OI    FLAGS,XSPFILEQ                                                   
         B     GTYPX                                                            
*                                                                               
         CLC   TYPENAME,=C'REP'     REP?                                        
         BE    GTYPSTAF                                                         
         CLC   TYPENAME,=C'MKT'     MARKET?                                     
         BE    GTYPSTAF                                                         
         CLC   TYPENAME,=C'STA'     STATIONS?                                   
         BNE   *+12                                                             
GTYPSTAF OI    FLAGS,STAFILEQ                                                   
         B     GTYPX                                                            
*                                                                               
         CLC   TYPENAME,=C'PAK'     PACKAGE?                                    
         BE    GTYPUNTF                                                         
         CLC   TYPENAME,=C'UNI'     UNIT?                                       
         BE    GTYPUNTF                                                         
         CLC   TYPENAME,=C'UAD'     UNIT ACTUAL DEMOS?                          
         BE    GTYPUNTF                                                         
         CLC   TYPENAME,=C'UND'     UNIT NIGHTLY DEMOS?                         
         BE    GTYPUNTF                                                         
         CLC   TYPENAME,=C'ECT'     ASSIGNED COST?                              
         BNE   GTYPX                                                            
GTYPUNTF OI    FLAGS,UNTFILEQ                                                   
         B     GTYPX                                                            
*                                                                               
GTYPX    J     YES                                                              
         DROP  RF                                                               
         LTORG                                                                  
***********************************************************************         
* DECREMENT MAXIMUM IO COUNT                                          *         
***********************************************************************         
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
***********************************************************************         
* CALL DMGR TO GET A RECORD                                           *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       PRIADDR      = DISK ADDRESS TO READ                           *         
* EXIT: CC EQUAL     = RECORD READ OK                                 *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
GETIT    NTR1  BASE=*,LABEL=*                                                   
         TM    FLAGS,UNTFILEQ                                                   
         BO    GETIT10                                                          
         TM    FLAGS,XSPFILEQ                                                   
         BO    GETIT15                                                          
         B     GETIT20                                                          
*                                                                               
GETIT10  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),UNTFIL,PRIADDR,(R2),DMWORK          
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'10'                                                      
         JO    LOW                                                              
         B     GETIT30                                                          
*                                                                               
GETIT15  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),XSPFIL,PRIADDR,(R2),DMWORK          
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'10'                                                      
         JO    LOW                                                              
         B     GETIT30                                                          
*                                                                               
GETIT20  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),SPTFIL,PRIADDR,(R2),DMWORK          
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'10'                                                      
         JO    LOW                                                              
*                                                                               
GETIT30  DS    0H                                                               
         GOTO1 =V(HEXOUT),PARM,PRIADDR,GETDA,L'PRIADDR,0                        
         GOTO1 (RF),(R1),DMCB+8,GETRC,1                                         
*                                                                               
         LA    R3,GETMSGL          OUTPUT DISK READ ERROR MESSAGE               
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     HIGH                                                             
*                                                                               
GETMSGL  DC    AL2(50)                                                          
GETMSG   DC    CL50' '                                                          
         ORG   GETMSG                                                           
         DC    C'DMGR GETREC ERROR - D/A = '                                    
GETDA    DC    CL8' '                                                           
         DC    C','                                                             
         DC    C' RC = '                                                        
GETRC    DC    CL2' '                                                           
         LTORG                                                                  
***********************************************************************         
* CALL DMGR TO PERFORM A READHI                                       *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       IOKEY        = KEY TO READ HIGH FOR                           *         
* EXIT: CC EQUAL     = RECORD READ OK OR EOF SET                      *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
READHI   NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEYSAV,IOKEY                                                   
*                                                                               
         TM    FLAGS,UNTFILEQ                                                   
         BO    READ10                                                           
         TM    FLAGS,XSPFILEQ                                                   
         BO    READ15                                                           
         TM    FLAGS,STAFILEQ                                                   
         BO    READ20                                                           
         B     READ30                                                           
*                                                                               
READ10   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,UNTDIR,IOKEY,(R2),DMWORK                    
         B     READ40                                                           
*                                                                               
READ15   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,XSPDIR,IOKEY,(R2),DMWORK                    
         B     READ40                                                           
*                                                                               
READ20   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,STAFIL,IOKEY,(R2),DMWORK                    
         B     READ40                                                           
*                                                                               
READ30   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,IOKEY,(R2),DMWORK                    
         B     READ40                                                           
*                                                                               
READ40   DS    0H                                                               
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'80'                                                      
         JO    YES                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,IOKEYSAV,RDHKEY,L'IOKEYSAV,0                     
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
***********************************************************************         
* COMPARE COPY AND CHANGE RECORDS TO SEE IF THEY ARE DIFFERENT        *         
*                                                                     *         
* NTRY:                                                               *         
* EXIT: CC EQ    RECORD TO BE PROCESSED                               *         
*     : CC NE    RECORD NOT TO BE PROCESSED                           *         
***********************************************************************         
RECCMP   NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB          GET CHANGE RECORD ADDRESS                    
         USING RECDS,R5                                                         
         CLI   RRECTY,3            ADD OF NEW RECORD?                           
         JE    YES                 YES                                          
*                                                                               
         LA    R2,RECVHDR+L'RECVHDR   A(CHANGE RECORD)                          
         L     R5,DXACPYB                                                       
         LA    R4,RECVHDR+L'RECVHDR   A(COPY RECORD)                            
*                                                                               
         CLI   RFILTY,SPTFILQ                                                   
         JE    RCSPT                                                            
         CLI   RFILTY,STAFILQ                                                   
         JE    RCSTA                                                            
         CLI   RFILTY,XSPFILQ                                                   
         JE    RCXSP                                                            
         CLI   RFILTY,UNTFILQ                                                   
         JE    RCUNT                                                            
         CLI   RFILTY,UNTDIRQ                                                   
         JNE   RCSPT                                                            
         CLC   =X'0D07',0(R4)      DAYPARTS - DIR ONLY SO EXTRACT               
         JE    YES                                                              
*                                                                               
RCSPT    MVC   HALF,L'CKEY(R2)     CHANGE REC LENGTH                            
         MVC   HALF2,L'CKEY(R4)    COPY REC LENGTH                              
         J     RC10                                                             
*                                                                               
RCSTA    MVC   HALF,L'STAKEY(R2)   CHANGE REC LENGTH                            
         MVC   HALF2,L'STAKEY(R4)  COPY REC LENGTH                              
         J     RC10                                                             
*                                                                               
RCXSP    MVC   HALF,L'GKEY(R2)     CHANGE REC LENGTH                            
         MVC   HALF2,L'GKEY(R4)    COPY REC LENGTH                              
         J     RC10                                                             
*                                                                               
RCUNT    MVC   HALF,L'NUKEY(R2)    CHANGE REC LENGTH                            
         MVC   HALF2,L'NUKEY(R4)   COPY REC LENGTH                              
         J     RC10                                                             
*                                                                               
RC10     CLC   HALF,HALF2          TEST COPY/CHANGE LENGTH                      
         JNE   YES                 RECORD LENGTH HAS CHANGED                    
         SR    R3,R3                                                            
         ICM   R3,3,HALF2                                                       
         LR    R5,R3                                                            
         CLCL  R2,R4               COMPARE TWO RECORDS                          
         JNE   YES                                                              
         J     NO                  RECORDS ARE IDENTICAL                        
         LTORG                                                                  
***********************************************************************         
* LOAD ALL RECORD DATA                                                *         
***********************************************************************         
LOADALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LOADTAB                                                       
*                                                                               
LOAD02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOAD04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOAD04   LA    R3,L'LOADTAB(R3)                                                 
         J     LOAD02                                                           
*                                                                               
LOADTAB  DS    0XL8                                                             
         DC    CL3'AGY',AL1(0),AL4(LOADAGY) AGENCY                              
         DC    CL3'CGR',AL1(4),AL4(LOADCGR) CLIENT GROUPS                       
         DC    CL3'CNT',AL1(0),AL4(LOADCNT) CLIENT                              
         DC    CL3'DPT',AL1(0),AL4(LOADDPT) DAYPARTS                            
         DC    CL3'ECT',AL1(0),AL4(LOADECT) EARNED COST                         
         DC    CL3'EDL',AL1(0),AL4(LOADEDL) ESTIMATE DEMO LIST                  
         DC    CL3'EST',AL1(0),AL4(LOADEST) ESTIMATE                            
         DC    CL3'GOL',AL1(0),AL4(LOADGOL) GOALS                               
         DC    CL3'GXL',AL1(5),AL4(LOADGXL) GOALS                               
         DC    CL3'PAK',AL1(0),AL4(LOADPAK) PACKAGE                             
         DC    CL3'PDG',AL1(0),AL4(LOADPDG) PRODUCT GROUP                       
         DC    CL3'PDL',AL1(0),AL4(LOADPDL) CLIENT PRODUCT LIST                 
         DC    CL3'PGM',AL1(0),AL4(LOADPGM) PROGRAM                             
         DC    CL3'PRD',AL1(0),AL4(LOADPRD) PRODUCT                             
         DC    CL3'REP',AL1(4),AL4(LOADREP) REP                                 
         DC    CL3'SGR',AL1(4),AL4(LOADSGR) STATION GROUPS                      
         DC    CL3'STA',AL1(0),AL4(LOADSTA) STATION                             
         DC    CL3'UAD',AL1(0),AL4(LOADUAD) UNIT ACTUAL DMEOS                   
         DC    CL3'UND',AL1(0),AL4(LOADUND) UNIT NIGHTLY DEMOS                  
         DC    CL3'UNI',AL1(0),AL4(LOADUNI) UNIT                                
         DC    CL3'FLT',AL1(6),AL4(LOADFLT) FLIGHTS                             
         DC    CL3'UCM',AL1(6),AL4(LOADUCM) UCOM                                
         DC    CL3'CML',AL1(0),AL4(LOADCML) COMMERCIAL                          
         DC    CL3'MKT',AL1(0),AL4(LOADMKT) MARKET                              
         DC    CL3'BFM',AL1(0),AL4(LOADBFM) BFORM                               
         DC    CL3'UNV',AL1(0),AL4(LOADUNV) UNIVERSE                            
         DC    CL3'PAT',AL1(0),AL4(LOADPAT) PATTERN                             
         DC    X'00'                                                            
         LTORG                                                                  
***********************************************************************         
* LOAD OPTICA DATA                                                    *         
***********************************************************************         
LOADOPT  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(2),DXDATEN+2                                                
         MVC   WORK+2(4),=C'0101'                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(2,DTEYRST)                                
                                                                                
         OI    FLAGS2,FL2OPTQ                                                   
         LA    R3,LOODTAB                                                       
*                                                                               
LOOD02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOOD04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOOD04   LA    R3,L'LOODTAB(R3)                                                 
         J     LOOD02                                                           
*                                                                               
LOODTAB  DS    0XL8                                                             
         DC    CL3'CNT',AL1(0),AL4(LOADCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(LOADPRD) PRODUCT                             
         DC    CL3'STA',AL1(0),AL4(LOADSTA) STATION                             
         DC    CL3'EST',AL1(0),AL4(LOADEST) ESTIMATE                            
         DC    CL3'DPT',AL1(0),AL4(LOADDPT) DAYPART                             
         DC    CL3'CGR',AL1(4),AL4(LOADCGR) CLIENT GROUPS                       
         DC    CL3'PGM',AL1(0),AL4(LOADPGM) PROGRAM                             
         DC    X'00'                                                            
         LTORG                                                                  
***********************************************************************         
* LOAD MDM DATA                                                       *         
***********************************************************************         
LOADMDM  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(2),DXDATEN+2                                                
         MVC   WORK+2(4),=C'0101'                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(2,DTEYRST)                                
                                                                                
         LA    R3,LMDMTAB                                                       
*                                                                               
LMDM02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LMDM04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LMDM04   LA    R3,L'LMDMTAB(R3)                                                 
         J     LMDM02                                                           
*                                                                               
LMDMTAB  DS    0XL8                                                             
         DC    CL3'AGY',AL1(0),AL4(LOADAGY) AGENCY                              
         DC    CL3'CNT',AL1(0),AL4(LOADCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(LOADPRD) PRODUCT                             
         DC    CL3'EST',AL1(0),AL4(LOADEST) ESTIMATE                            
         DC    CL3'EDL',AL1(0),AL4(LOADEDL) ESTIMATE DEMO LIST                  
         DC    CL3'STA',AL1(0),AL4(LOADSTA) STATION                             
         DC    CL3'DPT',AL1(0),AL4(LOADDPT) DAYPART                             
         DC    CL3'PAK',AL1(0),AL4(LOADPAK) PACKAGE                             
         DC    CL3'PGM',AL1(0),AL4(LOADPGM) PROGRAM                             
         DC    X'00'                                                            
         LTORG                                                                  
***********************************************************************         
* UPDATE ALL RECORD DATA                                              *         
***********************************************************************         
UPDTALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPDTTAB                                                       
UPDT02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDT04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDT04   LA    R3,L'UPDTTAB(R3)                                                 
         J     UPDT02                                                           
                                                                                
UPDTTAB  DS    0XL8                                                             
         DC    CL3'AGY',AL1(0),AL4(UPDTAGY) AGENCY                              
         DC    CL3'CNT',AL1(0),AL4(UPDTCNT) CLIENT                              
         DC    CL3'CGR',AL1(4),AL4(UPDTCGR) CLIENT GROUP                        
         DC    CL3'DPT',AL1(0),AL4(UPDTDPT) DAYPART                             
         DC    CL3'EST',AL1(0),AL4(UPDTEST) ESTIMATE                            
         DC    CL3'GOL',AL1(0),AL4(UPDTGOL) GOALS                               
         DC    CL3'GXL',AL1(5),AL4(UPDTGXL) GOALS                               
         DC    CL3'PAK',AL1(0),AL4(UPDTPAK) PACKAGE                             
         DC    CL3'PDG',AL1(0),AL4(UPDTPDG) PRODUCT GROUP                       
         DC    CL3'PDL',AL1(0),AL4(UPDTPDL) CLIENT PRODUCT LIST                 
         DC    CL3'PGM',AL1(0),AL4(UPDTPGM) PROGRAM                             
         DC    CL3'PRD',AL1(0),AL4(UPDTPRD) PRODUCT                             
         DC    CL3'REP',AL1(4),AL4(UPDTREP) REP INFO                            
         DC    CL3'SGR',AL1(4),AL4(UPDTSGR) STATION GROUP                       
         DC    CL3'STA',AL1(0),AL4(UPDTSTA) STATION                             
         DC    CL3'UNI',AL1(0),AL4(UPDTUNI) UNIT                                
         DC    CL3'FLT',AL1(6),AL4(UPDTFLT) FLIGHTS                             
         DC    CL3'UCM',AL1(6),AL4(UPDTUCM) UCOM                                
         DC    CL3'CML',AL1(0),AL4(UPDTCML) COMMERCIAL                          
         DC    CL3'MKT',AL1(0),AL4(UPDTMKT) MARKET                              
         DC    CL3'BFM',AL1(0),AL4(UPDTBFM) BFORM                               
         DC    CL3'UNV',AL1(0),AL4(UPDTUNV) UNIVERSE                            
         DC    CL3'PAT',AL1(0),AL4(UPDTPAT) PATTERN                             
         DC    X'00'                                                            
         LTORG                                                                  
***********************************************************************         
* UPDATE OPTICA DATA                                                  *         
***********************************************************************         
UPDTOPT  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(2),DXDATEN+2                                                
         MVC   WORK+2(4),=C'0101'                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(2,DTEYRST)                                
         OI    FLAGS2,FL2OPTQ                                                   
         LA    R3,UPDOTAB                                                       
UPDO02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDO04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDO04   LA    R3,L'UPDOTAB(R3)                                                 
         J     UPDO02                                                           
                                                                                
UPDOTAB  DS    0XL8                                                             
         DC    CL3'CNT',AL1(0),AL4(UPDTCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(UPDTPRD) PRODUCT                             
         DC    CL3'STA',AL1(0),AL4(UPDTSTA) STATION                             
         DC    CL3'EST',AL1(0),AL4(UPDTEST) ESTIMATE                            
         DC    CL3'DPT',AL1(0),AL4(UPDTDPT) DAYPART                             
         DC    CL3'PGM',AL1(0),AL4(UPDTPGM) PROGRAM                             
         DC    X'00'                                                            
         LTORG                                                                  
***********************************************************************         
* UPDATE MDM DATA                                                     *         
***********************************************************************         
UPDTMDM  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(2),DXDATEN+2                                                
         MVC   WORK+2(4),=C'0101'                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(2,DTEYRST)                                
         LA    R3,UPDMTAB                                                       
UPDM02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDM04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDM04   LA    R3,L'UPDMTAB(R3)                                                 
         J     UPDM02                                                           
                                                                                
UPDMTAB  DS    0XL8                                                             
         DC    CL3'AGY',AL1(0),AL4(UPDTAGY) AGENCY                              
         DC    CL3'CNT',AL1(0),AL4(UPDTCNT) CLIENT                              
         DC    CL3'PRD',AL1(0),AL4(UPDTPRD) PRODUCT                             
         DC    CL3'EST',AL1(0),AL4(UPDTEST) ESTIMATE                            
         DC    CL3'STA',AL1(0),AL4(UPDTSTA) STATION                             
         DC    CL3'DPT',AL1(0),AL4(UPDTDPT) DAYPART                             
         DC    CL3'PAK',AL1(0),AL4(UPDTPAK) PACKAGE                             
         DC    CL3'PGM',AL1(0),AL4(UPDTPGM) PROGRAM                             
         DC    X'00'                                                            
         LTORG                                                                  
***********************************************************************         
* INITIALISE ALL EXTRACT RECORDS                                      *         
* NTRY: R1 = LENGTH OF EXTRACT RECORD                                 *         
***********************************************************************         
INITALL  NTR1  BASE=*,LABEL=*                                                   
         L     R0,DXAXREC          R0=A(EXTRACT RECORD)                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         L     R3,DXAXREC          R3=A(EXTRACT RECORD AREA)                    
         USING DXHDRD,R3                                                        
*                                                                               
         LA    RF,DXHDRHL                                                       
         SLL   RF,16                                                            
         ST    RF,DXHDRLEN         SET MINIMUM RECORD LEN IN HDR                
         MVI   DXHDRRTY-1,MXTRTQ                                                
         MVC   DXHDRRTY,DXACTION                                                
         MVI   DXHDRCDT-1,MXTRTQ                                                
         CLI   DXMODE,DXLOADQ      LOAD MODE?                                   
         JE    IALL02              YES                                          
         CLC   SXDTTYP,=C'UND'     UNIT NIGHTLY DEMOS                           
         JE    IALL04                                                           
*                                                                               
         L     R5,DXARECB          HERE IF UPDATE MODE                          
         USING RECDS,R5                                                         
         GOTO1 VDATCON,DMCB,(3,RDATE),(20,DXHDRCDT)                             
         MVI   DXHDRCTI-1,MXTRTQ                                                
         ICM   RF,15,RTIME          FORMAT DATE & TIME FROM RECOVERY            
         TM    RTIME,X'80'                                                      
         JNO   *+12                                                             
         SLL   RF,1                                                             
         SRL   RF,5                                                             
         XC    DUB,DUB                                                          
         STCM  RF,15,DUB+4                                                      
         OI    DUB+7,X'0C'                                                      
         UNPK  DXHDRCTI(6),DUB+4(4)                                             
         OI    DXHDRCTI+5,X'F0'                                                 
         J     YES                                                              
*                                  HERE IF LOAD MODE                            
IALL02   MVC   DXHDRCDT,DXDATEN    SET TODAYS DATE                              
         MVI   DXHDRCTI-1,MXTRTQ                                                
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
IALL04   GOTO1 VDATCON,DMCB,(5,0),(20,DXHDRCDT)                                 
         MVC   DXHDRCTI(6),=C'000001'                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R3,R5                                                            
***********************************************************************         
* LOAD AGENCY RECORDS                                                           
***********************************************************************         
LOADAGY  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R8,MEDPARAM                                                      
*                                                                               
LAGY01   LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING AGYHDR,R2                                                        
         CLI   0(R8),X'FF'                                                      
         JE    YES                                                              
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,SXDTAGY                                                  
         DROP  R2                                                               
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LAGY02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VNTTAGYC,AINITAGY,AFILTAGY,(R8)                    
         JNE   NO                                                               
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LAGY02                                                           
         J     YES                                                              
         LTORG                                                                  
***********************************************************************         
*  MEDIA TABLE                                                                  
***********************************************************************         
         DC    X'FF'                                                            
MEDTAB   DS    0H                                                               
MEDBYTE  DC    C'I'                                                             
PROFBYTE DS    C                                                                
FULLNAME DS    CL11                   'INTERACTIVE'                             
REPNULL  DC    CL4'    '                                                        
REPNAME  DC    CL16'** UNASSIGNED **'                                           
REPPUB   DC    C'Y'                                                             
MEDEQU   EQU   *-MEDTAB                                                         
         DC    C'M'                                                             
         DS    C                                                                
         DS    CL11                   'MAGAZINES'                               
         DC    CL4'    '                                                        
         DC    CL16'** UNASSIGNED **'                                           
         DC    C'Y'                                                             
         DC    C'N'                                                             
         DS    C                                                                
         DC    X'FF'                                                            
         LTORG                                                                  
*---------------------------------------------------------------------*         
* UPDATE AGENCY RECORD DATA                                                     
*---------------------------------------------------------------------*         
UPDTAGY  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING AGYHDR,R2                                                        
*                                                                               
         GOTO1 AFILTAGY                                                         
         JNE   YES                                                              
         GOTO1 AINITAGY                                                         
*                                                                               
         GOTO1 AACCUPDT,DMCB,VNTTAGYC,0,(R8)                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*---------------------------------------------------------------------*         
* FILTER AGY RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTAGY  NTR1  BASE=*,LABEL=*                                                   
         USING AGYHDR,R2                                                        
         CLI   AGYKTYPE,X'06'                                                   
         JNE   NO                                                               
         CLC   AGYKAGY,SXDTAGY                                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE AGENCY RECORD                                                      
*---------------------------------------------------------------------*         
INITAGY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTMDDL          R1=L'AGY RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
***********************************************************************         
* LOAD CLIENT RECORDS                                                           
***********************************************************************         
LOADCNT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING CLTHDR,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,SXDTAGB                                                   
         DROP  R2                                                               
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCNT10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         MVC   4(4,R8),VOFFICER   PASS A(OFFICER)                               
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VNTTCNTC,AINITCNT,AFILTCNT,(R8)                    
         JNE   NO                                                               
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCNT10                                                           
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* UPDATE CNT RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTCNT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING CLTHDR,R2                                                        
*                                                                               
         GOTO1 AFILTCNT                                                         
         JNE   YES                                                              
         GOTO1 AINITCNT                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         MVI   0(R8),C'U'                                                       
         MVC   4(4,R8),VOFFICER   PASS A(OFFICER)                               
*                                                                               
         GOTO1 AACCUPDT,DMCB,VNTTCNTC,TYPECODE,(R8)                             
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
*---------------------------------------------------------------------*         
* FILTER CNT RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTCNT  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),X'00'                                                      
         JNE   NO                                                               
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         CLC   4(9,R2),=XL9'0'                                                  
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE CNT RECORD                                                         
*---------------------------------------------------------------------*         
INITCNT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTCNDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD PRD RECORDS                                                              
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADPRD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PRDHDR,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   PKEYAM,SXDTAGB                                                   
         DROP  R2                                                               
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPRD10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VNTTPRDC,AINITPRD,AFILTPRD,(R8)                    
         JNE   NO                                                               
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPRD10                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* UPDATE PRD RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTPRD  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PRDHDR,R2                                                        
*                                                                               
         GOTO1 AFILTPRD                                                         
         JNE   YES                                                              
         GOTO1 AINITPRD                                                         
         GOTO1 AACCUPDT,DMCB,VNTTPRDC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* FILTER PRD RECORD AT R2                                             *         
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
FILTPRD  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),X'00'                                                      
         JNE   NO                                                               
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         CLC   4(3,R2),=XL3'0'                                                  
         JE    NO                                                               
         CLC   7(6,R2),=XL6'0'                                                  
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE PRD RECORD                                                         
*---------------------------------------------------------------------*         
INITPRD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTPDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD RECORDS FOR CLIENT PRODUCT LIST                                          
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADPDL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PRDHDR,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   PKEYAM,SXDTAGB                                                   
         DROP  R2                                                               
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPDL10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         L     R8,ABIGWORK                                                      
         GOTO1 AACCLOAD,DMCB,VNTTPDLC,AINITPDL,AFILTPDL,(R8)                    
         JNE   NO                                                               
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPDL10                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* FILTER RECORD FOR CLIENT PRODUCT LIST                                         
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
FILTPDL  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),X'00'                                                      
         JNE   NO                                                               
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         CLC   4(3,R2),=XL3'0'                                                  
         JE    NO                                                               
         CLC   7(6,R2),=XL6'0'                                                  
         JNE   NO                                                               
*                                                                               
         XC    IOKEYSAV,IOKEYSAV                                                
         XC    IOKEYSA2,IOKEYSA2             *****                              
         MVC   IOKEYSAV(13),0(R2)                                               
         MVC   IOKEYSA2(13),0(R2)            *****                              
         MVC   IOKEYSAV+4(3),=X'000000'      CLEAR OUT THE PRODUCT              
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,IOKEYSAV,IO,DMWORK                   
         MVC   PRIADDR,IO+14                                                    
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),SPTFIL,PRIADDR,IO,DMWORK            
*                                                                               
         LA    R2,IO                                                            
         USING CLTHDR,R2                                                        
         L     R4,ABIGWORK                                                      
         LA    R6,CLIST                                                         
         LHI   R5,880                                                           
         LR    R7,R5                                                            
         MVCL  R4,R6               SAVE CLIST                                   
*                                                                               
         L     R4,ABIGWORK                                                      
         LHI   R5,880                                                           
         AR    R4,R5               BUMP PAST CLIST                              
         LA    R6,CLIST2                                                        
         LHI   R5,140                                                           
         LR    R7,R5                                                            
         MVCL  R4,R6               SAVE CLIST2                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,IOKEYSA2,IO,DMWORK                   
         MVC   PRIADDR,IO+14                                                    
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),SPTFIL,PRIADDR,IO,DMWORK            
*                                                                               
         J     YES                                                              
*                                                                               
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE RECORD FOR CLIENT PRODUCT LIST                                     
*---------------------------------------------------------------------*         
INITPDL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTPLDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* UPDATE CLIENT PRODUCT LIST DATA                                               
*---------------------------------------------------------------------*         
UPDTPDL  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTPDL                                                         
         JNE   YES                                                              
         GOTO1 AINITPDL                                                         
         L     R8,ABIGWORK                                                      
         GOTO1 AACCUPDT,DMCB,VNTTPDLC,TYPECODE,(R8)                             
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD EST RECORDS                                                              
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADEST  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING ESTHDR,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   EKEYAM,SXDTAGB                                                   
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LEST02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VNTTESTC,AINITEST,AFILTEST,(R8)                    
         JNE   NO                                                               
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LEST02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE EST RECORD DATA                                                        
*---------------------------------------------------------------------*         
UPDTEST  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTEST                                                         
         JNE   YES                                                              
         GOTO1 AINITEST                                                         
         GOTO1 AACCUPDT,DMCB,VNTTESTC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*---------------------------------------------------------------------*         
* FILTER EST RECORD AT R2                                             *         
*---------------------------------------------------------------------*         
FILTEST  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),X'00'                                                      
         JNE   NO                                                               
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         CLI   7(R2),0             ESTIMATE                                     
         JE    NO                                                               
         CLC   8(5,R2),=XL5'0'                                                  
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE EST RECORD                                                         
*---------------------------------------------------------------------*         
INITEST  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTESDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD STATION RECORDS                                                          
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADSTA  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING STAREC,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   STAKTYPE,STAKTYPQ                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSTA02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AACCLOAD,DMCB,VNTTSTAC,AINITSTA,AFILTSTA,(R8)                    
         JNE   NO                                                               
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSTA02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE STATION RECORD DATA                                                    
*---------------------------------------------------------------------*         
UPDTSTA  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,STAFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTSTA                                                         
         JNE   YES                                                              
         GOTO1 AINITSTA                                                         
         GOTO1 AACCUPDT,DMCB,VNTTSTAC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*---------------------------------------------------------------------*         
* FILTER STATION RECORD AT R2                                                   
*---------------------------------------------------------------------*         
FILTSTA  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),C'S'                                                       
         JNE   NO                                                               
         CLC   7(2,R2),SXDTAGY                                                  
         JNE   NO                                                               
*        CLI   1(R2),C'N'                                                       
*        JNE   NO                                                               
         CLC   9(3,R2),=C'000'                                                  
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE STATION RECORD                                                     
*---------------------------------------------------------------------*         
INITSTA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTSTDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD MARKET RECORDS                                                           
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADMKT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING MKTREC,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   MKTKTYPE,MKTKTYPQ                                                
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMKT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AACCLOAD,DMCB,VNTTMKTC,AINITMKT,AFILTMKT,(R8)                    
         JNE   NO                                                               
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMKT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE MARKET RECORD DATA                                                     
*---------------------------------------------------------------------*         
UPDTMKT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,STAFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTMKT                                                         
         JNE   YES                                                              
         GOTO1 AINITMKT                                                         
         GOTO1 AACCUPDT,DMCB,VNTTMKTC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*---------------------------------------------------------------------*         
* FILTER MARKET RECORD AT R2                                                    
*---------------------------------------------------------------------*         
FILTMKT  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),C'M'                                                       
         JNE   NO                                                               
         CLC   6(2,R2),SXDTAGY                                                  
         JNE   NO                                                               
         CLC   8(7,R2),=C'0000000'                                              
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE MARKET RECORD                                                      
*---------------------------------------------------------------------*         
INITMKT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTSTDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD BFORM RECORDS                                                            
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADBFM  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING BFREC,R2                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   BFKTYPE,BFKTYPEQ                                                 
         MVI   BFKSTYPE,BFKSTYPQ                                                
         MVC   BFKAGYMD,SXDTAGB       AGENCY/MEDIA                              
         OI    BFKAGYMD,X'03'         NET                                       
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LBFM10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AFILTBFM                                                         
         JNE   LBFMSEQ                                                          
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         GOTO1 AINITBFM                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),IOKEYSAV   PASS KEY                                     
*                                                                               
         GOTO1 VNTTBFMC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
LBFM30   GOTO1 VNTTBFMC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LBFMSEQ                                                          
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    LBFM40                                                           
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LBFM40   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LBFM30                                                           
*                                                                               
LBFMSEQ  XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(13),IOKEYSAV  RESTORE KEY                                  
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LBFM50   GOTO1 VDATAMGR,DMCB,DMRSEQ,SPTDIR,IOKEY,(R2),DMWORK                    
         CLC   IOKEY(3),0(R2)                                                   
         JNE   YES                                                              
*                                                                               
         MVC   IOKEYSAV,0(R2)                                                   
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LBFM10                                                           
         J     YES                                                              
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE BFORM RECORD DATA                                                      
*---------------------------------------------------------------------*         
UPDTBFM  NTR1  BASE=*,LABEL=*                                                   
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING BFREC,R2                                                         
*                                                                               
         GOTO1 AFILTBFM                                                         
         JNE   YES                                                              
         GOTO1 AINITBFM                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),IOKEYSAV   PASS KEY                                     
*                                                                               
         GOTO1 VNTTBFMC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
UBFM10   GOTO1 VNTTBFMC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         BE    UBFMX                                                            
*                                                                               
         L     RF,DXAXREC           A(RECORD)                                   
         CLI   SXDTPLFM,0                                                       
         BE    UBFM20                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
UBFM20   CLI   (NTTBFACT-NTTBFD)(RF),C'A'   ADD?                                
         BE    UBFM50                                                           
*                                                                               
         TM    BFRSTAT,X'80'        BFM DELETED?                                
         BZ    UBFM25                                                           
         MVI   (NTTBFACT-NTTBFD)(RF),C'D'                                       
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC              PUT RECORD & DECREMENT IO COUNT             
         J     UBFMX                                                            
*                                                                               
UBFM25   CLI   COPYFLAG,X'01'      FIRST TIME?                                  
         JE    UBFM40              NO - GO AHEAD AND ADD THEM                   
*                                                                               
         MVI   COPYFLAG,X'01'      YES - SET FIRST DELETE FOR TRIGGER           
         MVI   (NTTBFACT-NTTBFD)(RF),C'D'                                       
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC              PUT RECORD & DECREMENT IO COUNT             
         JNE   YES                                                              
         B     UBFM25                                                           
*                                                                               
UBFM40   L     RF,DXASQLB           RF=A(CONVERTED RECORD)                      
         MVI   (NTTBFACT-NTTBFD)(RF),C'A'                                       
*                                                                               
UBFM50   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC              PUT RECORD & DECREMENT IO COUNT             
         JNE   YES                                                              
         J     UBFM10                                                           
*                                                                               
UBFMX    J     YES                                                              
         LTORG                                                                  
         DROP  R5                                                               
*---------------------------------------------------------------------*         
* FILTER BFORM RECORD AT R2                                                     
*---------------------------------------------------------------------*         
         USING BFREC,R2                                                         
FILTBFM  NTR1  BASE=*,LABEL=*                                                   
         CLI   BFKTYPE,BFKTYPEQ                                                 
         JNE   NO                                                               
         CLI   BFKSTYPE,BFKSTYPQ                                                
         JNE   NO                                                               
         MVC   BYTE,BFKAGYMD                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE BFORM RECORD                                                       
*---------------------------------------------------------------------*         
INITBFM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTSTDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD REP RECORDS                                                              
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADREP  NTR1  BASE=*,LABEL=*                                                   
         CLI   SXDTVER,4                                                        
         JL    YES                                                              
*                                                                               
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING REPD,R2                                                          
         XC    IOKEY,IOKEY                                                      
         MVI   REPKTYPE,C'R'                                                    
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LREP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AACCLOAD,DMCB,VNTTREPC,AINITREP,AFILTREP,(R8)                    
         JNE   NO                                                               
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LREP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE REP RECORDS                                                            
*---------------------------------------------------------------------*         
UPDTREP  NTR1  BASE=*,LABEL=*                                                   
         CLI   SXDTVER,4                                                        
         JL    YES                                                              
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,STAFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTREP                                                         
         JNE   YES                                                              
         GOTO1 AINITREP                                                         
         GOTO1 AACCUPDT,DMCB,VNTTREPC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*---------------------------------------------------------------------*         
* FILTER REP RECORD AT R2                                                       
*---------------------------------------------------------------------*         
FILTREP  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),C'R'                                                       
         JNE   NO                                                               
         CLC   5(2,R2),SXDTAGY                                                  
         JNE   NO                                                               
*        CLI   1(R2),C'N'                                                       
*        JNE   NO                                                               
         CLC   7(8,R2),=C'00000000'                                             
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE REP RECORD                                                         
*---------------------------------------------------------------------*         
INITREP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTREPDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD PROGRAM RECORDS                                                          
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADPGM  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         BAS   RE,BLDMKTB          BUILD THE NETWORK/MK TABLE                   
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING NPGRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   NPGKTYP,X'0D'                                                    
         MVI   NPGKTYP+1,X'20'                                                  
         MVC   NPGKAM,SXDTAGB                                                   
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPGM02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         L     R8,ABIGWORK                                                      
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VNTTPGMC,AINITPGM,AFILTPGM,(R8)                    
*                                                                               
         JNE   NO                                                               
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPGM02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE PROGRAM RECORD DATA                                                    
*---------------------------------------------------------------------*         
UPDTPGM  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
*                                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         BAS   RE,BLDMKTB          BUILD THE NETWORK/MK TABLE                   
*                                                                               
         GOTO1 AFILTPGM                                                         
         JNE   YES                                                              
         GOTO1 AINITPGM                                                         
         L     R8,ABIGWORK                                                      
         GOTO1 AACCUPDT,DMCB,VNTTPGMC,TYPECODE,(R8)                             
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*---------------------------------------------------------------------*         
* FILTER PROGRAM RECORD AT R2                                                   
*---------------------------------------------------------------------*         
FILTPGM  NTR1  BASE=*,LABEL=*                                                   
         CLC   0(2,R2),=X'0D20'                                                 
         JNE   NO                                                               
         MVC   BYTE,2(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         OC    3(2,R2),3(R2)       ANY MARKET?                                  
         JZ    NO                                                               
*                                                                               
         TM    FLAGS2,FL2OPTQ                                                   
         JZ    YES                                                              
*                                                                               
         CLC   DTEYRST,NPGKEND-NPGKEY(R2)                                       
         JNL   NO                                                               
*                                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE PROGRAM RECORD                                                     
*---------------------------------------------------------------------*         
INITPGM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTPRDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* BUILD NETWORK/MKT TABLE                                                       
*---------------------------------------------------------------------*         
BLDMKTB  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         L     R3,ABIGWORK                                                      
*                                                                               
         LA    R2,IOKEYSAV         SET KEY TO READ FIRST RECORD                 
         USING STAREC,R2                                                        
         XC    IOKEYSAV,IOKEYSAV                                                
         MVI   STAKTYPE,STAKTYPQ                                                
         MVI   STAKMED,C'N'        ONLY NETWORK STATIONS                        
*                                                                               
         LA    R2,IO                                                            
         MVC   IOKEYSA2,IOKEYSAV                                                
         GOTO1 VDATAMGR,DMCB,DMRDHI,STAFIL,IOKEYSAV,IO,DMWORK                   
         B     BLDMK20                                                          
*                                                                               
BLDMKSEQ DS    0H                                                               
         MVC   IOKEYSA2,IOKEYSAV                                                
         GOTO1 VDATAMGR,DMCB,DMRSEQ,STAFIL,IOKEYSAV,IO,DMWORK                   
*                                                                               
BLDMK20  DS    0H                                                               
         CLI   STAKTYPE,STAKTYPQ                                                
         BNE   BLDMKX                                                           
         CLI   STAKMED,C'N'                                                     
         BNE   BLDMKX                                                           
         CLC   STAKAGY,SXDTAGY                                                  
         BNE   BLDMKSEQ                                                         
         CLC   =C'000000',STAKCLT                                               
         JNE   BLDMKSEQ                                                         
*&&DO                                                                           
         CLI   DXMODE,DXUPDTQ      UPDATE MODE?                                 
         BNE   BLDMK30             NO - THEN PROCESS ALL                        
*                                                                               
         CLC   SMKT,WORK           SAME NETWORK MARKET #                        
         BNE   BLDMKSEQ                                                         
*&&                                                                             
BLDMK30  MVC   0(4,R3),STAKCALL    CALL LETTERS                                 
*                                                                               
         XC    DUB,DUB                                                          
         PACK  DUB,SMKT            MARKET #                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,4(R3)                                                       
*                                                                               
         LA    R3,6(R3)                                                         
         B     BLDMKSEQ                                                         
*&&DO                                                                           
         CLI   DXMODE,DXUPDTQ      UPDATE MODE?                                 
         BNE   BLDMKSEQ            YES - EXIT                                   
*&&                                                                             
BLDMKX   DS    0H                                                               
         MVI   0(R3),X'FF'         END OF TABLE                                 
         J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD DAYPART RECORDS                                                          
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADDPT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING NDPTHDR,R2                                                       
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   NDPTKTY,NDPTKTYQ                                                 
         MVI   NDPTKST,NDPTKSTQ                                                 
*                                                                               
         MVC   NDPTAGM,SXDTAGB                                                  
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDPT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         GOTO1 AFILTDPT                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AACCLOAD,DMCB,VNTTDPTC,AINITDPT,AFILTDPT                         
*                                                                               
         JNE   NO                                                               
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDPT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE DAYPART RECORD DATA                                                    
*---------------------------------------------------------------------*         
UPDTDPT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         CLI   20(R2),X'80'        DELETED?                                     
         JNE   *+8                                                              
         MVI   DXACTION,C'D'                                                    
*                                                                               
         GOTO1 AFILTDPT                                                         
         JNE   YES                                                              
         GOTO1 AINITDPT                                                         
         GOTO1 AACCUPDT,DMCB,VNTTDPTC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*---------------------------------------------------------------------*         
* FILTER DAYPART RECORD AT R2                                                   
*---------------------------------------------------------------------*         
FILTDPT  NTR1  BASE=*,LABEL=*                                                   
         CLC   0(2,R2),=X'0D07'                                                 
         JNE   NO                                                               
         MVC   BYTE,2(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE DAYPART RECORD                                                     
*---------------------------------------------------------------------*         
INITDPT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTDPTL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD ESTIMATE DEMO LIST                                                       
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADEDL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING ESTHDR,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   EKEYAM,SXDTAGB                                                   
         OI    EKEYAM,X'03'        NET                                          
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LEDL10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AFILTEDL                                                         
         JNE   LEDLSEQ                                                          
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         GOTO1 AINITEDL                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),IOKEYSAV   PASS KEY                                     
*                                                                               
         GOTO1 VNTTEDLC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
*                                                                               
LEDL30   DS    0H                                                               
         GOTO1 VNTTEDLC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LEDLSEQ                                                          
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    LEDL40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LEDL40   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LEDL30                                                           
*                                                                               
LEDLSEQ  DS    0H                  GET NEXT RECORD                              
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(20),IOKEYSAV  RESTORE KEY                                  
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LEDL50   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,SPTDIR,IOKEY,(R2),DMWORK                    
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                                                              
*                                                                               
         MVC   IOKEYSAV,0(R2)                                                   
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LEDL10                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE ESTIMATE DEMO LIST                                                     
*---------------------------------------------------------------------*         
UPDTEDL  NTR1  BASE=*,LABEL=*                                                   
         MVI   COPYFLAG,X'00'      SET FLAG TO DIFFER COPY FROM CHANGE          
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
UEDL10   DS    0H                                                               
         GOTO1 AFILTEDL                                                         
         JNE   YES                                                              
         GOTO1 AINITEDL                                                         
*                                                                               
UEDL20   DS    0H                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),IOKEYSAV   PASS KEY                                     
*                                                                               
         GOTO1 VNTTEDLC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
*                                                                               
UEDL30   DS    0H                                                               
         GOTO1 VNTTEDLC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    UEDLX                                                            
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    UEDL60                                                           
*!!!     JE    UEDL40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*&&DO                                                                           
UEDL40   DS    0H                                                               
         CLI   (NTTEDACT-NTTEDD)(RF),C'A'       IF ADD SKIP TO PUT              
         JE    UEDL70                                                           
         MVI   (NTTEDACT-NTTEDD)(RF),C'D'                                       
         MVI   COPYFLAG,X'01'                                                   
         J     UEDL70                                                           
*                                                                               
UEDL50   DS    0H                                                               
         CLI   COPYFLAG,X'01'                                                   
         JNE   UEDL30                                                           
*&&                                                                             
UEDL60   DS    0H                                                               
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   (NTTEDACT-NTTEDD)(RF),C'A'                                       
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UEDL70   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     UEDL30                                                           
*!!!     J     UEDL50                                                           
*                                                                               
UEDLX    DS    0H                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*---------------------------------------------------------------------*         
* FILTER ESTIMATE DEMO LIST                                           *         
*---------------------------------------------------------------------*         
FILTEDL  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),X'00'                                                      
         JNE   NO                                                               
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         CLI   4(R2),0             PRODUCT?                                     
         JE    NO                                                               
         CLI   7(R2),0             ESTIMATE                                     
         JE    NO                                                               
         CLC   8(5,R2),=XL5'0'                                                  
         JNE   NO                                                               
*                                                                               
         MVC   IOKEYSAV,0(R2)                                                   
*                                                                               
FILTEDLX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE ESTIMATE DEMO LIST                                                 
*---------------------------------------------------------------------*         
INITEDL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTEDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD EARNED COST                                                              
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADECT  NTR1  BASE=*,LABEL=*                                                   
         L     RF,=F'200000000'                                                 
         ST    RF,DUB                                                           
         MVC   MAXIOS,DUB                                                       
*                                                                               
         L     R1,=A(VCLTREC)                                                   
         MVC   ACLTREC,0(R1)                                                    
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING NURECD,R2                                                        
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,SXDTAGB       AGENCY/MEDIA                                 
         OI    NUKAM,X'03'         NET                                          
*                                                                               
         OC    DXKEY,DXKEY         UNIT KEY FILTER FROM JCL?                    
         BZ    LECT05                                                           
         GOTO1 =V(HEXIN),DMCB,DXKEY,IOKEYSAV,40                                 
         MVC   IOKEY(20),IOKEYSAV                                               
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         BRAS  RE,GETCLT                                                        
         MVC   PREVCLT,NUKCLT      GET CLIENT ON NEXT RECORD                    
         J     LECT15                                                           
*                                                                               
LECT05   L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         BRAS  RE,GETCLT                                                        
         MVC   PREVCLT,NUKCLT      1ST TIME THROUGH                             
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,SXDTAGB       AGENCY/MEDIA                                 
         OI    NUKAM,X'03'         NET                                          
         MVC   NUKCLT,PREVCLT      CLI                                          
         MVC   NUKDATE,DXFDATEC    DATE FILTER (0 IF NONE)                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LECT10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLI   0(R2),X'04'         FINISHED WITH AGENCY?                        
         JNE   YES                                                              
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   YES                                                              
*                                                                               
         CLC   PREVCLT,NUKCLT      THIS TEST IS IF THE 1ST CLI DOESN'T          
         BE    LECT15              HAVE A UNIT FOR THAT DATE FILTER             
LECT12   BRAS  RE,GETCLT                                                        
         MVC   PREVCLT,NUKCLT                                                   
*                                                                               
         LA    R2,IOKEY                                                         
         USING NUKEY,R2                                                         
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,SXDTAGB      AGENCY/MEDIA                                  
         OI    NUKAM,X'03'        NET                                           
         MVC   NUKCLT,PREVCLT     NEW CLI                                       
         MVC   NUKDATE,DXFDATEC   SKIP READ TO THIS DATE                        
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         CLI   0(R2),X'04'        FINISHED WITH AGENCY?                         
         JNE   YES                                                              
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'         TURN OFF MEDIA BYTES                          
         CLC   BYTE,SXDTAGB                                                     
         JNE   YES                                                              
*                                                                               
         CLC   PREVCLT,NUKCLT     CHANGE IN CLIENT?                             
         JNE   LECT12             SKIPPED ENTIRE CLI - GET NEXT ONE             
*                                                                               
LECT15   GOTO1 AFILTECT                                                         
         JNE   LECTSEQ                                                          
*                                                                               
         CLC   PREVCLT,NUKCLT      CHANGE IN CLIENT?                            
         BE    LECT25                                                           
         BRAS  RE,GETCLT           GET CLIENT RECORD                            
         MVC   PREVCLT,NUKCLT      GET CLIENT ON NEXT RECORD                    
         B     LECT25                                                           
*                                                                               
LECT25   MVC   PRIADDR,NUDA        A(UNIT)                                      
         GOTO1 AGETIT                                                           
         JNE   LECTSEQ                                                          
*                                                                               
         GOTO1 AINITECT                                                         
*                                                                               
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         L     R8,ABIGWORK                                                      
         USING UINFOD,R8                                                        
         MVC   UIUNTKEY,UUNTKEY     UNIT KEY                                    
         MVC   UICLTREC,ACLTREC     A(CLIENT RECORD)                            
         DROP  R8                                                               
*                                                                               
         GOTO1 VNTTECTC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   NO                                                               
*                                                                               
LECT30   GOTO1 VNTTECTC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LECTSEQ                                                          
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    LECT40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LECT40   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     LECT30                                                           
*                                                                               
LECTSEQ  XC    IOKEY,IOKEY         GET NEXT UNIT RECORD                         
         MVC   IOKEY(20),UUNTKEY   RESTORE UNIT KEY                             
*                                                                               
         OC    DXKEY,DXKEY         UNIT KEY FILTER FROM JCL?                    
         JNZ   YES                 YES, EXIT                                    
*                                                                               
LECT50   GOTO1 VDATAMGR,DMCB,DMRSEQ,UNTDIR,IOKEY,(R2),DMWORK                    
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                                                              
*                                                                               
         CLI   0(R2),X'04'         FINISHED WITH AGENCY?                        
         JNE   YES                                                              
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   YES                                                              
*                                                                               
         CLI   NUKSUB,X'C1'        CHECK IF TRAFFIC UNIT                        
         JNL   LECT50                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LECT10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* UPDATE EARNED COST                                                            
*---------------------------------------------------------------------*         
UPDTECT  NTR1  BASE=*,LABEL=*                                                   
UECTX    DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* FILTER EARNED COST                                                            
*---------------------------------------------------------------------*         
FILTECT  NTR1  BASE=*,LABEL=*                                                   
         USING NURECD,R2                                                        
*                                                                               
         CLI   0(R2),X'04'                                                      
         JNE   NO                                                               
         MVC   BYTE,NUKAM                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         CLI   NUKSUB,X'C1'        TRAFFIC UNIT?                                
         JNL   NO                                                               
*                                                                               
         CLI   DXFDATEC,0          ANY DATE RANGE?                              
         JE    *+14                                                             
         CLC   DXFDATEC,NUKDATE    DATE CARD < AIR DATE?                        
         JH    NO                  NO - SKIP THIS UNIT                          
*                                                                               
         MVC   UUNTKEY,0(R2)                                                    
         DROP  R2                                                               
*                                                                               
FILTECTX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE PACKAGE RECORD                                                     
*---------------------------------------------------------------------*         
INITECT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTECDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD PACKAGE RECORDS                                                          
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADPAK  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING NPRECD,R2                                                        
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,SXDTAGB                                                    
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPAK10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         GOTO1 AFILTPAK                                                         
         JNE   YES                                                              
*                                                                               
         L     R8,ABIGWORK                                                      
         MVC   PRIADDR,21(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VNTTPAKC,AINITPAK,AFILTPAK,(R8)                    
*                                                                               
         JNE   NO                                                               
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPAK10                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE PACKAGE RECORD DATA                                                    
*---------------------------------------------------------------------*         
UPDTPAK  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,UNTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTUPK                                                         
         JNE   YES                                                              
         GOTO1 AINITPAK                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         GOTO1 AACCUPDT,DMCB,VNTTPAKC,TYPECODE,(R8)                             
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*---------------------------------------------------------------------*         
* FILTER PACKAGE RECORD AT R2 - UPDATE MODE                                     
*---------------------------------------------------------------------*         
FILTUPK  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),X'02'                                                      
         JNE   NO                                                               
         MVC   BYTE,11(R2)                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
*                                                                               
         USING NPRECD,R2                                                        
         MVC   BYTE,NPAKDP                                                      
         DROP  R2                                                               
*                                                                               
         XC    IOKEYSA2,IOKEYSA2                                                
         MVC   IOKEYSA2(20),0(R2)                                               
*                                                                               
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R3,IOKEYSAV         LOOK UP AGENCY LEVEL DPT FIRST               
         USING NDPTHDR,R3                                                       
*                                                                               
         MVC   NDPTKTYP,=X'0D07'                                                
         MVC   NDPTAGM,11(R2)      AGENCY MEDIA                                 
         MVC   NDPTDPTE,BYTE       DAYPART EQUATE                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,UNTDIR,IOKEYSAV,IOKEYSA3,DMWORK             
         CLC   IOKEYSA3(6),IOKEYSAV                                             
         BE    FUPK50                                                           
*                                                                               
         XC    IOKEYSAV,IOKEYSAV   LOOK UP CLIENT LEVEL DPT NOW                 
*                                                                               
         MVC   NDPTKTYP,=X'0D07'                                                
         MVC   NDPTAGM,11(R2)      AGENCY MEDIA                                 
         MVC   NDPTCLT,12(R2)      CLIENT                                       
         MVC   NDPTDPTE,BYTE       DAYPART EQUATE                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,UNTDIR,IOKEYSAV,IOKEYSA3,DMWORK             
*                                                                               
FUPK50   DS    0H                                                               
         LA    R3,IOKEYSA3                                                      
         L     RF,ABIGWORK                                                      
         MVC   0(2,RF),NDPTDPTA SAVE AWAY 2 CHAR DAYPART                        
*!!!!    MVC   BIGWORK(2),NDPTDPTA SAVE AWAY 2 CHAR DAYPART                     
         DROP  R3                                                               
*                                                                               
FILTUPKX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* FILTER PACKAGE RECORD AT R2                                                   
*---------------------------------------------------------------------*         
FILTPAK  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),X'02'                                                      
         JNE   NO                                                               
         MVC   BYTE,11(R2)                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
*                                                                               
         MVC   PRIADDR,21(R2)                                                   
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),UNTFIL,PRIADDR,IO,DMWORK            
*                                                                               
         LA    R3,IO                                                            
         USING NPRECD,R3                                                        
         MVC   BYTE,NPAKDP                                                      
         DROP  R3                                                               
*                                                                               
         XC    IOKEYSA2,IOKEYSA2                                                
         MVC   IOKEYSA2(20),0(R2)                                               
*                                                                               
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R3,IOKEYSAV         LOOK UP AGENCY LEVEL DPT FIRST               
         USING NDPTHDR,R3                                                       
*                                                                               
         MVC   NDPTKTYP,=X'0D07'                                                
         MVC   NDPTAGM,11(R2)      AGENCY MEDIA                                 
         MVC   NDPTDPTE,BYTE       DAYPART EQUATE                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,UNTDIR,IOKEYSAV,IOKEYSA3,DMWORK             
         CLC   IOKEYSA3(6),IOKEYSAV                                             
         BE    FPAK50                                                           
*                                                                               
         XC    IOKEYSAV,IOKEYSAV   LOOK UP CLIENT LEVEL DPT NOW                 
*                                                                               
         MVC   NDPTKTYP,=X'0D07'                                                
         MVC   NDPTAGM,11(R2)      AGENCY MEDIA                                 
         MVC   NDPTCLT,12(R2)      CLIENT                                       
         MVC   NDPTDPTE,BYTE       DAYPART EQUATE                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,UNTDIR,IOKEYSAV,IOKEYSA3,DMWORK             
*                                                                               
FPAK50   DS    0H                                                               
         LA    R3,IOKEYSA3                                                      
         L     RF,ABIGWORK                                                      
         MVC   0(2,RF),NDPTDPTA SAVE AWAY 2 CHAR DAYPART                        
*!!!!!   MVC   BIGWORK(2),NDPTDPTA SAVE AWAY 2 CHAR DAYPART                     
         DROP  R3                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,UNTDIR,IOKEYSA2,IO,DMWORK                   
*                                                                               
FILTPAKX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE PACKAGE RECORD                                                     
*---------------------------------------------------------------------*         
INITPAK  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTPKDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD UNIT RECORDS                                                             
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADUNI  NTR1  BASE=*,LABEL=*                                                   
         XC    FILTCLI,FILTCLI                                                  
         XC    FILTCLIE,FILTCLIE                                                
         CLI   DXCLIS,0             IS CLIENT RANGE REQUESTED                   
         JNE   LUNI02                                                           
         CLI   DXCLI,0                                                          
         JE    LUNI04                                                           
         OC    DXCLI,=C'   '                                                    
         GOTO1 =V(CLPACK),DMCB,DXCLI,FILTCLI                                    
         B     LUNI04                                                           
*                                                                               
* SET UP CLIENT RANGE FILTER                                                    
LUNI02   OC    DXCLIS,=C'   '                                                   
         GOTO1 =V(CLPACK),DMCB,DXCLIS,FILTCLI                                   
         OC    DXCLIE,=C'   '                                                   
         GOTO1 =V(CLPACK),DMCB,DXCLIE,FILTCLIE                                  
*                                                                               
LUNI04   BRAS  RE,BLDUINF          BUILD INFO FOR UNIT/UAD LOAD                 
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING NURECD,R2                                                        
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,SXDTAGB       AGENCY/MEDIA                                 
         OI    NUKAM,X'03'         NET                                          
         MVC   NUKCLT,FILTCLI                                                   
*                                                                               
         OC    DXKEY,DXKEY         UNIT KEY FILTER FROM JCL?                    
         BZ    LUNI07                                                           
*                                                                               
         GOTO1 =V(HEXIN),DMCB,DXKEY,IOKEYSAV,40                                 
         MVC   IOKEY(20),IOKEYSAV                                               
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         BRAS  RE,GETCLT                                                        
         BRAS  RE,GETEST                                                        
         BRAS  RE,GETPROS          GET PROFILES FOR DEMO LOOKUPS                
         MVC   PREVCLT,NUKCLT      GET CLIENT ON NEXT RECORD                    
         MVC   PREVEST,NUKEST                                                   
         J     LUNI15                                                           
*                                                                               
LUNI07   L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         BRAS  RE,GETCLT                                                        
         BRAS  RE,GETEST                                                        
         BRAS  RE,GETPROS          GET PROFILES FOR DEMO LOOKUPS                
         MVC   PREVCLT,NUKCLT      1ST TIME THROUGH                             
         MVC   PREVEST,NUKEST                                                   
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,SXDTAGB       AGENCY/MEDIA                                 
         OI    NUKAM,X'03'         NET                                          
         MVC   NUKCLT,PREVCLT      CLI                                          
         MVC   NUKDATE,DXFDATEC    DATE FILTER (0 IF NONE)                      
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LUNI10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLI   0(R2),X'04'         FINISHED WITH AGENCY?                        
         JNE   YES                                                              
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   YES                                                              
*                                                                               
         CLC   PREVCLT,NUKCLT      THIS TEST IS IF THE 1ST CLI DOESN'T          
         BE    LUNI15              HAVE A UNIT FOR THAT DATE FILTER             
*                                                                               
LUNI12   BRAS  RE,GETCLT                                                        
         BRAS  RE,GETEST                                                        
         BRAS  RE,GETPROS          GET PROFILES FOR DEMO LOOKUPS                
         MVC   PREVCLT,NUKCLT                                                   
         MVC   PREVEST,NUKEST                                                   
*                                                                               
         LA    R2,IOKEY                                                         
         USING NUKEY,R2                                                         
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,SXDTAGB      AGENCY/MEDIA                                  
         OI    NUKAM,X'03'        NET                                           
         MVC   NUKCLT,PREVCLT     NEW CLI                                       
         MVC   NUKDATE,DXFDATEC   SKIP READ TO THIS DATE                        
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         CLI   0(R2),X'04'        FINISHED WITH AGENCY?                         
         JNE   YES                                                              
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'         TURN OFF MEDIA BYTES                          
         CLC   BYTE,SXDTAGB                                                     
         JNE   YES                                                              
*                                                                               
         CLC   PREVCLT,NUKCLT     CHANGE IN CLIENT?                             
         JNE   LUNI12             SKIPPED ENTIRE CLI - GET NEXT ONE             
*                                                                               
LUNI15   GOTO1 AFILTUNI                                                         
         JNE   LUNISEQ                                                          
*                                                                               
         CLC   PREVCLT,NUKCLT      CHANGE IN CLIENT?                            
         BE    LUNI20                                                           
*                                                                               
         BRAS  RE,GETCLT           GET CLIENT RECORD                            
         BRAS  RE,GETEST                                                        
         BRAS  RE,GETPROS          GET PROFILES FOR DEMO LOOKUPS                
         MVC   PREVCLT,NUKCLT      GET CLIENT ON NEXT RECORD                    
         MVC   PREVEST,NUKEST                                                   
         B     LUNI22                                                           
*                                                                               
LUNI20   CLC   PREVEST,NUKEST      CHANGE IN ESTIMATE?                          
         BE    LUNI22                                                           
         BRAS  RE,GETEST                                                        
         MVC   PREVEST,NUKEST                                                   
*                                                                               
LUNI22   CLI   FILTCLIE,0          CLIENT RANGE FILTER                          
         JE    LUNI25                                                           
         CLC   NUKCLT,FILTCLIE                                                  
         JH    YES                                                              
         B     LUNI27                                                           
*                                                                               
LUNI25   CLI   FILTCLI,0           CLIENT FILTER?                               
         JE    *+14                                                             
         CLC   NUKCLT,FILTCLI                                                   
         JH    YES                                                              
*                                                                               
LUNI27   CLI   DXNET,0             NETWORK FILTER?                              
         JE    *+14                                                             
         CLC   NUKNET,DXNET                                                     
         JNE   LUNISEQ                                                          
*                                                                               
         BRAS  RE,GETNTI                                                        
*                                                                               
         MVC   PRIADDR,NUDA        A(UNIT)                                      
         GOTO1 AGETIT                                                           
         JNE   LUNISEQ                                                          
*                                                                               
         GOTO1 AINITUNI                                                         
*                                                                               
         BRAS  RE,BLDUPARM         BUILD UNIT/UAD LOAD PARMS TO ROUTS           
*                                                                               
         L     R8,ABIGWORK                                                      
         GOTO1 VNTTUNIC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   NO                                                               
*                                                                               
LUNI30   GOTO1 VNTTUNIC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LUNISEQ                                                          
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    LUNI40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LUNI40   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     LUNI30                                                           
*                                                                               
LUNISEQ  XC    IOKEY,IOKEY         GET NEXT UNIT RECORD                         
         MVC   IOKEY(20),UUNTKEY   RESTORE UNIT KEY                             
*                                                                               
         OC    DXKEY,DXKEY         UNIT KEY FILTER FROM JCL?                    
         JNZ   YES                 YES, EXIT                                    
*                                                                               
LUNI50   GOTO1 VDATAMGR,DMCB,DMRSEQ,UNTDIR,IOKEY,(R2),DMWORK                    
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                                                              
*                                                                               
         CLI   0(R2),X'04'         FINISHED WITH AGENCY?                        
         JNE   YES                                                              
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   YES                                                              
         CLI   NUKSUB,X'C1'        CHECK IF TRAFFIC UNIT                        
         JNL   LUNI50                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LUNI10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* UPDATE UNIT RECORD DATA                                                       
*---------------------------------------------------------------------*         
UPDTUNI  NTR1  BASE=*,LABEL=*                                                   
         MVI   COPYFLAG,0                                                       
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,UNTFILQ                                                   
         JNE   YES                                                              
*                                                                               
         BRAS  RE,BLDUUINF         BUILD INFO FOR UNIT/UAD UPDATE               
*                                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING NURECD,R2                                                        
*                                                                               
         OC    DXKEY,DXKEY         UNIT KEY FILTER FROM JCL?                    
         JZ    UUNI10                                                           
         GOTO1 =V(HEXIN),DMCB,DXKEY,IOKEYSAV,40                                 
         CLC   0(20,R2),IOKEYSAV                                                
         JNE   YES                                                              
*                                                                               
UUNI10   GOTO1 AFILTUUN                                                         
         JNE   YES                                                              
*                                                                               
         BRAS  RE,GETCLT                                                        
         BRAS  RE,GETEST                                                        
         BRAS  RE,GETNTI                                                        
         BRAS  RE,GETPROS          GET PROFILES FOR DEMO LOOKUPS                
*                                                                               
         GOTO1 AINITUNI                                                         
*                                                                               
         BRAS  RE,BLDUPARM         BUILD UNIT/UAD UPDATE PARMS TO ROUTS         
*                                                                               
         L     R8,ABIGWORK                                                      
         GOTO1 VNTTUNIC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
*                                                                               
UUNI30   GOTO1 VNTTUNIC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    UUNIX                                                            
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    UUNI40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
UUNI40   CLI   (NTTUNACT-NTTUND)(RF),C'A'       IF ADD SKIP TO PUT              
         JE    UUNI70                                                           
*                                                                               
         TM    NURSTAT,X'80'       IS THIS UNIT DELETED?                        
         BO    UUNI45                                                           
*                                                                               
         MVI   COPYFLAG,X'01'                                                   
*                                                                               
         CLC   (NTTUNTYP-NTTUND)(5,RF),=C'06510'                                
         JNE   UUNI50                                                           
UUNI45   MVI   (NTTUNACT-NTTUND)(RF),C'D'                                       
         J     UUNI70                                                           
*                                                                               
UUNI50   CLI   COPYFLAG,X'01'                                                   
         JNE   UUNI30                                                           
*                                                                               
         L     RF,DXASQLB                                                       
         MVI   (NTTUNACT-NTTUND)(RF),C'A'                                       
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UUNI70   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
*                                                                               
         TM    NURSTAT,X'80'       IS THIS UNIT DELETED?                        
         BO    UUNIX                                                            
*                                                                               
         J     UUNI50                                                           
*                                                                               
UUNIX    DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
         DROP  R2,R5                                                            
*---------------------------------------------------------------------*         
* FILTER UNIT RECORD AT R2                                                      
*---------------------------------------------------------------------*         
FILTUNI  NTR1  BASE=*,LABEL=*                                                   
         USING NURECD,R2                                                        
*                                                                               
         CLI   0(R2),X'04'                                                      
         JNE   NO                                                               
         MVC   BYTE,NUKAM                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         MVC   UUNTKEY,0(R2)                                                    
*                                                                               
         CLI   NUKSUB,X'C1'        TRAFFIC UNIT?                                
         JNL   NO                                                               
*                                                                               
         CLI   DXFDATEC,0          ANY DATE RANGE?                              
         JE    *+14                                                             
         CLC   DXFDATEC,NUKDATE    DATE CARD < AIR DATE?                        
         JH    NO                  NO - SKIP THIS UNIT                          
*                                                                               
         CLI   DXTDATEC,0          ANY END DATE FILTER?                         
         JE    *+14                                                             
         CLC   DXTDATEC,NUKDATE    DATE CARD < AIR DATE?                        
         JL    NO                  YES - SKIP THIS UNIT                         
         DROP  R2                                                               
*                                                                               
FILTUNIX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* FILTER UNIT RECORD AT R2 - UPDATE MODE                                        
*---------------------------------------------------------------------*         
FILTUUN  NTR1  BASE=*,LABEL=*                                                   
         USING NURECD,R2                                                        
*                                                                               
         CLI   0(R2),X'04'                                                      
         JNE   NO                                                               
         MVC   BYTE,NUKAM                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         MVC   UUNTKEY,0(R2)                                                    
*                                                                               
         CLI   NUKSUB,X'C1'        TRAFFIC UNIT?                                
         JNL   NO                                                               
*                                                                               
         CLI   DXFDATEC,0          ANY DATE RANGE?                              
         JE    *+14                                                             
         CLC   DXFDATEC,NUKDATE    DATE CARD < AIR DATE?                        
         JH    NO                  NO - SKIP THIS UNIT                          
*                                                                               
         CLI   DXTDATEC,0          ANY END DATE FILTER?                         
         JE    *+14                                                             
         CLC   DXTDATEC,NUKDATE    DATE CARD < AIR DATE?                        
         JL    NO                  YES - SKIP THIS UNIT                         
         DROP  R2                                                               
*                                                                               
FILTUUNX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE UNIT RECORD                                                        
*---------------------------------------------------------------------*         
INITUNI  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTUNDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD UNIT ACTUAL DEMOS                                                        
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADUAD  NTR1  BASE=*,LABEL=*                                                   
         XC    FILTCLI,FILTCLI                                                  
         XC    FILTCLIE,FILTCLIE                                                
         CLI   DXCLIS,0             IS CLIENT RANGE REQUESTED                   
         JNE   LUAD02                                                           
         CLI   DXCLI,0                                                          
         JE    LUAD04                                                           
         OC    DXCLI,=C'   '                                                    
         GOTO1 =V(CLPACK),DMCB,DXCLI,FILTCLI                                    
         B     LUAD04                                                           
*                                                                               
* SET UP CLIENT RANGE FILTER                                                    
LUAD02   OC    DXCLIS,=C'   '                                                   
         GOTO1 =V(CLPACK),DMCB,DXCLIS,FILTCLI                                   
         OC    DXCLIE,=C'   '                                                   
         GOTO1 =V(CLPACK),DMCB,DXCLIE,FILTCLIE                                  
*                                                                               
LUAD04   BRAS  RE,BLDUINF          BUILD INFO FOR UNIT/UAD LOAD                 
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING NURECD,R2                                                        
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,SXDTAGB       AGENCY/MEDIA                                 
         OI    NUKAM,X'03'         NET                                          
         MVC   NUKCLT,FILTCLI                                                   
*                                                                               
         OC    DXKEY,DXKEY         UNIT KEY FILTER FROM JCL?                    
         JZ    LUAD07                                                           
         GOTO1 =V(HEXIN),DMCB,DXKEY,IOKEYSAV,40                                 
         MVC   IOKEY(20),IOKEYSAV                                               
*                                                                               
LUAD07   L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         MVC   PREVCLT,NUKCLT      1ST TIME THROUGH                             
         MVC   PREVEST,NUKEST                                                   
         BRAS  RE,GETCLT                                                        
         BRAS  RE,GETEST                                                        
         BRAS  RE,GETPROS          GET PROFILES FOR DEMO LOOKUPS                
*                                                                               
LUAD10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         CLI   0(R2),X'04'         FINISHED WITH AGENCY?                        
         JNE   YES                                                              
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   YES                                                              
*                                                                               
         GOTO1 AFILTUAD                                                         
         JNE   LUADSEQ                                                          
*                                                                               
         CLC   PREVCLT,NUKCLT      CHANGE IN CLIENT?                            
         BE    LUAD15                                                           
*                                                                               
         BRAS  RE,GETCLT           GET CLIENT RECORD                            
         BRAS  RE,GETEST                                                        
         BRAS  RE,GETPROS          GET PROFILES FOR DEMO LOOKUPS                
         MVC   PREVCLT,NUKCLT      GET CLIENT ON NEXT RECORD                    
         MVC   PREVEST,NUKEST                                                   
         B     LUAD20                                                           
*                                                                               
LUAD15   CLC   PREVEST,NUKEST      CHANGE IN ESTIMATE?                          
         BE    LUAD20                                                           
         BRAS  RE,GETEST                                                        
         MVC   PREVEST,NUKEST                                                   
*                                                                               
LUAD20   TM    DXFLAGS,DXFFUAD     FILTER ACTUAL DEMOS?                         
         BZ    LUAD22                                                           
         CLI   DXTDATEC,0          ANY END DATE FILTER?                         
         JE    *+14                                                             
         CLC   DXTDATEC,NUKDATE    DATE CARD < AIR DATE?                        
         JL    LUADSEQ             YES - SKIP THIS UNIT                         
         CLC   NUKDATE,DTETODAY    IS AIRDATE > TODAY'S DATE?                   
         JH    LUADSEQ                                                          
         CLC   NUKDATE,DTEFILT     IS AIRDATE < START DATE FILTER?              
         JL    LUADSEQ                                                          
*                                                                               
LUAD22   CLI   FILTCLIE,0          CLIENT RANGE FILTER                          
         JE    LUAD25                                                           
         CLC   NUKCLT,FILTCLIE                                                  
         JH    YES                                                              
         B     LUAD27                                                           
*                                                                               
LUAD25   CLI   FILTCLI,0           CLIENT FILTER?                               
         JE    *+14                                                             
         CLC   NUKCLT,FILTCLI                                                   
         JH    YES                                                              
*                                                                               
LUAD27   CLI   DXNET,0             NETWORK FILTER?                              
         JE    *+14                                                             
         CLC   NUKNET,DXNET                                                     
         JNE   LUADSEQ                                                          
*                                                                               
         BRAS  RE,GETNTI                                                        
*                                                                               
         MVC   PRIADDR,NUDA        A(UNIT)                                      
         GOTO1 AGETIT                                                           
         JNE   LUADSEQ                                                          
*                                                                               
         GOTO1 AINITUAD                                                         
*                                                                               
         BRAS  RE,BLDUPARM         BUILD UNIT/UAD LOAD PARMS TO ROUTS           
*                                                                               
         L     R8,ABIGWORK                                                      
         GOTO1 VNTTUADC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   NO                                                               
*                                                                               
LUAD30   GOTO1 VNTTUADC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LUADSEQ                                                          
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    LUAD40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LUAD40   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     LUAD30                                                           
*                                                                               
LUADSEQ  XC    IOKEY,IOKEY         GET NEXT UNIT RECORD                         
         MVC   IOKEY(20),UUNTKEY   RESTORE UNIT KEY                             
*                                                                               
         OC    DXKEY,DXKEY         UNIT KEY FILTER FROM JCL?                    
         JNZ   YES                 YES, EXIT                                    
*                                                                               
LUAD50   GOTO1 VDATAMGR,DMCB,DMRSEQ,UNTDIR,IOKEY,(R2),DMWORK                    
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                                                              
*                                                                               
         CLI   0(R2),X'04'         FINISHED WITH AGENCY?                        
         JNE   YES                                                              
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   YES                                                              
         CLI   NUKSUB,X'C1'        CHECK IF TRAFFIC UNIT                        
         JNL   LUAD50                                                           
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LUAD10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* UPDATE UNIT RECORD DATA                                                       
*---------------------------------------------------------------------*         
UPDTUAD  NTR1  BASE=*,LABEL=*                                                   
UUADX    DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* FILTER UNIT RECORD AT R2                                                      
*---------------------------------------------------------------------*         
FILTUAD  NTR1  BASE=*,LABEL=*                                                   
         USING NURECD,R2                                                        
*                                                                               
         CLI   0(R2),X'04'                                                      
         JNE   NO                                                               
         MVC   BYTE,NUKAM                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         MVC   UUNTKEY,0(R2)                                                    
*                                                                               
         CLI   NUKSUB,X'C1'        TRAFFIC UNIT?                                
         JNL   NO                                                               
*                                                                               
         CLI   DXFDATEC,0          ANY DATE RANGE?                              
         JE    *+14                                                             
         CLC   DXFDATEC,NUKDATE    DATE CARD < AIR DATE?                        
         JH    NO                  NO - SKIP THIS UNIT                          
*                                                                               
         CLI   DXTDATEC,0          ANY END DATE FILTER?                         
         JE    *+14                                                             
         CLC   DXTDATEC,NUKDATE    DATE CARD < AIR DATE?                        
         JL    NO                  YES - SKIP THIS UNIT                         
         DROP  R2                                                               
*                                                                               
FILTUADX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* FILTER UNIT RECORD AT R2 - UPDATE MODE                                        
*---------------------------------------------------------------------*         
FILTUUA  NTR1  BASE=*,LABEL=*                                                   
FILTUUAX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE UNIT RECORD                                                        
*---------------------------------------------------------------------*         
INITUAD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTADDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD UNIT ACTUAL DEMOS (NIGHTLY)                                              
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADUND  NTR1  BASE=*,LABEL=*                                                   
         MVI   COPYFLAG,0                                                       
*                                                                               
         BRAS  RE,GETDBK            GET DEMO BOOK VALUES                        
         JNE   YES                  NO BOOKS CAME IN, SO EXIT                   
*                                                                               
         BRAS  RE,BLDUINF           BUILD INFO FOR UNIT/UAD LOAD                
         BRAS  RE,BLDSTAT           BUILD TABLE OF STATION MASTER RECS          
*                                                                               
         LA    RF,DBKTAB                                                        
         ST    RF,ADBKPAR           FIRST PARENT RANGE                          
*                                                                               
LUND20   LA    R2,IOKEY             SET KEY TO READ UNIT RECORD ON FILE         
         USING NURECD,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,SXDTAGB        AGENCY/MEDIA                                
         OI    NUKAM,X'03'          NET                                         
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         BRAS  RE,GETUSUP           GET CLI/EST/PROF                            
*                                                                               
         LA    R2,IOKEY             SET KEY TO READ FIRST RECORD                
         XC    IOKEY,IOKEY          FOR PARENT RANGE IN THIS CLIENT             
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,SXDTAGB        AGENCY/MEDIA                                
         OI    NUKAM,X'03'          NET                                         
         MVC   NUKCLT,PREVCLT       CLI                                         
*                                                                               
         L     RF,ADBKPAR           SKIP READ W/ PARENT START DATE              
         MVC   NUKDATE,0(RF)                                                    
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LUND30   GOTO1 AFILTUND                                                         
         JE    LUND50                                                           
*                                                                               
         L     RF,ADBKPAR                                                       
         AHI   RF,5                 BUMP TO NEXT PARENT RANGE                   
LUND40   CLI   0(RF),X'FF'          NO MORE RANGES?                             
         JE    YES                  FINISHED UNITS - EXIT                       
         ST    RF,ADBKPAR           UPDATE POINTER                              
         CLI   4(RF),0              PARENT?                                     
         BE    LUND20                                                           
         AHI   RF,5                                                             
         B     LUND40                                                           
*                                                                               
LUND50   CLC   PREVCLT,NUKCLT       DOES THIS CLI HAVE A UNIT WITH              
         BE    LUND70               THIS PARENT DATE FILTER?                    
*                                                                               
LUND60   BRAS  RE,GETUSUP           GET CLI/EST/PROF                            
*                                                                               
         LA    R2,IOKEY                                                         
         USING NUKEY,R2                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,SXDTAGB        AGENCY/MEDIA                                
         OI    NUKAM,X'03'          NET                                         
         MVC   NUKCLT,PREVCLT       NEW CLI                                     
*                                                                               
         L     RF,ADBKPAR           GET PARENT DATE FILTER                      
         MVC   NUKDATE,0(RF)                                                    
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
         CLI   0(R2),X'04'         FINISHED WITH AGENCY?                        
         JNE   YES                                                              
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   YES                                                              
*                                                                               
         CLC   PREVCLT,NUKCLT       CHANGE IN CLIENT?                           
         JNE   LUND60               SKIPPED ENTIRE CLI - GET NEXT ONE           
*                                                                               
LUND70   GOTO1 AFILTUND                                                         
         JNE   LUNDSEQ                                                          
*                                                                               
         L     RF,ADBKPAR                                                       
         CLC   NUKDATE,2(RF)        UNIT WITHIN PARENT RANGE?                   
         BH    LUND100                                                          
         CLC   NUKDATE,0(RF)                                                    
         BL    LUND100                                                          
*                                                                               
         XC    DUB,DUB              GET POSTING TYPE FROM STATAB                
         MVC   DUB(L'NUKNET),NUKNET                                             
*                                                                               
         SR    R5,R5                                                            
         LH    R5,NUMBINST          NUM OF ENTRIES IN STATAB                    
         GOTO1 =V(BINSRCH),DMCB,(0,DUB),ASTATAB,(R5),5,(0,4),1500               
         CLI   DMCB,X'01'           RECORD NOT FOUND?                           
         BNE   *+6                  ERROR                                       
         DC    H'00'                                                            
*                                                                               
         L     RE,DMCB                                                          
         MVC   UPOSTTYP,4(RE)       NETWORK POSTING TYPE                        
*                                                                               
         L     RF,ADBKPAR                                                       
         AHI   RF,5                 BUMP TO 1ST CHILD ENTRY                     
LUND80   CLI   4(RF),X'FF'                                                      
         JE    LUNDSEQ                                                          
         CLI   4(RF),0              FINISHED CHILD ENTRIES?                     
         JE    LUNDSEQ              YES - NO MATCH, GET NEXT UNIT               
*                                                                               
         CLC   NUKDATE,2(RF)        WITHIN CHILD RANGE?                         
         BH    LUND90                                                           
         CLC   NUKDATE,0(RF)                                                    
         BL    LUND90                                                           
*                                                                               
         CLC   UPOSTTYP,4(RF)       SAME POSTING TYPE?                          
         JE    LUND110              FOUND MATCH, KEEP UNIT                      
LUND90   AHI   RF,5                 BUMP TO NEXT CHILD RANGE                    
         J     LUND80                                                           
*                                                                               
LUND100  LA    R2,IOKEY             BUMP TO NEXT CLIENT                         
         USING NUKEY,R2             FOR THIS PARENT RANGE                       
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,SXDTAGB        AGENCY/MEDIA                                
         OI    NUKAM,X'03'          NET                                         
         MVC   NUKCLT,PREVCLT       NEW CLI                                     
         MVI   NUKDATE,X'FF'        READ NEXT CLIENT                            
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
         J     LUND30                                                           
*                                                                               
LUND110  CLC   PREVCLT,NUKCLT       CHANGE IN CLIENT?                           
         BE    *+12                                                             
         BRAS  RE,GETUSUP           GET CLI/EST/PROF                            
         B     LUND120                                                          
*                                                                               
         CLC   PREVEST,NUKEST       CHANGE IN ESTIMATE?                         
         BE    LUND120                                                          
         BRAS  RE,GETEST                                                        
         MVC   PREVEST,NUKEST                                                   
*                                                                               
LUND120  BRAS  RE,GETNTI                                                        
         MVI   COPYFLAG,0                                                       
*                                                                               
         MVC   PRIADDR,NUDA         A(UNIT)                                     
         GOTO1 AGETIT                                                           
         JNE   LUNDSEQ                                                          
*                                                                               
         GOTO1 AINITUND                                                         
*                                                                               
         BRAS  RE,BLDUPARM          BUILD UNIT/UAD LOAD PARMS TO ROUTS          
*                                                                               
         L     R8,ABIGWORK                                                      
         GOTO1 VNTTUADC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
*                                                                               
LUND130  GOTO1 VNTTUADC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LUNDSEQ                                                          
*                                                                               
         L     RF,DXAXREC           A(RECORD)                                   
         CLI   SXDTPLFM,0                                                       
         JE    LUND140                                                          
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
         CLC   (NTTADTYP-NTTADD)(5,RF),=C'06521'                                
         JNE   LUND135                                                          
*                                                                               
         CLI   COPYFLAG,X'01'                                                   
         JE    LUND135                                                          
*                                                                               
         MVI   COPYFLAG,X'01'                                                   
         MVI   (NTTADACT-NTTADD)(RF),C'D'                                       
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         L     RF,DXASQLB                                                       
         MVI   (NTTADACT-NTTADD)(RF),C'A'                                       
         J     LUND140                                                          
*                                                                               
LUND135  MVI   (NTTADACT-NTTADD)(RF),C'D'                                       
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         L     RF,DXASQLB                                                       
         MVI   (NTTADACT-NTTADD)(RF),C'A'                                       
LUND140  GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     LUND130                                                          
*                                                                               
LUNDSEQ  XC    IOKEY,IOKEY          GET NEXT UNIT RECORD                        
         MVC   IOKEY(20),UUNTKEY                                                
LUND150  GOTO1 VDATAMGR,DMCB,DMRSEQ,UNTDIR,IOKEY,(R2),DMWORK                    
         CLC   IOKEY(2),0(R2)                                                   
         JNE   LUND30                                                           
*                                                                               
         CLI   0(R2),X'04'         FINISHED WITH AGENCY?                        
         JNE   YES                                                              
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   YES                                                              
         CLI   NUKSUB,X'C1'         SKIP TRAFFIC UNITS                          
         JNL   LUND150                                                          
*                                                                               
         OC    MAXIOS,MAXIOS        EXIT IF MAXIMUM IOS EXCEEDED                
         JNZ   LUND30                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* UPDATE UNIT RECORD DATA                                                       
*---------------------------------------------------------------------*         
UPDTUND  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOADUND                                                       
UUNDX    DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* FILTER UNIT RECORD AT R2                                                      
*---------------------------------------------------------------------*         
FILTUND  NTR1  BASE=*,LABEL=*                                                   
         USING NURECD,R2                                                        
*                                                                               
         CLI   0(R2),X'04'                                                      
         JNE   NO                                                               
         MVC   BYTE,NUKAM                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         MVC   UUNTKEY,0(R2)                                                    
*                                                                               
         CLI   NUKSUB,X'C1'        TRAFFIC UNIT?                                
         JNL   NO                                                               
*                                                                               
         CLI   DXFDATEC,0          ANY DATE RANGE?                              
         JE    *+14                                                             
         CLC   DXFDATEC,NUKDATE    DATE CARD < AIR DATE?                        
         JH    NO                  NO - SKIP THIS UNIT                          
*                                                                               
         CLI   DXTDATEC,0          ANY END DATE FILTER?                         
         JE    *+14                                                             
         CLC   DXTDATEC,NUKDATE    DATE CARD < AIR DATE?                        
         JL    NO                  YES - SKIP THIS UNIT                         
         DROP  R2                                                               
*                                                                               
FILTUNDX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE UNIT RECORD                                                        
*---------------------------------------------------------------------*         
INITUND  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTADDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* GET UNIT SUPPORT INFO                                                         
*---------------------------------------------------------------------*         
GETUSUP  NTR1  BASE=*,LABEL=*                                                   
         USING NURECD,R2                                                        
         BRAS  RE,GETCLT                                                        
         BRAS  RE,GETEST                                                        
         BRAS  RE,GETPROS           GET PROFILES FOR DEMO LOOKUPS               
         MVC   PREVCLT,NUKCLT       1ST TIME THROUGH                            
         MVC   PREVEST,NUKEST                                                   
         DROP  R2                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* GET TABLE OF DEMO BOOKS THAT CAME IN TODAY                                    
*---------------------------------------------------------------------*         
GETDBK   NTR1  BASE=*,LABEL=*                                                   
         XC    DXFDATEC,DXFDATEC                                                
         L     RE,=V(UTL)                                                       
         MVI   4(RE),X'0A'          CONTROL                                     
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,DTETODAY)                                  
*                                                                               
         OC    DXKEY,DXKEY         SPECIAL RE-RUN FROM JCL?                     
         BZ    GDBK10                                                           
         GOTO1 VDATCON,DMCB,(0,DXKEY),(2,DTETODAY)                              
*                                                                               
GDBK10   XC    NUMBINDM,NUMBINDM                                                
*                                                                               
         SR    R0,R0               A OF REC TO BE ADDED                         
         LA    R1,IO               A OF BINTBL                                  
         SR    R2,R2               NUM OF REC IN TBL,UPDATED BY BINSRCH         
         LA    R3,L'DBKB           LENGTH OF REC                                
         LA    R4,L'DBKB           DISP OF KEY INTO REC                         
         L     R5,=F'20'           MAX RECS IN BINTBL                           
         STM   R0,R5,BINDMCB                                                    
*                                                                               
         MVI   IO,X'FF'                                                         
*                                                                               
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R3,IOKEYSAV                                                      
         USING CTDURECD,R3                                                      
*                                                                               
         MVI   CTDURTYP,CTDURTYQ                                                
         MVI   CTDUSRTP,CTDUSRTQ                                                
         MVC   CTDULDAT,DXFDATEC    DATE FILTER?                                
         CLI   DXFDATEC,0                                                       
         BNE   *+10                                                             
         MVC   CTDULDAT,DTETODAY    TODAY'S DATE                                
*                                                                               
         MVC   IOKEYSA2,IOKEYSAV                                                
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,IOKEYSAV,IOKEYSAV,DMWORK             
         B     GDBK20                                                           
*                                                                               
GDBKSEQ  MVC   IOKEYSA2,IOKEYSAV                                                
         GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,IOKEYSAV,IOKEYSAV,DMWORK             
GDBK20   CLC   IOKEYSAV(CTDUBFLG-CTDUKEY),IOKEYSA2                              
         BNE   GDBK30                                                           
*                                                                               
         MVC   DBKB(2),CTDUSDAT     START DATE                                  
         MVC   DBKB+2(1),CTDUBFLG   MONTHLY/WEEKLY                              
         MVC   DBKB+3(2),CTDUEDAT   END DATE                                    
         MVC   DBKB+5(1),CTDUPTYP   POSTING TYPE                                
*                                                                               
         GOTO1 =V(BINSRCH),BINDMCB,(1,DBKB),IO                                  
*                                                                               
         SR    RE,RE                                                            
         LH    RE,NUMBINDM                                                      
         AHI   RE,1                                                             
         STH   RE,NUMBINDM                                                      
*                                                                               
         J     GDBKSEQ                                                          
*                                                                               
GDBK30   SR    RE,RE                                                            
         LH    RE,NUMBINDM                                                      
         MH    RE,=H'6'             L'DBKB                                      
         LA    RF,IO                                                            
         AR    RF,RE                                                            
         MVI   0(RF),X'FF'                                                      
*                                                                               
         L     RE,=V(UTL)                                                       
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         CLI   IO,X'FF'             ANY BOOKS COME IN TODAY?                    
         JE    NO                   NO - DON'T PROCESS                          
*                                                                               
         XC    DBKTAB,DBKTAB                                                    
*                                                                               
         LA    R2,IO                A(DEMOBOOK TABLE)                           
         ST    R2,ADBKORST                                                      
         LA    R4,DBKTAB            A(NEW DEMO PARENT/CHILD TABLE)              
         ST    R4,ADBKNEW                                                       
*                                                                               
         MVC   0(2,R4),0(R2)        INIT PARENT START DATE                      
         MVC   2(2,R4),3(R2)        INIT PARENT END DATE                        
         AHI   R2,L'DBKB                                                        
*                                                                               
GDBK40   CLI   0(R2),X'FF'          FINISHED W/ TABLE?                          
         BE    GDBK100                                                          
         CLC   0(2,R2),2(R4)        FALL IN THIS DATE RANGE?                    
         BH    *+12                                                             
         AHI   R2,L'DBKB                                                        
         B     GDBK40                                                           
*                                                                               
         L     RF,ADBKORST                                                      
         AHI   R4,5                                                             
GDBK50   CR    RF,R2                                                            
         BNL   GDBK60                                                           
         MVC   0(2,R4),0(RF)        START DATE                                  
         MVC   2(2,R4),3(RF)        END DATE                                    
         MVC   4(1,R4),5(RF)        POSTING TYPE                                
         AHI   R4,5                                                             
         AHI   RF,L'DBKB                                                        
         B     GDBK50                                                           
*                                                                               
GDBK60   ST    RF,ADBKORST                                                      
         L     R2,ADBKORST                                                      
         MVC   0(2,R4),0(R2)        NEW PARENT START DATE                       
         MVC   2(2,R4),3(R2)        NEW PARENT END DATE                         
         AHI   R2,L'DBKB                                                        
         B     GDBK40                                                           
*                                                                               
GDBK100  L     RF,ADBKORST          PUT IN FINAL ENTRIES                        
         AHI   R4,5                                                             
GDBK110  CR    RF,R2                                                            
         BNL   GDBK120                                                          
         MVC   0(2,R4),0(RF)        START DATE                                  
         MVC   2(2,R4),3(RF)        END DATE                                    
         MVC   4(1,R4),5(RF)        POSTING TYPE                                
         AHI   R4,5                                                             
         AHI   RF,L'DBKB                                                        
         B     GDBK110                                                          
*                                                                               
GDBK120  MVI   0(R4),X'FF'                                                      
*                                                                               
         J     YES                                                              
         DROP  R3                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* BUILD TABLE OF STATIONS/POSTING TYPES                                         
*---------------------------------------------------------------------*         
BLDSTAT  NTR1  BASE=*,LABEL=*                                                   
         XC    NUMBINST,NUMBINST                                                
*                                                                               
         SR    R0,R0               A OF REC TO BE ADDED                         
         L     R1,=A(VSTATAB)                                                   
         MVC   ASTATAB,0(R1)                                                    
         LA    R1,ASTATAB          A OF BINTBL                                  
         SR    R2,R2               NUM OF REC IN TBL,UPDATED BY BINSRCH         
         LA    R3,L'STATB          LENGTH OF REC                                
         LA    R4,L'STATB-1        DISP OF KEY INTO REC                         
         L     R5,=F'1500'         MAX RECS IN BINTBL                           
         STM   R0,R5,BINDMCB                                                    
*                                                                               
         LA    R2,IO               SET KEY TO READ FIRST RECORD                 
         USING STAREC,R2                                                        
         XC    IO(50),IO                                                        
         MVI   STAKTYPE,STAKTYPQ                                                
*                                                                               
         MVC   IOKEYSAV,IO                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,STAFIL,IO,IO,DMWORK                         
         B     BSTAT10                                                          
*                                                                               
BSTATSEQ MVC   IOKEYSAV,IO                                                      
         GOTO1 VDATAMGR,DMCB,DMRSEQ,STAFIL,IO,IO,DMWORK                         
BSTAT10  CLI   IOKEYSAV,STAKTYPQ                                                
         BNE   BSTATX                                                           
         CLC   STAKAGY,SXDTAGY                                                  
         BNE   BSTATSEQ                                                         
         CLC   STAKCLT(6),=C'000000'                                            
         BNE   BSTATSEQ                                                         
*                                                                               
         MVC   STATB(4),STAKCALL                                                
         MVC   STATB+4(1),SPTYPE                                                
         CLI   SOVBKTYP,0                                                       
         BE    *+10                                                             
         MVC   STATB+4(1),SOVBKTYP                                              
*                                                                               
         GOTO1 =V(BINSRCH),BINDMCB,(1,STATB),ASTATAB                            
*                                                                               
         SR    RE,RE                                                            
         LH    RE,NUMBINST                                                      
         AHI   RE,1                                                             
         STH   RE,NUMBINST                                                      
*                                                                               
         B     BSTATSEQ                                                         
*                                                                               
BSTATX   J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* BUILD TABLE OF DAYPARTS FOR UNIT LOAD                                         
*---------------------------------------------------------------------*         
BLDDAYP  NTR1  BASE=*,LABEL=*                                                   
         XC    NUMBINDP,NUMBINDP                                                
*                                                                               
         SR    R0,R0               A OF REC TO BE ADDED                         
         L     R1,=A(VDPTBL)                                                    
         MVC   ADPTBL,0(R1)                                                     
         LA    R1,ADPTBL           A OF BINTBL                                  
         SR    R2,R2               NUM OF REC IN TBL,UPDATED BY BINSRCH         
         LA    R3,5                LENGTH OF REC                                
         LA    R4,3                DISP OF KEY INTO REC                         
         L     R5,=F'1100'         MAX RECS IN BINTBL                           
         STM   R0,R5,BINDMCB                                                    
*                                                                               
BDAYP10  DS    0H                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING NDPTHDR,R2                                                       
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   NDPTKTY,NDPTKTYQ                                                 
         MVI   NDPTKST,NDPTKSTQ                                                 
*                                                                               
         MVC   NDPTAGM,SXDTAGB                                                  
         OI    NDPTAGM,X'03'       FOR NET                                      
*                                                                               
         XC    IOKEYSAV,IOKEYSAV                                                
         MVC   IOKEYSAV(20),IOKEY                                               
         OI    FLAGS,UNTFILEQ                                                   
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
         B     BDAYP30                                                          
*                                                                               
BDAYP20  DS    0H                                                               
         MVC   IOKEYSAV(20),IOKEY                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,UNTDIR,IOKEY,IOKEY,DMWORK                   
*                                                                               
BDAYP30  DS    0H                                                               
         CLC   IOKEYSAV(3),IOKEY   SAME AGY?                                    
         JNE   NO                                                               
*                                                                               
         XC    DAYPB,DAYPB                                                      
         MVC   DAYPB(2),NDPTCLT     CLIENT                                      
         MVC   DAYPB+2(1),NDPTDPTE    DAYPART EQUATE                            
         MVC   DAYPB+3(2),NDPTDPTA    ALPHA DAYPART                             
*                                                                               
         GOTO1 =V(BINSRCH),BINDMCB,(1,DAYPB),ADPTBL                             
*                                                                               
         SR    RE,RE                                                            
         LH    RE,NUMBINDP                                                      
         AHI   RE,1                                                             
         STH   RE,NUMBINDP                                                      
*                                                                               
         B     BDAYP20                                                          
*---------------------------------------------------------------------*         
* GET CLIENT RECORD                                                             
*---------------------------------------------------------------------*         
GETCLT   NTR1  BASE=*,LABEL=*                                                   
         USING NURECD,R2                                                        
*                                                                               
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R3,IOKEYSAV                                                      
         USING CLTHDR,R3                                                        
*                                                                               
         MVC   CKEYAM,NUKAM        AGENCY                                       
         MVC   CKEYCLT,NUKCLT      CLIENT                                       
         DROP  R2                                                               
*                                                                               
         L     R4,ACLTREC                                                       
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,IOKEYSAV,IOKEYSAV,DMWORK             
         GOTO1 VDATAMGR,DMCB,GETREC,SPTFIL,IOKEYSAV+14,(R4),DMWORK              
*                                                                               
         L     R3,ACLTREC                                                       
         MVC   SVCOFF,COFFICE      CLIENT OFFICE                                
         MVC   SVCPROF,CPROF                                                    
         GOTO1 =V(CLUNPK),DMCB,(CPROF+6,CKEYCLT),SVCLTA                         
         DROP  R3                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* GET ESTIMATE RECORD                                                           
*---------------------------------------------------------------------*         
GETEST   NTR1  BASE=*,LABEL=*                                                   
         USING NURECD,R2                                                        
*                                                                               
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R3,IOKEYSAV                                                      
         USING ESTHDR,R3                                                        
*                                                                               
         MVC   EKEYAM,NUKAM        AGY/MEDIA                                    
         MVC   EKEYCLT,NUKCLT      CLIENT                                       
         MVC   EKEYEST,NUKEST      ESTIMATE                                     
         MVC   EKEYPRD,=C'POL'     POL ESTIMATE                                 
         DROP  R2                                                               
*                                                                               
         L     R4,AESTREC                                                       
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,IOKEYSAV,IOKEYSAV,DMWORK             
         GOTO1 VDATAMGR,DMCB,GETREC,SPTFIL,IOKEYSAV+14,(R4),DMWORK              
*                                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* GET ALPHA DAYPART FROM 1 BYTE DAYPART EQUATE                                  
*---------------------------------------------------------------------*         
GETDPTA  NTR1  BASE=*,LABEL=*                                                   
         USING NURECD,R2                                                        
*                                                                               
         XC    UDPTA,UDPTA                                                      
*                                                                               
         SR    R5,R5                                                            
         LH    R5,NUMBINDP          NUM OF ENTRIES IN DAYP TABLE                
*                                                                               
*        FIND  CLIENT LEVEL DAYPART FIRST, IF NOT, THEN AGENCY                  
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,(0,DUB),ADPTBL,(R5),5,(0,3),1100                
         CLI   DMCB,X'01'           RECORD NOT FOUND?                           
         BNE   GETDPT20             TRY AGENCY LEVEL                            
*                                                                               
         XC    DUB(2),DUB           CLEAR OUT CLIENT PORTION                    
         GOTO1 =V(BINSRCH),DMCB,(0,DUB),ADPTBL,(R5),5,(0,3),1100                
         CLI   DMCB,X'01'           RECORD NOT FOUND?                           
         BNE   *+6                  ERROR                                       
         DC    H'00'                                                            
*                                                                               
GETDPT20 L     RE,DMCB                                                          
         MVC   UDPTA,3(RE)         SAVE AWAY ALPHA DAYPART                      
*                                                                               
GETDPTAX J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* BUILD PARMS TO PASS TO ROUTS FOR UNIT/UAD LOAD                                
*---------------------------------------------------------------------*         
BLDUPARM NTR1  BASE=*,LABEL=*                                                   
         CLC   SXDTTYP,=C'UND'     UNIT NIGHTLY DEMOS                           
         JE    BUPARM02                                                         
         USING NURECD,R2                                                        
         XC    DUB,DUB                                                          
         MVC   DUB(2),NUKCLT        CLIENT                                      
         MVC   DUB+2(1),NUKDP       DAYPART                                     
         BRAS  RE,GETDPTA          GET ALPHA DAYPART                            
         DROP  R2                                                               
*                                                                               
BUPARM02 L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         USING UINFOD,R8                                                        
*                                                                               
         MVC   UIUNTKEY,UUNTKEY     UNIT KEY                                    
         MVC   UICLOFF,SVCOFF       CLIENT OFFICE                               
         MVC   UICLPROF,SVCPROF     CLIENT PROFILES                             
         MVC   UIUNDAY,VUNDAY       A(UNDAY)                                    
         MVC   UITRPACK,VTRPACK     A(TRPACK)                                   
         MVC   UIDEMCON,VDEMCON     A(DEMCON)                                   
         MVC   UIDEFINE,VDEFINE     A(DEFINE)                                   
         MVC   UICLTREC,ACLTREC     A(CLIENT RECORD)                            
         MVC   UIESTREC,AESTREC     A(ESTIMATE RECORD)                          
         MVC   UIDPTA,UDPTA         ALPHA DAYPART                               
         MVC   UIN0PROF,UN0PROF     N0 PROFILE                                  
         MVC   UIN2PROF,UN2PROF     N2 PROFILE                                  
         MVC   UIN1PROF,UN1PROF     N1 PROFILE                                  
         MVC   UINTISTA,UNTISTA     NTI STATION                                 
         MVC   UIAGYFL2,UAGYFLG2    AGENCY FLAG 2                               
         MVC   UINTBLRD,=V(NETBLRDR)                                            
         DROP  R8                                                               
*                                                                               
BLDUPARX J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* BUILD INFORMATION FOR UNIT/UAD UPDATE                                         
*---------------------------------------------------------------------*         
BLDUUINF NTR1  BASE=*,LABEL=*                                                   
         L     R1,=A(VDPTBL)                                                    
         MVC   ADPTBL,0(R1)                                                     
         L     R1,=A(VCLTREC)                                                   
         MVC   ACLTREC,0(R1)                                                    
         L     R1,=A(VESTREC)                                                   
         MVC   AESTREC,0(R1)                                                    
*                                                                               
         BRAS  RE,GETAGY           GET AGENCY VALUES                            
*                                                                               
         TM    MYFLAG,GOTVDEM                                                   
         BNZ   BUUINFX                                                          
*                                                                               
         XC    PARM,PARM                                                        
         SR    R0,R0                                                            
         MVC   DUB,=CL8'T00A0F'    <--LOAD LIVE VERSION                         
         GOTO1 =V(LOADER),PARM,DUB,(R0)                                         
         MVC   VUNDAY,PARM+4                                                    
*                                                                               
         OC    VUNDAY,VUNDAY                                                    
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         XC    PARM,PARM                                                        
         SR    R0,R0                                                            
         MVI   PARM+7,QTRPACK                                                   
         MVC   PARM+4(4),=X'D9000AFE'  <--LOAD LIVE VERSION                     
         GOTO1 =V(CALLOFF),PARM                                                 
         CLI   PARM+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'00'                                                            
         MVC   VTRPACK,PARM                                                     
*                                                                               
         XC    PARM,PARM                                                        
         SR    R0,R0                                                            
         MVC   DUB,=CL8'T00AE0'    <--LOAD LIVE VERSION                         
         GOTO1 =V(LOADER),PARM,DUB,(R0)                                         
         MVC   VDEMCON,PARM+4                                                   
*                                                                               
         OC    VDEMCON,VDEMCON                                                  
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         XC    PARM,PARM                                                        
         SR    R0,R0                                                            
         MVC   DUB,=CL8'T00A26'    <--LOAD LIVE VERSION                         
         GOTO1 =V(LOADER),PARM,DUB,(R0)                                         
         MVC   VDEFINE,PARM+4                                                   
*                                                                               
         OC    VDEFINE,VDEFINE                                                  
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         OI    MYFLAG,GOTVDEM                                                   
*                                                                               
BUUINFX  J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* BUILD INFORMATION FOR UNIT/UAD LOADS                                          
*---------------------------------------------------------------------*         
BLDUINF  NTR1  BASE=*,LABEL=*                                                   
         L     R1,=A(VDPTBL)                                                    
         MVC   ADPTBL,0(R1)                                                     
         L     R1,=A(VCLTREC)                                                   
         MVC   ACLTREC,0(R1)                                                    
         L     R1,=A(VESTREC)                                                   
         MVC   AESTREC,0(R1)                                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,DUB)                                       
         MVC   DTETODAY,DUB                                                     
*                                                                               
         MVC   DTEFILT,DXFDATEC                                                 
         OC    DXFDATEC,DXFDATEC                                                
         BNZ   BLDUI10                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(0,DUB)                                       
         GOTO1 =V(ADDAY),DMCB,(C'M',DUB),WORK,-3                                
         GOTO1 VDATCON,DMCB,(0,WORK),(2,DTEFILT)                                
*                                                                               
BLDUI10  BRAS  RE,GETAGY           GET AGENCY VALUES                            
*                                                                               
         XC    PARM,PARM                                                        
         SR    R0,R0                                                            
         MVC   DUB,=CL8'T00A0F'    <--LOAD LIVE VERSION                         
         GOTO1 =V(LOADER),PARM,DUB,(R0)                                         
         MVC   VUNDAY,PARM+4                                                    
*                                                                               
         OC    VUNDAY,VUNDAY                                                    
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         XC    PARM,PARM                                                        
         SR    R0,R0                                                            
         MVI   PARM+7,QTRPACK                                                   
         MVC   PARM+4(4),=X'D9000AFE'  <--LOAD LIVE VERSION                     
         GOTO1 =V(CALLOFF),PARM                                                 
         CLI   PARM+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'00'                                                            
         MVC   VTRPACK,PARM                                                     
*                                                                               
         XC    PARM,PARM                                                        
         SR    R0,R0                                                            
         MVC   DUB,=CL8'T00AE0'    <--LOAD LIVE VERSION                         
         GOTO1 =V(LOADER),PARM,DUB,(R0)                                         
         MVC   VDEMCON,PARM+4                                                   
*                                                                               
         OC    VDEMCON,VDEMCON                                                  
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         XC    PARM,PARM                                                        
         SR    R0,R0                                                            
         MVC   DUB,=CL8'T00A26'    <--LOAD LIVE VERSION                         
         GOTO1 =V(LOADER),PARM,DUB,(R0)                                         
         MVC   VDEFINE,PARM+4                                                   
*                                                                               
         OC    VDEFINE,VDEFINE                                                  
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     RF,=F'200000000'                                                 
         ST    RF,DUB                                                           
         MVC   MAXIOS,DUB                                                       
*                                                                               
BLDUINFX J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* GET AGENCY VALUES                                                             
*---------------------------------------------------------------------*         
GETAGY   NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEYSAV,IOKEYSAV                                                
         MVI   UAGYFLG2,0                                                       
*                                                                               
         LA    R3,IOKEYSAV                                                      
         USING AGYHDRD,R3                                                       
*                                                                               
         MVI   AGYKTYPE,AGYKTYPQ                                                
         MVC   AGYKAGY,SXDTAGY                                                  
*                                                                               
         MVC   IOKEYSA2,IOKEYSAV                                                
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,IOKEYSAV,IOKEYSAV,DMWORK             
         CLC   IOKEYSAV(13),IOKEYSA2                                            
         BNE   GETAGYX                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,GETREC,SPTFIL,IOKEYSAV+14,IO,DMWORK                
*                                                                               
         LA    R3,IO                                                            
         MVC   UAGYFLG2,AGYFLAG2   SAVE OFF AGY FLAG                            
         DROP  R3                                                               
*                                                                               
GETAGYX  DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* GET NTI STATION                                                               
*---------------------------------------------------------------------*         
GETNTI   NTR1  BASE=*,LABEL=*                                                   
         USING NURECD,R2                                                        
*                                                                               
         XC    IOKEYSAV,IOKEYSAV                                                
         LA    R3,IOKEYSAV                                                      
         USING SLSRECD,R3                                                       
*                                                                               
         XC    UNTISTA,UNTISTA                                                  
*                                                                               
         MVC   IOKEYSAV(2),=XL2'0D75'                                           
         MVC   SLSKAGMD,NUKAM     AGENCY                                        
         MVC   SLSKSTA(4),NUKNET  NETWORK                                       
         MVI   SLSKSTA+4,C'N'                                                   
         DROP  R2                                                               
*                                                                               
         MVC   IOKEYSA2,IOKEYSAV                                                
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,IOKEYSAV,IOKEYSAV,DMWORK             
         CLC   IOKEYSAV(13),IOKEYSA2                                            
         BNE   GETNTIX                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,GETREC,SPTFIL,IOKEYSAV+14,IO,DMWORK                
*                                                                               
         LA    R3,IO                                                            
         MVC   UNTISTA,SLSNTI     NTI STATION                                   
         DROP  R3                                                               
*                                                                               
GETNTIX  DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* GET PROFILES FOR DEMO LOOKUPS                                                 
*---------------------------------------------------------------------*         
GETPROS  NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEYSAV,IOKEYSAV                                                
         MVC   IOKEYSAV(3),=C'S0N'                                              
         MVC   IOKEYSAV+4(2),SXDTAGY                                            
         MVI   IOKEYSAV+6,C'N'                                                  
         MVC   IOKEYSAV+7(3),SVCLTA                                             
         MVI   IOKEYSAV+10,C'*'                                                 
         MVC   IOKEYSAV+11(1),SVCOFF                                            
*                                                                               
         L     RE,=V(UTL)                                                       
         MVI   4(RE),X'0A'         CONTROL                                      
*                                                                               
         MVI   IOKEYSAV+3,C'0'                                                  
         GOTO1 =V(GETPROF),DMCB,IOKEYSAV,WORK,VDATAMGR                          
         MVI   IOKEYSAV+3,C'2'                                                  
         GOTO1 =V(GETPROF),DMCB,IOKEYSAV,WORK+16,VDATAMGR                       
         MVI   IOKEYSAV+3,C'1'                                                  
         GOTO1 =V(GETPROF),DMCB,IOKEYSAV,WORK+32,VDATAMGR                       
*                                                                               
         MVC   UN0PROF,WORK                                                     
         MVC   UN2PROF,WORK+16                                                  
         MVC   UN1PROF,WORK+32                                                  
*                                                                               
         L     RE,=V(UTL)                                                       
         MVC   4(1,RE),DXSENUM                                                  
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD NET GOAL RECORDS                                                         
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADGOL  NTR1  BASE=*,LABEL=*                                                   
         CLI   SXDTVER,5                                                        
         JNL   YES                                                              
*                                                                               
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY             SET KEY TO READ FIRST RECORD                
         USING GOALHDRD,R2                                                      
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   GKEYTYPE,X'02'                                                   
         MVC   GKEYAM,SXDTAGB       AGENCY/MEDIA                                
         OI    GKEYAM,X'03'         NET                                         
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGOL10   TM    DMCB+8,X'80'         ALL DONE IF EOF                             
         JO    YES                                                              
         GOTO1 AFILTGOL                                                         
         JNE   LGOLSEQ                                                          
*                                                                               
LGOL20   MVC   PRIADDR,GKDA                                                     
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         GOTO1 AINITGOL                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),IOKEYSAV    PASS GOAL KEY                               
         L     RF,=V(GETBROAD)                                                  
         STCM  RF,15,50(R8)                                                     
*                                                                               
         GOTO1 VNTTGOLC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
LGOL30   GOTO1 VNTTGOLC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LGOLSEQ                                                          
*                                                                               
         L     RF,DXAXREC           A(RECORD)                                   
         CLI   SXDTPLFM,0                                                       
         JE    LGOL40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LGOL40   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC              PUT RECORD & DECREMENT IO COUNT             
         JE    LGOL30                                                           
         J     YES                                                              
*                                                                               
LGOLSEQ  XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'GKEY),0(R2)                                              
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGOL50   GOTO1 VDATAMGR,DMCB,DMRSEQ,SPTDIR,IOKEY,(R2),DMWORK                    
         CLC   IOKEY(GKEYCLT-GKEY),0(R2)                                        
         JNE   YES                                                              
         OC    MAXIOS,MAXIOS        EXIT IF MAXIMUM IOS EXCEEDED                
         JNZ   LGOL10                                                           
*                                                                               
LGOLX    J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* UPDATE GOAL RECORD DATA                                                       
*---------------------------------------------------------------------*         
UPDTGOL  NTR1  BASE=*,LABEL=*                                                   
         CLI   SXDTVER,5                                                        
         JNL   YES                                                              
*                                                                               
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING GOALHDRD,R2                                                      
*                                                                               
         GOTO1 AFILTGOL                                                         
         JNE   YES                                                              
         GOTO1 AINITGOL                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),0(R2)       PASS GOAL KEY                               
         L     RF,=V(GETBROAD)                                                  
         STCM  RF,15,50(R8)                                                     
*                                                                               
         GOTO1 VNTTGOLC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
UGOL10   GOTO1 VNTTGOLC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         BE    UGOLX                                                            
*                                                                               
         L     RF,DXAXREC           A(RECORD)                                   
         CLI   SXDTPLFM,0                                                       
         BE    UGOL20                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
UGOL20   CLI   (NTTGHACT-NTTGHDD)(RF),C'A'   ADD?                               
         BE    UGOL50                                                           
         TM    GCNTRLS,X'80'        GOAL DELETED?                               
         BO    UGOL30                                                           
*                                                                               
         MVI   COPYFLAG,X'01'                                                   
         CLC   (NTTGHTYP-NTTGHDD)(5,RF),=C'06516'                               
         BNE   UGOL40                                                           
UGOL30   MVI   (NTTGHACT-NTTGHDD)(RF),C'D'                                      
         B     UGOL50                                                           
*                                                                               
UGOL40   CLI   COPYFLAG,X'01'                                                   
         BNE   UGOL10                                                           
         L     RF,DXASQLB           RF=A(CONVERTED RECORD)                      
         MVI   (NTTGHACT-NTTGHDD)(RF),C'A'                                      
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UGOL50   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC              PUT RECORD & DECREMENT IO COUNT             
         JNE   YES                                                              
         TM    GCNTRLS,X'80'        GOAL DELETED?                               
         BZ    UGOL40                                                           
*                                                                               
UGOLX    J     YES                                                              
         DROP  R2,R5                                                            
         LTORG                                                                  
*---------------------------------------------------------------------*         
* FILTER GOAL RECORD AT R2                                                      
*---------------------------------------------------------------------*         
FILTGOL  NTR1  BASE=*,LABEL=*                                                   
         USING GOALHDRD,R2                                                      
*                                                                               
         CLI   0(R2),X'02'                                                      
         JNE   NO                                                               
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         TM    GKEYAGY,GKEYTAR      SKIP PLANNED GOALS                          
         JO    NO                                                               
*                                                                               
FILTGOLX J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE GOAL RECORD                                                        
*---------------------------------------------------------------------*         
INITGOL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTGHDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD PRODUCT GROUP RECORDS                                                    
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADPDG  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING PRGRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,SXDTAGB    AGENCY/MEDIA                                 
         OI    PRGKAGMD,X'03'      NET                                          
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPDG10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AFILTPDG                                                         
         JNE   YES                                                              
*                                                                               
         OC    6(2,R2),6(R2)       ANY GROUP NUMBER?                            
         JZ    LPDGSEQ                                                          
*                                                                               
LPDG20   DS    0H                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         GOTO1 AINITPDG                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),IOKEYSAV   PASS GOAL KEY                                
*                                                                               
         GOTO1 VNTTPDGC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
*                                                                               
LPDG30   DS    0H                                                               
         GOTO1 VNTTPDGC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LPDGSEQ                                                          
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    LPDG40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LPDG40   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LPDG30                                                           
*                                                                               
LPDGSEQ  DS    0H                  GET NEXT GOAL RECORD                         
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(13),IOKEYSAV  RESTORE GOAL KEY                             
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPDG50   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,SPTDIR,IOKEY,(R2),DMWORK                    
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPDG10                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE PRODUCT GROUP DATA                                                     
*---------------------------------------------------------------------*         
UPDTPDG  NTR1  BASE=*,LABEL=*                                                   
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTPDG                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPDG                                                         
*                                                                               
UPDG25   DS    0H                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),IOKEYSAV   PASS GOAL KEY                                
*                                                                               
         GOTO1 VNTTPDGC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
*                                                                               
UPDG30   DS    0H                                                               
         GOTO1 VNTTPDGC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    UPDGX                                                            
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    UPDG40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
UPDG40   DS    0H                                                               
         CLI   (NTTPHACT-NTTPHD)(RF),C'A'       IF ADD SKIP TO PUT              
         JE    UPDG70                                                           
*                                                                               
         TM    15(R2),X'80'        IS THIS GOAL DELETED?                        
         BO    UPDG45                                                           
*                                                                               
         MVI   COPYFLAG,X'01'                                                   
*                                                                               
         CLC   (NTTPHTYP-NTTPHD)(5,RF),=C'06518'                                
         JNE   UPDG50                                                           
UPDG45   MVI   (NTTPHACT-NTTPHD)(RF),C'D'                                       
         J     UPDG70                                                           
*                                                                               
UPDG50   DS    0H                                                               
         CLI   COPYFLAG,X'01'                                                   
         JNE   UPDG30                                                           
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   (NTTPHACT-NTTPHD)(RF),C'A'                                       
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UPDG70   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
*                                                                               
         TM    15(R2),X'80'        IS THIS GOAL DELETED?                        
         BO    UPDGX                                                            
*                                                                               
         J     UPDG50                                                           
*                                                                               
UPDGX    DS    0H                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*---------------------------------------------------------------------*         
* FILTER PRODUCT GROUP RECORDS AT R2                                            
*---------------------------------------------------------------------*         
FILTPDG  NTR1  BASE=*,LABEL=*                                                   
         CLC   0(2,R2),=X'0D01'                                                 
         JNE   NO                                                               
         MVC   BYTE,2(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
*                                                                               
         MVC   IOKEYSAV,0(R2)                                                   
*                                                                               
FILTPDGX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE PRODUCT GROUP RECORDS                                              
*---------------------------------------------------------------------*         
INITPDG  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTTPHDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD STATION GROUP RECORDS                                                    
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADSGR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING GRPRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVC   GRPKTYP(2),=X'0D05'                                              
         MVC   GRPKAGMD,SXDTAGB    AGENCY/MEDIA                                 
         OI    GRPKAGMD,X'03'      NET                                          
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSGR10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AFILTSGR                                                         
         JE    *+8                                                              
         J     YES                                                              
*                                                                               
         OC    GRPKCODE,GRPKCODE   JUST WANT DEFINITION RECORD                  
         JNZ   LSGR50                                                           
*                                                                               
LSGR20   DS    0H                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         GOTO1 AINITSGR                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),IOKEYSAV   PASS GOAL KEY                                
*                                                                               
         GOTO1 VNTTSGRC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
*                                                                               
LSGR30   DS    0H                                                               
         GOTO1 VNTTSGRC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LSGRSEQ                                                          
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    LSGR40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LSGR40   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LSGR30                                                           
*                                                                               
LSGRSEQ  DS    0H                  GET NEXT GOAL RECORD                         
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(13),IOKEYSAV  RESTORE GOAL KEY                             
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSGR50   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,SPTDIR,IOKEY,(R2),DMWORK                    
*                                                                               
         CLC   IOKEY(3),0(R2)                                                   
         JNE   YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSGR10                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE STATION GROUP DATA                                                     
*---------------------------------------------------------------------*         
UPDTSGR  NTR1  BASE=*,LABEL=*                                                   
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTSGR                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITSGR                                                         
*                                                                               
USGR25   DS    0H                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),IOKEYSAV   PASS GOAL KEY                                
*                                                                               
         GOTO1 VNTTSGRC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
*                                                                               
USGR30   DS    0H                                                               
         GOTO1 VNTTSGRC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    USGRX                                                            
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    USGR40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
USGR40   DS    0H                                                               
*        CLI   (NTSGDACT-NTSGDD)(RF),C'A'       IF ADD SKIP TO PUT              
*        JE    USGR70                                                           
*                                                                               
         TM    15(R2),X'80'        IS THIS DELETED?                             
         BZ    USGR45                                                           
         MVI   (NTSGDACT-NTSGDD)(RF),C'D'                                       
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     USGRX                                                            
*                                                                               
USGR45   MVI   (NTSGDACT-NTSGDD)(RF),C'D'                                       
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   (NTSGDACT-NTSGDD)(RF),C'A'                                       
USGR70   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     USGR30                                                           
*                                                                               
USGRX    DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* FILTER PRODUCT GROUP RECORDS AT R2                                            
*---------------------------------------------------------------------*         
FILTSGR  NTR1  BASE=*,LABEL=*                                                   
         CLC   0(2,R2),=X'0D05'                                                 
         JNE   NO                                                               
         MVC   BYTE,2(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
*                                                                               
         MVC   IOKEYSAV,0(R2)                                                   
*                                                                               
FILTSGRX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE PRODUCT GROUP RECORDS                                              
*---------------------------------------------------------------------*         
INITSGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTSGDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD CLIENT  GROUP RECORDS                                                    
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADCGR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING CLGRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVC   CLGKTYP(2),=X'0D06'                                              
         MVC   CLGKAGMD,SXDTAGB    AGENCY/MEDIA                                 
*        OI    CLGKAGMD,X'03'      NET                                          
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCGR10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AFILTCGR                                                         
         JE    *+8                                                              
         J     YES                                                              
*                                                                               
         OC    CLGKGRP,CLGKGRP     JUST WANT DEFINITION RECORD                  
         JNZ   LCGR50                                                           
*                                                                               
LCGR20   DS    0H                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         GOTO1 AINITCGR                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),IOKEYSAV   PASS GOAL KEY                                
*                                                                               
         GOTO1 VNTTCGRC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
*                                                                               
LCGR30   DS    0H                                                               
         GOTO1 VNTTCGRC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LCGRSEQ                                                          
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    LCGR40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LCGR40   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LCGR30                                                           
*                                                                               
LCGRSEQ  DS    0H                  GET NEXT GOAL RECORD                         
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(13),IOKEYSAV  RESTORE GOAL KEY                             
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCGR50   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,SPTDIR,IOKEY,(R2),DMWORK                    
*                                                                               
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                                                              
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCGR10                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* UPDATE CLIENT  GROUP DATA                                                     
*---------------------------------------------------------------------*         
UPDTCGR  NTR1  BASE=*,LABEL=*                                                   
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTCGR                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITCGR                                                         
*                                                                               
UCGR25   DS    0H                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),IOKEYSAV   PASS GOAL KEY                                
*                                                                               
         GOTO1 VNTTCGRC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
*                                                                               
UCGR30   DS    0H                                                               
         GOTO1 VNTTCGRC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    UCGRX                                                            
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    UCGR40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
UCGR40   DS    0H                                                               
*        CLI   (NTCGDACT-NTCGDD)(RF),C'A'       IF ADD SKIP TO PUT              
*        JE    UCGR70                                                           
*                                                                               
         TM    15(R2),X'80'        IS THIS DELETED?                             
         BZ    UCGR45                                                           
         MVI   (NTCGDACT-NTCGDD)(RF),C'D'                                       
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UCGRX                                                            
*                                                                               
UCGR45   MVI   (NTCGDACT-NTCGDD)(RF),C'D'                                       
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   (NTCGDACT-NTCGDD)(RF),C'A'                                       
UCGR70   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     UCGR30                                                           
*                                                                               
UCGRX    DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* FILTER CLIENT  GROUP RECORDS AT R2                                            
*---------------------------------------------------------------------*         
FILTCGR  NTR1  BASE=*,LABEL=*                                                   
         CLC   0(2,R2),=X'0D06'                                                 
         JNE   NO                                                               
         MVC   BYTE,2(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
*                                                                               
         MVC   IOKEYSAV,0(R2)                                                   
*                                                                               
FILTCGRX DS    0H                                                               
         J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE PRODUCT GROUP RECORDS                                              
*---------------------------------------------------------------------*         
INITCGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTCGDDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD NET GOAL RECORDS                                                         
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADGXL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY             SET KEY TO READ FIRST RECORD                
         USING GOALHDRD,R2                                                      
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   GXKEYTYP,X'02'                                                   
         MVC   GXKEYAM,SXDTAGB      AGENCY/MEDIA                                
         OI    GXKEYAM,X'03'        NET                                         
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGXL10   TM    DMCB+8,X'80'         ALL DONE IF EOF                             
         JO    YES                                                              
         GOTO1 AFILTGXL                                                         
         JNE   LGXLSEQ                                                          
*                                                                               
LGXL20   MVC   PRIADDR,GXKDA                                                    
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         GOTO1 AINITGXL                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),0(R2)       PASS GOAL KEY                               
         L     RF,=V(GETBROAD)                                                  
         STCM  RF,15,50(R8)                                                     
*                                                                               
         GOTO1 VNTTGXLC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
LGXL30   GOTO1 VNTTGXLC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LGXLSEQ                                                          
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    LGXL40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LGXL40   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JE    LGXL30                                                           
         J     YES                                                              
*                                                                               
LGXLSEQ  XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'GXKEY),0(R2)                                             
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGXL50   GOTO1 VDATAMGR,DMCB,DMRSEQ,XSPDIR,IOKEY,(R2),DMWORK                    
         CLC   IOKEY(GXKEYCLT-GXKEY),0(R2)                                      
         JNE   YES                                                              
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LGXL10                                                           
*                                                                               
LGXLX    J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* UPDATE GOAL RECORD DATA                                                       
*---------------------------------------------------------------------*         
UPDTGXL  NTR1  BASE=*,LABEL=*                                                   
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,XSPFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING GOALHDRD,R2                                                      
*                                                                               
         GOTO1 AFILTGXL                                                         
         JNE   YES                                                              
         GOTO1 AINITGXL                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),0(R2)       PASS GOAL KEY                               
         L     RF,=V(GETBROAD)                                                  
         STCM  RF,15,50(R8)                                                     
*                                                                               
         GOTO1 VNTTGXLC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
UGXL10   GOTO1 VNTTGXLC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         BE    UGXLX                                                            
*                                                                               
         L     RF,DXAXREC           A(RECORD)                                   
         CLI   SXDTPLFM,0                                                       
         BE    UGXL20                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
UGXL20   CLI   (NTGXHACT-NTGXHDD)(RF),C'A'                                      
         BE    UGXL50                                                           
         TM    GXRCNTRL,X'80'       GOAL DELETED?                               
         BO    UGXL30                                                           
*                                                                               
         MVI   COPYFLAG,X'01'                                                   
         CLC   (NTGXHTYP-NTGXHDD)(5,RF),=C'06529'                               
         BNE   UGXL40                                                           
UGXL30   MVI   (NTGXHACT-NTGXHDD)(RF),C'D'                                      
         B     UGXL50                                                           
*                                                                               
UGXL40   CLI   COPYFLAG,X'01'                                                   
         BNE   UGXL10                                                           
         L     RF,DXASQLB           RF=A(CONVERTED RECORD)                      
         MVI   (NTGXHACT-NTGXHDD)(RF),C'A'                                      
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UGXL50   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         TM    GXRCNTRL,X'80'       GOAL DELETED?                               
         BZ    UGXL40                                                           
*                                                                               
UGXLX    J     YES                                                              
         DROP  R2,R5                                                            
         LTORG                                                                  
*---------------------------------------------------------------------*         
* FILTER GOAL RECORD AT R2                                                      
*---------------------------------------------------------------------*         
FILTGXL  NTR1  BASE=*,LABEL=*                                                   
         USING GOALHDRD,R2                                                      
*                                                                               
         CLI   0(R2),X'02'                                                      
         JNE   NO                                                               
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         TM    GXKEYAGY,GKEYNHRQ   SKIP HISTORY GOALS                           
         JO    NO                                                               
*                                                                               
FILTGXLX J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE GOAL RECORD                                                        
*---------------------------------------------------------------------*         
INITGXL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTGXHDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD NET FLIGHT RECORDS                                                       
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADFLT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY             SET KEY TO READ FIRST RECORD                
         USING WFLIGHTD,R2                                                      
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVI   WFKTYP,WFKTYPQ                                                   
         MVI   WFKSTYP,WFKSTYPQ                                                 
         MVC   WFKAM,SXDTAGB        AGENCY/MEDIA                                
         OI    WFKAM,X'03'          NET                                         
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LFLT10   TM    DMCB+8,X'80'         ALL DONE IF EOF                             
         JO    YES                                                              
         GOTO1 AFILTFLT                                                         
         JNE   LFLTSEQ                                                          
*                                                                               
LFLT20   MVC   PRIADDR,WFKDA                                                    
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         GOTO1 AINITFLT                                                         
*                                                                               
         XC    UDPTA,UDPTA                                                      
         XC    DUB,DUB                                                          
         MVC   DUB(2),WFKCLT        CLIENT                                      
         MVC   DUB+2(1),WFIDPT      DAYPART                                     
         CLI   WFIDPT,0                                                         
         JE    *+8                                                              
         BRAS  RE,GETDPTA                                                       
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),0(R2)       PASS FLIGHT KEY                             
*                                                                               
         GOTO1 VNTTFLTC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
LFLT30   GOTO1 VNTTFLTC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LFLTSEQ                                                          
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    LFLT40                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LFLT40   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JE    LFLT30                                                           
         J     YES                                                              
*                                                                               
LFLTSEQ  XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'WFKEY),0(R2)                                             
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LFLT50   GOTO1 VDATAMGR,DMCB,DMRSEQ,XSPDIR,IOKEY,(R2),DMWORK                    
         CLC   IOKEY(WFKCLT-WFKEY),0(R2)                                        
         JNE   YES                                                              
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LFLT10                                                           
*                                                                               
LFLTX    J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* UPDATE FLIGHT                                                                 
*---------------------------------------------------------------------*         
UPDTFLT  NTR1  BASE=*,LABEL=*                                                   
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,XSPFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING WFLIGHTD,R2                                                      
*                                                                               
         GOTO1 AFILTFLT                                                         
         JNE   YES                                                              
         GOTO1 AINITFLT                                                         
*                                                                               
         XC    UDPTA,UDPTA                                                      
         XC    DUB,DUB                                                          
         MVC   DUB(2),WFKCLT        CLIENT                                      
         MVC   DUB+2(1),WFIDPT      DAYPART                                     
         CLI   WFIDPT,0                                                         
         JE    *+8                                                              
         BRAS  RE,GETDPTA                                                       
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   0(42,R8),0(R2)       PASS FLIGHT KEY                             
         MVC   50(L'UDPTA,R8),UDPTA    DAYPART                                  
*                                                                               
         GOTO1 VNTTFLTC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
UFLT10   GOTO1 VNTTFLTC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         BE    UFLTX                                                            
*                                                                               
         L     RF,DXAXREC           A(RECORD)                                   
         CLI   SXDTPLFM,0                                                       
         BE    UFLT20                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
UFLT20   CLI   (NTFLTACT-NTFLTD)(RF),C'A'                                       
         BE    UFLT50                                                           
         TM    WFRCNTRL,X'80'       GOAL DELETED?                               
         BO    UFLT30                                                           
*                                                                               
         MVI   COPYFLAG,X'01'                                                   
         CLC   (NTFLTTYP-NTFLTD)(5,RF),=C'06532'                                
         BE    UFLT30                                                           
         CLC   (NTFLTTYP-NTFLTD)(5,RF),=C'06533'                                
         BNE   UFLT40                                                           
UFLT30   MVI   (NTFLTACT-NTFLTD)(RF),C'D'                                       
         B     UFLT50                                                           
*                                                                               
UFLT40   CLI   COPYFLAG,X'01'                                                   
         BNE   UFLT10                                                           
         L     RF,DXASQLB           RF=A(CONVERTED RECORD)                      
         MVI   (NTFLTACT-NTFLTD)(RF),C'A'                                       
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UFLT50   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         TM    WFRCNTRL,X'80'       GOAL DELETED?                               
         BZ    UFLT40                                                           
*                                                                               
UFLTX    J     YES                                                              
         DROP  R2,R5                                                            
         LTORG                                                                  
*---------------------------------------------------------------------*         
* FILTER FLIGHTS                                                                
*---------------------------------------------------------------------*         
FILTFLT  NTR1  BASE=*,LABEL=*                                                   
         USING WFLIGHTD,R2                                                      
*                                                                               
         CLC   0(2,R2),=X'0E0C'                                                 
         JNE   NO                                                               
         MVC   BYTE,WFKAM                                                       
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
*                                                                               
FILTFLTX J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE FLIGHTS                                                            
*---------------------------------------------------------------------*         
INITFLT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTFLTDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD UCOM RECORDS                                                             
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADUCM  NTR1  BASE=*,LABEL=*                                                   
         XC    FILTCLI,FILTCLI                                                  
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         LA    R2,IOKEY             SET KEY TO READ FIRST RECORD                
         USING UCOMHDRD,R2                                                      
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVC   UCOMKTYP,=XL2'0D0C'                                              
         MVC   UCOMKAGY,SXDTAGB        AGENCY/MEDIA                             
         OI    UCOMKAGY,X'03'          NET                                      
         OI    UCOMCTYP,C'U'           UCOMMENT TYPE                            
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LUCM10   TM    DMCB+8,X'80'         ALL DONE IF EOF                             
         JO    YES                                                              
         GOTO1 AFILTUCM                                                         
         JNE   YES                                                              
*                                                                               
LUCM20   MVC   PRIADDR,14(R2)                                                   
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         GOTO1 AINITUCM                                                         
         L     R8,ABIGWORK                                                      
         MVC   0(20,R8),0(R2)       PASS UCOM KEY IN BIGWORK                    
*                                                                               
         OC    UCOMKPRD(4),UCOMKPRD   CHECK IF CLIENT LEVEL UCOM                
         BNZ   LUCM60                                                           
* SAVE OFF CLIENT UCOM ELEMENTS IN BIGWORK                                      
         LR    R4,R2                                                            
         LA    R4,(SUCMELEM-UCOMHDR)(R4)   FIRST CLIENT UCOM ELEMENT            
         L     R5,ABIGWORK                                                      
         LA    R5,20(R5)                                                        
*                                                                               
LUCM50   DS    0H                                                               
         CLI   0(R4),X'00'         EOR                                          
         BE    LUCMSEQ                                                          
*                                                                               
         ZIC   RF,1(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)                                                    
         AHI   RF,1                                                             
         AR    R5,RF                                                            
         AR    R4,RF                                                            
         MVI   0(R5),X'FF'                                                      
         B     LUCM50                                                           
*                                                                               
LUCM60   DS    0H                                                               
         GOTO1 VNTTUCMC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
LUCM70   GOTO1 VNTTUCMC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LUCMSEQ                                                          
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    LUCM90                                                           
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LUCM90   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JE    LUCM70                                                           
         J     YES                                                              
*                                                                               
****LUCMSEQ  XC    IOKEY,IOKEY                                                  
****         MVC   IOKEY(L'WFKEY),0(R2)                                         
****         GOTO1 AREADHI                                                      
****         JNE   NO                                                           
*                                                                               
LUCMSEQ  GOTO1 VDATAMGR,DMCB,DMRSEQ,SPTDIR,IOKEY,(R2),DMWORK                    
         CLC   IOKEY(UCOMCTYP-UCOMKEY),0(R2)                                    
         JNE   YES                                                              
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LUCM10                                                           
*                                                                               
LUCMX    J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* UPDATE UCOM                                                                   
*---------------------------------------------------------------------*         
UPDTUCM  NTR1  BASE=*,LABEL=*                                                   
         XC    FILTCLI,FILTCLI                                                  
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING UCOMHDRD,R2                                                      
* CHECK CLIENT LEVEL UPDATE                                                     
         OC    UCOMKPRD(4),UCOMKPRD                                             
         BNZ   *+10                                                             
         MVC   FILTCLI,UCOMKCLT                                                 
*                                                                               
         GOTO1 AFILTUCM                                                         
         JNE   YES                                                              
         GOTO1 AINITUCM                                                         
*                                                                               
* READ CLIENT LEVEL UCOM SAVE OFF ELEMENTS                                      
*                                                                               
         XC    IOKEY,IOKEY                                                      
*                                                                               
         MVC   IOKEY(UCOMKPRD-UCOMKEY),UCOMKTYP                                 
         L     R2,DXACPYB                                                       
         GOTO1 AREADHI              GET CLIENT LEVEL UCOM                       
         CLC   IOKEY(13),IOKEYSAV                                               
         JE    *+6                                                              
         DC    H'0'                 MUST BE CLIENT LEVEL RECORD                 
*                                                                               
         GOTO1 AFILTUCM                                                         
         JNE   YES                                                              
*                                                                               
UUCM10   MVC   PRIADDR,14(R2)                                                   
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         OC    UCOMKPRD(4),UCOMKPRD   CHECK IF CLIENT LEVEL UCOM                
         BNZ   UUCM60                                                           
*                                                                               
* SAVE OFF CLIENT UCOM ELEMENTS IN BIGWORK                                      
         LA    R2,(SUCMELEM-UCOMHDR)(R2)   FIRST CLIENT UCOM ELEMENT            
         L     R5,ABIGWORK                                                      
         LA    R5,20(R5)                                                        
*                                                                               
UUCM40   DS    0H                                                               
         CLI   0(R2),X'00'         EOR                                          
         BE    UUCM50                                                           
*                                                                               
         ZIC   RF,1(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R5),0(R2)                                                    
         AHI   RF,1                                                             
         AR    R5,RF                                                            
         AR    R2,RF                                                            
         MVI   0(R5),X'FF'                                                      
         J     UUCM40                                                           
*                                                                               
UUCM50   L     R2,DXACPYB                                                       
         OC    FILTCLI,FILTCLI          CLIENT LEVEL UPDATE                     
         BNZ   UUCMSEQ                  YES GET FIRST RECORD AFTER CLI          
         L     R5,DXARECB               RESET POINTER TO RECOVERY HDR           
         LA    R2,RECVHDR+L'RECVHDR     RESET RECOVERY POINTER                  
         MVC   0(20,R8),0(R2)           PASS UCOM KEY                           
*                                                                               
UUCM60   DS    0H                                                               
         GOTO1 VNTTUCMC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
UUCM80   GOTO1 VNTTUCMC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         BE    UUCM200                                                          
*                                                                               
         L     RF,DXAXREC           A(RECORD)                                   
         CLI   SXDTPLFM,0                                                       
         BE    UUCM100                                                          
*                                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
UUCM100  CLI   (NTUCMACT-NTUCMD)(RF),C'A'                                       
         BE    UUCM180                                                          
*                                                                               
         MVI   COPYFLAG,X'01'                                                   
         MVI   (NTUCMACT-NTUCMD)(RF),C'D'                                       
         B     UUCM180                                                          
*                                                                               
UUCM160  CLI   COPYFLAG,X'01'                                                   
         BNE   UUCM80                                                           
         L     RF,DXASQLB           RF=A(CONVERTED RECORD)                      
         MVI   (NTUCMACT-NTUCMD)(RF),C'A'                                       
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UUCM180  GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         TM    UCOMCTL,X'80'       UCOM                                         
         BZ    UUCM160                                                          
         B     UUCM80              PROCESS NEXT ELEMENT                         
*                                                                               
UUCM200  OC    FILTCLI,FILTCLI                                                  
         BZ    UUCMX                                                            
*                                                                               
UUCMSEQ  GOTO1 VDATAMGR,DMCB,DMRSEQ,SPTDIR,IOKEY,(R2),DMWORK                    
         CLC   IOKEY(UCOMKPRD-UCOMKEY),0(R2)                                    
         JNE   YES                                                              
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   UUCM10                                                           
*                                                                               
UUCMX    J     YES                                                              
         DROP  R2,R5                                                            
         LTORG                                                                  
*---------------------------------------------------------------------*         
* FILTER UCOM                                                                   
*---------------------------------------------------------------------*         
FILTUCM  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
******   LA    R2,RECVHDR+L'RECVHDR                                             
         USING UCOMHDRD,R2                                                      
*                                                                               
         CLC   UCOMKTYP,=XL2'0D0C'                                              
         JNE   NO                                                               
         MVC   BYTE,UCOMKAGY                                                    
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         CLI   UCOMCTYP,C'U'                                                    
         JNE   NO                                                               
*-CHECK CLIENT LEVEL UPDATE                                                     
         OC    FILTCLI,FILTCLI                                                  
         JZ    YES                                                              
         CLC   UCOMKCLT,FILTCLI                                                 
         JNE   NO                                                               
*                                                                               
FILTUCMX J     YES                                                              
         DROP  R2,R5                                                            
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE UCOM                                                               
*---------------------------------------------------------------------*         
INITUCM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTUCMDL          R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD COMMERCIAL RECORDS                                                       
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADCML  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING CMLKEY,R2                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,SXDTAGB                                                   
         DROP  R2                                                               
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCML10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AACCLOAD,DMCB,VNTTCMLC,AINITCML,AFILTCML,(R8)                    
         JNE   NO                                                               
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCML10                                                           
         J     YES                                                              
*                                                                               
LCMLX    J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* UPDATE COMMERCIAL RECORDS                                                     
*---------------------------------------------------------------------*         
UPDTCML  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING CMLKEY,R2                                                        
*                                                                               
         GOTO1 AFILTCML                                                         
         JNE   YES                                                              
         GOTO1 AINITCML                                                         
         GOTO1 AACCUPDT,DMCB,VNTTCMLC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
UCMLX    J     YES                                                              
         LTORG                                                                  
*                                                                               
*---------------------------------------------------------------------*         
* FILTER COMMERCIAL RECORDS                                                     
*---------------------------------------------------------------------*         
FILTCML  NTR1  BASE=*,LABEL=*                                                   
         USING CMLRECD,R2                                                       
         CLC   =X'0A21',CMLKID                                                  
         JNE   NO                                                               
*                                                                               
         MVC   BYTE,CMLKAM                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
*                                                                               
         CLC   CMLKCML,=XL8'00'                                                 
         JE    NO                                                               
         CLC   CMLKCML,=8C'9'                                                   
         JE    NO                                                               
         J     YES                                                              
*                                                                               
FILTCLMX J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE COMMERCIAL RECORD                                                  
*---------------------------------------------------------------------*         
INITCML  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTCMDL           R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD UNVERSE                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADUNV  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING NUNRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   NUNKEY(2),=X'0D22'                                               
         MVC   NUNKAGY,SXDTAGY                                                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LUNV10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*                                                                               
         GOTO1 AFILTUNV                                                         
         JNE   LUNVSEQ                                                          
         MVC   PRIADDR,14(R2)                                                   
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
         GOTO1 AINITUNV                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
         MVC   0(42,R8),IOKEYSAV   PASS KEY                                     
*                                                                               
         GOTO1 VNTTUNVC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
LUNV30   GOTO1 VNTTUNVC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LUNVSEQ                                                          
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    LUNV40                                                           
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LUNV40   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LUNV30                                                           
*                                                                               
LUNVSEQ  XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(20),IOKEYSAV  RESTORE KEY                                  
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LUNV50   GOTO1 VDATAMGR,DMCB,DMRSEQ,SPTDIR,IOKEY,(R2),DMWORK                    
         CLC   IOKEY(2),0(R2)                                                   
         JNE   YES                                                              
         MVC   IOKEYSAV,0(R2)                                                   
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LUNV10                                                           
         J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* UPDATE UNIVERSE                                                               
*---------------------------------------------------------------------*         
UPDTUNV  NTR1  BASE=*,LABEL=*                                                   
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,SPTFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING NUNRECD,R2                                                       
*                                                                               
         GOTO1 AFILTUNV                                                         
         JNE   YES                                                              
         GOTO1 AINITUNV                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
         MVC   0(42,R8),IOKEYSAV   PASS KEY                                     
*                                                                               
         GOTO1 VNTTUNVC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
UUNV10   GOTO1 VNTTUNVC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         BE    UUNVX                                                            
*                                                                               
         L     RF,DXAXREC           A(RECORD)                                   
         CLI   SXDTPLFM,0                                                       
         BE    UUNV20                                                           
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
UUNV20   CLI   (NTUVACT-NTUVD)(RF),C'A'   ADD?                                  
         BE    UUNV50                                                           
         TM    NUNCNTL,X'80'        UNIVERSE DELETED?                           
         BO    UUNV30                                                           
         MVI   COPYFLAG,X'01'                                                   
         CLC   (NTUVTYP-NTUVD)(5,RF),=C'06538'                                  
         BNE   UUNV40                                                           
UUNV30   MVI   (NTUVACT-NTUVD)(RF),C'D'                                         
         B     UUNV50                                                           
*                                                                               
UUNV40   CLI   COPYFLAG,X'01'                                                   
         BNE   UUNV10                                                           
         L     RF,DXASQLB           RF=A(CONVERTED RECORD)                      
         MVI   (NTUVACT-NTUVD)(RF),C'A'                                         
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UUNV50   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC              PUT RECORD & DECREMENT IO COUNT             
         JNE   YES                                                              
         TM    NUNCNTL,X'80'        UNIVERSE DELETED?                           
         BZ    UUNV40                                                           
*                                                                               
UUNVX    J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*---------------------------------------------------------------------*         
* FILTER ESTIMATE DEMO LIST                                           *         
*---------------------------------------------------------------------*         
         USING NUNRECD,R2                                                       
FILTUNV  NTR1  BASE=*,LABEL=*                                                   
         CLC   =X'0D22',NUNKEY                                                  
         JNE   NO                                                               
         CLC   NUNKAGY,SXDTAGY                                                  
         JNE   NO                                                               
         MVC   IOKEYSAV,0(R2)                                                   
         J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE ESTIMATE DEMO LIST                                                 
*---------------------------------------------------------------------*         
INITUNV  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTUDDL           R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* LOAD PATTERN                                                                  
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
LOADPAT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING NPTRECD,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   NPTXKID(2),=X'0A61'                                              
         MVC   NPTXAM,SXDTAGB                                                   
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPAT10   TM    DMCB+8,X'80'         ALL DONE IF EOF                             
         JO    YES                                                              
         GOTO1 AFILTPAT                                                         
         JNE   LPATSEQ                                                          
*                                                                               
LPAT20   MVC   PRIADDR,36(R2)                                                   
         GOTO1 AGETIT                                                           
         JNE   NO                                                               
*                                                                               
         GOTO1 AINITPAT                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         L     R1,=A(VDAYUNPK)                                                  
         MVC   0(4,R8),0(R1)                                                    
         L     R1,=A(VUNTIME)                                                   
         MVC   4(4,R8),0(R1)                                                    
*                                                                               
         LARL  RF,NTXCOMM                                                       
         USING NTXCOMMD,RF                                                      
         MVC   8(4,R8),ATRPACK                                                  
         DROP  RF                                                               
*                                                                               
         GOTO1 VNTTPATC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
LPAT30   GOTO1 VNTTPATC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         JE    LPATSEQ                                                          
*                                                                               
         L     RF,DXAXREC          A(RECORD)                                    
         CLI   SXDTPLFM,0                                                       
         JE    LPAT40                                                           
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LPAT40   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LPAT30                                                           
*                                                                               
LPATSEQ  XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(32),0(R2)     RESTORE KEY                                  
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPAT50   GOTO1 VDATAMGR,DMCB,DMRSEQ,XSPDIR,IOKEY,(R2),DMWORK                    
         CLC   IOKEY(3),0(R2)                                                   
         JNE   YES                                                              
         OC    NPTXOR3G,NPTXOR3G                                                
         JNZ   LPAT50                                                           
         CLI   NPTXPSSV,0          PASSIVE FOR LIST?                            
         JNE   LPAT50                                                           
         MVC   IOKEYSAV,0(R2)                                                   
*                                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPAT10                                                           
         J     YES                                                              
*---------------------------------------------------------------------*         
* UPDATE PATTERN                                                                
*---------------------------------------------------------------------*         
UPDTPAT  NTR1  BASE=*,LABEL=*                                                   
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         CLI   RFILTY,XSPFILQ                                                   
         JNE   YES                                                              
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING NPTRECD,R2                                                       
*                                                                               
         GOTO1 AFILTPAT                                                         
         JNE   YES                                                              
         GOTO1 AINITPAT                                                         
*                                                                               
         L     R8,ABIGWORK                                                      
         L     RE,ABIGWORK                                                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         L     R1,=A(VDAYUNPK)                                                  
         MVC   0(4,R8),0(R1)                                                    
         L     R1,=A(VUNTIME)                                                   
         MVC   4(4,R8),0(R1)                                                    
*                                                                               
         LARL  RF,NTXCOMM                                                       
         USING NTXCOMMD,RF                                                      
         MVC   8(4,R8),ATRPACK                                                  
         DROP  RF                                                               
*                                                                               
*                                                                               
         GOTO1 VNTTPATC,DMCB,DXAXREC,(R2),(1,0),(R6),(R8)                       
UPAT10   GOTO1 VNTTPATC,DMCB,DXAXREC,(R2),(2,0),(R6),(R8)                       
         CLI   8(R1),X'FF'                                                      
         BE    UPATX                                                            
*                                                                               
         L     RF,DXAXREC           A(RECORD)                                   
         CLI   SXDTPLFM,0                                                       
         BE    UPAT20                                                           
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
UPAT20   CLI   (NTPTACT-NTPTD)(RF),C'A'   ADD?                                  
         BE    UPAT50                                                           
         TM    NPTXSTAB,X'80'       DELETED?                                    
         BO    UPAT30                                                           
         MVI   COPYFLAG,X'01'                                                   
         CLC   (NTPTTYP-NTPTD)(5,RF),=C'06541'                                  
         BNE   UPAT40                                                           
UPAT30   MVI   (NTPTACT-NTPTD)(RF),C'D'                                         
         B     UPAT50                                                           
*                                                                               
UPAT40   CLI   COPYFLAG,X'01'                                                   
         BNE   UPAT10                                                           
         L     RF,DXASQLB           RF=A(CONVERTED RECORD)                      
         MVI   (NTPTACT-NTPTD)(RF),C'A'                                         
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UPAT50   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC              PUT RECORD & DECREMENT IO COUNT             
         JNE   YES                                                              
         TM    NPTXSTAB,X'80'       DELETED?                                    
         BZ    UPAT40                                                           
*                                                                               
UPATX    J     YES                                                              
         LTORG                                                                  
*---------------------------------------------------------------------*         
* FILTER PATTERN                                                                
*---------------------------------------------------------------------*         
         USING NPTRECD,R2                                                       
FILTPAT  NTR1  BASE=*,LABEL=*                                                   
         CLC   =X'0A61',NPTXKID                                                 
         JNE   NO                                                               
         MVC   BYTE,NPTXAM                                                      
         NI    BYTE,X'F0'          TURN OFF MEDIA BYTES                         
         CLC   BYTE,SXDTAGB                                                     
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         LTORG                                                                  
*---------------------------------------------------------------------*         
* INITIALISE PATTERN                                                            
*---------------------------------------------------------------------*         
INITPAT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,NTPTDL           R1=L'RECORD (LONGEST)                        
         GOTO1 AINITALL                                                         
         J     YES                                                              
         LTORG                                                                  
***********************************************************************         
* USED TO BE SUBROUTINE TO EXTRACT ACCOUNT RECORDS IN LOAD MODE       *         
* NOW ITS SUBROUTINE FOR ALL RECORDS THAT HAVE (1-1) DIFFICULTY LEVEL *         
* R2 = A(ACCOUNT DIRECTORY RECORD BUFFER)                             *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
* P4 = A(OF MEDIA TABLE WHICH CONTANE LOTS OF USEFULL STUFF) ALWAYS R8*         
* ALSO PRIPFLG (GLOBAL) IS A SWICH BETWEEN DIFFERENT RECORDS          *         
* REPMED IS LOCAL STORAGE                                             *         
***********************************************************************         
ACCLOAD  NTR1  BASE=*,LABEL=*                                                   
         L     R8,12(R1)                                                        
         LM    R3,R5,0(R1)                                                      
         LTR   R5,R5               FILTER ROUTINE?                              
         JZ    ALOA02                                                           
         GOTO1 (R5)                FILTER RECORD                                
         JNE   ALOA06              NOT VALID - GET NEXT                         
*                                                                               
ALOA02   TM    FLAGS,STAFILEQ                                                   
         BO    ALOA03              WHOLE RECORD IS IN DIR                       
         TM    FLAGS,DIRONLY                                                    
         BO    ALOA03                                                           
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
ALOA03   DS    0H                                                               
         GOTO1 (R4)                INITIALISE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),(R8)                               
         BE    *+12                                                             
         NI    FLAGS,X'FF'-NORDSEQ     RE-SET THE FLAG                          
         B     ALOA06                                                           
*                                                                               
         CLC   TYPENAME,=C'EDL'                                                 
         BNE   ALA04                                                            
         L     RF,ABIGWORK                                                      
         OC    0(100,RF),0(RF)      FINISHED WITH THIS REC?                     
         BNZ   ALA04                                                            
         NI    FLAGS,X'FF'-NORDSEQ                                              
         B     ALA04                                                            
*                                                                               
ALA04    TM    DMCB+8,X'80'                                                     
         JO    ALOA06              ERROR - NO WRITE                             
         CLI   DMCB+8,X'FF'                                                     
         JE    ALOA06              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   ALOA06              CONTROLLER REQUESTS NO WRITE                 
         L     RF,DXAXREC                                                       
*                                                                               
         CLI   SXDTPLFM,0                                                       
         JE    ALOA05              PUT UNCONVERTED RECORD ONLY                  
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         BE    *+6                                                              
         DC    H'0'                DEAL WITH ERROR STATE HERE??                 
*                                  PUT CONVERTED RECORD TO FILE                 
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     ALOA06                                                           
*                                                                               
ALOA05   DS    0H                  PUT UNCONVERTED RECORD TO FILE               
         GOTO1 DXPUT,DMCB,DXAXREC,(R7)                                          
*                                                                               
ALOA06   GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JNE   NO                  TOO MANY IOS                                 
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(13),0(R2)     L(SPOTDIR KEY)                               
*                                                                               
         TM    FLAGS,STAFILEQ                                                   
         BNO   *+14                                                             
         MVC   IOKEY(15),0(R2)     L(STAFILE KEY)                               
         B     ALOA7                                                            
*                                                                               
         TM    FLAGS,UNTFILEQ                                                   
         BNO   *+10                                                             
         MVC   IOKEY(20),0(R2)     L(UNTDIR KEY)                                
*                                                                               
ALOA7    TM    FLAGS,NORDSEQ                                                    
         BNO   ALOA15                                                           
         TM    FLAGS,UNTFILEQ                                                   
         BZ    ALOA8                                                            
         GOTO1 VDATAMGR,DMCB,DMRDHI,UNTDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
*                                                                               
ALOA8    GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
*                                                                               
ALOA15   TM    FLAGS,STAFILEQ                                                   
         BO    ALOA18                                                           
         TM    FLAGS,UNTFILEQ                                                   
         BZ    ALOA23                                                           
         TM    FLAGS,DIRONLY                                                    
         BO    ALOA20                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,UNTDIR,IOKEY,(R2),DMWORK                    
         B     ALOA25                                                           
*                                                                               
ALOA18   GOTO1 VDATAMGR,DMCB,DMRSEQ,STAFIL,IOKEY,(R2),DMWORK                    
         B     ALOA25                                                           
*                                                                               
ALOA20   TM    FLAGS,STAFILEQ                                                   
         BO    ALOA23                                                           
         GOTO1 VDATAMGR,DMCB,DMRSEQ,UNTDIR,IOKEY,(R2),DMWORK                    
         B     ALOA25                                                           
*                                                                               
ALOA23   GOTO1 VDATAMGR,DMCB,DMRSEQ,SPTDIR,IOKEY,(R2),DMWORK                    
*                                                                               
ALOA25   J     YES                                                              
*                                                                               
REPMED   DC    C' '           STORAGE FOR ONE BYTE MEDIA                        
***********************************************************************         
* R2 = A(ACCOUNT RECORD BUFFER)                                       *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(3 CHAR MNEMONIC)                                             *         
***********************************************************************         
ACCUPDT  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)                                                      
         L     R8,8(R1)                                                         
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),(R8)                               
         CLI   DMCB+8,FF           DATA NOT COMPLETE - DO NOT WRITE             
         JE    NO                                                               
         TM    DMCB+8,X'80'                                                     
         JO    NO                                                               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPDL40              DO NOT CONVERT RECORD                        
*                                                                               
* IF NET SFM PROGRAM NEEDS TO INCREASE ESTIMATE RECORD SIZE                     
* IT DOES THIS BY ADDING A BIGGER RECORD TO DISK                                
* AND PUTTING THE NEW RECORD'S DISK ADDRESS IN THE OLD KEY                      
* AS A RESULT, ACTION CHANGE SHOWS UP ON RECOVERY AS AN "ADD",                  
* CAUSING DUPLICATE KEY ERRORS ON DB SIDE, AND CAUSING THE CODE BELOW           
* TO SKIP THE ESTIMATE DEMO LIST LOGIC                                          
*                                                                               
         CLC   NTESQ,(NTTESTYP-NTTESD)(RF) ESTIMATE?                            
         JNE   *+12                                                             
         CLI   DXACTION,C'A'       ACTION = ADD?                                
         JE    UPDL10                                                           
*                                                                               
         CLI   DXACTION,C'C'       SPECIAL CODE FOR CHANGES                     
         JNE   UPDL20                                                           
*                                                                               
         L     R0,ACOPYBUF         R0=A(EXTRACT RECORD AREA FOR COPY)           
         LH    R1,=Y(L'COPYBUFF)                                                
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         GOTO1 (R3),DMCB,ACOPYBUF,(R2),0,(R6),(R8)    BUILD COPY REC            
*                                                                               
         L     R0,DXAXREC          R0=A(CHANGE SQL RECORD)                      
         L     RE,ACOPYBUF         RE=A(COPY SQL RECORD)                        
*                                                                               
         CLC   NTESQ,(NTTESTYP-NTTESD)(RE)    ALWAYS PASS ESTIMATE              
         JE    UPDL10                                                           
*                                                                               
         LH    RF,0(RE)            RF=L'RECORD                                  
         LH    R1,=AL2(NTTMDAGY-NTTMDD) DISP TO PRIALPHA                        
         AR    R0,R1               BUMP TO PRIALPHA CODE ALL RECORDS            
         AR    RE,R1                                                            
         SR    RF,R1               REDUCE LENGTH APPROPRIATELY                  
*                                  DON'T LOOK AT TRAILING BYTES                 
         LR    R1,RF                                                            
         CLCL  R0,RE               IF EXTRACT VERSIONS OF COPY/CHANGE           
         JE    YES                 ARE THE SAME THEN SKIP                       
*                                                                               
UPDL10   L     RF,DXAXREC                     IF ESTIMATE RECORD, THEN          
         CLC   NTESQ,(NTTESTYP-NTTESD)(RF)    DO ESTIMATE DEMO LIST             
         JNE   UPDL20                                                           
*                                                                               
         L     RF,DXAXREC                                                       
         MVI   10(RF),C'D'                                                      
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
         L     RF,DXAXREC                                                       
         MVI   10(RF),C'A'                                                      
*                                                                               
UPDL20   DS    0H                                                               
         GOTO1 VNTXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UPDL40   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
*                                                                               
         L     RF,DXASQLB                     IF ESTIMATE RECORD, THEN          
         CLC   NTESQ,(NTTESTYP-NTTESD)(RF)    DO ESTIMATE DEMO LIST             
         JNE   YES                                                              
*                                                                               
         L     RE,=A(UPDTEDL)                 ESTIMATE DEMO LIST UPDATE         
         GOTO1 (RE),DMCB,VNTTEDLC,AINITEDL,AFILTEDL,(R8)                        
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
***********************************************************************         
* NTXRECID                                                                      
***********************************************************************         
         PRINT OFF                                                              
NTXRCIDT DS    0C                                                               
       ++INCLUDE NTXRECID                                                       
         PRINT ON                                                               
***********************************************************************         
* BROADCAST DATE BLOCK                                                          
***********************************************************************         
DATEBLK  DS    0C                                                               
CURBST   DS    XL2                 COMPRESSED CURRENT BROADCAST START           
CURBND   DS    XL2                 COMPRESSED CURRENT BROADCAST END             
PRIBST   DS    XL2                 COMPRESSED PRIOR BROADCAST START             
PRIBND   DS    XL2                 COMPRESSED PRIOR BROADCAST END               
         SPACE 3                                                                
***********************************************************************         
* FAKE SPACEND TABLE FOR SETVAL IN RXROUTS                                      
***********************************************************************         
STAXREC DC     XL72'0000'                                                       
         SPACE 3                                                                
***********************************************************************         
* AREA USED FOR DETERMINING WHETHER CHANGES NEED TO BE EXTRACTED                
***********************************************************************         
BIGWORK  DS    4500X                                                            
COPYBUFF DS    CL10000                                                          
*                                                                               
***********************************************************************         
* LOADED MODULES, ACCESSIBLE THROUGHOUT EXTRACT                       *         
***********************************************************************         
NTXCOMM  DS    0D                                                               
         DS    (NTXCOMMDLQ)X                                                    
*                                                                               
***********************************************************************         
* TF SSB                                                              *         
***********************************************************************         
         DS    0L                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      CSECT                                                                  
         DC    H'0'                                                             
         DC    X'FF'               OFFLINE EXTENDED                             
         DC    XL256'00'                                                        
***********************************************************************         
* COPY OF COMFACS FOR THOSE DAMN DEMOS                                *         
***********************************************************************         
COMFACS  CSECT                     COMMON FACILITIES LIST                       
         DC    (COMFACSL/4)A(0)                                                 
         ORG   COMFACS                                                          
         DC    V(DATAMGR)                                                       
         DC    V(CALLOFF)          CALLOFF)                                     
         DC    A(0)                GETMSG)                                      
         DC    V(FAGETTXT)         GETTXT)                                      
         DC    A(0)                SWITCH)                                      
         DC    V(HELLO)            HELLO)                                       
         DC    A(0)                SCANNER)                                     
         DC    A(0)                UNSCAN)                                      
         DC    V(HEXIN)            HEXIN)                                       
         DC    V(HEXOUT)                                                        
         DC    A(0)                CASHVAL)                                     
         DC    A(0)                DATVAL)                                      
         DC    V(DATCON)                                                        
         DC    A(0)                TERMVAL)                                     
         DC    A(0)                SCUNKEY)                                     
         DC    V(ADDAY)                                                         
         DC    V(GETDAY)                                                        
         DC    V(GETPROF)          GETPROF)                                     
         DC    V(PERVERT)                                                       
         DC    A(0)                GETFACT)                                     
         DC    A(0)                XSORT)                                       
         DC    A(0)                REQTWA)                                      
         DC    A(0)                GETFLD)                                      
*&&UK                                                                           
         DC    V(PERVAL)                                                        
         DC    V(DLFLD)                                                         
         DC    V(GENERAL)                                                       
         DC    18A(0)                                                           
*&&                                                                             
*&&US                                                                           
         DC    A(0)                DDISPSRT)                                    
VDEMADDR DC    A(0)                                                             
VDEMDISP DC    A(0)                DEMDISP)                                     
VDEMTABS DC    A(0)                DEMTABS)                                     
         DC    A(0)                DSTATION)                                    
         DC    A(0)                DMASTER)                                     
         DC    A(0)                DFORMULA)                                    
         DC    A(0)                DNAME)                                       
         DC    A(0)                DCODE)                                       
         DC    A(0)                DCONTROL)                                    
         DC    A(0)                DADJUST)                                     
VDEMOUT  DC    A(0)                DEMOUT)                                      
VDEMEL   DC    A(0)                DEMEL)                                       
VDEMAINT DC    A(0)                DEMAINT)                                     
VDEMAND  DC    A(0)                DEMAND)                                      
VDEMMATH DC    A(0)                DEMOMATH)                                    
VDEMOVAL DC    A(0)                DEMOVAL)                                     
         DC    A(0)                GENERAL)                                     
         DC    V(PERVAL)                                                        
         DC    A(0)                DLFLD)                                       
         DC    A(0)                                                             
*&&                                                                             
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
         DC    V(BINSRCH)          BINSRCH)                                     
         DC    A(0)                PROTON)                                      
         DC    A(0)                PROTOFF)                                     
         DC    V(HELEN)            HELEN)                                       
         DC    A(0)                MQIO)                                        
         DC    A(0)                EUREKA                                       
         DC    V(LOCKUP)                                                        
         DC    V(MASTC)            MASTC                                        
         DC    V(LOCKSPC)          LOCKSPACE                                    
         ORG                                                                    
*                                                                               
VSTATAB  DC    A(0)                                                             
VDPTBL   DC    A(0)                                                             
VCLTREC  DC    A(0)                                                             
VESTREC  DC    A(0)                                                             
VDAYUNPK DC    A(0)                                                             
VUNTIME  DC    A(0)                                                             
***********************************************************************         
* MASTC CSECT                                                         *         
***********************************************************************         
MASTC    CSECT                                                                  
       ++INCLUDE DDMASTC                                                        
*                                                                               
***********************************************************************         
* DSECT TO COVER GLOBAL WORKING STORAGE                               *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
DMWORK   DS    12D                                                              
ATYPTAB  DS    A                   A(TYPTAB) - TYPTABD                          
PRIADDR  DS    CL4                                                              
KEY      DS    CL48                                                             
SV84KEY  DS    XL42                                                             
SV04KEY  DS    XL42                                                             
IOKEY    DS    CL64                                                             
IOKEYSAV DS    CL42                                                             
IOKEYSA2 DS    CL42                                                             
IOKEYSA3 DS    CL42                                                             
*                                                                               
VUNDAY   DS    A                   A(UNDAY)                                     
VDEMCON  DS    A                   A(DEMOCON)                                   
VDEFINE  DS    A                   A(DEFINE)                                    
VTRPACK  DS    A                   A(TRPACK)                                    
VOFFICER DS    A                   A(OFFICER)                                   
VCALLOFF DS    A                   A(CALLOV)                                    
MYFLAG   DS    XL1                                                              
GOTVDEM  EQU   X'01'               GOT THE A(DEMAND)                            
*                                                                               
DATADISP DS    H                                                                
ELCODE   DS    XL1                                                              
BYTE     DS    XL1                                                              
HALF     DS    XL2                                                              
HALF2    DS    XL2                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
HEADFLAG DS    XL1                                                              
RECTYPE  DS    XL1                                                              
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
*                                                                               
PRIALPHA DS    XL2                                                              
VERSION  DS    XL1                                                              
PLATFORM DS    XL1                                                              
*                                                                               
PRIPFLG  DS    XL1                 PUB '81'  REP '11'                           
REPFLAG  DS    XL1                 '11'                                         
FLAGMED  DS    XL1                                                              
*                                                                               
ADPTBL   DS    A                                                                
ACLTREC  DS    A                                                                
AESTREC  DS    A                                                                
ASTATAB  DS    A                                                                
*                                                                               
NUMBINDP DS    H                   NUMBER OF ENTRIES IN BINSRCH TAB             
DAYPB    DS    XL5                                                              
*                                                                               
NUMBINST DS    H                   NUMBER OF ENTRIES IN BINSRCH TAB             
STATB    DS    XL5                                                              
*                                                                               
NUMBINDM DS    H                   NUMBER OF ENTRIES IN BINSRCH TAB             
DBKB     DS    XL6                 (2) START DATE                               
*                                  (1) MONTHLY/WEEKLY                           
*                                  (2) END DATE                                 
*                                  (1) POSTING TYPE                             
COPYFLAG DS    XL1                                                              
FLAGS2   DS    X                                                                
FL2OPTQ  EQU   X'01'               OPTICA (MASTER DATABASE) EXTRACT             
*                                                                               
TYPECODE DS    CL3                                                              
TYPENAME DS    CL3                                                              
TYPEDEEP DS    XL1                                                              
TYPEFLAG DS    XL1                                                              
TYPEALOD DS    A                                                                
TYPEAUPD DS    A                                                                
*                                                                               
MEDPARAM DS    0X                                                               
FLAGS    DS    X                                                                
NORDSEQ  EQU   X'01'                                                            
STAFILEQ EQU   X'02'                                                            
UNTFILEQ EQU   X'04'                                                            
DIRONLY  EQU   X'08'               DON'T DO GETREC                              
XSPFILEQ EQU   X'10'               DON'T DO GETREC                              
ELCOUNT  DS    X                                                                
CTRL4    DS    X                                                                
CTRL5    DS    X                                                                
*                                                                               
BINDMCB  DS    6F                                                               
*                                                                               
WORK     DS    XL64                                                             
*                                                                               
PREVCLT  DS    XL2                 CLIENT                                       
PREVEST  DS    XL1                 ESTIMATE                                     
*                                                                               
SVCLTA   DS    CL3                 3 CHAR CLIENT CODE                           
SVCOFF   DS    XL1                 CLIENT OFFICE                                
SVCPROF  DS    CL13                CLIENT PROFILES                              
*                                                                               
UUNTKEY  DS    XL20                UNIT KEY                                     
UUNDAY   DS    XL4                 A(UNDAY)                                     
UTRPACK  DS    XL4                 A(TRPACK)                                    
UDEMCON  DS    XL4                 A(DEMCON)                                    
UDEFINE  DS    XL4                 A(DEFINE)                                    
UDPTA    DS    XL2                 ALPHA DAYPART                                
UN0PROF  DS    XL16                N0PROFILE                                    
UN2PROF  DS    XL16                N2PROFILE                                    
UN1PROF  DS    XL16                N1PROFILE                                    
UNTISTA  DS    XL4                 NTI STATION                                  
UAGYFLG2 DS    XL1                 AGENCYFLAG2                                  
UAGYFLG2_2DP   EQU  X'20'          2 DECIMAL PRECISION                          
UPOSTTYP DS    CL1                 NETWORK POSTING TYPE                         
*                                                                               
VGETBRD  DS    A                   A(GETBROAD)                                  
DTETODAY DS    XL2                 TODAY'S DATE COMPRESSED                      
DTEFILT  DS    XL2                 DATE FILTER TO START                         
DTEYRST  DS    XL2                 YEAR START                                   
*                                                                               
TEMPSTD  DS    XL3                                                              
TEMPEND  DS    XL3                                                              
ADBKORST DS    A                                                                
ADBKNEW  DS    A                                                                
ADBKPAR  DS    A                                                                
*                                                                               
FILTCLI  DS    XL2                                                              
FILTCLIE DS    XL2                 USED FOR RANGE OF CLIENTS                    
*                                                                               
DBKTAB   DS    XL250               DEMO BOOK TABLE                              
*                                                                               
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    4096X                                                            
*                                                                               
WORKL    EQU   *-WORKD                                                          
***********************************************************************         
* DSECT TO COVER UNIT INFO PASSED TO UNIT/UAD LOAD                    *         
***********************************************************************         
UINFOD   DSECT                                                                  
UIUNTKEY DS    XL20                UNIT KEY                                     
UICLOFF  DS    XL1                 CLIENT OFFICE                                
UICLPROF DS    XL16                CLIENT PROFILES                              
UIUNDAY  DS    XL4                 A(UNDAY)                                     
UITRPACK DS    XL4                 A(TRPACK)                                    
UIDEMCON DS    XL4                 A(DEMCON)                                    
UIDEFINE DS    XL4                 A(DEFINE)                                    
UICLTREC DS    XL4                 A(CLIENT RECORD)                             
UIESTREC DS    XL4                 A(ESTIMATE RECORD)                           
UIDPTA   DS    XL2                 ALPHA DAYPART                                
UIN0PROF DS    XL16                N0PROFILE                                    
UIN2PROF DS    XL16                N2PROFILE                                    
UIN1PROF DS    XL16                N1PROFILE                                    
UINTISTA DS    XL4                 NTI STATION                                  
UIAGYFL2 DS    XL1                 AGENCYFLAG2                                  
UINTBLRD DS    XL4                 A(NET BILL READER)                           
UINFOLQ  EQU   *-UINFOD                                                         
***********************************************************************         
* DSECT TO COVER TYPTAB TABLE                                         *         
***********************************************************************         
TYPTABD  DSECT                                                                  
TYPNAME  DS    XL3                 3 CHAR MNEMONIC FOR RECORD TYPE              
TYPLDEEP DS    XL1                 DEPTH INTO LEDGER FOR COMPARE (LOAD)         
TYPFLAG  DS    XL1                 FLAGS                                        
TYPSTAT  DS    XL1                 STATUS BYTE                                  
TYPSSUP  EQU   X'80'               SUPPORT TYPE RECORD                          
         DS    XL2                 N/D                                          
TYPLOAD  DS    XL4                 A(LOAD ROUTINE)                              
TYPUPDT  DS    XL4                 A(UPDATE ROUTINE)                            
TYPTABLQ EQU   *-TYPTABD                                                        
***********************************************************************         
* DSECT TO COVER COMMON ADDRESSES                                     *         
***********************************************************************         
ADDRESSD DSECT                                                                  
COMMON   DS    CL8                                                              
VDATAMGR DS    V                                                                
VDMOD000 DS    V                                                                
VDADDS   DS    V                                                                
VLOGIO   DS    V                                                                
VDATCON  DS    V                                                                
VNTXCNVX DS    V                                                                
AMEDTAB  DS    A                                                                
VNETIO   DS    V                                                                
VNETVALU DS    V                                                                
         DS    V                                                                
*                                                                               
VNTTAGYC DS    V          MEDIA/AGENCY                                          
VNTTCNTC DS    V          CLIENT                                                
VNTTPRDC DS    V          PRODUCT                                               
VNTTPDLC DS    V          CLIENT PRODUCT LIST                                   
VNTTESTC DS    V          ESTIMATE                                              
VNTTSTAC DS    V          STATION                                               
VNTTPGMC DS    V          PROGRAM                                               
VNTTDPTC DS    V          DAYPART                                               
VNTTEDLC DS    V          ESTIMATE DEMO LIST                                    
VNTTPAKC DS    V          PACKAGE                                               
VNTTUNIC DS    V          UNIT                                                  
VNTTGOLC DS    V          NET GOAL                                              
VNTTPDGC DS    V          PRODUCT GROUP RECORDS                                 
VNTTUADC DS    V          UNIT ACTUAL DEMOS                                     
VNTTECTC DS    V          EARNED COST                                           
VNTTSGRC DS    V          STATION GROUP                                         
VNTTCGRC DS    V          CLIENT  GROUP                                         
VNTTGXLC DS    V          NET GOAL                                              
VNTTREPC DS    V          REP                                                   
VNTTFLTC DS    V          FLIGHTS                                               
VNTTUCMC DS    V          UCOM                                                  
VNTTCMLC DS    V          COMMERCIAL                                            
VNTTMKTC DS    V          MARKET                                                
VNTTBFMC DS    V          BFORM                                                 
VNTTUNVC DS    V          UNIVERSE                                              
VNTTPATC DS    V          PATTERN                                               
         DS    CL8                 COMMON INTERNAL ROUTINES                     
AACCLOAD DS    A                                                                
AACCUPDT DS    A                                                                
ADECIOC  DS    A                                                                
         DS    A                                                                
         DS    A                   SPARE                                        
AGETTYP  DS    A                                                                
AGETIT   DS    A                                                                
AREADHI  DS    A                                                                
ARECCMP  DS    A                                                                
         DS    CL8                 LOAD ROUTINES                                
ALOADAGY DS    A          MEDIA/AGENCY                                          
ALOADCNT DS    A          CLIENT                                                
ALOADPRD DS    A          PRODUCT                                               
ALOADPDL DS    A          PRODUCT LIST                                          
ALOADEST DS    A          ESTIMATE                                              
ALOADSTA DS    A          STATION                                               
ALOADPGM DS    A          PROGRAM                                               
ALOADDPT DS    A          DAYPART                                               
ALOADEDL DS    A          ESTIMATE DEMO LIST                                    
ALOADPAK DS    A          PACKAGE                                               
ALOADUNI DS    A          UNIT                                                  
ALOADGOL DS    A          NET GOAL                                              
ALOADPDG DS    A          PRODUCT GROUP RECORDS                                 
ALOADUAD DS    A          UNIT ACTUAL DEMOS                                     
ALOADECT DS    A          EARNED COST                                           
ALOADSGR DS    A          STATION GROUPS                                        
ALOADCGR DS    A          CLIENT GROUPS                                         
ALOADUND DS    A          UNIT ACTUAL DEMOS                                     
ALOADGXL DS    A          NET GOAL                                              
ALOADREP DS    A          REP                                                   
ALOADFLT DS    A          FLIGHTS                                               
ALOADUCM DS    A          UCOM                                                  
ALOADCML DS    A          COMMERCIAL                                            
ALOADMKT DS    A          MARKET                                                
ALOADBFM DS    A          BFORM                                                 
ALOADUNV DS    A          UNIVERSE                                              
ALOADPAT DS    A          PATTERN                                               
         DS    CL8                 UPDATE ROUTINES                              
AUPDTAGY DS    A          MEDIA/AGENCY                                          
AUPDTCNT DS    A          CLIENT                                                
AUPDTPRD DS    A          PRODUCT                                               
AUPDTPDL DS    A          PRODUCT LIST                                          
AUPDTEST DS    A          ESTIMATE                                              
AUPDTSTA DS    A          STATION                                               
AUPDTPGM DS    A          PROGRAM                                               
AUPDTDPT DS    A          DAYPART                                               
AUPDTEDL DS    A          ESTIMATE DEMO LIST                                    
AUPDTPAK DS    A          PACKAGE                                               
AUPDTUNI DS    A          UNIT                                                  
AUPDTGOL DS    A          NET GOAL                                              
AUPDTPDG DS    A          PRODUCT GROUP RECORDS                                 
AUPDTUAD DS    A          UNIT ACTUAL DEMOS                                     
AUPDTECT DS    A          EARNED COST                                           
AUPDTSGR DS    A          STATION GROUPS                                        
AUPDTCGR DS    A          CLIENT GROUPS                                         
AUPDTUND DS    A          UNIT ACTUAL DEMOS                                     
AUPDTGXL DS    A          NET GOAL                                              
AUPDTREP DS    A          REP                                                   
AUPDTFLT DS    A          FLIGHTS                                               
AUPDTUCM DS    A          UCOM                                                  
AUPDTCML DS    A          COMMERCIAL                                            
AUPDTMKT DS    A          MARKET                                                
AUPDTBFM DS    A          BFORM                                                 
AUPDTUNV DS    A          UNIVERSE                                              
AUPDTPAT DS    A          PATTERN                                               
         DS    CL8                 FILTER ROUTINES                              
AFILTAGY DS    A          MEDIA/AGENCY                                          
AFILTCNT DS    A          CLIENT                                                
AFILTPRD DS    A          PRODUCT                                               
AFILTPDL DS    A          PRODUCT LIST                                          
AFILTEST DS    A          ESTIMATE                                              
AFILTSTA DS    A          STATION                                               
AFILTPGM DS    A          PROGRAM                                               
AFILTDPT DS    A          DAYPART                                               
AFILTEDL DS    A          ESTIMATE DEMO LIST                                    
AFILTPAK DS    A          PACKAGE                                               
AFILTUPK DS    A          PACKAGE                                               
AFILTUNI DS    A          UNIT                                                  
AFILTUUN DS    A          UPDATE FILTER                                         
AFILTGOL DS    A          NET GOAL                                              
AFILTPDG DS    A          PRODUCT GROUP RECORDS                                 
AFILTUAD DS    A          UNIT ACTUAL DEMOS                                     
AFILTUUA DS    A          UNIT ACTUAL DEMOS - UPDATE                            
AFILTECT DS    A          EARNED COST                                           
AFILTSGR DS    A          STATION GROUPS                                        
AFILTCGR DS    A          CLIENT GROUPS                                         
AFILTUND DS    A          UNIT ACTUAL DEMOS                                     
AFILTGXL DS    A          NET GOAL                                              
AFILTREP DS    A          REP                                                   
AFILTFLT DS    A          FLIGHTS                                               
AFILTUCM DS    A          UCOM                                                  
AFILTCML DS    A          COMMERCIAL                                            
AFILTMKT DS    A          MARKET                                                
AFILTBFM DS    A          BFORM                                                 
AFILTUNV DS    A          UNIVERSE                                              
AFILTPAT DS    A          PATTERN                                               
         DS    CL8                 INITIALISATION ROUTINES                      
AINITALL DS    A                   GENERAL INITIALISATION                       
AINITAGY DS    A          MEDIA/AGENCY                                          
AINITCNT DS    A          CLIENT                                                
AINITPRD DS    A          PRODUCT                                               
AINITPDL DS    A          PRODUCT LIST                                          
AINITEST DS    A          ESTIMATE                                              
AINITSTA DS    A          STATION                                               
AINITPGM DS    A          PROGRAM                                               
AINITDPT DS    A          DAYPART                                               
AINITEDL DS    A          ESTIMATE DEMO LIST                                    
AINITPAK DS    A          PACKAGE                                               
AINITUNI DS    A          UNIT                                                  
AINITGOL DS    A          NET GOAL                                              
AINITPDG DS    A          PRODUCT GROUP RECORDS                                 
AINITUAD DS    A          UNIT ACTUAL DEMOS                                     
AINITECT DS    A          EARNED COST                                           
AINITSGR DS    A          STATION GROUP                                         
AINITCGR DS    A          CLIENT GROUP                                          
AINITUND DS    A          UNIT ACTUAL DEMOS                                     
AINITGXL DS    A          NET GOAL                                              
AINITREP DS    A          REP                                                   
AINITFLT DS    A          FLIGHTS                                               
AINITUCM DS    A          UCOM                                                  
AINITCML DS    A          COMMERCIAL                                            
AINITMKT DS    A          MARKET                                                
AINITBFM DS    A          BFORM                                                 
AINITUNV DS    A          UNIVERSE                                              
AINITPAT DS    A          PATTERN                                               
*                                                                               
DMOPEN   DS    CL7                                                              
DMREAD   DS    CL7                                                              
DMRSEQ   DS    CL7                                                              
DMRDHI   DS    CL7                                                              
DMCLSE   DS    CL7                                                              
DMFAST   DS    CL7                                                              
GETREC   DS    CL7                                                              
DMRFIL   DS    CL7                                                              
CONTROL  DS    CL7                                                              
CTFILE   DS    CL7                                                              
XSPDIR   DS    CL7                                                              
XSPFIL   DS    CL7                                                              
SPTDIR   DS    CL7                                                              
SPTFIL   DS    CL7                                                              
STAFIL   DS    CL7                                                              
UNTDIR   DS    CL7                                                              
UNTFIL   DS    CL7                                                              
DMDA     DS    F                                                                
DTFADDR  DS    F                                                                
ABIGWORK DS    A                                                                
ACOPYBUF DS    A                                                                
ACTIVITY DS    CL1                                                              
SPACES   DS    CL76                                                             
***********************************************************************         
* DSECT TO COVER RECOVERY HEADER                                      *         
***********************************************************************         
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
***********************************************************************         
* OTHER DSECTS REQUIRED FOR THIS PROGRAM                              *         
***********************************************************************         
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDCOMFACS                                                      
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENSLST                                                      
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENGRP                                                       
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENCLG                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE SPGENWBFLT                                                     
       ++INCLUDE NEGENDPT                                                       
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE CTGENFILE                                                      
GOALHDRD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
UCOMHDRD DSECT                                                                  
       ++INCLUDE SPGENUCOM                                                      
       ++INCLUDE SPTRCMML                                                       
REPD     DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE RXUSERD                                                        
       ++INCLUDE NTXRECD                                                        
       ++INCLUDE DXDSECTS                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DMGREQUS                                                       
       ++INCLUDE FACTRYEQUS                                                     
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE NTXCOMMON                                                      
       ++INCLUDE SPGENBFML                                                      
       ++INCLUDE SPGENUNIV                                                      
       ++INCLUDE SPTRNPAT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081NTXTRACT  03/02/21'                                      
         END                                                                    
