*          DATA SET GXTRACT    AT LEVEL 013 AS OF 01/29/20                      
*PHASE GXTRACTA                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE GXROUTS                                                                
*INCLUDE GXCNVX                                                                 
         TITLE 'GXTRACT - EXTRACT CONTROL SYSTEM FILE DATA'                     
**********************************************************                      
* CONTROL SYSTEM EXTRACT CONTROL MODULE                  *                      
*                                                        *                      
* CONTROL IS PASSED FROM DXTRACT WITH PARAMETERS:        *                      
* R1=A(EXTRACT CONTROL DATA BLOCK - SEE DSECT DXBLOCKD)  *                      
*                                                        *                      
* MODULE ENTERED WITH ONE OF FOLLOWING MODES IN DXMODE:  *                      
*   DXOPENQ  - OPEN SYSTEM FILES                         *                      
*   DXCLOSEQ - CLOSE SYSTEM FILES                        *                      
*   DXLOADQ  - EXTRACT FROM FILES IN LOAD MODE           *                      
*   DXUPDTQ  - EXTRACT FROM FILES IN UPDATE MODE         *                      
*                                                        *                      
* FOR DXLOADQ AND DXUPDTQ MODES,                         *                      
* DXBLOCKD CONTAINS DXSTPTR WHICH IS:                    *                      
* A(CURRENT ENTRY IN SUB SYSTEM EXTRACT DRIVER TABLE -   *                      
*     SEE DSECT SYSTABD)                                 *                      
*                                                        *                      
*                                                        *                      
* RETURN CC .NE. IF ERROR CONDITION ELSE CC .EQ. FOR OK  *                      
*                                                        *                      
**********************************************************                      
GXTRACT  CSECT                                                                  
         ENTRY SSB                 FOR DATAMGR                                  
*                                                                               
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**GXTR**,WORK=A(WORKC),CLEAR=YES                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* THESE USINGS STAY THROUGHOUT THE PROGRAM                                      
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R7,0(R1)            R7=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R7                                                      
GXP      USING GXPARMD,GXPARMS                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         J     MAIN                                                             
*                                                                               
$$DATA   LOCTR ,                   DATA LOCATION CLOSE TO RB                    
*                                                                               
$$CODE   LOCTR ,                   CODE AFTER DATA                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
         BRAS  RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
MOPEN    CLI   DXMODE,DXOPENQ                                                   
         JNE   MCLOSE                                                           
         BRAS  RE,PROCOPEN         OPEN SYSTEM FILES                            
         JNE   MERR                  EXIT IF ERROR                              
         J     MXIT                                                             
*                                                                               
MCLOSE   CLI   DXMODE,DXCLOSEQ                                                  
         JNE   MLOAD                                                            
         BRAS  RE,PROCCLOS         CLOSE SYSTEM FILES                           
         JNE   MERR                  EXIT IF ERROR                              
         J     MXIT                                                             
*                                                                               
MLOAD    CLI   DXMODE,DXLOADQ                                                   
         JNE   MUPDT                                                            
         BRAS  RE,PROCLOAD         EXTRACT FROM FILES IN LOAD MODE              
         JNE   MERR                  EXIT IF ERROR                              
         J     MXIT                                                             
*                                                                               
MUPDT    CLI   DXMODE,DXUPDTQ                                                   
         JNE   MERR                                                             
         BRAS  RE,PROCUPDT         EXTRACT FROM FILES IN UPDATE MODE            
         JNE   MERR                  EXIT IF ERROR                              
         J     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,0                                                        
         J     MXIT                                                             
*                                                                               
MXIT     XMOD1 1                                                                
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
GENINIT  NTR1                                                                   
* COPY THE DSPACE BYTE FROM CONTROLLER'S SSB TO LOCAL SSB                       
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),DXDSPAC                           
*                                                                               
         ICM   RF,15,=V(DDSIO)                                                  
         JZ    *+10                                                             
         MVC   0(8,RF),DXDDSIO                                                  
*                                                                               
         L     RF,DXARECB                                                       
         CLI   DXMODE,DXLOADQ      TEST IF LOAD MODE                            
         JE    *+8                                                              
         LA    RF,4+L'RECVHDR(RF)                                               
         ST    RF,AREC                                                          
* SET UP GXPARMS                                                                
         ST    R7,GXP.GXPDXBLK     SAVE A(DXBLOCK)                              
*                                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DXMODE = DXOPENQ - OPEN SYSTEM FILES AND INITIALISE TABLES          *         
***********************************************************************         
PROCOPEN NTR1  ,                                                                
         L     RE,=V(UTL)                                                       
         MVI   4(RE),SYSCONQ                                                    
*                                  OPEN SYSTEM DISK FILES                       
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,CONTROL,CTFILES,IOL                      
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE SYSTEM FILES                             *          
***********************************************************************         
PROCCLOS NTR1                                                                   
         L     RE,=V(UTL)                                                       
         MVI   4(RE),SYSCONQ                                                    
         GOTO1 =V(DATAMGR),DMCB,DMCLSE,=C'FILES',,IOL                           
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* PROCESS CONTROL FILE SECURITY FILE DATA IN LOAD MODE                *         
***********************************************************************         
PROCLOAD NTR1                                                                   
         L     RF,DXSTPTR          RF=A(EXTRACT SYSTEM TABLE ENTRY)             
         MVC   TYPECODE,SXDTTYP-SXDTABD(RF)                                     
*                                                                               
         BRAS  RE,GETTYP           SET UP RECORD TYPE TABLE DATA                
*                                                                               
         L     RF,ARECTYP          A(RECORD TYPE) IN TYPTAB                     
         L     RF,TYPTLOAD-TYPTABD(RF) A(LOAD ROUTINE)                          
         BASR  RE,RF                                                            
         JNE   NEQXIT                                                           
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* PROCESS CONTROL FILE SECURITY DATA IN UPDATE MODE                   *         
***********************************************************************         
PROCUPDT NTR1                                                                   
         L     R6,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R6                                                         
*                                                                               
         CLI   RFILTY,X'A1'        CTFILE?                                      
         BE    PROCUP20                                                         
         CLI   RFILTY,X'AF'        GENFIL?                                      
         JNE   EQXIT                                                            
*                                                                               
         LA    R1,RFILTY                                                        
         BRAS  RE,SETFILE          SET FILE-SPECIFIC VARIABLES                  
*                                                                               
PROCUP20 DS    0H                                                               
         L     RF,DXSTPTR          RF=A(EXTRACT SYSTEM TABLE ENTRY)             
         MVC   TYPECODE,SXDTTYP-SXDTABD(RF)                                     
         BRAS  RE,GETTYP           SET TYPE TABLE DATA                          
*                                                                               
         BRAS  RE,PROCKEY          PROCESS RECORD KEY VALUES                    
         JNE   EQXIT                                                            
*                                                                               
         L     RF,ARECTYP          A(RECORD TYPE) IN TYPTAB                     
         L     RF,TYPTUPDT-TYPTABD(RF) A(UPDATE ROUTINE)                        
         BASR  RE,RF                                                            
         JNE   NEQXIT                                                           
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* PROCESS KEY VALUES IN RECOVERY RECORD                               *         
***********************************************************************         
PROCKEY  NTR1  ,                                                                
         LLC   R5,STATDISP                                                      
         AHI   R5,L'RECVHDR+4                                                   
*                                                                               
         USING RECDS,R6            RECOVERY HEADER                              
*                                                                               
         L     R6,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         LA    R4,0(R6,R5)         ADVANCE TO CONTROL BYTE                      
         TM    0(R4),X'80'         RECORD DELETED?                              
         JZ    PKEY110                                                          
*                                                                               
* RECORD DELETED HERE                                                           
*                                                                               
         CLI   DXACTION,C'C'                                                    
         JNE   PKEYNO                                                           
         CLI   RRECTY,X'02'                                                     
         JNE   PKEYNO                                                           
*                                                                               
         L     R6,DXACPYB                                                       
         LTR   R6,R6               DO WE HAVE A COPY BUFFER?                    
         JZ    PKEY100             NO, PROCESS AS "DELETE"                      
         LA    R4,0(R6,R5)         ADVANCE TO CONTROL BYTE                      
         TM    0(R4),X'80'         COPY ALSO DELETED?                           
         JO    PKEYNO              AVOID DELETED 'CHANGED'                      
*                                                                               
PKEY100  DS    0H                                                               
         MVI   DXACTION,C'D'                                                    
         J     PKEYOK                                                           
*                                                                               
* RECORD NOT DELETED HERE                                                       
* TEST FOR RESTORED RECORD                                                      
PKEY110  CLI   RRECTY,X'02'        ACTION CHANGE?                               
         JNE   PKEYOK                                                           
         L     R6,DXACPYB          COPY BUFFER                                  
         LTR   R6,R6               DO WE HAVE A COPY BUFFER?                    
         JZ    PKEYOK              NO, PROCESS AS "COPY"                        
         LA    R4,0(R6,R5)         ADVANCE TO CONTROL BYTE                      
         TM    0(R4),X'80'         COPY DELETED ALSO?                           
         JZ    PKEYOK              NO, PROCESS AS "COPY"                        
         MVI   DXACTION,C'A' IF COPY WAS DELETED, THEN IT IS A RESTORE          
         J     PKEYOK                                                           
*                                                                               
PKEYNO   J     NEQXIT                                                           
PKEYOK   J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* TYPE INITIALISATION                                                 *         
***********************************************************************         
INITTYP  NTR1                                                                   
         MVI   FLAG1,X'00'                                                      
         MVI   GXP.GXPFLAG1,X'00'                                               
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         L     R2,ARECTYP          A(RECORD TYPE) IN TYPTAB                     
         USING TYPTABD,R2                                                       
*                                                                               
         LA    R1,TYPTFILE                                                      
         BRAS  RE,SETFILE          SET FILE-SPECIFIC VARIABLES                  
*                                                                               
         J     EQXIT                                                            
*                                                                               
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FILE INITIALISATION                                                 *         
* R1 EXPECTED TO POINT TO FILE NUMBER                                 *         
***********************************************************************         
SETFILE  NTR1                                                                   
         CLI   0(R1),X'A1'         CTFILE?                                      
         JNE   SETFIL10                                                         
*                                                                               
         MVC   VARDIR,CTFILE                                                    
         MVC   VARFIL,=7C' '                                                    
         MVC   DADISP,=X'0000'     DISPLACEMENT TO DA, NONE FOR CTFILE          
         MVC   DATADISP,=X'001C'   28 FOR CTFILE                                
         MVI   STATDISP,X'1B'      27 FOR CTFILE                                
         OI    FLAG1,F1NOGET                                                    
         J     EQXIT                                                            
*                                                                               
SETFIL10 DS    0H                                                               
         CLI   0(R1),X'AF'         GENFIL                                       
         JNE   EQXIT                                                            
*                                                                               
         MVC   VARDIR,=CL7'GENDIR'                                              
         MVC   VARFIL,=CL7'GENFIL'                                              
         MVC   DADISP,=X'0024'     DISPLACEMENT TO DA, 36 FOR GENFIL            
         MVC   DATADISP,=X'002A'   42 FOR GENFIL                                
         MVI   STATDISP,X'22'                                                   
         NI    FLAG1,X'FF'-F1NOGET                                              
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* SUBROUTINE TO EXTRACT CONTROL RECORDS IN LOAD MODE                            
* DXARECB = A(RECORD)                                                           
* ARECTYP = A(RECORD TYPE) IN TYPTAB                                            
***********************************************************************         
GXLOAD   NTR1                                                                   
         L     RF,ARECTYP                                                       
         ICM   RF,15,TYPTFILT-TYPTABD(RF) A(FILTER ROUTINE)                     
         JZ    GXLO010             NO ADDRESS PASSED                            
*                                                                               
         BASR  RE,RF               CALL RECORD FILTER ROUTINE                   
         JNE   GXLO050                                                          
*                                                                               
GXLO010  DS    0H                                                               
         TM    FLAG1,F1NOGET       SKIP GETREC                                  
         JO    GXLO015                                                          
         TM    GXP.GXPFLAG1,GXPF1_MULTIQ                                        
         JO    GXLO015                                                          
*                                                                               
         L     RF,DXARECB                                                       
         AH    RF,DADISP                                                        
         ST    RF,VARDA                                                         
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'00',GETREC),VARFIL,VARDA,           X        
               DXARECB,DMWORK                                                   
         CLI   8(R1),X'00'                                                      
         JNE   GXLO050                                                          
*                                                                               
GXLO015  DS    0H                                                               
         BRAS  RE,INITALL          INITIALIZE OUTPUT RECORD                     
*                                                                               
         L     RF,ARECTYP                                                       
         L     RF,TYPTEXTR-TYPTABD(RF) A(EXTRACT ROUTINE)                       
         GOTO1 (RF),GXPARMS                                                     
         JNE   GXLO050                                                          
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   GXLO050                                                          
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 =V(GXCNVX),DMCB,(R7)                                             
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         ASI   MAXIOS,-1           ADD SIGNED IMMEDIATE                         
         JZ    NEQXIT                                                           
*                                                                               
GXLO050  DS    0H                  READ NEXT RECORD                             
         TM    FLAG1,F1NOMORE                                                   
         JO    NEQXIT                                                           
         TM    GXP.GXPFLAG1,GXPF1_MULTIQ                                        
         JO    EQXIT                                                            
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'00',DMRSEQ),VARDIR,IOKEY,DXARECB             
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* SUBROUTINE TO EXTRACT CONTROL TEMPO RECORD DATA IN UPDATE MODE      *         
* R2 = A(CONTROL SECURITY RECORD BUFFER)                              *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(FORMAT CONVERT ROUTINE)                                      *         
* P3 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
***********************************************************************         
GXUPDT   NTR1                                                                   
         BRAS  RE,INITALL                                                       
*                                                                               
         L     RF,ARECTYP                                                       
         L     RF,TYPTEXTR-TYPTABD(RF) A(EXTRACT ROUTINE)                       
         GOTO1 (RF),GXPARMS                                                     
         JNE   EQXIT                                                            
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   EQXIT                                                            
*                                                                               
         GOTO1 =V(GXCNVX),DMCB,(R7)                                             
*                                                                               
* FOR BUYING AGENCY ACTION "CHANGE"                                             
* GENERATE DELETE-ADD RECORD PAIRS                                              
*                                                                               
         L     RF,DXASQLB                                                       
         USING DXHDRD,RF                                                        
*                                                                               
         CLC   GXBAGQ,DXHDRTYP     BUYING AGENCY?                               
         BNE   GXUP80                                                           
         CLI   DXHDRRTY,C'C'                                                    
         BNE   GXUP80                                                           
*                                                                               
         MVI   DXHDRRTY,C'D'                                                    
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         L     RF,DXASQLB                                                       
         MVI   DXHDRRTY,C'A'                                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
GXUP80   DS    0H                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         ASI   MAXIOS,-1           ADD SIGNED IMMEDIATE                         
         JZ    NEQXIT                                                           
         J     EQXIT                                                            
*                                                                               
       ++INCLUDE GXRECID                                                        
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* INITIALISE ALL EXTRACT RECORDS                                                
***********************************************************************         
INITALL  NTR1                                                                   
         L     R3,DXAXREC          R3=A(EXTRACT RECORD AREA)                    
         USING DXHDRD,R3                                                        
         L     R6,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R6                                                         
*                                                                               
         L     RE,ARECTYP                                                       
         XR    R2,R2                                                            
         ICM   R2,3,TYPTRLEN-TYPTABD(RE) R2=REC LEN                             
*                                                                               
         LR    R0,R3                                                            
         LR    R1,R2                                                            
         LA    RE,*                                                             
         SR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         SLL   R2,16                                                            
         ST    R2,DXHDRLEN         SET MINIMUM RECORD LEN IN HDR                
*                                                                               
         MVI   DXHDRRTY-1,MXTRTQ                                                
         MVC   DXHDRRTY,DXACTION                                                
         MVI   DXHDRCDT-1,MXTRTQ                                                
**                                                                              
         CLI   DXMODE,DXLOADQ      TEST IF LOAD MODE                            
         JE    IALL100                                                          
*                                  HERE IF UPDATE MODE                          
*                                  FORMAT DATE AND TIME FROM RCVHDR             
         ICM   RF,15,DXCOMFAC                                                   
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,RDATE),(X'20',DXHDRCDT+2)                           
         MVC   DXHDRCDT(2),DXCENT                                               
         MVI   DXHDRCTI-1,MXTRTQ                                                
         ICM   RF,15,RTIME                                                      
         TM    RTIME,X'80'                                                      
         JZ    *+12                                                             
         SLL   RF,1                                                             
         SRL   RF,5                                                             
         XC    DUB,DUB                                                          
         STCM  RF,15,DUB+4                                                      
         OI    DUB+7,X'0C'                                                      
         UNPK  DXHDRCTI(6),DUB+4(4)                                             
         OI    DXHDRCTI+5,X'F0'                                                 
         J     EQXIT                                                            
*                                                                               
* HERE IF LOAD MODE                                                             
*                                                                               
IALL100  MVC   DXHDRCDT,DXDATEN    SET TODAYS DATE                              
         MVI   DXHDRCTI-1,MXTRTQ                                                
         MVC   DXHDRCTI,DXTIMEN                                                 
         CLI   DXDAFORM,C'Y'                                                    
         JNE   EQXIT                                                            
*                                                                               
         MVI   DXHDRCDT+00,C''''                                                
         MVC   DXHDRCDT+01(6),DXDATEN+2                                         
         MVC   DXHDRCDT+07(2),=C'  '                                            
         MVC   DXHDRCDT+09(2),DXTIMEN                                           
         MVI   DXHDRCDT+11,C':'                                                 
         MVC   DXHDRCDT+12(2),DXTIMEN+2                                         
         MVI   DXHDRCDT+14,C''''                                                
*                                                                               
         J     EQXIT                                                            
         DROP  R6,R3                                                            
*                                                                               
*                                                                               
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* GET RECORD TYPE TABLE VALUES FROM 3 CHARACTER CODE                  *         
***********************************************************************         
GETTYP   LARL  RF,TYPTAB                                                        
         USING TYPTABD,RF                                                       
*                                                                               
GTYP010  CLI   0(RF),X'FF'         END OF TABLE                                 
         JE    *+2                                                              
*                                                                               
         CLC   TYPECODE,TYPTNAM    COMPARE NAME                                 
         JNE   GTYP020                                                          
*                                                                               
         ST    RF,ARECTYP                                                       
         BR    RE                                                               
*                                                                               
GTYP020  LA    RF,TYPTABDLQ(RF)    GET NEXT ENTRY                               
         J     GTYP010                                                          
         DROP  RF                                                               
*                                                                               
*                                                                               
*                                                                               
         DS    0D                                                               
TYPTAB   DS    0X                                                               
         DC    CL3'ALL'            ALL RECORD TYPES                             
         DC    AL1(0)              FILE NUMBER                                  
         DC    A(0)                FILTER ROUTINE                               
         DC    A(LOADALL)          LOAD ROUTINE                                 
         DC    A(UPDTALL)          UPDATE ROUTINE                               
         DC    A(0)                EXTRACT ROUTINE                              
         DC    AL2(0)              EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
TYPTABLQ EQU   *-TYPTAB                                                         
*                                                                               
         DC    CL3'DAR'            DARE ("UMBRELLA", SEE TYPTABDAR)             
         DC    AL1(0)              FILE NUMBER                                  
         DC    A(0)                FILTER ROUTINE                               
         DC    A(LOADDAR)          LOAD ROUTINE                                 
         DC    A(UPDTDAR)          UPDATE ROUTINE                               
         DC    A(0)                EXTRACT ROUTINE                              
         DC    AL2(0)              EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'MDM'            MDM ("UMBRELLA", SEE TYPTABMDM)              
         DC    AL1(0)              FILE NUMBER                                  
         DC    A(0)                FILTER ROUTINE                               
         DC    A(LOADMDM)          LOAD ROUTINE                                 
         DC    A(UPDTMDM)          UPDATE ROUTINE                               
         DC    A(0)                EXTRACT ROUTINE                              
         DC    AL2(0)              EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
TYPTABALL DS   0H                  INDIVIDUAL RECORDS                           
*                                                                               
         DC    CL3'DST'  NON-RADIO DSTA - DARE STATION RECORDS 05521            
         DC    X'AF'               GENFIL                                       
         DC    A(FILTDST)          FILTER ROUTINE                               
         DC    A(LOADDST)          LOAD ROUTINE                                 
         DC    A(UPDTDST)          UPDATE ROUTINE                               
         DC    V(GXDSTC)           EXTRACT ROUTINE                              
         DC    AL2(GXDSTDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'DSR' RADIO-ONLY DSTA - DARE STATION RECORDS 05527            
         DC    X'AF'               GENFIL                                       
         DC    A(FILTDSR)          FILTER ROUTINE                               
         DC    A(LOADDST)          LOAD ROUTINE                                 
         DC    A(UPDTDSR)          UPDATE ROUTINE                               
         DC    V(GXDSTRC)          EXTRACT ROUTINE                              
         DC    AL2(GXDSTDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'PAR'            DARE PARTNER RECORDS 05522                   
         DC    X'AF'               GENFIL                                       
         DC    A(FILTPAR)          FILTER ROUTINE                               
         DC    A(LOADPAR)          LOAD ROUTINE                                 
         DC    A(UPDTPAR)          UPDATE ROUTINE                               
         DC    V(GXPARC)           EXTRACT ROUTINE                              
         DC    AL2(GXPARDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'FEA'            DARE FEATURE RECORDS 05523                   
         DC    X'AF'               GENFIL                                       
         DC    A(FILTFEA)          FILTER ROUTINE - SAME AS VENDOR              
         DC    A(LOADFEA)          LOAD ROUTINE                                 
         DC    A(UPDTFEA)          UPDATE ROUTINE                               
         DC    V(GXFEAC)           EXTRACT ROUTINE                              
         DC    AL2(GXFEADL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'VEN'            DARE VENDOR RECORDS 05524                    
         DC    X'AF'               GENFIL                                       
         DC    A(FILTVEN)          FILTER ROUTINE                               
         DC    A(LOADVEN)          LOAD ROUTINE                                 
         DC    A(UPDTVEN)          UPDATE ROUTINE                               
         DC    V(GXVENC)           EXTRACT ROUTINE                              
         DC    AL2(GXVENDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'VEF'            DARE VENDOR FEATURE RECORDS 05525            
         DC    X'AF'               GENFIL                                       
         DC    A(FILTVEN)          FILTER ROUTINE - SAME AS VENDOR              
         DC    A(LOADVEN)          LOAD ROUTINE                                 
         DC    A(UPDTVEN)          UPDATE ROUTINE                               
         DC    V(GXVEFC)           EXTRACT ROUTINE                              
         DC    AL2(GXVEFDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'IDI'            IDI RECORDS 05526                            
         DC    X'A1'               CTFILE                                       
         DC    A(FILTIDI)          FILTER ROUTINE                               
         DC    A(LOADIDI)          LOAD ROUTINE                                 
         DC    A(UPDTIDI)          UPDATE ROUTINE                               
         DC    V(GXIDIC)           EXTRACT ROUTINE                              
         DC    AL2(GXIDIDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'BAG'            BUYING AGENCY 05528                          
         DC    X'AF'               GENFIL                                       
         DC    A(FILTBAG)          FILTER ROUTINE                               
         DC    A(LOADBAG)          LOAD ROUTINE                                 
         DC    A(UPDTBAG)          UPDATE ROUTINE                               
         DC    V(GXBAGC)           EXTRACT ROUTINE                              
         DC    AL2(GXBAGDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
TYPTABX  DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD ALL RECORDS                                                    *         
***********************************************************************         
LOADALL  NTR1                                                                   
         LARL  R3,TYPTABALL        INDIVIDUAL RECORDS                           
         USING TYPTABD,R3                                                       
*                                                                               
LOAA02   CLI   0(R3),X'FF'         EOT?                                         
         JE    EQXIT                                                            
*                                                                               
         MVC   TYPECODE,0(R3)                                                   
         ST    R3,ARECTYP                                                       
*                                                                               
         L     RF,TYPTLOAD         LOAD ROUTINE                                 
         BASR  RE,RF                                                            
         JNE   NEQXIT                                                           
*                                                                               
LOAA04   LA    R3,TYPTABDLQ(R3)                                                 
         J     LOAA02                                                           
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD DARE RECORDS                                                   *         
***********************************************************************         
LOADDAR  NTR1                                                                   
         LARL  R3,TYPTABDAR                                                     
         USING TYPTABD,R3                                                       
*                                                                               
LDAR02   CLI   0(R3),X'FF'         EOT?                                         
         JE    EQXIT                                                            
*                                                                               
         MVC   TYPECODE,0(R3)                                                   
         ST    R3,ARECTYP                                                       
*                                                                               
         L     RF,TYPTLOAD         LOAD ROUTINE                                 
         BASR  RE,RF                                                            
         JNE   NEQXIT                                                           
*                                                                               
         LA    R3,TYPTABDLQ(R3)                                                 
         J     LDAR02                                                           
         DROP  R3                                                               
*                                                                               
         DS    0D                                                               
TYPTABDAR DS   0X                  DARE RECORDS FOR BLOCKCHAIN                  
         DC    CL3'DST'  NON-RADIO DSTA - DARE STATION RECORDS 05521            
         DC    X'AF'               GENFIL                                       
         DC    A(FILTDST)          FILTER ROUTINE                               
         DC    A(LOADDST)          LOAD ROUTINE                                 
         DC    A(UPDTDST)          UPDATE ROUTINE                               
         DC    V(GXDSTC)           EXTRACT ROUTINE                              
         DC    AL2(GXDSTDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'DSR' RADIO-ONLY DSTA - DARE STATION RECORDS 05527            
         DC    X'AF'               GENFIL                                       
         DC    A(FILTDSR)          FILTER ROUTINE                               
         DC    A(LOADDST)          LOAD ROUTINE                                 
         DC    A(UPDTDST)          UPDATE ROUTINE                               
         DC    V(GXDSTRC)          EXTRACT ROUTINE                              
         DC    AL2(GXDSTDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'PAR'            DARE PARTNER RECORDS 05522                   
         DC    X'AF'               GENFIL                                       
         DC    A(FILTPAR)          FILTER ROUTINE                               
         DC    A(LOADPAR)          LOAD ROUTINE                                 
         DC    A(UPDTPAR)          UPDATE ROUTINE                               
         DC    V(GXPARC)           EXTRACT ROUTINE                              
         DC    AL2(GXPARDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'FEA'            DARE FEATURE RECORDS 05523                   
         DC    X'AF'               GENFIL                                       
         DC    A(FILTFEA)          FILTER ROUTINE - SAME AS VENDOR              
         DC    A(LOADFEA)          LOAD ROUTINE                                 
         DC    A(UPDTFEA)          UPDATE ROUTINE                               
         DC    V(GXFEAC)           EXTRACT ROUTINE                              
         DC    AL2(GXFEADL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'VEN'            DARE VENDOR RECORDS 05524                    
         DC    X'AF'               GENFIL                                       
         DC    A(FILTVEN)          FILTER ROUTINE                               
         DC    A(LOADVEN)          LOAD ROUTINE                                 
         DC    A(UPDTVEN)          UPDATE ROUTINE                               
         DC    V(GXVENC)           EXTRACT ROUTINE                              
         DC    AL2(GXVENDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'VEF'            DARE VENDOR FEATURE RECORDS 05525            
         DC    X'AF'               GENFIL                                       
         DC    A(FILTVEN)          FILTER ROUTINE - SAME AS VENDOR              
         DC    A(LOADVEN)          LOAD ROUTINE                                 
         DC    A(UPDTVEN)          UPDATE ROUTINE                               
         DC    V(GXVEFC)           EXTRACT ROUTINE                              
         DC    AL2(GXVEFDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
         DC    CL3'IDI'            IDI RECORDS 05526                            
         DC    X'A1'               CTFILE                                       
         DC    A(FILTIDI)          FILTER ROUTINE                               
         DC    A(LOADIDI)          LOAD ROUTINE                                 
         DC    A(UPDTIDI)          UPDATE ROUTINE                               
         DC    V(GXIDIC)           EXTRACT ROUTINE                              
         DC    AL2(GXIDIDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
TYPTABDARX DC  X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD MDM RECORDS                                                    *         
***********************************************************************         
LOADMDM  NTR1                                                                   
         LARL  R3,TYPTABMDM                                                     
         USING TYPTABD,R3                                                       
*                                                                               
LMDM02   CLI   0(R3),X'FF'         EOT?                                         
         JE    EQXIT                                                            
*                                                                               
         MVC   TYPECODE,0(R3)                                                   
         ST    R3,ARECTYP                                                       
*                                                                               
         L     RF,TYPTLOAD         LOAD ROUTINE                                 
         BASR  RE,RF                                                            
         JNE   NEQXIT                                                           
*                                                                               
         LA    R3,TYPTABDLQ(R3)                                                 
         J     LMDM02                                                           
         DROP  R3                                                               
*                                                                               
         DS    0D                                                               
TYPTABMDM DS   0X                                                               
         DC    CL3'BAG'            BUYING AGENCY 05528                          
         DC    X'AF'               GENFIL                                       
         DC    A(FILTBAG)          FILTER ROUTINE                               
         DC    A(LOADBAG)          LOAD ROUTINE                                 
         DC    A(UPDTBAG)          UPDATE ROUTINE                               
         DC    V(GXBAGC)           EXTRACT ROUTINE                              
         DC    AL2(GXBAGDL)        EXTRACT RECORD LENGTH                        
         DC    2X'00'              SPARE                                        
*                                                                               
TYPTABMDMX DC  X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD IDI RECORDS                                                              
***********************************************************************         
LOADIDI  NTR1                                                                   
         BRAS  RE,INITTYP                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            SET KEY TO READ FIRST USER ID RECORD         
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
* SET ID FIELD TO C' ' TO GO STRAIGHT TO CHARACTER ID RECORDS,                  
* SKIPPING ALL THE BINARIES                                                     
         MVI   CTIKID,X'40'                                                     
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'00',DMRDHI),CTFILE,IOKEY,DXARECB             
*                                                                               
LIDI010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    EQXIT                                                            
*                                                                               
         BRAS  RE,GXLOAD                                                        
         JNE   EQXIT                                                            
         J     LIDI010             EXTRACT NEXT IDI RECORD                      
*                                                                               
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD DARE STATION RECORDS                                           *         
***********************************************************************         
LOADDST  NTR1                                                                   
         BRAS  RE,INITTYP                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            SET KEY TO READ FIRST DSTA RECORD            
         USING STAKEYD,R2                                                       
         MVI   STAKSYS,STAKSYSQ    X'00'                                        
         MVI   STAKTYP,STAKTYPQ    X'5A'                                        
         MVI   STAKMEDA,X'01'                                                   
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'00',DMRDHI),=CL8'GENDIR',           X        
               IOKEY,DXARECB                                                    
*                                                                               
LDST010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    EQXIT                                                            
*                                                                               
         BRAS  RE,GXLOAD                                                        
         JNE   EQXIT                                                            
         J     LDST010                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD DARE PARTNER RECORDS                                           *         
***********************************************************************         
LOADPAR  NTR1                                                                   
         BRAS  RE,INITTYP                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            SET KEY TO READ FIRST PARTNER REC            
         USING CTEPRECD,R2                                                      
         MVI   CTEPKTYP,CTEPKTYQ   X'00'                                        
         MVI   CTEPKSTY,CTEPKSTQ   X'39'                                        
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'00',DMRDHI),=CL8'GENDIR',           X        
               IOKEY,DXARECB                                                    
*                                                                               
LPAR010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    EQXIT                                                            
*                                                                               
         BRAS  RE,GXLOAD                                                        
         JNE   EQXIT                                                            
         J     LPAR010                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD DARE VENDOR AND DARE VENDOR FEATURE RECORDS                    *         
* NOTE, THIS LOAD ROUTINE IS USED FOR BOTH VEN AND VEF TYPES          *         
***********************************************************************         
LOADVEN  NTR1                                                                   
         BRAS  RE,INITTYP                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            SET KEY TO READ FIRST VENDOR REC             
         USING CTEVRECD,R2                                                      
         MVI   CTEVKTYP,CTEVKTYQ   X'00'                                        
         MVI   CTEVKSTY,CTEVKSTQ   X'38'                                        
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'00',DMRDHI),=CL8'GENDIR',           X        
               IOKEY,DXARECB                                                    
*                                                                               
LVEN010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    EQXIT                                                            
*                                                                               
         BRAS  RE,GXLOAD                                                        
         JNE   EQXIT                                                            
         J     LVEN010                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD DARE FEATURE RECORDS                                           *         
***********************************************************************         
LOADFEA  NTR1                                                                   
         BRAS  RE,INITTYP                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            SET KEY TO READ FIRST FEATURE                
         USING CTEFRECD,R2                                                      
         MVI   CTEFKTYP,CTEFKTYQ   X'00'                                        
         MVI   CTEFKSTY,CTEFKSTQ   X'3B'                                        
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'00',DMRDHI),=CL8'GENDIR',           X        
               IOKEY,DXARECB                                                    
*                                                                               
LFEA010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    EQXIT                                                            
*                                                                               
         BRAS  RE,GXLOAD                                                        
         JNE   EQXIT                                                            
         J     LFEA010                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOAD BUYING AGENCY RECORDS                                                    
***********************************************************************         
LOADBAG  NTR1                                                                   
         BRAS  RE,INITTYP                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            SET KEY TO READ FIRST FEATURE                
         USING BAGRECD,R2                                                       
         MVI   BAGKMIN,BAGKMINQ    C'T'                                         
         MVI   BAGKTYP,BAGKTYPQ    C'B'                                         
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'00',DMRDHI),=CL8'GENDIR',           X        
               IOKEY,DXARECB                                                    
*                                                                               
LBAG010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    EQXIT                                                            
*                                                                               
         BRAS  RE,GXLOAD                                                        
         JNE   EQXIT                                                            
         J     LBAG010                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE ALL RECORD DATA                                              *         
***********************************************************************         
UPDTALL  NTR1                                                                   
         LARL  R3,TYPTABALL                                                     
         USING TYPTABD,R3                                                       
*                                                                               
UALL010  CLI   0(R3),X'FF'         EXIT IF END OF TABLE                         
         JE    EQXIT                                                            
*                                                                               
         MVC   TYPECODE,0(R3)                                                   
         ST    R3,ARECTYP                                                       
         L     RF,TYPTUPDT         A(UPDATE ROUTINE)                            
         BASR  RE,RF                                                            
         JNE   NEQXIT                                                           
*                                                                               
UALL020  LA    R3,TYPTABDLQ(R3)    GET NEXT ENTRY                               
         J     UALL010                                                          
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE DAR RECORD DATA                                              *         
***********************************************************************         
UPDTDAR  NTR1                                                                   
         LARL  R3,TYPTABDAR                                                     
         USING TYPTABD,R3                                                       
*                                                                               
UDAR010  CLI   0(R3),X'FF'         EXIT IF END OF TABLE                         
         JE    EQXIT                                                            
*                                                                               
         MVC   TYPECODE,0(R3)                                                   
         ST    R3,ARECTYP                                                       
         L     RF,TYPTUPDT         A(UPDATE ROUTINE)                            
         BASR  RE,RF                                                            
         JNE   NEQXIT                                                           
*                                                                               
UDAR020  LA    R3,TYPTABDLQ(R3)    GET NEXT ENTRY                               
         J     UDAR010                                                          
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE MDM RECORD DATA                                              *         
***********************************************************************         
UPDTMDM  NTR1                                                                   
         LARL  R3,TYPTABMDM                                                     
         USING TYPTABD,R3                                                       
*                                                                               
UMDM010  CLI   0(R3),X'FF'         EXIT IF END OF TABLE                         
         JE    EQXIT                                                            
*                                                                               
         MVC   TYPECODE,0(R3)                                                   
         ST    R3,ARECTYP                                                       
         L     RF,TYPTUPDT         A(UPDATE ROUTINE)                            
         BASR  RE,RF                                                            
         JNE   NEQXIT                                                           
*                                                                               
UMDM020  LA    R3,TYPTABDLQ(R3)    GET NEXT ENTRY                               
         J     UMDM010                                                          
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE IDI RECORD DATA                                                        
***********************************************************************         
UPDTIDI  NTR1                                                                   
         BRAS  RE,INITTYP                                                       
         BRAS  RE,FILTIDI                                                       
         JNE   EQXIT                                                            
*                                                                               
         BRAS  RE,GXUPDT                                                        
         JNE   NEQXIT                                                           
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE DARE STATION RECORD DATA, NON-RADIO                                    
***********************************************************************         
UPDTDST  NTR1                                                                   
         BRAS  RE,INITTYP                                                       
         BRAS  RE,FILTDST                                                       
         JNE   EQXIT                                                            
*                                                                               
         BRAS  RE,GXUPDT                                                        
         JNE   NEQXIT                                                           
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE DARE STATION RECORD DATA, RADIO                                        
***********************************************************************         
UPDTDSR  NTR1                                                                   
         BRAS  RE,INITTYP                                                       
         BRAS  RE,FILTDSR                                                       
         JNE   EQXIT                                                            
*                                                                               
         BRAS  RE,GXUPDT                                                        
         JNE   NEQXIT                                                           
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE DARE PARTNER RECORD DATA                                               
***********************************************************************         
UPDTPAR  NTR1                                                                   
         BRAS  RE,INITTYP                                                       
         BRAS  RE,FILTPAR                                                       
         JNE   EQXIT                                                            
*                                                                               
         BRAS  RE,GXUPDT                                                        
         JNE   NEQXIT                                                           
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE DARE VENDOR RECORD DATA                                                
* NOTE, THIS LOAD ROUTINE IS USED FOR BOTH VEN AND VEF TYPES          *         
***********************************************************************         
UPDTVEN  NTR1                                                                   
         BRAS  RE,INITTYP                                                       
         BRAS  RE,FILTVEN                                                       
         JNE   EQXIT                                                            
*                                                                               
         CLC   TYPECODE,=C'VEF'                                                 
         JNE   *+8                                                              
         OI    GXP.GXPFLAG1,GXPF1_MULTIQ                                        
*                                                                               
UPDTV10  DS    0H                                                               
         BRAS  RE,GXUPDT                                                        
         JNE   NEQXIT                                                           
*                                                                               
         TM    GXP.GXPFLAG1,GXPF1_MULTIQ                                        
         JO    UPDTV10                                                          
*                                                                               
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE DARE FEATURE RECORDS                                                   
***********************************************************************         
UPDTFEA  NTR1                                                                   
         BRAS  RE,INITTYP                                                       
         BRAS  RE,FILTFEA                                                       
         JNE   EQXIT                                                            
*                                                                               
         BRAS  RE,GXUPDT                                                        
         JNE   NEQXIT                                                           
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* UPDATE BUYING AGENCY RECORDS                                                  
***********************************************************************         
UPDTBAG  NTR1                                                                   
         BRAS  RE,INITTYP                                                       
         BRAS  RE,FILTBAG                                                       
         JNE   EQXIT                                                            
*                                                                               
         BRAS  RE,GXUPDT                                                        
         JNE   NEQXIT                                                           
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FILTER IDI RECORD DATA                                                        
***********************************************************************         
FILTIDI  NTR1                                                                   
         L     R6,AREC             RECOVERY RECORD BUFFER                       
         USING CTIREC,R6                                                        
*                                                                               
         CLI   CTIKTYP,CTIKTYPQ                                                 
         JE    *+12                                                             
         OI    FLAG1,F1NOMORE                                                   
         J     NEQXIT                                                           
*                                                                               
         CLI   CTIKID,X'00'                                                     
         JNH   NEQXIT                                                           
*                                                                               
* ONLY EXTRACT THOSE THAT HAVE DARE PARTNER FILLED IN                           
*                                                                               
         MVI   ELCODE,CTUSAELQ     X'33' AGENCY EXTRA INFO                      
         BRAS  RE,GETEL                                                         
         JNE   NEQXIT                                                           
         CLI   CTUSADPI-CTUSAD(R6),C' ' DARE PARTNER PRESENT?                   
         JNH   NEQXIT              NO                                           
         J     EQXIT                                                            
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FILTER DSTA RECORD DATA - BOTH RADIO, AND NON-RADIO                           
***********************************************************************         
FILTDS   NTR1                                                                   
         L     R6,AREC             RECOVERY RECORD BUFFER                       
         USING STAKEYD,R6                                                       
*                                                                               
         CLI   STAKSYS,STAKSYSQ    X'00'                                        
         JNE   FILTDSX                                                          
         CLI   STAKTYP,STAKTYPQ    X'5A'                                        
         JE    EQXIT                                                            
*                                                                               
FILTDSX  DS    0H                                                               
         OI    FLAG1,F1NOMORE                                                   
         J     NEQXIT                                                           
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FILTER DSTA RECORD DATA, NON-RADIO                                            
***********************************************************************         
FILTDST  NTR1                                                                   
         BRAS  RE,FILTDS                                                        
         JNE   NEQXIT                                                           
*                                                                               
         L     R6,AREC             RECOVERY RECORD BUFFER                       
         USING STAKEYD,R6                                                       
         CLI   STAKMEDA,C'R'                                                    
         JE    NEQXIT                                                           
*                                                                               
FILTDSTX DS    0H                                                               
         J     EQXIT                                                            
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FILTER DSTA RECORD DATA, RADIO ONLY                                           
***********************************************************************         
FILTDSR  NTR1                                                                   
         BRAS  RE,FILTDS                                                        
         JNE   NEQXIT                                                           
*                                                                               
         L     R6,AREC             RECOVERY RECORD BUFFER                       
         USING STAKEYD,R6                                                       
         CLI   STAKMEDA,C'R'                                                    
         JNE   NEQXIT                                                           
*                                                                               
FILTDSRX DS    0H                                                               
         J     EQXIT                                                            
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FILTER DARE PARTNER RECORDS                                                   
***********************************************************************         
FILTPAR  NTR1                                                                   
         L     R6,AREC             RECOVERY RECORD BUFFER                       
         USING CTEPRECD,R6                                                      
*                                                                               
         CLI   CTEPKTYP,CTEPKTYQ   X'00'                                        
         JNE   FILTPARX                                                         
         CLI   CTEPKSTY,CTEPKSTQ   X'39'                                        
         JNE   FILTPARX                                                         
         CLI   CTEPKRSP,X'00'                                                   
         JE    EQXIT                                                            
*                                                                               
FILTPARX DS    0H                                                               
         OI    FLAG1,F1NOMORE                                                   
         J     NEQXIT                                                           
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FILTER DARE VENDOR RECORDS                                                    
* NOTE, THIS LOAD ROUTINE IS USED FOR BOTH VEN AND VEF TYPES          *         
***********************************************************************         
FILTVEN  NTR1                                                                   
         L     R6,AREC             RECOVERY RECORD BUFFER                       
         USING CTEVRECD,R6                                                      
*                                                                               
         CLI   CTEVKTYP,CTEVKTYQ   X'00'                                        
         JNE   FILTVENX                                                         
         CLI   CTEVKSTY,CTEVKSTQ   X'38'                                        
         JE    EQXIT                                                            
*                                                                               
FILTVENX DS    0H                                                               
         OI    FLAG1,F1NOMORE                                                   
         J     NEQXIT                                                           
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FILTER DARE FEATURE RECORDS                                                   
***********************************************************************         
FILTFEA  NTR1                                                                   
         L     R6,AREC             RECOVERY RECORD BUFFER                       
         USING CTEFRECD,R6                                                      
*                                                                               
         CLI   CTEFKTYP,CTEFKTYQ   X'00'                                        
         JNE   FILTFEAX                                                         
         CLI   CTEFKSTY,CTEFKSTQ   X'3B'                                        
         JE    EQXIT                                                            
*                                                                               
FILTFEAX DS    0H                                                               
         OI    FLAG1,F1NOMORE                                                   
         J     NEQXIT                                                           
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FILTER BUYING AGENCY RECORDS                                                  
***********************************************************************         
FILTBAG  NTR1                                                                   
         L     R6,AREC             RECOVERY RECORD BUFFER                       
         USING BAGRECD,R6                                                       
*                                                                               
         CLI   BAGKMAJ,X'00'                                                    
         JNE   FILTBAGX                                                         
         CLI   BAGKMIN,BAGKMINQ    C'T'                                         
         JNE   FILTBAGX                                                         
         CLI   BAGKTYP,BAGKTYPQ    C'B'                                         
         JE    EQXIT                                                            
*                                                                               
FILTBAGX DS    0H                                                               
         OI    FLAG1,F1NOMORE                                                   
         J     NEQXIT                                                           
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
WORKC    CSECT                                                                  
         DS    (64*1024)X             WORKING STORAGE POOL                      
         DS    0D                                                               
*                                                                               
*                                                                               
*                                                                               
$$DATA   LOCTR ,                 DATA LOCATION CLOSE TO RB                      
MXTRTQ   EQU   X'5E'               FIELD SEPERATOR CHR                          
SPACES   DC    80C' '                                                           
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMCLSE   DC    C'DMCLSE '                                                       
DMFAST   DC    C'DMFAST '                                                       
GETREC   DC    C'GETREC '                                                       
DMRFIL   DC    C'RECOVER'                                                       
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
CTFILES  DC    CL8'NCTFILE '                                                    
         DC    CL8'NGENDIR '                                                    
         DC    CL8'NGENFIL '                                                    
         DC    CL8'X       '                                                    
DTFADDR  DC    F'0'                                                             
*                                                                               
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
         DS    0D                                                               
         DC    C'**SSB***'                                                      
       ++INCLUDE FASSBOFF                                                       
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         DC    X'02'               FOR DATAMGR (OFFLINE NO RECOVERY)            
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
*                                                                               
*                                                                               
*                                                                               
* DSECT TO COVER RECOVERY HEADER                                                
*                                                                               
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
*                                                                               
*                                                                               
**********************************************************                      
WORKD    DSECT                      GLOBAL WORKING STORAGE                      
*                                                                               
DUB      DS    D                                                                
ARECTYP  DS    A                   A(RECORD) IN TYPTAB                          
AREC     DS    A                   A(RECORD) IN DXARECB                         
DMCB     DS    6F                                                               
FULL     DS    F                                                                
IOKEY    DS    CL40                                                             
IOKEYSV  DS    CL40                                                             
VARDIR   DS    CL7                                                              
VARFIL   DS    CL7                                                              
DMWORK   DS    12D                                                              
DATADISP DS    H                                                                
DADISP   DS    H                                                                
STATDISP DS    X                                                                
VARDA    DS    A                                                                
ELCODE   DS    XL1                                                              
BYTE     DS    XL1                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
ACTIONSV DS    CL1                                                              
PLATFORM DS    CL1                                                              
*                                                                               
FLAG1    DS    X                                                                
F1NOGET  EQU   X'01'               NO GETREC CALL                               
F1NOMORE EQU   X'02'               STOP READING RECORDS                         
*                                                                               
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
*                                                                               
TYPECODE DS    CL3                 TYPE CODE                                    
*                                                                               
TODAYC   DS    XL2                                                              
*                                                                               
GXPARMS  DS    XL(GXPARMDLQ)                                                    
*                                                                               
WORK     DS    XL256                                                            
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    2048X                                                            
WORKX    DS    0D                                                               
*                                                                               
*                                                                               
*                                                                               
TYPTABD  DSECT                                                                  
TYPTNAM  DS    CL3                 RECORT TYPE NAME                             
TYPTFILE DS    X                   FILE NUMBER (DMFILTAB)                       
TYPTFILT DS    A                   A(FILTER ROUTINE)                            
TYPTLOAD DS    A                   A(LOAD ROUTINE)                              
TYPTUPDT DS    A                   A(UPDATE ROUTINE)                            
TYPTEXTR DS    A                   A(EXTRACT)                                   
TYPTRLEN DS    XL2                 RECORD LENGTH                                
         DS    XL2                 SPARE                                        
TYPTABDLQ EQU  *-TYPTABD                                                        
*                                                                               
*                                                                               
*                                                                               
       ++INCLUDE DXDSECTS                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE CTGENSTAD                                                      
       ++INCLUDE FASYSEQUS                                                      
       ++INCLUDE GXRECD                                                         
       ++INCLUDE GXPARMD                                                        
       ++INCLUDE DXHDRD                                                         
       ++INCLUDE GEGENEDI                                                       
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE GEGENBAG                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013GXTRACT   01/29/20'                                      
         END                                                                    
