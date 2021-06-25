*          DATA SET DEDEMADDR  AT LEVEL 112 AS OF 08/29/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE T00ADEE                                                                  
*INCLUDE BINSR31                                                                
         TITLE 'DEMADDR - BUILD AND ADDRESS DEMO TABLES'                        
         PRINT NOGEN                                                            
DEMADDR  RSECT                                                                  
         NMOD1 WORKL,*DEMADDR,RR=RE,CLEAR=YES                                   
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
         ST    R1,CALLR1           SAVE CALLERS R1                              
         STAR  CLEAR=Y,ARS=OFF                                                  
         USING DMSPACED,DSPHD                                                   
*                                                                               
         LTR   R1,R1               IF R1=0, THIS IS OFFLINE APPLICATION         
         JZ    XMOD                NO OFFLINE INIT NECESSARY                    
*                                                                               
         BRAS  RE,INIT             INITIALISE ADDRESSES                         
*                                                                               
         LTR   R2,R1               R2=A(PARM LIST)                              
         JZ    XMOD                                                             
*                                                                               
         XC    FILTERS,FILTERS                                                  
*                                                                               
         L     R4,0(R2)                                                         
         CLI   0(R2),X'FE'         CLEAR TABLE REQUEST?                         
         JNE   *+12                                                             
         MVI   REINIT,C'Y'         CLEAR TABLE AND RE-INITIALIZE                
         B     DEM06               ASSUME LIST PASSED                           
*                                                                               
         CLI   0(R2),X'FF'         TEST IF LIST PASSED                          
         JE    DEM06                                                            
*                                  FIND TABLE NAME IN CTRLTAB                   
         LARL  R3,CTLTAB                                                        
         USING CTLTABD,R3                                                       
         XC    0(4,R2),0(R2)       BUILD SINGLE ENTRY LIST IN PARM LIST         
         MVI   4(R2),X'FF'         FORCE EOT                                    
*                                                                               
DEM02    CLI   CTLNAME,EOT         EOT?                                         
         JE    DEM04                                                            
         CLC   CTLNAME(4),0(R4)    MATCH TABLE NAME?                            
         JE    *+12                                                             
         LA    R3,CTLLNQ(R3)       NEXT ENTRY                                   
         J     DEM02                                                            
*                                                                               
         LR    R4,R2                                                            
         MVC   0(1,R4),CTLPHSN     SET TABLE NUMBER IN LIST                     
         J     DEM12                                                            
*                                                                               
DEM04    DC    H'0',C'INVALID PHASE NAME'                                       
*                                                                               
DEM06    CLI   0(R4),X'FF'         EOT?                                         
         JNE   *+14                                                             
         MVC   8(2,R2),=AL2(OCT_82) RETURN EFFECTIVE NEW BOOK VALUE             
         J     XMOD                                                             
*                                                                               
         LARL  R3,CTLTAB                                                        
DEM08    CLI   CTLNAME,EOT         REACHED END OF TABLE?                        
         JE    DEM10                                                            
         CLC   CTLPHSN,0(R4)       MATCH PHASE NUMBER?                          
         JE    *+12                                                             
         LA    R3,CTLLNQ(R3)       NEXT IN TABLE                                
         J     DEM08                                                            
         MVC   FILTERS,1(R4)       COPY ACROSS FILTERS                          
         J     DEM12                                                            
*                                                                               
DEM10    DC    H'0',C'INVALID PHASE NUMBER'                                     
         EJECT                                                                  
***********************************************************************         
* RETURN A(TABLE) IN LIST                                             *         
***********************************************************************         
         SPACE 1                                                                
DEM12    CLI   0(R4),X'E4'         E4 IS SPECIAL - FILE MUST BE OPEN            
         JNE   DEM14                                                            
         GOTO1 VDMGR,DMCB,(0,=C'DTFAD'),=C'NTIFIL',0,0                          
         L     RE,DMCB+12                                                       
         USING DTFPHD,RE                                                        
         TM    DTFOPEN,X'20'                                                    
         JO    DEM14                                                            
         XC    ATABLE,ATABLE                                                    
         J     DEM24                                                            
         DROP  RE                                                               
*                                                                               
DEM14    TM    CTLFLAG,CTLCORE     PHASE CORE RESIDENT?                         
         JZ    DEM18               NO                                           
         L     RF,VCOM                                                          
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         JZ    *+6                                                              
         BASR  RE,RF               TURN OFF STORAGE PROTECTION                  
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,CTLCOMF                                                     
         A     RF,VCOM                                                          
         ICM   RF,15,0(RF)                                                      
         ST    RF,ATABLE                                                        
         TM    CTLFLAG,CTLHDR      16-BYTE DYNAMIC HEADER?                      
         BZ    *+12                NO                                           
         AHI   RF,16                                                            
         STCM  RF,15,ATABHDR       SET A(HEADER) IN USER LIST                   
*                                                                               
         L     RF,ATABLE                                                        
         CLI   REINIT,C'Y'         REINIT TABLE                                 
         JE    *+14                                                             
         CLC   0(8,RF),=XL8'00'    HAS RESOURCE BEEN BUILT?                     
         JNE   *+8                 GO BUILD RESOURCE - YOU ARE #1               
         BRAS  RE,INITTAB                                                       
*                                                                               
DEM16    L     RF,VCOM                                                          
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         JZ    *+6                                                              
         BASR  RE,RF               TURN ON STORAGE PROTECTION                   
*                                                                               
         L     RF,ATABHDR                                                       
         STCM  RF,15,0(R4)         SET A(HEADER) IN USER LIST                   
         LA    R4,4(R4)            BUMP TO NEXT LIST ENTRY                      
         J     DEM06                                                            
*                                                                               
DEM18    MVC   LOCKID,CTLID                                                     
         BRAS  RE,ELOCKSPC         ENQUIRE ON RESOURCE                          
         CLC   DSPLOCK,=XL4'00'                                                 
         JE    DEM20               RESOURCE IS NOT LOCKED                       
*                                                                               
         BRAS  RE,LOCKSPC      *** THIS WILL CAUSE WAIT UNTIL THE               
         BRAS  RE,ULOCKSPC     *** TABLE HAS BEEN BUILT BY ANOTHER APP          
*                                                                               
DEM20    DS    0H                                                               
         SAM31 ,                   SWITCH INTO XA                               
*                                                                               
         ICM   R5,15,DSPTFRST      PICK UP A(TABLE)                             
         LAM   AR5,AR5,ALET                                                     
         SAC   512                                                              
         CLI   REINIT,C'Y'         REINIT TABLE                                 
         BE    DEM22                                                            
         CLC   0(8,R5),=XL8'00'    HAS RESOURCE BEEN BUILT?                     
         JE    DEM22               GO BUILD RESOURCE - YOU ARE #1               
*                                                                               
         SAC   0                                                                
         LAM   AR5,AR5,=F'0'                                                    
         MVC   ATABLE,DSPTFRST     SAVE A(TABLE)                                
         L     R5,ATABLE                                                        
         AHI   R5,16                                                            
         ST    R5,ATABHDR                                                       
         SAM24 ,                   BACK TO 24-BIT                               
         B     DEM24                                                            
*                                                                               
DEM22    SAC   0                                                                
         LAM   AR5,AR5,=F'0'                                                    
*                                                                               
         L     RF,VCOM                                                          
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         JZ    *+6                                                              
         BASR  RE,RF               TURN OFF STORAGE PROTECTION                  
*                                                                               
         LA    RF,INITTAB          BUILD TABLE IN DATASPACE                     
         BASSM RE,RF                                                            
         SAM24 ,                   TURN OFF XA MODE                             
*                                                                               
         L     RF,VCOM                                                          
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         JZ    *+6                                                              
         BASR  RE,RF               TURN ON STORAGE PROTECTION                   
*                                                                               
         CLI   OKAY,C'Y'           TABLE BUILT SUCCESSFULLY?                    
         JE    DEM24               OK                                           
                                                                                
         LA    RF,DMCB             ERROR ON BUILD                               
         L     R1,CALLR1                                                        
         ST    RF,4(R1)            RETURN A(MY DMCB) IN P2                      
         MVI   0(R1),0                                                          
         J     XMOD                                                             
*                                                                               
DEM24    L     RF,ATABLE           SET A(HEADER) IN USER LIST                   
         TM    CTLFLAG,CTLHDR      16-BYTE DYNAMIC HEADER?                      
         BZ    *+8                 NO                                           
         L     RF,ATABHDR                                                       
*                                                                               
         SAM24 ,                   NO REASON TO BE IN XA ANY LONGER             
*                                                                               
         STCM  RF,15,0(R4)         SET A(HEADER) IN USER LIST                   
         LA    R4,4(R4)            BUMP TO NEXT LIST ENTRY                      
         J     DEM06                                                            
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
         ANSR                                                                   
         SPACE 2                                                                
XMOD     DS    0H                                                               
         REAR  ARS=OFF             MODULE EXIT POINT                            
*                                                                               
         SAM24 ,                   TURN OFF XA                                  
*                                                                               
         XMOD1 ,                   RETURN IN 24-BIT MODE WITH ARS OFF           
*                                                                               
         TITLE 'DEMADDR - INITIALISATION ROUTINE'                               
         EJECT                                                                  
***********************************************************************         
* INITALISATION ROUTINE - OBTAINS ALL ADDRESSES REQUIRED              *         
* NTRY: R1     = A(CALLING PARAMETER LIST)                            *         
* EXIT: CC EQ  = ALL OK                                               *         
*     : CC LO  = INITIALISATION ERROR                                 *         
***********************************************************************         
INIT     NTR1  ,                                                                
*                                                                               
         CLC   =C'$BLD',12(R1)     INTENTIONAL TABLE REBUILD?                   
         BNE   *+8                                                              
         MVI   DELIBFLG,C'Y'       YES                                          
*                                                                               
         ICM   RF,15,4(R1)         ACQUIRE SWITCH                               
         JZ    NO                                                               
*                                                                               
         ST    RF,VCOM                                                          
         MVC   VSWITCH,CSWITCH-COMFACSD(RF)                                     
         MVC   VDMGR,CDATAMGR-COMFACSD(RF)                                      
*                                                                               
         OC    VSWITCH,VSWITCH     WILL BE ZERO IF OFFLINE                      
         BNZ   INIT30                                                           
         MVC   AMASTC,CMASTC-COMFACSD(RF)                                       
         L     RF,AMASTC                                                        
         MVC   ASSB,MCSSB-MASTD(RF)                                             
         L     RF,ASSB             OBTAIN ALET FOR TABS DSPACE                  
         USING SSBD,RF                                                          
         MVC   ALET,SSBTBLET                                                    
         OC    ALET,ALET                                                        
         BNZ   INIT40                                                           
         MVC   LOCKID,=AL4(DTDDISP)                                             
         BRAS  RE,ELOCKSPC         ENQUIRE ON RESOURCE                          
         L     RF,ASSB             OBTAIN ALET FOR TABS DSPACE                  
         CLC   SSBTBLET,=XL4'00'                                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ALET,SSBTBLET                                                    
         B     INIT40                                                           
         DROP  RF                                                               
*                                                                               
INIT30   XC    DUB,DUB             SPECIAL CALL FOR A(SYSFACS)                  
         MVC   DUB(4),=XL4'FEFFFFFF'                                            
         GOTO1 VSWITCH,DUB                                                      
         MVC   VSYSFAC,DUB                                                      
         MVI   VSYSFAC,0           IN CASE WE ARE IN XA                         
*                                                                               
         L     RF,VSYSFAC                                                       
         MVC   ASSB,VSSB-SYSFACD(RF)                                            
*                                                                               
         L     RF,ASSB             OBTAIN ALET FOR TABS DSPACE                  
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
*                                                                               
INIT40   DS    0H                                                               
         L     RF,=V(BINSRCH)      RELOCATE BINSRCH                             
         A     RF,RELO                                                          
         ST    RF,VBINSRCH                                                      
         J     YES                                                              
*                                                                               
         TITLE 'DEMADDR - TABLE INITIALISATION ROUTINE'                         
         EJECT                                                                  
***********************************************************************         
* INITIALIZE A SPECIFIC TABLE                                         *         
* NTRY: R3     = A(CTLTAB)                                            *         
* EXIT: FIELD 'OKAY' = 'Y': TABLE BUILT OK                            *         
*                    = 'N': TABLE NOT BUILT                           *         
***********************************************************************         
         USING CTLTABD,R3                                                       
INITTAB  NTR1  ,                                                                
*                                                                               
         STAR  CLEAR=Y,ARS=OFF                                                  
         XC    TOTLEN,TOTLEN       CLEAR TOTAL TABLE LENGTH                     
         MVC   LOCKID,CTLID        SAVE TABLE IDENTIFIER                        
         ICM   RF,15,CTLADDR                                                    
         A     RF,RELO                                                          
         ST    RF,ARTN             SET A(INITIALIZATION ROUTINE)                
*                                                                               
         L     R5,ATABLE                                                        
         TM    CTLFLAG,CTLCORE     PHASE CORE RESIDENT?                         
         JO    ITAB02                                                           
*                                                                               
         MVC   WRNTBLID,CTLNAME    TABLE ID                                     
         BRAS  RE,SNDMAIL          SEND WARNING E-MAIL IF APPROPRIATE           
*                                                                               
         BRAS  RE,LOCKSPC          LOCK THE TABLE                               
*                                                                               
         SAM31 ,                   TABLES SHOULD BE XA COMPATIBLE               
*                                                                               
         ICM   R5,15,DSPTFRST      R5=TABLE ADDRESS                             
         STCM  R5,15,ATABLE                                                     
         LAM   AR5,AR5,ALET        GET TABS ALET                                
         SAC   512                 ACCESS REGISTERS ARE ON                      
*                                                                               
ITAB02   CLI   REINIT,C'Y'                                                      
         BNE   *+10                                                             
         XC    0(16,R5),0(R5)      CLEAR HEADER                                 
         CLC   0(8,R5),=XL8'00'    WAS TABLE BUILT BY ANOTHER?                  
         JNE   ITAB18              YES                                          
*                                                                               
         CLI   CTLPHSN,X'E2'       IS THIS A HARD-CODED TABLE?                  
         JNE   ITAB04                                                           
         CPYA  AR6,AR5                                                          
         LR    R6,R5                                                            
         LA    RE,E2TAB                                                         
         LHI   RF,E2TABL                                                        
         LR    R7,RF                                                            
         MVCL  R6,RE               MOVE TABLE INTO DSPACE                       
         JNO   *+6                 DESTRUCTIVE MOVE?                            
         DC    H'0'                YES!                                         
         LAM   AR6,AR6,=F'0'                                                    
         J     ITAB18                                                           
*                                                                               
ITAB04   XC    8(8,R5),8(R5)       LENGTH OF TABLE                              
         AHI   R5,16                                                            
         STCM  R5,15,ATABHDR       SAVE A(START OF TABLE)                       
*                                                                               
         SAM24 ,                   BACK TO 24-BIT                               
*                                                                               
         CLI   CTLKLET,C' '        READING CONTROL FILE?                        
         JH    ITAB06              YES                                          
         L     R5,ATABLE                                                        
         GOTO1 ARTN                CALL ROUTINE ONCE TO BUILD TABLE             
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         J     ITAB14              AND FINISH                                   
*                                                                               
ITAB06   LA    R6,IO               R6=A(IO AREA)                                
         ST    R6,AREC                                                          
         XC    KEY,KEY             GET FIRST RECORD                             
         MVC   KEY(1),CTLKLET                                                   
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,(R6)                    
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         SAC   0                   TURN OFF ACCESS REGISTERS AFTER SAFE         
         CLI   8(R1),0                                                          
         JNE   ITABL               READHI ERROR                                 
         CLC   CTLKLET,0(R6)                                                    
         JNE   ITABL               NO RECORDS OF THIS TYPE                      
*                                                                               
         GOTO1 ARTN,FIRST          FIRST TIME CALL TO ROUTINE                   
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         SAC   0                   TURN OFF ACCESS REGISTERS AFTER SAFE         
         J     ITAB10                                                           
*                                                                               
ITAB08   MVC   KEY,0(R6)           SAVE LAST KEY                                
         ST    R6,AREC                                                          
         GOTO1 VDMGR,DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,(R6)                    
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         SAC   0                   TURN OFF ACCESS REGISTERS AFTER SAFE         
         TM    8(R1),X'80'                                                      
         JO    ITAB12              EOF ON FILE                                  
         CLI   8(R1),0                                                          
         JNZ   ITABL               ERROR                                        
*                                                                               
         CLC   CTLKLET,0(R6)                                                    
         JNE   ITAB12              NO MORE RECORDS OF THIS TYPE                 
*                                                                               
ITAB10   GOTO1 ARTN,PROCESS        PROCESS CALL TO ROUTINE                      
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         SAC   0                                                                
         J     ITAB08                                                           
*                                                                               
ITAB12   GOTO1 ARTN,LAST           LAST TIME CALL TO ROUTINE                    
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         SAC   0                                                                
*&&DO                                                                           
*  CLEAR THE END OF EACH TABLE BUILD BY 16 BYTES                                
         GOTO1 CLRLAST                                                          
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         SAC   0                                                                
*&&                                                                             
                                                                                
ITAB14   L     R5,ATABLE                                                        
         TM    CTLFLAG,CTLCORE     PHASE CORE RESIDENT?                         
         JO    ITAB16                                                           
*                                                                               
         LAM   AR5,AR5,ALET                                                     
         SAC   512                                                              
*                                                                               
         SAM31 ,                   TURN XA ON                                   
*                                                                               
ITAB16   MVC   0(8,R5),CTLNAME     1ST 8 BYTES ARE NAME (SHOWS BUILT)           
         MVC   8(4,R5),TOTLEN      NEXT 4 BYTES ARE LENGTH                      
         SAC   0                                                                
         LAM   AR5,AR5,=F'0'                                                    
*                                                                               
ITAB18   DS    0H                                                               
         SAM24 ,                   TURN XA OFF                                  
*                                                                               
         REAR  ARS=OFF                                                          
         TM    CTLFLAG,CTLCORE     PHASE CORE RESIDENT?                         
         JO    ITABYES                                                          
*                                                                               
         BRAS  RE,ULOCKSPC         FREE LOCK ON TABLE                           
         BRAS  RE,XLOCKS                                                        
         SAC   0                                                                
         LAM   AR0,ARF,=16F'0'                                                  
         J     ITABYES                                                          
*                                                                               
ITABL    DS    0H                                                               
         SAM24 ,                   TURN XA OFF                                  
*                                                                               
         REAR  ARS=OFF             INITIALISATION ERROR                         
         TM    CTLFLAG,CTLCORE     PHASE CORE RESIDENT?                         
         JO    ITABYES                                                          
*                                                                               
         BRAS  RE,ULOCKSPC         FREE LOCK ON TABLE                           
         BRAS  RE,XLOCKS                                                        
         SAC   0                                                                
         LAM   AR0,ARF,=16F'0'                                                  
         MVI   OKAY,C'N'                                                        
         J     XIT                                                              
*                                                                               
ITABYES  MVI   OKAY,C'Y'                                                        
         J     XIT                                                              
         DROP  R3                                                               
*                                                                               
***********************************************************************         
* LIST OF VALID FILES & SUBFILES EQUATING TO INTERNAL FILE & SUB-FILE           
***********************************************************************         
         SPACE 1                                                                
E2TAB    DS    0CL6                                                             
         DC    C'TP ',C'T',C'TT'                                                
         DC    C'TP ',C'C',C'TC'                                                
         DC    C'TP ',C'E',C'TE'       CSI NETWORK                              
         DC    C'TP ',C'R',C'TR'                                                
         DC    C'TP ',C'P',C'TP'                                                
         DC    C'TP ',C'O',C'TO'                                                
         DC    C'TP ',C'W',C'TW'                                                
         DC    C'TP ',C'N',C'PN'   <-- SPOT NETWORK CALL                        
         DC    C'TP ',X'0',C'TT'                                                
         DC    C'NTI',C'T',C'PN'                                                
         DC    C'NTI',C'N',C'PN'                                                
         DC    C'NTI',C'C',C'CN'                                                
         DC    C'NTI',C'W',C'PW'                                                
         DC    C'NTI',X'0',C'PN'                                                
         DC    C'NAD',C'H',C'NH'                                                
         DC    C'NAD',C'T',C'NN'                                                
         DC    C'NAD',C'N',C'NN'                                                
         DC    C'NAD',X'0',C'NN'                                                
         DC    C'CAB',C'N',C'CN'   CABLE                                        
         DC    C'CAB',X'0',C'CN'                                                
         DC    C'RDP',C'C',C'RC'                                                
         DC    C'RDP',C'R',C'TR'                                                
         DC    C'RDP',X'0',C'TR'                                                
         DC    C'RTP',C'C',C'RS'                                                
         DC    C'RTP',C'R',C'TR'                                                
         DC    C'RTP',X'0',C'TR'                                                
         DC    C'PAV',C'T',C'PT'                                                
         DC    C'PAV',C'N',C'PN'                                                
         DC    C'PAV',X'0',C'PT'                                                
         DC    C'PAV',C'O',C'PO'       <-- NOV02/04 OVERNIGHT PAV               
         DC    C'MPA',C'T',C'TP'                                                
         DC    C'MPA',C'P',C'TP'                                                
         DC    C'MPA',X'0',C'TP'                                                
         DC    C'EIN',C'I',C'EI'                                                
         DC    C'EIN',X'0',C'EI'                                                
         DC    C'EVN',C'V',C'EV'                                                
         DC    C'EVN',X'0',C'EV'                                                
         DC    C'INV',C'R',C'TR'                                                
         DC    C'INV',C'T',C'PT'                                                
         DC    C'INV',C'U',C'IU'                                                
         DC    C'INV',X'0',C'PT'                                                
         DC    C'IUN',C'U',C'IU'                                                
         DC    C'IUN',X'0',C'IU'                                                
         DC    C'DPT',C'D',C'TD'                                                
         DC    C'DPT',X'0',C'TD'                                                
         DC    C'WTP',C'W',C'TW'   WEEKLY TIME PERIOD                           
         DC    C'WTP',X'0',C'TW'                                                
         DC    C'TVQ',X'0',C'TV'                                                
         DC    C'OPI',X'0',C'OP'                                                
         DC    C'CUN',C'U',C'CU'                                                
         DC    C'CUN',X'0',C'CU'                                                
         DC    C'RUA',C'U',C'RU'   ARBITRON RADIO COUNTY COVERAGE               
         DC    C'RUA',X'0',C'RU'                                                
         DC    X'FF'                                                            
E2TABL   EQU   *-E2TAB                                                          
*                                                                               
         TITLE 'DEMADDR - LOCKSPC ROUTINES FOR TABS DATASPACE'                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CLEAR OUT THE LAST ENTRY OF A TABLE JUST BUILT           *         
* REASON IS SOMETIMES REBUILDING OF TABLES RESULTS IN GARBAGE LEFT    *         
* BEHIND                                                              *         
***********************************************************************         
CLRLAST  NTR1                                                                   
*&&DO                                                                           
         OC    ALSTNTRY,ALSTNTRY                                                
         JZ    XIT                                                              
                                                                                
         LAM   AR7,AR7,ALET                                                     
         ICM   R7,15,ALSTNTRY                                                   
         SAC   512                                                              
                                                                                
         XC    0(16,R7),0(R7)                                                   
*&&                                                                             
         USING CTLTABD,R3                                                       
         TM    CTLFLAG,CTLHDR   ONLY FOR TABLES WITH 16 BYTE HEADERS            
         JZ    XIT              ELSE WE CANT GET THE LENGTH OF TABLE            
                                                                                
         LAM   AR7,AR7,ALET                                                     
         L     R7,ATABLE        GET ADDRESS OF BEGINNING OF TABLE               
         SAC   512                                                              
         CPYA  AR1,AR7                                                          
         ICM   R1,15,TOTLEN     GET TABLE LENGTH                                
         AR    R1,R7            A(BEGINNING TABLE)+L(TABLE)                     
         LA    R1,16(R1)        ADD 16 BYTES FOR TABLE HEADER                   
         XC    0(16,R1),0(R1)   CLEAR OUT END OF TABLE BY 16 BYTES              
                                                                                
         J     XIT                                                              
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ENQUIRE ON A DATASPACE ENTRY                             *         
* NTRY: LOCKID = TABLE IDENTIFIER                                     *         
* EXIT: DSPHD  = TABLE HEADER FROM DSPACE                             *         
***********************************************************************         
         SPACE 1                                                                
ELOCKSPC NTR1  ,                                                                
*                                                                               
         STAR  CLEAR=Y,ARS=OFF                                                  
         XC    DMCB,DMCB                                                        
         MVC   DMCB(4),LOCKID                                                   
         MVC   DMCB(2),=X'2000'                                                 
         GOTO1 VDMGR,P0,(0,=C'LOCKSP')  ENQUIRE ON CORRECT TABLE                
         ICM   RF,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DSPHD,0(RF)         SAVE DSPACE HEADER                           
         NC    DSPTFRST,=XL4'0FFFFFFF'  TURN OFF X'40' POST BIT                 
         REAR  ARS=OFF                                                          
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UNLOCK A DATASPACE ENTRY                                 *         
* NTRY: LOCKID = TABLE IDENTIFIER                                     *         
***********************************************************************         
         SPACE 1                                                                
ULOCKSPC NTR1  ,                                                                
*                                                                               
         STAR  CLEAR=Y,ARS=OFF                                                  
         XC    DMCB,DMCB                                                        
         MVC   DMCB(4),LOCKID                                                   
         MVC   DMCB(2),=X'1000'                                                 
         GOTO1 VDMGR,P0,(0,=C'LOCKSP')   UNLOCK CORRECT TABLE                   
         REAR  ARS=OFF                                                          
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UNLOCK ADDITIONAL RESOURCES IF ALLOCATED                            *         
***********************************************************************         
         SPACE 1                                                                
XLOCKS   NTR1  ,                                                                
*                                                                               
         STAR  CLEAR=Y,ARS=OFF                                                  
         MVC   FULL,LOCKID                                                      
         TM    DSPFLG,(DSPFLCK+DSPFLCK1)                                        
         JZ    XLCK04                                                           
         TM    DSPFLG,DSPFLCK                                                   
         JZ    XLCK02                                                           
         MVC   LOCKID,DSPXID                                                    
         BRAS  RE,ULOCKSPC         FREE TABLE LOCK                              
*                                                                               
XLCK02   TM    DSPFLG,DSPFLCK1                                                  
         JZ    XLCK04                                                           
         MVC   LOCKID,DSPXID1                                                   
         BRAS  RE,ULOCKSPC         FREE TABLE LOCK                              
*                                                                               
XLCK04   XC    DSPFLG,DSPFLG                                                    
         XC    DSPXID,DSPXID                                                    
         XC    DSPXID1,DSPXID1                                                  
         MVC   LOCKID,FULL                                                      
         REAR  ARS=OFF                                                          
         J     YES                                                              
         EJECT                                                                  
         TITLE 'DEMADDR - TABLE BUILD ROUTINES'                                 
***********************************************************************         
* FIX DATES IN MASTER DISPLACEMENT TABLE FOR NEW BOOKS                *         
* NTRY: R5     = A(CURRENT IN DATASPACE)                              *         
*       R6     = A(RECORD TO ADD TO TABLE)                            *         
*       AR MODE OFF/XA MODE OFF                                       *         
***********************************************************************         
         SPACE 1                                                                
DEMDISP  NTR1  ,                                                                
*                                                                               
         L     R5,ATABHDR                                                       
DDIS02   OC    0(2,R5),0(R5)       TEST E-O-T                                   
         BZ    DDIS04                                                           
         ICM   R1,7,7(R5)          BUMP TO NEXT ENTRY                           
         LA    R5,1(R1,R5)                                                      
         B     DDIS02                                                           
*                                                                               
DDIS04   LA    R5,2(R5)            CALCULATE TOTAL LENGTH                       
         S     R5,ATABLE                                                        
         ST    R5,TOTLEN           SET TOTAL TABLE LENGTH                       
         J     YES                                                              
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOCK A DATASPACE ENTRY                                   *         
* NTRY: LOCKID = TABLE IDENTIFIER                                     *         
* EXIT: DSPHD  = TABLE HEADER FROM DSPACE                             *         
*       YOU HAVE EXCLUSIVE CONTROL OF TABLE UNTIL RELEASED WITH UNLOCK*         
***********************************************************************         
         SPACE 1                                                                
LOCKSPC  NTR1  BASE=(*,LOCKSPCX),LABEL=*                                        
*                                                                               
         STAR  CLEAR=Y,ARS=OFF                                                  
         XC    DMCB,DMCB                                                        
         MVC   DMCB(4),LOCKID                                                   
         MVC   DMCB(2),=X'8000'    ASK FOR LONG ALLOCATE                        
         GOTO1 VDMGR,P0,(0,=C'LOCKSP')  LOCK CORRECT TABLE                      
         ICM   RF,15,DMCB+4                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DSPHD,0(RF)         SAVE DSPACE HEADER                           
         NC    DSPTFRST,=XL4'0FFFFFFF'  TURN OFF X'40' POST BIT                 
         REAR  ARS=OFF                                                          
         J     YES                                                              
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
LOCKSPCX EQU   *                                                                
         SPACE 2                                                                
         DROP  RB                                                               
         SPACE 1                                                                
***********************************************************************         
* BUILD BOOK TABLE                                                    *         
* NTRY: R5     = A(CURRENT IN DATASPACE)                              *         
*       R6     = A(RECORD TO ADD TO TABLE)                            *         
*       AR MODE OFF/XA MODE OFF                                       *         
***********************************************************************         
         SPACE 1                                                                
DEMBOOK  NTR1  BASE=(*,DEMBOOKX),LABEL=*                                        
*                                                                               
         B     *+0(R1)                                                          
         B     DBOKF               FIRST                                        
         B     DBOKP               PROCESS                                      
         B     DBOKL               LAST                                         
*                                                                               
DBOKF    J     YES             *** FIRST TIME                                   
*                                                                               
DBOKP    LAM   AR5,AR5,ALET    *** PROCESS RECORD                               
         SAC   512                                                              
*                                                                               
         SAM31 ,                   TURN XA ON                                   
*                                                                               
         LA    R6,CTBDATA-CTBREC(R6)                                            
         USING CTBKD,R6                                                         
DBOKF02  CLI   CTBKEL,CTBKELEQ     LATEST BOOK ELEMENT                          
         JE    DBOKF06                                                          
         CLI   CTBKEL,0                                                         
         JNE   DBOKF04                                                          
*                                                                               
         SAM24 ,                   TURN XA OFF                                  
         J     YES                                                              
*                                                                               
DBOKF04  ZIC   R1,CTBKLN                                                        
         AR    R6,R1                                                            
         J     DBOKF02                                                          
*                                                                               
DBOKF06  LA    RF,L'CTBKSRC+L'CTBKMED+L'CTBKYM-1                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),CTBKSRC     COPY IN ELEMENT DATA                         
         DROP  R6                                                               
*                                                                               
         L     R1,TOTLEN           BUMP TABLE LENGTH                            
         LA    R1,1(RF,R1)                                                      
         ST    R1,TOTLEN                                                        
         LA    R5,1(RF,R5)         NEXT IN TABLE                                
* CLEARING OUT 16 BYTES OF CURRENT END OF TABLE                                 
         XC    0(16,R5),0(R5)                                                   
         J     DBOKF04                                                          
*                                                                               
DBOKL    J     YES             *** LAST TIME                                    
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DEMBOOKX EQU   *                                                                
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD DEMO NAME TABLE                                               *         
* NTRY: R5     = A(CURRENT IN DATASPACE)                              *         
*       R6     = A(RECORD TO ADD TO TABLE)                            *         
*       AR MODE OFF/XA MODE OFF                                       *         
***********************************************************************         
         SPACE 1                                                                
DEMNAME  NTR1  BASE=(*,DEMNAMEX),LABEL=*                                        
*                                                                               
         B     *+0(R1)                                                          
         B     DNAMF               FIRST                                        
         B     DNAMP               PROCESS                                      
         B     DNAML               LAST                                         
*                                                                               
DNAMF    MVC   TABLEN,=H'37'   *** FIRST TIME                                   
         XC    ALSTNTRY,ALSTNTRY                                                
         J     YES                                                              
*                                                                               
         USING CTDREC,R6                                                        
DNAMP    CLI   CTDKTYP+1,0     *** PROCESS RECORD                               
         JNE   YES                 NOT A NAME RECORD                            
*                                                                               
         MVC   DUB(1),CTDKDEMO                                                  
         LAM   AR7,AR7,ALET                                                     
         CPYA  AR5,AR7                                                          
         SAC   512                                                              
         ICM   R7,15,ALSTNTRY                                                   
         JNZ   *+10                                                             
         LR    R7,R5                                                            
         J     DNAMP02                                                          
*                                                                               
         XR    R5,R5                                                            
         ICM   R5,7,7(R7)                                                       
         CLC   CTDKFILE(5),0(R7)                                                
         BE    DNAMP04                                                          
         MVI   0(R5),0                                                          
         LA    R7,1(R5)                                                         
*                                                                               
DNAMP02  MVC   0(5,R7),CTDKFILE    BUILD TABLE HEADER                           
         MVC   5(2,R7),TABLEN                                                   
         LA    R5,10(R7)                                                        
*                                                                               
* CLEARING CURRENT EOT BY 16 BYTES                                              
         XC    0(16,R5),0(R5)                                                   
*                                                                               
         STCM  R5,7,7(R7)                                                       
         ST    R7,ALSTNTRY                                                      
*                                                                               
DNAMP04  LA    R6,CTDDATA                                                       
         DROP  R6                                                               
         XR    R1,R1                                                            
*                                                                               
DNAMP06  CLI   0(R6),0                                                          
         JE    YES                                                              
         CLI   0(R6),X'02'         DEMO NAME ELEMENT                            
         BE    *+14                                                             
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DNAMP06                                                          
*                                                                               
         MVC   0(1,R5),DUB                                                      
         LH    R1,TABLEN                                                        
         SHI   R1,2                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R5),2(R6)                                                    
         AH    R5,TABLEN                                                        
         STCM  R5,7,7(R7)          SET A(NEXT ELEMENT)                          
*                                                                               
* CLEARING OUT 16 BYTES OF CURRENT END OF TABLE                                 
         XC    0(16,R5),0(R5)                                                   
         J     YES                                                              
*                                                                               
DNAML    L     R5,ALSTNTRY     *** LAST TIME                                    
         LAM   AR5,AR5,ALET                                                     
         SAC   512                                                              
         XR    RF,RF                                                            
         ICM   RF,7,7(R5)                                                       
         LA    RF,1(RF)                                                         
         S     RF,ATABLE                                                        
         ST    RF,TOTLEN           SET TOTAL LENGTH OF TABLE                    
         J     YES                                                              
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DEMNAMEX EQU   *                                                                
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD STATION RENAME TABLE                                          *         
* NTRY: R5     = A(CURRENT IN DATASPACE)                              *         
*       R6     = A(RECORD TO ADD TO TABLE)                            *         
*       AR MODE OFF/XA MODE OFF                                       *         
***********************************************************************         
         SPACE 1                                                                
DEMSTAT  NTR1  BASE=(*,DEMSTATX),LABEL=*                                        
*                                                                               
         B     *+0(R1)                                                          
         B     DSTNF               FIRST                                        
         B     DSTNP               PROCESS                                      
         B     DSTNL               LAST                                         
*                                                                               
DSTNF    MVC   TABLEN,=H'10'   *** FIRST TIME                                   
         XC    FLAG4,FLAG4                                                      
         XC    ALSTNTRY,ALSTNTRY                                                
         J     YES                                                              
*                                                                               
         USING CTSREC,R6                                                        
DSTNP    CLI   CTSKSRC,C'C'        COMSCORE?                                    
         JE    YES                 YES-SKIP IT                                  
         MVC   DUB(2),CTSKBOOK *** PROCESS RECORD                               
         XC    DUB(2),=X'FFFF'     CONVERT BOOK TO NORMAL                       
         CLC   CTSKSRC(2),=C'TR'   STATION EQUIV RECORDS FOR TRITON             
         JE    *+10                BUILT FROM DEMDIR                            
         CLC   CTSKSRC(2),=C'AR'   STATION EQUIV RECORDS FOR ARB RADIO          
         JNE   DSTNP02             BUILT FROM DEMDIR STARTING 1993              
*                                                                               
         TM    FLAG4,F4STARTN      DON'T RE-READ DEMDIRR                        
         JO    DSTNP02                                                          
         MVI   SOURCE,C'A'         ARBITRON                                     
         BRAS  RE,DEMRDIR          READ DEMDIRR TO BUILD TABLE                  
         OI    FLAG4,F4STARTN      AND CHANGE FLAG TO "YES"                     
*                                                                               
DSTNP02  CLI   DUB,YR_1994         BYPASS OLD BOOKS                             
         JL    YES                                                              
         CLC   CTSKSRC(2),=C'AR'   ARBITRON RADIO?                              
         JNE   *+14                NO                                           
         CLC   DUB(2),=X'5D00'     BYPASS ANYTHING STARTING 1993                
         JH    YES                                                              
                                                                                
         LAM   AR7,AR7,ALET                                                     
         CPYA  AR5,AR7                                                          
         SAC   512                                                              
*                                                                               
         ICM   R7,15,ALSTNTRY                                                   
         JNZ   *+10                                                             
         LR    R7,R5                                                            
         J     DSTNP04                                                          
*                                                                               
         ICM   R5,7,6(R7)                                                       
         MVI   0(R5),0                                                          
         LA    R7,1(R5)                                                         
*                                                                               
DSTNP04  MVC   0(4,R7),CTSKSRC     BUILD TABLE HEADER                           
         MVC   4(2,R7),TABLEN                                                   
         LA    R5,9(R7)                                                         
         STCM  R5,7,6(R7)                                                       
         MVI   0(R5),0             MARK END OF ENTRY                            
*                                                                               
         LA    R6,CTSDATA                                                       
         XR    R1,R1                                                            
*                                                                               
DSTNP06  CLI   0(R6),0                                                          
         JE    DSTNP12                                                          
         CLI   0(R6),X'02'         STATION RENAME ELEMENT                       
         JE    DSTNP10                                                          
*                                                                               
DSTNP08  IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         J     DSTNP06                                                          
*                                  ADD A TABLE ELEMENT                          
DSTNP10  LH    R1,TABLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),2(R6)                                                    
         LA    R5,1(R1,R5)                                                      
         J     DSTNP08                                                          
*                                                                               
DSTNP12  STCM  R5,7,6(R7)          SET A(END OF TABLE) IN HDR                   
         MVI   0(R5),0             MARK END OF ENTRY                            
* CLEARING OUT 16 BYTES OF CURRENT END OF TABLE                                 
         XC    0(16,R5),0(R5)                                                   
         ST    R7,ALSTNTRY                                                      
         J     YES                                                              
*                                                                               
DSTNL    DS    0H                                                               
         MVI   SOURCE,C'T'         BUILD TRITON LAST                            
         BRAS  RE,DEMRDIR          READ DEMDIRR TO BUILD TABLE                  
*                                                                               
         L     R7,ALSTNTRY                                                      
         LAM   AR7,AR7,ALET                                                     
         CPYA  AR6,AR7                                                          
         SAC   512                                                              
         ICM   R6,7,6(R7)                                                       
         XC    0(4,R6),0(R6)        CLEAR AWAY ANY IMMEDIATE JUNK               
         LAM   AR6,AR6,=F'0'                                                    
         LA    R6,1(R6)                                                         
         L     R5,ATABLE                                                        
         AHI   R5,16                                                            
         SR    R6,R5               SET TOTAL LENGTH OF TABLE                    
         ST    R6,TOTLEN                                                        
         XC    FLAG4,FLAG4         RESET FLAG                                   
         J     YES                                                              
         DROP  R6                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DEMSTATX EQU   *                                                                
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD STATION RENAME TABLE FROM DEMDIRR                             *         
* NTRY: AR MODE OFF/XA MODE OFF                                       *         
*                                                                     *         
* BUILD STATION RENAME TABLE FROM STATION EQUIVALENCE RECS IN DEMDIRR *         
* ONLY CALLED FROM WITHIN DEMSTAT                                     *         
***********************************************************************         
         SPACE 1                                                                
DEMRDIR  NTR1  BASE=(*,DEMRDIRXX),LABEL=*                                       
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=C'DMREAD'),=C'SYSFLES',0                          
         ICM   R2,15,12(R1)         GET A(LIST OF FILES)                        
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         XR    R3,R3                                                            
         ICM   R3,3,2(R2)          R2=NUMBER OF FILES                           
         LA    R2,4(R2)            R2=A(1ST FILE ENTRY)                         
*                                                                               
DEMDR02  CLI   3(R2),DEMDQ         DEMDIRR IS X'30'                             
         JE    DEMDR04                                                          
         LA    R2,8(R2)                                                         
         BRCT  R3,DEMDR02                                                       
         DC    H'0'                                                             
         J     DEMRDIRX            IF NOT FOUND, THEN EXIT                      
*                                                                               
DEMDR04  TM    0(R2),X'80'         NO-OP?                                       
         JNO   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R8,IO2                                                           
         USING DSEKEY,R8                                                        
         XC    KEY2,KEY2                                                        
         MVC   KEY2(3),=C'ERA'                                                  
         CLI   SOURCE,C'T'       TRITON?                                        
         BNE   *+8                                                              
         MVI   KEY2+2,C'T'                                                      
         MVI   KEY2+3,DSEIBKHI                                                  
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),=C'DEMDIR',KEY2,IO2                    
         J     DEMDR08                                                          
*                                                                               
DEMDR06  GOTO1 VDMGR,DMCB,(0,=C'DMRSEQ'),=C'DEMDIR',KEY2,IO2                    
*                                                                               
DEMDR08  CLI   8(R1),0                                                          
         JNE   DEMRDIRX                                                         
         CLI   DSECODE,DSECDEQU    STATION EQUIV RECORD?                        
         JNE   DEMRDIRX            NO, EXIT                                     
         CLI   DSEMEDIA,C'R'       RADIO?                                       
         JNE   DEMRDIRX            NO, EXIT                                     
         CLI   DSESRC,C'T'         TRITON SHOULD ALSO CREATE                    
         JNE   NOTRITON                                                         
         JE    *+4                                                              
***NOTRITON CLI   DSESRC,C'A'         ARBITRON                                  
NOTRITON CLC   SOURCE,DSESRC                                                    
         JNE   DEMRDIRX            NO, EXIT                                     
         CLI   DSEIND,DSEIBKHI                                                  
         BNE   DEMRDIRX                                                         
         OC    DSECLOLD,DSECLOLD   OLD CALL LETTERS NOT PROVIDED                
         BZ    DEMDR06             DON'T ADD TO TABLE                           
         OC    DSECLNEW,DSECLNEW   NEW CALL LETTERS NOT PROVIDED                
         BZ    DEMDR06             DON'T ADD TO TABLE                           
*                                                                               
         LAM   AR7,AR7,ALET                                                     
         CPYA  AR5,AR7                                                          
         SAC   512                                                              
*                                                                               
         CLC   DSEBKEFF,CURBOOK    ARE WE STILL ON THE SAME BOOK?               
         JE    DEMDR12             YES                                          
*                                                                               
         MVC   CURBOOK,DSEBKEFF    NO, DO SOME ADMINISTRATIVE STUFF             
         USING STAHDRD,R7                                                       
*                                                                               
         ICM   R7,15,ALSTNTRY      GET A(CURRENT TABLE HDR)                     
         JNZ   *+10                                                             
         LR    R7,R5               R7=A(1ST TABLE HEADER)                       
         J     DEMDR10                                                          
*                                                                               
         XR    R5,R5                                                            
         ICM   R5,7,STAAET         GET A(EOTABLE) INTO R5                       
         MVI   0(R5),0                                                          
         LA    R7,1(R5)            R7=A(NEXT TABLE HEADER)                      
*                                                                               
DEMDR10  STCM  R7,15,ALSTNTRY      SAVE A(NEXT TABLE HDR)                       
         MVC   STASRC,DSESRC       SOURCE INTO TABLE HEADER                     
         MVC   STAMED,DSEMEDIA     MEDIA INTO TABLE HEADER                      
         MVC   STASBOOK,DSEBKEFF   BOOK INTO TABLE HEADER                       
         MVC   STALDE,TABLEN                                                    
         LA    R5,STAHDRLN(R7)                                                  
         STCM  R5,7,STAAET                                                      
*                                                                               
DEMDR12  LH    R1,TABLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,DEMDRMVE                                                      
         LA    R5,1(R1,R5)                                                      
         STCM  R5,7,STAAET         STORE A(EOTABLE) IN HEADER                   
*                                                                               
         SAC   0                   GET OUT OF TROUBLE...                        
         LAM   AR5,AR7,=3F'0'      CLEAR ACCESS REGISTERS                       
         J     DEMDR06                                                          
*                                                                               
DEMDRMVE MVC   0(0,R5),DSECLOLD    MOVE CALL LETTERS INTO TABLE                 
         DROP  R7,R8                                                            
*                                                                               
DEMRDIRX MVC   KEY,IO                                                           
         GOTO1 VDMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,IO                      
         CLI   8(R1),0             RESTORE READ OF CTFILE                       
         JE    YES                                                              
         DC    H'0'                                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DEMRDIRXX EQU   *                                                               
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD MASTER DEMO & FORMULA TABLES                                  *         
* NTRY: R6     = A(RECORD TO ADD TO TABLE)                            *         
*       R5     = A(FIRST TABLE ENTRY)                                 *         
*       AR MODE OFF/XA MODE OFF                                       *         
* EXIT:        =                                                      *         
***********************************************************************         
         SPACE 1                                                                
DEMFORM  NTR1  BASE=(*,DEMFORMX),LABEL=*                                        
*                                                                               
         B     *+0(R1)                                                          
         B     DFRMF               FIRST                                        
         B     DFRMP               PROCESS                                      
         B     DFRML               LAST                                         
*                                                                               
DFRMF    LA    R0,MSTDTALN     *** FIRST TIME                                   
         STH   R0,TABLEN                                                        
         XC    ALSTNTRY,ALSTNTRY                                                
         MVC   FULL,LOCKID         SAVE CURRENT LOCK ID                         
         MVC   WORK,DSPHD          SAVE CURRENT DSPACE HEADER                   
*                                                                               
         MVI   DSPFLG,DSPFLCK      FLAG ADDITIONAL DSPACE LOCKED                
         MVC   DSPXID,=AL4(DTDFRMLA)                                            
         MVC   LOCKID,=AL4(DTDFRMLA)                                            
         BRAS  RE,LOCKSPC          LOCK DFORMULA TABLE ALSO                     
*                                                                               
         MVC   ALSTFRM,DSPTFRST    SAVE FIRST AND LAST ADDRESSES                
         MVC   ALSTFRMX,DSPTEND                                                 
         L     RF,ALSTFRM                                                       
         LA    RF,16(RF)                                                        
         ST    RF,ALSTFRM1         SAVE A(FIRST TABLE ENTRY)                    
*                                                                               
         ICM   R5,15,ALSTFRM                                                    
         LAM   AR5,AR5,ALET                                                     
         SAC   512                                                              
         XC    0(16,R5),0(R5)      RESET DFORMULA INITIALISATION FLAGS          
         SAC   0                                                                
*                                                                               
         MVC   LOCKID,FULL         RESTORE CURRENT LOCK ID                      
         MVC   DSPHD,WORK          RESTORE CURRENT DSPACE HEADER                
         XC    FLAG1,FLAG1         RESET BUILD FLAGS                            
         J     YES                                                              
*                                                                               
         USING CTGREC,R6                                                        
DFRMP    ST    R6,AREC                                                          
         OC    CTGKEY+1(14),CTGKEY+1                                            
         JNZ   YES                 WHAT IS THIS RECORD ?!?                      
         CLI   FILTERS,0           APPLY FILE/MEDIA/SOURCE FILTERS              
         JE    *+14                                                             
         CLC   CTGKFILE,FILTERS                                                 
         JNE   YES                                                              
         CLI   FILTERS+1,0                                                      
         JE    *+14                                                             
         CLC   CTGKMED,FILTERS+1                                                
         JNE   YES                                                              
         CLI   FILTERS+2,0                                                      
         JE    *+14                                                             
         CLC   CTGKSRC,FILTERS+2                                                
         JNE   YES                                                              
*                                                                               
         USING MSTHDRD,R7                                                       
         LAM   AR7,AR7,ALET                                                     
         SAC   512                                                              
         ICM   R7,15,ALSTNTRY      FIRST TIME IN?                               
         JNZ   *+14                NO                                           
         MVC   ALSTNTRY,ATABHDR    A(FIRST ENTRY)                               
         J     DFRMP58                                                          
*                                                                               
         CLC   MSTFMS,CTGKFMS      MATCH FILE/MEDIA/SOURCE                      
         JE    DFRMP62             SAME                                         
         DROP  R6                                                               
*                                                                               
DFRMP02  L     RF,VCOM                                                          
         ICM   R5,15,CT00AD0-COMFACSD(RF)                                       
                                                                                
         AHI   R5,16                                                            
         USING DSPHDRD,R5                                                       
*                                                                               
         LAM   AR0,ARF,=16F'0'                                                  
         LAM   AR7,AR7,ALET        R7=A(CURRENT ENTRY)                          
         SAC   512                                                              
*                                                                               
DFRMP04  OC    DSPFILE(2),DSPFILE  EOT?                                         
         JZ    DFRMP12             YES                                          
         CLC   DSPFMS,MSTFMS       MATCH ON FILE/MEDIA/SOURCE                   
         JNE   DFRMP10                                                          
*                                                                               
         LA    R3,DSPHDRD+DSPHDRLN                                              
         USING DSPDTAD,R3          R3=A(DISPLACEMENT TABLE ENTRY)               
*                                                                               
DFRMP06  CLI   DSPMOD,X'FF'        TEST E-O-T                                   
         JE    DFRMP10                                                          
         CLI   DSPDEMO,0           TEST DEMO NUMBER ZERO                        
         JE    DFRMP08                                                          
*                                                                               
         LA    R8,WORK             BUILD MS1DTA ENTRY IN WORK                   
         USING MS1DTAD,R8                                                       
         XC    MS1DTAD(MS1DTALN),MS1DTAD                                        
         MVC   MS1MODC,DSPMOD                                                   
         MVI   MS1DEMO,0                                                        
         MVC   MS1DEMO+1(1),DSPDEMO                                             
         MVC   MS1BOOK,DSPSBOOK                                                 
         MVI   MS1INDS,MSTTABEQ                                                 
         GOTO1 SETBOOK,1                                                        
         MVC   MS1ELCD,DSPELCD                                                  
         MVC   MS1FLDN,DSPFLDN                                                  
         MVC   MS1IPRC,DSPPREC                                                  
*                                                                               
         LA    R1,BPAR                                                          
         USING BSPARA,R1                                                        
         MVI   BSPLENR,1           SET INSERT IF NOT FOUND                      
         MVI   BSPLENR+1,X'80'     SET USING ACCESS REGISTER                    
         ST    R8,BSPAREC                                                       
         GOTO1 VBINSRCH,(R1)                                                    
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         TM    0(R1),X'80'         TEST RECORD ADDED                            
         JO    DFRMP08             YES                                          
         DROP  R1                                                               
*                                                                               
         XR    R8,R8                                                            
         ICM   R8,7,1(R1)          R8=A(EXISTING ENTRY)                         
         JNZ   *+6                                                              
         DC    H'0'                DIE IF TABLE FULL                            
*                                                                               
         CPYA  AR8,AR7                                                          
         MVC   MS1ELCD,DSPELCD     COPY IN DATA TO EXISTING ENTRY               
         MVC   MS1FLDN,DSPFLDN                                                  
         MVC   MS1IPRC,DSPPREC                                                  
         OI    MS1INDS,MSTTABEQ                                                 
         DROP  R8                                                               
         LAM   AR8,AR8,=F'0'                                                    
*                                                                               
DFRMP08  LA    R3,DSPDTALN(,R3)                                                 
         J     DFRMP06                                                          
*                                                                               
DFRMP10  XR    RE,RE                                                            
         ICM   RE,7,DSPAET                                                      
         LA    R5,1(RE,R5)                                                      
         J     DFRMP04                                                          
         DROP  R3                                                               
         DROP  R5                                                               
*                                                                               
DFRMP12  ICM   RF,15,BPAR+8        TEST ANYTHING IN TABLE                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,MSTBKNDX         SORT BOOK INDEX INTO ASCENDING SEQ           
         CPYA  AR2,AR7                                                          
         CPYA  AR3,AR7                                                          
         XR    R0,R0                                                            
         ICM   R0,1,MSTBKNUM                                                    
         SHI   R0,1                                                             
         BZ    DFRMP20             EXIT IF ONE BOOK ONLY                        
*                                                                               
DFRMP14  LA    R3,3(,R2)                                                        
         LR    RE,R0                                                            
*                                                                               
DFRMP16  CLC   0(2,R2),0(R3)                                                    
         JNH   DFRMP18                                                          
         XC    0(3,R2),0(R3)                                                    
         XC    0(3,R3),0(R2)                                                    
         XC    0(3,R2),0(R3)                                                    
*                                                                               
DFRMP18  LA    R3,3(,R3)                                                        
         BCT   RE,DFRMP16                                                       
         LA    R2,3(,R2)                                                        
         BCT   R0,DFRMP14                                                       
*                                                                               
DFRMP20  DS    0H                                                               
         LAM   AR2,AR3,=2F'0'      CLEAR AR'S 2 & 3                             
*                                                                               
*                                  SET MASTER DISP BOOK NUMBER                  
         ZIC   R0,MSTBKNUM         ASSIGN MASTER DISPLACEMENT BOOKS             
*                                                                               
         LR    R2,R0                                                            
         BCTR  R2,0                                                             
         MHI   R2,3                                                             
         LA    R2,MSTBKNDX(R2)     POINT TO LAST ENTRY                          
         CPYA  AR2,AR7                                                          
*                                                                               
DFRMP22  TM    2(R2),1             TEST THIS IS A MDT BOOK                      
         JZ    *+6                                                              
         LR    R1,R0                                                            
         STCM  R1,1,2(R2)                                                       
         AHI   R2,-3                                                            
         BRCT  R0,DFRMP22                                                       
         LAM   AR2,AR2,=F'0'                                                    
*                                                                               
DFRMP24  LA    R8,MSTHDRD+MSTHDRLN CONVERT TABLE TO COMPRESSED FORMAT           
         CPYA  AR8,AR7                                                          
         USING MSTDTAD,R8                                                       
         LR    R5,R8                                                            
         CPYA  AR5,AR8                                                          
         LA    R6,WORK                                                          
         USING MS1DTAD,R6                                                       
         L     R4,BPAR+8           R4=N'TABLE ENTRIES                           
*                                                                               
DFRMP26  MVC   MS1DTAD(MS1DTALN),0(R5)                                          
         XC    MSTDTAD(MSTDTALN),MSTDTAD                                        
         MVC   MSTMODC,MS1MODC                                                  
         MVC   MSTDEMO,MS1DEMO                                                  
*                                                                               
         BRAS  RE,GETBOOK                                                       
*                                                                               
         STCM  R1,1,MSTBKNO                                                     
         XC    BYTE,BYTE                                                        
         CLI   MS1IPRC,0                                                        
         BE    DFRMP28                                                          
         MVC   BYTE,MS1IPRC                                                     
         TM    BYTE,X'0F'          TEST DECIMAL PRECISION                       
         BZ    *+12                                                             
         TM    BYTE,X'80'                                                       
         BZ    *+8                                                              
         OI    BYTE,X'08'                                                       
         NI    BYTE,X'0F'                                                       
         PACK  MSTPREC,BYTE                                                     
*                                                                               
DFRMP28  CLI   MS1OPRC,0           TEST OUTPUT PRECISION SET                    
         BE    DFRMP30             NO - USE INPUT PRECISION                     
         MVC   BYTE,MS1OPRC                                                     
         TM    BYTE,X'0F'          TEST DECIMAL PRECISION                       
         BZ    *+12                                                             
         TM    BYTE,X'80'                                                       
         BZ    *+8                                                              
         OI    BYTE,X'08'                                                       
         NI    BYTE,X'0F'                                                       
         OC    MSTPREC,BYTE                                                     
*                                                                               
DFRMP30  MVC   MSTINDS,MS1INDS                                                  
         MVC   MSTDISP,MS1DISP                                                  
         MVC   MSTELCD,MS1ELCD                                                  
         MVC   MSTFLDN,MS1FLDN                                                  
         AHI   R5,MS1DTALN                                                      
         AHI   R8,MSTDTALN                                                      
         BRCT  R4,DFRMP26                                                       
         MVI   0(R8),0             SET NEW END OF TABLE                         
* CLEARING CURRENT EOT BY 16 BYTES                                              
         XC    1(16,R8),1(R8)                                                   
*                                                                               
         LAM   AR8,AR8,=F'0'                                                    
         LAM   AR5,AR5,=F'0'                                                    
         DROP  R6                                                               
*                                                                               
         L     R6,AREC                                                          
*                                                                               
         LHI   R0,256              ALL POSSIBLE ENCODED MODIFIERS               
         CPYA  AR1,AR7                                                          
         LA    R1,MSTMODNX         CLEAR ENTIRE ARRAY                           
         XC    0(MSTMODNX_LEN,R1),0(R1)                                         
         LA    R1,MSTMODNX_LEN(,R1)                                             
         BRCT  R0,*-10                                                          
         LAM   AR1,AR1,=F'0'                                                    
*                                  R8 = A(MODIFIER TABLE)                       
         LA    R8,MSTHDRD+MSTHDRLN BUILD MODIFIER INDEX TABLE                   
         CPYA  AR8,AR7             AR7 = A(MSTDRD)                              
         XC    FLAG2,FLAG2         MODIFIER CODE                                
*                                                                               
DFRMP32  CLI   MSTMODC,0           TEST E-O-T                                   
         JE    DFRMP36                                                          
         CLC   MSTMODC,FLAG2       TEST MODIFIER C/B                            
         JE    DFRMP34             NO                                           
*                                                                               
         CPYA  AR2,AR7                                                          
         LLC   R2,MSTMODC          ENCODED MODIFIER                             
         MHI   R2,MSTMODNX_LEN     L'EACH TABLE ENTRY                           
         LA    R2,MSTMODNX(R2)     R2 = A(ENTRY) FOR THIS MODIFIER              
*                                  SET A(FIRST DEMO FOR MODIFIER)               
         STCM  R8,7,(MSTMODNX_ASTART-MSTMODNX_ENTRY)(R2)                        
         LAM   AR2,AR2,=F'0'                                                    
*                                                                               
         MVC   FLAG2,MSTMODC                                                    
*                                                                               
DFRMP34  AHI   R8,MSTDTALN                                                      
         LR    R0,R8               A(NEXT MODIFIER TABLE)                       
         BCTR  R0,0                A(LAST BYTE OF PREVIOUS TABLE)               
         CPYA  AR2,AR7                                                          
         LLC   R2,FLAG2            ENCODED MODIFIER                             
         MHI   R2,MSTMODNX_LEN     L'EACH TABLE ENTRY                           
         LA    R2,MSTMODNX(R2)     R2 = A(ENTRY) FOR THIS MODIFIER              
*                                  SAVE A(END) IN MODIFIER INDEX TABLE          
         STCM  R0,7,(MSTMODNX_AEND-MSTMODNX_ENTRY)(R2)                          
         LAM   AR2,AR2,=F'0'                                                    
*                                                                               
         J     DFRMP32                                                          
*                                                                               
DFRMP36  LAM   AR8,AR8,=F'0'                                                    
         LAM   AR2,AR2,=F'0'                                                    
         STCM  R8,7,MSTAET                                                      
         CPYA  AR8,AR7                                                          
         MVI   0(R8),0                                                          
* CLEARING CURRENT EOT BY 16 BYTES                                              
         XC    1(16,R8),1(R8)                                                   
*                                                                               
         LAM   AR8,AR8,=F'0'                                                    
         LA    R8,1(,R8)                                                        
         STCM  R8,15,ALSTNTRY                                                   
*                                                                               
         TM    FLAG1,F1EQU         TEST ANY EQUATED DEMOS FOUND                 
         JZ    DFRMP50                                                          
         LA    R8,MSTHDRD+MSTHDRLN                                              
         CPYA  AR8,AR7                                                          
*                                                                               
DFRMP38  CLI   0(R8),0             TEST E-O-T                                   
         JE    DFRMP50                                                          
         TM    MSTINDS,MSTDEMEQ    TEST THIS DEMO EQUATED                       
         JZ    DFRMP48                                                          
*                                                                               
         LLC   R1,MSTDISP          ENCODED MODIFIER                             
         MVI   MSTDISP,0           RESET MODIFIER                               
         MHI   R1,MSTMODNX_LEN     L'EACH TABLE ENTRY                           
         LA    R2,MSTMODNX(R1)     R2 = A(ENTRY) FOR THIS MODIFIER              
         CPYA  AR2,AR8                                                          
         OC    0(MSTMODNX_LEN,R2),0(R2)                                         
         LAM   AR2,AR2,=F'0'                                                    
         JZ    DFRMP46             LAM DOES NOT AFFECT THE CC                   
*                                                                               
         CPYA  AR2,AR7                                                          
         XR    RF,RF                                                            
*                                  SET RF TO A(LAST ENTRY FOR MODIFIER)         
         ICM   RF,7,(MSTMODNX_AEND-MSTMODNX_ENTRY)(R2)                          
*                                  SET R2 TO A(1ST ENTRY FOR MODIFIER)          
         ICM   R2,7,(MSTMODNX_ASTART-MSTMODNX_ENTRY)(R2)                        
         LA    R2,0(R2)            NOW IT'S OKAY TO CLEAR HOB OF R2             
*                                                                               
         LA    RE,MSTDTALN                                                      
M        USING MSTDTAD,R2                                                       
*                                                                               
DFRMP40  CLC   MSTDEMO,M.MSTDEMO                                                
         JH    DFRMP44                                                          
         JL    DFRMP46                                                          
         OC    MSTDEMO,MSTDEMO     TEST MACRO ENTRY                             
         JZ    DFRMP42                                                          
         TM    M.MSTINDS,MSTFRMEQ                                               
         JZ    DFRMP44                                                          
         TM    M.MSTINDS,MSTFPREQ                                               
         JNZ   DFRMP44                                                          
*                                                                               
DFRMP42  MVC   FLAG2,M.MSTPREC                                                  
         NI    FLAG2,MSTOPRC                                                    
         NI    MSTPREC,MSTIPRC                                                  
         OC    MSTPREC,FLAG2       SET OUTPUT PRECISION                         
         SR    R2,R7               SET A(EQUATED DEMO ENTRY)                    
         STCM  R2,7,MSTDISP                                                     
         J     DFRMP48                                                          
*                                                                               
DFRMP44  BRXLE R2,RE,DFRMP40       USES R2,RE,RF                                
*                                                                               
DFRMP46  NI    MSTINDS,255-MSTDEMEQ                                             
         OI    MSTINDS,MSTDIREQ                                                 
*                                                                               
DFRMP48  AHI   R8,MSTDTALN          BUMP TO NEXT TABLE ENTRY                    
         LAM   AR2,AR2,=F'0'                                                    
         J     DFRMP38                                                          
         DROP  M                                                                
*                                                                               
DFRMP50  LA    R8,MSTHDRD+MSTHDRLN RESOLVE ELEMENT CODE/FIELD NUMBERS           
         CPYA  AR8,AR7                                                          
*                                                                               
DFRMP52  CLI   0(R8),0             TEST E-O-T                                   
         JE    DFRMP58                                                          
         OC    MSTDEMO,MSTDEMO     TEST MACRO ENTRY                             
         JZ    DFRMP56                                                          
         TM    MSTINDS,MSTTABEQ+MSTDIREQ                                        
         JNZ   DFRMP56                                                          
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,1,MSTBKNO        TRANSLATE BOOK TO EFFECTIVE BOOK             
         BCTR  R1,0                                                             
         MHI   R1,3                                                             
         LA    R2,MSTBKNDX(R1)                                                  
         CPYA  AR2,AR7                                                          
         XR    R0,R0                                                            
         ICM   R0,1,2(R2)          R0=EFFECTIVE BOOK                            
         JZ    DFRMP56                                                          
*                                                                               
         LA    R2,MSTDTAD+MSTDTALN R2=A(NEXT ENTRY)                             
M        USING MSTDTAD,R2                                                       
*                                                                               
DFRMP54  CLC   MSTMODC(3),M.MSTMODC                                             
         JNE   DFRMP56                                                          
         CLM   R0,1,M.MSTBKNO                                                   
         JE    *+12                                                             
         AHI   R2,MSTDTALN                                                      
         J     DFRMP54                                                          
*                                                                               
         TM    M.MSTINDS,MSTTABEQ                                               
         JZ    DFRMP56                                                          
         MVC   MSTELCD,M.MSTELCD                                                
         MVC   MSTFLDN,M.MSTFLDN                                                
         OI    MSTINDS,MSTTABEQ                                                 
         MVC   FLAG5,M.MSTPREC                                                  
         NI    FLAG5,MSTIPRC                                                    
         OC    MSTPREC,FLAG5                                                    
         DROP  M                                                                
*                                                                               
DFRMP56  AHI   R8,MSTDTALN          BUMP TO NEXT TABLE ENTRY                    
         LAM   AR2,AR2,=F'0'                                                    
         J     DFRMP52                                                          
*                                                                               
DFRMP58  LAM   AR2,AR2,=F'0'                                                    
         LAM   AR8,AR8,=F'0'                                                    
         DROP  R8                                                               
         ICM   R7,15,ALSTNTRY      CURRENT TABLE ENTRY                          
         LAM   AR7,AR7,ALET                                                     
         SAC   512                                                              
*                                                                               
         TM    FLAG1,F1LAST        EOF?                                         
         JZ    DFRMP60             NO                                           
*                                                                               
         L     RF,ALSTNTRY         SET TOTAL TABLE LENGTHS                      
         LA    RF,1(RF)                                                         
         S     RF,ATABHDR                                                       
         ST    RF,TOTLEN           TOTAL L'MASTER DEMO TABLE                    
*                                                                               
         L     R7,ALSTFRM1         SET DEMO FORMULA DETAILS                     
         XC    0(2,R7),0(R7)       SET END MARKER                               
         LA    RF,2(R7)                                                         
         L     R7,ALSTFRM                                                       
         LA    R1,16(R7)                                                        
         SR    RF,R1                                                            
         STCM  RF,15,8(R7)         TOTAL L'FORMULA TABLE                        
         LARL  RF,CDFORM                                                        
         MVC   0(8,R7),0(RF)       SET FORMULA TABLE INITIALIZED                
         J     YES                                                              
*                                                                               
         USING CTGREC,R6                                                        
DFRMP60  L     R6,AREC                                                          
         STCM  R7,15,ALSTNTRY      BUILD MASTER DEMO TABLE HEADER               
*                                                                               
         CPYA  ARE,AR7                                                          
         LA    RE,MSTHDRD                                                       
         LHI   RF,MSTHDRLN                                                      
         XCEF                                                                   
         LAM   ARE,ARE,=F'0'                                                    
*                                                                               
         MVC   MSTFMS,CTGKFMS                                                   
         MVC   MSTAFM,ALSTFRM1+1                                                
*                                                                               
         XC    BPAR,BPAR                                                        
         LA    R1,BPAR             BUILD BINSRCH PARAMETER LIST                 
         USING BSPARA,R1                                                        
         LA    RE,MSTHDRD+MSTHDRLN                                              
         ST    RE,BSPSTRT          SET A(TABLE)                                 
         LA    RE,MS1DTALN                                                      
         ST    RE,BSPLENR          SET L'RECORD                                 
         LA    RE,MS1BOOK+2-MS1MODC                                             
         ST    RE,BSPLENK          SET L'KEY                                    
         XR    RE,RE                                                            
         STC   RE,BSPKEYD          SET DISPLACEMENT TO KEY                      
         MVC   BSPEND,=AL4(5000)   SET MAX RECORDS                              
         MVC   BSPARS,ALET         SET ALET                                     
         DROP  R1                                                               
*                                                                               
DFRMP62  LA    R8,WORK             BUILD TABLE ENTRY IN LOCAL WORK              
         USING MS1DTAD,R8                                                       
         XC    MS1DTAD(MS1DTALN),MS1DTAD                                        
         MVC   MS1MODC,CTGKDEMO                                                 
         MVI   MS1DEMO,0                                                        
         CLI   CTGKCODE,X'FF'                                                   
         JE    *+10                                                             
         MVC   MS1DEMO(1),CTGKCODE                                              
         MVC   MS1DEMO+1(1),CTGKDEMO+1                                          
         MVC   MS1BOOK,CTGKSTRT                                                 
         MVI   MS1INDS,MSTRECEQ                                                 
         GOTO1 SETBOOK,0                                                        
*                                                                               
         LA    R4,CTGDATA          PROCESS RECORD                               
         DROP  R6                                                               
DFRMP64  CLI   0(R4),0             TEST E-O-R                                   
         JE    DFRMP84                                                          
         CLI   0(R4),CTPRECDQ      PRECISION ELEMENT                            
         JE    DFRMP68                                                          
         CLI   0(R4),CTDPFCDQ      DEMO FORMULA ELEMENT                         
         JE    DFRMP72                                                          
*                                                                               
DFRMP66  XR    R1,R1                                                            
         ICM   R1,1,1(R4)                                                       
         AR    R4,R1                                                            
         J     DFRMP64                                                          
*                                                                               
         USING CTPRECSD,R4                                                      
DFRMP68  MVC   MS1OPRC,CTPRECCD    SET OUTPUT FIELD PRECISION                   
         TM    MS1OPRC,CTPRECCD_FORMULA_PRECISION_ADJUSTMENT                    
         JZ    *+12                                                             
         OI    MS1INDS,MSTFPREQ    SET FORMULA PRECISION                        
         NI    MS1OPRC,255-CTPRECCD_FORMULA_PRECISION_ADJUSTMENT                
*                                                                               
         TM    CTPRECFL,CTPRECFL_NO_DIRECT_TRANSFER                             
         JZ    *+8                                                              
         OI    MS1INDS,MSTDIREQ    SET FLAG: NOT A DIRECT TRANSFER              
         TM    CTPRECFL,CTPRECFL_INDEXED_DEMO                                   
         JZ    *+8                 NO                                           
         OI    MS1INDS,MSTNDXEQ    SET FLAG: INDEXED DEMO                       
         TM    CTPRECFL,CTPRECFL_EQUATED_FORMULA                                
         JZ    DFRMP70                                                          
*                                                                               
         MVI   MS1OPRC,0                                                        
         NI    MS1INDS,255-MSTRECEQ                                             
         OI    MS1INDS,MSTDEMEQ                                                 
         MVC   MS1DISP(1),CTPRECCD                                              
         OI    FLAG1,F1EQU         SET EQUATED DEMO FOUND                       
*                                                                               
DFRMP70  TM    CTPRECFL,CTPRECFL_NO_OP_DEMO  NO-OP DEMO/DEFAULT MACRO?          
         JZ    *+8                 NO                                           
         OI    MS1INDS,MSTNOPEQ                                                 
         J     DFRMP66                                                          
         DROP  R4                                                               
*                                                                               
         USING CTDPFCD,R4                                                       
DFRMP72  XC    FORMWRK,FORMWRK     REFORM TO 3CHAR DEMO NUMBER                  
         LA    RF,FORMWRK+2                                                     
         LLC   R1,CTDPFLEN                                                      
         LR    R0,R1                                                            
         LA    R4,CTDPFORM         R4 = A(INPUT FORMULA)                        
         SHI   R1,CTDPFCFQ         R1 = L'FORMULA                               
*                                                                               
* THE CODE BELOW IS THE ONLY REASON WE KNOW OF WHICH PREVENTS US FROM           
* USING MODIFIERS < X'40'. THIS CODE PARSES THE DEMO FORMULA ELEMENT,           
* AND KNOWS THAT IS HAS REACHED THE END OF THE DEMO FIELDS BY CHECKING          
* AGAINST X'40'. IF WE EVER NEED TO HAVE MODIFIERS < X'40', WE COULD            
* CHANGE THE DEMO FORMULA MAINTENANCE PROGRAM TO INSERT A FIXED                 
* DELIMITER VALUE AT THE END OF THE DEMO FIELDS, WHICH THIS CODE COULD          
* LOOK FOR, INSTEAD OF THE TEST IT IS DOING NOW.                                
*                                                                               
DFRMP74  CLI   0(R4),X'40'         END OF DEMO FIELDS                           
         JL    DFRMP76                                                          
         MVC   0(1,RF),0(R4)       REFORMAT THE DEMO CODE                       
         MVC   2(2,RF),1(R4)                                                    
         LA    R4,3(R4)                                                         
         LA    RF,4(RF)                                                         
         SHI   R1,3                                                             
         AHI   R0,1                                                             
         J     DFRMP74                                                          
*                                                                               
DFRMP76  STC   R0,FORMWRK+1        SAVE MODIFIED LENGTH                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RF),0(R4)       MOVE IN ACTUAL FORMULA                       
         DROP  R4                                                               
*                                                                               
         L     R2,ALSTFRM1         RF=A(NEXT FORMULA SLOT)                      
         CPYA  AR2,AR7                                                          
         LA    R4,FORMWRK                                                       
         CLC   10(4,R4),=X'01024E7E'  1 2 + =                                   
         JNE   DFRMP78                                                          
         MVC   0(8,R2),2(R4)                                                    
         MVI   8(R2),X'30'                                                      
         LA    R0,9(R2)                                                         
         J     DFRMP82                                                          
*                                                                               
DFRMP78  CLC   10(4,R4),=X'0102607E'  1 2 - =                                   
         JNE   DFRMP80                                                          
         MVC   0(8,R2),2(R4)                                                    
         MVI   8(R2),X'31'                                                      
         LA    R0,9(R2)                                                         
         J     DFRMP82                                                          
*                                                                               
DFRMP80  ZIC   R1,1(R4)                                                         
         SHI   R1,3                R1=L'FORMULA-1                               
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),2(R4)       MOVE FORMULA TO TABLE                        
         LA    R0,1(R1,R2)                                                      
*                                                                               
DFRMP82  ST    R0,ALSTFRM1         SET A(NEXT SLOT)                             
         SR    RE,RE                                                            
         ICM   RE,7,MSTAFM                                                      
         SR    R2,RE                                                            
         STCM  R2,7,MS1DISP        SET A(DEMO FORMULA)                          
         OI    MS1INDS,MSTFRMEQ    SET FORMULA FOUND                            
* CLEARING OUT 16 BYTES OF CURRENT EOT                                          
         L     R2,ALSTFRM1                                                      
         XC    0(16,R2),0(R2)                                                   
*                                                                               
         LAM   AR2,AR2,=F'0'                                                    
         J     DFRMP66                                                          
*                                                                               
DFRMP84  LA    R1,BPAR             ADD AN ENTRY TO TABLE                        
         USING BSPARA,R1                                                        
         MVI   BSPLENR,1           SET INSERT IF NOT FOUND                      
         MVI   BSPLENR+1,X'80'     SET USING ACCESS REGISTER                    
         LA    RE,MS1DTAD                                                       
         ST    RE,BSPAREC                                                       
         GOTO1 VBINSRCH,(R1)                                                    
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         TM    0(R1),X'80'         TEST RECORD ADDED                            
         JO    XIT                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
         DROP  R8                                                               
*                                                                               
DFRML    OI    FLAG1,F1LAST    *** LAST TIME                                    
         ICM   R7,15,ALSTNTRY                                                   
         JNZ   DFRMP02                                                          
         DC    H'0'                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD BOOK TO LIST OF BOOKS IN MASTER DEMO TABLE HEADER    *         
***********************************************************************         
         SPACE 1                                                                
         USING MSTHDRD,R7                                                       
         USING MS1DTAD,R8                                                       
SETBOOK  NTR1  ,                                                                
         STCM  R1,1,FLAG5          SET FLAG FOR BOOK TYPE                       
*                                  ON = MASTER DISPLACEMENT BOOK                
         XR    R1,R1                                                            
         ICM   R1,1,MSTBKNUM       R1=N'BOOK ENTRIES                            
         JZ    SETBK4                                                           
         LR    R0,R1                                                            
         LA    R2,MSTBKNDX         RF=A(BOOK INDEX)                             
         CPYA  AR2,AR7                                                          
*                                                                               
SETBK2   CLC   MS1BOOK(2),0(R2)    SEARCH TABLE FOR BOOK                        
         JE    SETBK6                                                           
         LA    R2,3(,R2)                                                        
         BRCT  R0,SETBK2                                                        
*                                                                               
SETBK4   LA    R1,1(R1)            NOT FOUND - ADD BOOK ENTRY                   
         STCM  R1,1,MSTBKNUM                                                    
         BCTR  R1,0                                                             
         MHI   R1,3                                                             
         LA    R2,MSTBKNDX(R1)                                                  
         CPYA  AR2,AR7                                                          
         MVC   0(2,R2),MS1BOOK                                                  
         MVI   2(R2),0                                                          
*                                                                               
SETBK6   MVC   2(1,R2),FLAG5       SET INDICATOR BYTE                           
         LAM   AR2,AR2,=F'0'                                                    
         XIT1  ,                                                                
         DROP  R7,R8                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT BOOK TO SEQUENCE NUMBER                          *         
***********************************************************************         
         SPACE 1                                                                
         USING MSTHDRD,R7                                                       
         USING MS1DTAD,R6                                                       
GETBOOK  NTR1  ,                                                                
         ZIC   R0,MSTBKNUM                                                      
         LA    R1,1                                                             
         LA    R2,MSTBKNDX         RF=A(BOOK INDEX)                             
         LAM   AR2,AR2,ALET                                                     
*                                                                               
GETBK2   CLC   MS1BOOK,0(R2)       SEARCH TABLE FOR BOOK                        
         JE    GETBK4                                                           
         LA    R2,3(R2)                                                         
         LA    R1,1(R1)                                                         
         BRCT  R0,GETBK2                                                        
         DC    H'0'                                                             
*                                                                               
GETBK4   LAM   AR2,AR2,=F'0'       SEARCH TABLE FOR BOOK                        
         XIT1  REGS=(R1)                                                        
         DROP  R6,R7                                                            
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DEMFORMX EQU   *                                                                
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD DEMO CODE TABLE                                               *         
* NTRY: R5     = A(CURRENT IN DATASPACE)                              *         
*       R6     = A(RECORD TO ADD TO TABLE)                            *         
*       AR MODE OFF/XA MODE OFF                                       *         
***********************************************************************         
         SPACE 1                                                                
DEMCODE  NTR1  BASE=(*,DEMCODEX),LABEL=*                                        
*                                                                               
         B     *+0(R1)                                                          
         B     DCODF               FIRST                                        
         B     DCODP               PROCESS                                      
         B     DCODL               LAST                                         
*                                                                               
DCODF    MVC   TABLEN,=H'6'    *** FIRST TIME                                   
         XC    ALSTNTRY,ALSTNTRY                                                
         J     XIT                                                              
*                                                                               
         USING CTNREC,R6                                                        
DCODP    LAM   AR7,AR7,ALET    *** PROCESS RECORD                               
         CPYA  AR5,AR7                                                          
         SAC   512                                                              
*                                                                               
         ICM   R7,15,ALSTNTRY                                                   
         JNZ   *+10                                                             
         LR    R7,R5                                                            
         J     DCODP02                                                          
*                                                                               
         ICM   R5,7,7(R7)                                                       
         MVI   0(R5),0                                                          
         LA    R7,1(R5)                                                         
*                                  BUILD TABLE HEADER                           
DCODP02  MVC   0(5,R7),CTNKFILE                                                 
         MVC   5(2,R7),TABLEN                                                   
         LA    R5,10(R7)                                                        
         STCM  R5,7,7(R7)                                                       
*                                                                               
         LA    R6,CTNDATA                                                       
         DROP  R6                                                               
         XR    R1,R1                                                            
DCODP04  CLI   0(R6),0                                                          
         JE    DCODP10                                                          
         CLI   0(R6),X'02'         DEMO CODE ELEMENT                            
         JE    DCODP08                                                          
*                                                                               
DCODP06  IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         J     DCODP04                                                          
*                                                                               
DCODP08  LH    R1,TABLEN           ADD ENTRY TO TABLE                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),2(R6)                                                    
         LA    R5,1(R5,R1)         GET A(NEXT ENTRY)                            
*                                                                               
* CLEARING OUT 16 BYTES OF CURRENT EOT                                          
         XC    0(16,R5),0(R5)                                                   
*                                                                               
         J     DCODP06                                                          
*                                  SET A(NEXT TABLE ENTRY)                      
DCODP10  STCM  R5,7,7(R7)                                                       
         STCM  R7,15,ALSTNTRY                                                   
         J     YES                                                              
*                                                                               
DCODL    L     R7,ALSTNTRY     *** LAST TIME                                    
         S     R7,ATABLE                                                        
         ST    R7,TOTLEN           SET TOTAL LENGTH OF TABLE                    
         J     YES                                                              
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DEMCODEX EQU   *                                                                
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD AGENCY CONTROL TABLE                                          *         
* NTRY: R5     = A(CURRENT IN DATASPACE)                              *         
*       R6     = A(RECORD TO ADD TO TABLE)                            *         
*       AR MODE OFF/XA MODE OFF                                       *         
***********************************************************************         
         SPACE 1                                                                
DEMCTRL  NTR1  BASE=(*,DEMCTRLX),LABEL=*                                        
*                                                                               
         B     *+0(R1)                                                          
         B     DCTLF               FIRST                                        
         B     DCTLP               PROCESS                                      
         B     DCTLL               LAST                                         
                                                                                
DCTLF    STCM  R5,15,ALSTNTRY  *** FIRST TIME                                   
         XC    PRKSRC,PRKSRC                                                    
         XC    APRKSRC,APRKSRC                                                  
         XC    ASRCPTR,ASRCPTR                                                  
         J     YES                                                              
                                                                                
         USING CTRREC,R6                                                        
DCTLP    LAM   AR7,AR7,ALET    *** PROCESS ENTRY                                
         ICM   R7,15,ALSTNTRY                                                   
         CPYA  AR5,AR7                                                          
                                                                                
         LAM   AR3,AR3,ALET        POINT R3 TO BEGINNING OF PREVIOUS            
         SR    R3,R3                                                            
         ICM   R3,15,APRKSRC       SOURCE/MED  IN DATASPACE                     
                                                                                
         SAC   512                                                              
                                                                                
*  CREATE AN 2 BYTE AGY ID OR USERID AND A(LINK) INTO OUR BUFFER                
*  FOR EVERY UNIQUE USERID OR AGENCY ID                                         
*  AT THIS POINT R7 SHOULD BE POINTING TO THE EMTPY AREA IN DATASPACE           
*  APRKSRC  SHOULD BE POINTING TO THE BEGINNING OF EACH UNIQUE                  
*  SOURCE MEDIA IN THE DATASPACE TABLE                                          
                                                                                
         OC    APRKSRC,APRKSRC                                                  
         BZ    DCTLP02                                                          
         CLC   PRKSRC,CTRKSRC      ONCE SRC/MED CHANGES WE CAN ADD IN           
         JE    DCTLP02             OUR DATA INTO DATASPACE                      
* FIRST MOVE IN OUR DUMMY DCON RECORD                                           
DCTLP00  XC    0(18,R7),0(R7)                                                   
         MVC   0(2,R7),=X'FF0F'    SAVE ADDRESS OF DUMMY RECORD TO              
         STCM  R7,15,ADUMMY        SET A(LINK) AFTER WE BUFFER DATA             
         MVI   17(R7),X'FF'                                                     
         AHI   R7,18               BUMP PAST DUMMY RECORD                       
                                                                                
         XC    PRVKAGY,PRVKAGY                                                  
DCTLP01  CLC   =X'FF0F',0(R3)      DO UNTIL WE REACH THE DUMMY RECORD           
         BE    DCTLP1C             UP TOP WE HAVE JUST CREATED                  
         OC    2(2,R3),2(R3)       IF SRC/MED DEFAULT DCON                      
         BNZ   DCTLP1A             WE DONT BUFFER IT                            
         AHI   R3,18               JUST BUMP PAST IT                            
         B     DCTLP01                                                          
DCTLP1A  CLC   PRVKAGY,2(R3)       ONLY ADD FOR UNIQUE AGY/UID                  
         BE    DCTLP1B             ELSE READ NEXT                               
         MVC   PRVKAGY,2(R3)                                                    
         MVC   0(2,R7),2(R3)       USERID                                       
         STCM  R3,15,2(R7)         SET A(LINK-1)                                
         AHI   R7,6                                                             
DCTLP1B  ICM   R3,7,10(R3)         GET A(NEXT RECORD-1)                         
         AHI   R3,1                BUMP PAST "FF" END OF RECORD                 
         J     DCTLP01                                                          
                                                                                
*  ONCE WE HAVE FINISH ADDING ALL THE AGY/ID A(LINK) TABLE ENTRIES INTO         
*  DCON TABLE, SET THE ADDRESS OF THE A(NEXT-1) LINK OF THE DUMMY DCON          
*  RECORD WE CREATED BEFORE  ( END OF TABLE IS MARKED BY X'0000')               
* SINCE THIS CAN NOT POSSIBLY BE EQUIVALENT TO AN AGENCY CODE OR USERID         
                                                                                
DCTLP1C  MVC   0(2,R7),=X'0000'   MARK END OF TABLE                             
         AHI   R7,1               POINT TO THE END OF AGY/USERID TABLE          
         ICM   R1,15,ADUMMY                                                     
         CPYA  AR1,AR7                                                          
         STCM  R7,7,10(R1)        STORE IN DUMMY DCON RECORD THE                
         LAM   AR1,AR1,=F'0'                                                    
         AHI   R7,1               A(NEXT RECORD-1)                              
                                                                                
DCTLP02  CLC   PRKSRC,CTRKSRC      SAME SOURCE MEDIA                            
         JE    *+8                                                              
         BRAS  RE,BSRCLNK          NO - SET AN OPTIMIZATION LINK                
                                                                                
         MVC   0(10,R7),CTRKSRC                                                 
         LA    R5,13(R7)                                                        
         LA    R6,CTRDATA                                                       
         DROP  R6                                                               
                                                                                
DCTLP03  CLI   0(R6),0                                                          
         JE    DCTLP12                                                          
         CLI   0(R6),CTROECDQ      AGENCY CONTROLS ELEMENT (X'03')              
         JE    DCTLP06                                                          
         CLI   0(R6),CTRMECDQ      MARKET CONTROL ELEMENTS (X'04')              
         JE    DCTLP08                                                          
                                                                                
DCTLP04  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         J     DCTLP03                                                          
                                                                                
         USING CTROD,R6                                                         
DCTLP06  MVC   0(4,R5),CTROOPT1    (ALSO CTROOPT2,CTROOPT3,CTROOPT4)            
         LA    R5,4(R5)                                                         
         J     DCTLP04                                                          
         DROP  R6                                                               
                                                                                
         USING CTRMD,R6                                                         
DCTLP08  LA    R1,CTRMMKTS         R1=A(FIRST MARKET)                           
         ZIC   R0,1(R6)                                                         
         SHI   R0,CTRMFXLQ                                                      
*                                  L'CTRMFXLQ = 2                               
         SRL   R0,1                R0=N'MARKETS                                 
                                                                                
DCTLP10  MVC   0(2,R5),0(R1)       MARKET NUMBER                                
         MVC   2(1,R5),CTRMTYPE    MARKET LIST TYPE (VALID OR INVALID)          
         AHI   R5,3                                                             
         AHI   R1,L'CTRMMKTS                                                    
         BRCT  R0,DCTLP10                                                       
         J     DCTLP04                                                          
         DROP  R6                                                               
                                                                                
DCTLP12  STCM  R5,7,10(R7)         SET A(END OF TABLE) IN HDR                   
         MVI   0(R5),X'FF'                                                      
         LA    R5,1(R5)                                                         
*                                                                               
* CLEARING OUT 16 BYTES OF CURRENT EOT                                          
         XC    0(16,R5),0(R5)                                                   
*                                                                               
         STCM  R5,15,ALSTNTRY                                                   
         J     YES                                                              
                                                                                
DCTLL    DS    0H                                                               
DCTLL02  L     R7,ALSTNTRY      *** LAST TIME                                   
         SR    R7,R5                                                            
         ST    R7,TOTLEN           SET TOTAL LENGTH OF TABLE                    
*                                  WHEN WE EXIT SET ALSTNTRY                    
*                                  TO POINT PAST OUR LAST TABLE                 
*                                  CREATED                                      
         J     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO BUILD SRC/MEDIA OPTIMIZATION LINKS                       *         
* NTRY: R5     = A(CURRENT IN DATASPACE)                              *         
*       R6     = A(RECORD TO ADD TO TABLE)                            *         
*       R7     = ALSTNTRY                                             *         
*       AR MODE ON/XA MODE OFF                                        *         
*       AR5    = TABS DATASPACE ALET                                  *         
*       AR7    = TABS DATASPACE ALET                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CTRREC,R6                                                        
BSRCLNK  ICM   R1,15,APRKSRC       PREVIOUS OPT LINK?                           
         JZ    BSRCLNK2                                                         
         CPYA  AR1,AR7                                                          
                                                                                
*  NO LONGER POINT TO NEXT SRC/MED- POINT TO OUR FF0F RECORD                    
         ICM   R0,15,ADUMMY                                                     
         SHI   R0,1                                                             
         STCM  R0,7,CONAET-CONHDRD(R1)                                          
                                                                                
         LAM   AR1,AR1,=F'0'                                                    
*                                                                               
BSRCLNK2 STCM  R7,15,APRKSRC       SAVE AND CLEAR LINK AREA                     
         XC    0(CONHDRLN,R7),0(R7)                                             
         MVC   PRKSRC,CTRKSRC      SAVE SOURCE FOR COMPARE                      
         MVC   0(2,R7),CTRKSRC     SET UP LINK ENTRY                            
         LA    R7,CONHDRLN(,R7)     BUMP PAST IT                                
         MVI   0(R7),X'FF'                                                      
         LA    R7,1(R7)                                                         
         STCM  R7,15,ALSTNTRY                                                   
*                                                                               
         L     R1,TOTLEN           UPDATE TOTAL LENGTH                          
         AHI   R1,CONHDRLN                                                      
         ST    R1,TOTLEN                                                        
         BR    RE                                                               
         DROP  R6                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DEMCTRLX EQU   *                                                                
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD AGENCY ADJUSTMENT TABLE                                       *         
* NTRY: R5     = A(CURRENT IN DATASPACE)                              *         
*       R6     = A(RECORD TO ADD TO TABLE)                            *         
*       AR MODE OFF/XA MODE OFF                                       *         
***********************************************************************         
         SPACE 1                                                                
DEMADJT  NTR1  BASE=(*,DEMADJTX),LABEL=*                                        
*                                                                               
         B     *+0(R1)                                                          
         B     DADJF               FIRST                                        
         B     DADJP               PROCESS                                      
         B     DADJL               LAST                                         
*                                                                               
DADJF    ST    R5,ALSTNTRY     *** FIRST TIME                                   
         J     YES                                                              
*                                                                               
         USING CTQREC,R6                                                        
DADJP    L     R7,ALSTNTRY     *** PROCESS ENTRY                                
         LAM   AR7,AR7,ALET                                                     
         SAC   512                                                              
         MVC   0(11,R7),CTQKSRC    BUILD TABLE HEADER                           
         CPYA  AR5,AR7                                                          
         LA    R5,14(R7)                                                        
         LA    R6,CTQDATA                                                       
         DROP  R6                                                               
*                                                                               
DADJP02  CLI   0(R6),0                                                          
         JE    DADJP12                                                          
         CLI   0(R6),X'03'         MODIFIER RULES ELEMENT                       
         JE    DADJP06                                                          
         CLI   0(R6),X'04'         HUT ADJUSTMENT ELEMENTS                      
         JE    DADJP10                                                          
*                                                                               
DADJP04  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         J     DADJP02                                                          
*                                                                               
DADJP06  ZIC   R0,1(R6)                                                         
         SRL   R0,1                                                             
         BCTR  R0,0                R0=N'ENTRIES IN ELEMENT                      
         LA    R1,2(R6)            R1=A(FIRST ENTRY)                            
*                                                                               
DADJP08  MVC   0(2,R5),0(R1)                                                    
         LA    R5,2(R5)                                                         
         LA    R1,2(R1)                                                         
         BRCT  R0,DADJP08                                                       
         J     DADJP04                                                          
*                                                                               
DADJP10  MVC   0(128,R5),3(R6)     MOVE ADJUSTMENT LIST TO TABLE                
         CLI   2(R6),1                                                          
         JNE   *+8                                                              
         MVI   0(R5),X'FF'         DELIMIT MODIFIER RULES LIST                  
         LA    R5,128(R5)                                                       
         J     DADJP04                                                          
*                                                                               
DADJP12  STCM  R5,7,11(R7)         SET A(END OF TABLE) IN HDR                   
         MVI   0(R5),X'FF'                                                      
         LA    R5,1(R5)                                                         
*                                                                               
* CLEARING OUT 16 BYTES OF CURRENT EOT                                          
         XC    0(16,R5),0(R5)                                                   
*                                                                               
         ST    R5,ALSTNTRY                                                      
         J     YES                                                              
*                                                                               
DADJL    L     R7,ALSTNTRY     *** LAST TIME                                    
         SR    R7,R5                                                            
         ST    R7,TOTLEN           SET TOTAL LENGTH OF TABLE                    
         J     YES                                                              
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DEMADJTX EQU   *                                                                
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD ALPHA MARKET TABLE (T00AE3)                                   *         
* NTRY: R5     = A(CURRENT IN DATASPACE)                              *         
*       R6     = A(RECORD TO ADD TO TABLE)                            *         
*       AR MODE OFF/XA MODE OFF                                       *         
***********************************************************************         
         SPACE 1                                                                
DEMAMKT  NTR1  BASE=(*,DEMAMKTX),LABEL=*                                        
*                                                                               
         XC    KEY,KEY                                                          
         XC    PREV,PREV           CLEAR PREVIOUS VALUES                        
         MVC   TABLEN,=Y(DAMDTAL)                                               
         L     R5,ATABHDR          SET R5 TO A(FIRST HEADER)                    
         USING DAMHDRD,R5                                                       
         LA    R7,DAMHDRL(R5)                                                   
         USING DAMDTAD,R7                                                       
         LA    R6,IO                                                            
         USING CTDMREC,R6                                                       
         LAM   AR5,AR5,ALET                                                     
         CPYA  AR7,AR5                                                          
         SAC   512                                                              
*                                                                               
         MVI   KEY+(CTDMKTYP-CTDMREC),CTDMKTEQ   RE-INITIALISE READ             
         MVI   KEY+(CTDMKTY2-CTDMREC),CTDMKT2E   TO ALPHA MKT RECORDS           
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                      
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         CLI   8(R1),0             READ ERROR?                                  
         JNE   *+14                                                             
         CLC   KEY(2),IO           ALPHA MKT RECORD?                            
         JE    DALPM04                                                          
         TM    8(R1),X'80'         EOF/NF?                                      
         JO    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LAM   AR6,AR6,ALET                                                     
         LR    R6,R5                                                            
         SAC   512                                                              
         LA    RE,HALPH            BUILD DUMMY TABLE                            
         LA    RF,HALPHL                                                        
         LR    R7,RF                                                            
         MVCL  R6,RE                                                            
         JNO   *+6                 DESTRUCTIVE MOVE?                            
         DC    H'0'                YES!                                         
         SAC   0                                                                
         LAM   AR6,AR6,=F'0'                                                    
*                                                                               
         MVC   TOTLEN,HALPH+8      INIT NOT SUCCESSFUL - EXIT NOW               
         J     YES                                                              
*                                                                               
DALPM02  MVC   KEY,IO                                                           
         GOTO1 VDMGR,DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,IO                      
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
*                                                                               
DALPM04  CLI   CTDMKTYP,CTDMKTEQ                                                
         JNE   DALPL                                                            
         CLI   CTDMKTY2,CTDMKT2E   MAKE SURE WE HAVE ALPH MKT RECD              
         JNE   DALPL                                                            
*                                                                               
         OC    PREV,PREV           FIRST TIME IN?                               
         JNZ   *+14                NO                                           
         MVC   PREV,CTDMKMED       SAVE MEDIA SOURCE                            
         J     DALPM05                                                          
*                                                                               
         CLC   PREV,CTDMKMED       MEDIA / SOURCE CHANGED - NEW TABLE           
         MVC   PREV,CTDMKMED                                                    
         JE    DALPM06                                                          
*                                                                               
         LR    R5,R7                                                            
         MVI   0(R5),0                                                          
         LA    R5,1(R5)            R5=A(NEXT TABLE ENTRY)                       
                                                                                
DALPM05  MVC   DAMHMED,CTDMKMED    MOVE IN NEW MEDIA/SOURCE                     
         MVC   DAMHSRC,CTDMKSRC                                                 
         MVC   DAMHLDE,TABLEN      AND L'DATA ELEMENT                           
         LA    R7,DAMHDRL(R5)                                                   
*                                                                               
DALPM06  MVC   DAMDAMKT,CTDMKMKT   ALPHA MARKET                                 
         MVC   DAMDBTYP,CTDMKBKT   BOOK TYPE                                    
         MVC   DAMDNMKT,CTDMKNUM   MARKET NUMBER                                
         AHI   R7,DAMDTAL                                                       
         MVI   0(R7),0                                                          
* CLEARING OUT 16 BYTES OF CURRENT EOT                                          
         XC    1(16,R7),1(R7)                                                   
*                                                                               
         STCM  R7,15,DAMHAET       SET NEW END OF TABLE ADDRESS                 
         J     DALPM02                                                          
         DROP  R5,R6,R7                                                         
*                                                                               
DALPL    MVC   1(2,R7),=X'FFFF'    2-BYTE EOTABLE MARKER                        
         LA    R7,3(,R7)                                                        
* CLEARING OUT 16 BYTES OF CURRENT EOT                                          
         XC    0(16,R7),0(R7)                                                   
*                                                                               
         L     RF,ATABLE                                                        
         SR    R7,RF                                                            
         ST    R7,TOTLEN           SET TOTAL LENGTH OF TABLE                    
         J     YES                                                              
*                                                                               
* ALPHA MARKET DUMMY TABLE IN CASE BUILD FAILS                                  
*                                                                               
HALPH    DC    CL8'DALPHMKT',AL4(20),XL4'00',X'FFFF'                            
HALPHL   EQU   *-HALPH                                                          
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DEMAMKTX EQU   *                                                                
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* DENADUNV - BUILD TABLE CONTAINING NAD UNVS FROM NAD PNNUUUU RECDS  *          
**********************************************************************          
         SPACE 1                                                                
DENADUNV NTR1  BASE=(*,DENADUNVX),LABEL=*                                       
*                                                                               
         XC    DSPFLAG,DSPFLAG                                                  
*                                  SET UP CORRECT FILE TO READ                  
         MVC   READDIR,=CL8'NTIDIR'                                             
         MVC   READFIL,=CL8'NTIFIL'                                             
         MVI   READEQU,NTIDQ                                                    
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=C'DTFAD'),READFIL,0,0                             
         L     RE,12(R1)                                                        
         USING DTFPHD,RE                                                        
         TM    DTFOPEN,X'20'                                                    
         BZ    DNAD32                                                           
         DROP  RE                                                               
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=C'DMREAD'),=C'SYSFLES',0                          
         ICM   R2,15,12(R1)         GET A(LIST OF FILES)                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,2(R2)          R0=NUMBER OF FILES                           
         LA    R2,4(R2)            R2=A(1ST FILE ENTRY)                         
*                                                                               
DNAD02   CLC   READEQU,3(R2)                                                    
         JE    DNAD04                                                           
         LA    R2,8(R2)                                                         
         BRCT  R0,DNAD02                                                        
         J     DNAD32              SYSTEM UNAVAIL - TABLE NOT INIT              
*                                                                               
DNAD04   TM    0(R2),X'80'         IS IT NOOP--FILE UNAVAILABLE                 
         JO    DNAD32               FILE UNAVAIL - EXIT W/TABLE NOT INT         
*                                                                               
         XC    KEY2,KEY2           SEE IF ANY UNIV RECDS OUT THERE              
         MVC   KEY2(8),=CL8'PNNUUUUN'                                           
DNAD06   GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),READDIR,KEY2,IO2                       
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         SAC   0                                                                
         CLI   8(R1),0             ANY READ ERRORS                              
         BNE   DNAD32              DATAMGR ERROR--JUST EXIT CLEANLY             
         CLC   KEY2(8),IO2         KEY READ IN                                  
         BNE   DNAD32              NONE FOUND, DON'T BUILD TABLE                
*                                                                               
         LA    R8,IO2              R8=IO2                                       
         USING PRKEY,R8                                                         
         CLC   PRBOOK,=AL2(MAY_95) MAY95 IS 1ST UNIV BOOK                       
         JE    DNAD08              USE IT                                       
*                                                                               
* DEIS AND SCHO SAY: THE CODE BELOW IS NEVER BEING EXECUTED, BECAUSE            
* THE FIRST BOOK ON THE FILE IS MAY/95. THIS CODE FORCES THE DATASPACE          
* TABLE TO BE BUILT STARTING WITH THE FIRST SEPTEMBER THAT FOLLOWS              
* CHRONOLOGICALLY AFTER THE FIRST RECORD FOUND. WE AREN'T SURE WHY THIS         
* WAS EVER DESIRABLE.                                                           
*                                                                               
*========                                                                       
         ZIC   R1,PRBOOK           R1= BK ACTUALLY FOUND                        
         CLI   PRBOOK+1,X'09'                                                   
         JE    DNAD08                                                           
         JL    *+8                                                              
         LA    R1,1(R1)            BUMP YEAR                                    
         LA    R8,KEY2                                                          
         STC   R1,PRBOOK                                                        
         MVI   PRBOOK+1,X'09'                                                   
         J     DNAD06                                                           
*========                                                                       
         DROP  R8                                                               
*                                                                               
DNAD08   MVC   KEY2(PRSTYP-PRKEY),IO2                                           
         MVC   SVDA,PRNDXDA-PRKEY(R8)   R8=IO2                                  
         MVC   SVSTATUS,PRKSTAT-PRKEY(R8)   R8=IO2                              
         MVC   IO2(L'KEY2),KEY2                                                 
         MVC   IO2+PRRSTAT-PRKEY(1),SVSTATUS                                    
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),READFIL,SVDA,IO2                       
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         SAC   0                                                                
         CLI   8(R1),0                                                          
         JNE   DNAD32                                                           
         CLC   KEY2(18),IO2        SAME KEY?                                    
         JNE   DNAD32              NO, DON'T BUILD A TABLE                      
*                                                                               
         L     R5,ATABHDR                                                       
         BRAS  RE,NAD5ON           XA & AR5 SET ON                              
*                                                                               
UNVTB    USING UNVTAB,R5                                                        
         XC    UNVTB.UNVHDR,UNVTB.UNVHDR   CLEAR DATASPACE TABLE HEADER         
         LA    R5,UNVTB.UNVENTRY   A(DATASPACE TABLE)                           
         LHI   R0,NADUNVMAXBOOKS+1 CLEAR ALL ENTRIES PLUS EOT ENTRY             
         XC    0(UNVTABLQ,R5),0(R5)                                             
         LA    R5,UNVTABLQ(,R5)                                                 
         BCT   R0,*-10                                                          
         STCM  R5,15,ALSTNTRY      SAVE A(ELEMENT STORE AREA)                   
         DROP  UNVTB                                                            
         BRAS  RE,NAD5OFF                                                       
*                                                                               
         LA    R7,UNVENTRY         R7=A(1ST ENTRY IN TABLE)                     
         XC    CURDEMS,CURDEMS     DEMOS CURRENTLY PROCESSING                   
         XC    PRVDEMS,PRVDEMS     PREV DEMOS SAVED IN TABLE                    
*                                                                               
         XC    UNVHDR,UNVHDR       CLEAR WORK TABLE HEADER                      
         LA    RE,UNVENTRY         A(WORK TABLE)                                
         LHI   R0,NADUNVMAXBOOKS+1 CLEAR ALL ENTRIES PLUS EOT ENTRY             
         XC    0(UNVTABLQ,RE),0(RE)                                             
         LA    RE,UNVTABLQ(,RE)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         XC    ALSTPTR,ALSTPTR     CLEAR A(NEXT ADDRESS IN DSPACE)              
         EJECT                                                                  
***********************************************************************         
* READ THRU UNV RECDS AND SAVE IN NAD TABLE                           *         
* R7 = CORE NAD ENTRY                                                 *         
* R5 = DATASPACE TABLE                                                *         
***********************************************************************         
         SPACE 1                                                                
DNAD10   LA    R0,UNVTABX          IS UNVTAB FULL?                              
         CR    R0,R7                                                            
         BH    DNAD11              NO                                           
         GOTO1 VDMGR,DMCB,(0,=C'OPMSG'),=C'AUTONOTE*US-MFDEMOSPROGRAMME+        
               RS:NADUNV TABLE HEADER FULL. INCREASE NADUNVMAXBOOKS IN +        
               DEDEMADDR.'                                                      
         B     DNAD34              EXIT WITH WHAT WE HAVE SO FAR                
*                                                                               
DNAD11   DS    0H                                                               
         LA    R6,IO2              POINT TO UNIV RECORD KEY                     
         MVC   0(2,R7),PRBOOK-PRKEY(R6)    SAVE BOOK JUST READ                  
         STCM  R5,15,2(R7)         SAVE POINTER TO DATA ELEMENTS                
*                                                                               
         LA    R7,L'UNVENTRY(R7)   BUMP TO NEXT SLOT                            
         MVC   0(2,R7),=X'FFFF'    EOT MARKER                                   
*                                                                               
* CLEARING 16 BYTES FROM CURRENT EOT                                            
         XC    2(16,R7),2(R7)                                                   
*                                                                               
         STCM  R7,15,UNVLST        PTR TO LAST ENTRY ('FFFFF')                  
         MVC   PRVDEMS,CURDEMS     RESET ADDR OF LAST SET OF DEMS               
         STCM  R5,15,CURDEMS       SET CURRENT ADDR                             
*                                                                               
DNAD12   BRAS  RE,NAD5ON                                                        
         LA    R8,PRFRSTEL-PRKEY(,R6)  POINT TO 1ST ELEMENT                     
         XR    R0,R0                                                            
*                                                                               
DNAD14   CLI   0(R8),X'00'         ENDREC?                                      
         JE    DNAD18                                                           
         CLI   0(R8),X'23'                                                      
         JNL   DNAD16              COPY MKTBRK ELEM & DEMO ELEMENTS             
         ICM   R0,1,1(R8)          BUMP TO NEXT ELEMENT                         
         AR    R8,R0               NEXT ELEMENT                                 
         J     DNAD14                                                           
*                                                                               
DNAD16   ZIC   R1,1(R8)            COPY ELEMENT                                 
         LA    RE,0(R1,R5)         RE = POST-INSERTION A(END-OF-TABLE)          
         C     RE,DSPTEND          IS THERE ROOM FOR THIS ELEMENT?              
         JH    *+2                 NO: DNADUNV DATASPACE TBL OVERFLOW!          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R8)       MOVE ELEMENT FROM IO2 TO DATASPACE           
         LA    R1,1(R1)                                                         
         AR    R5,R1               INCREMENT DATASPACE POINTER                  
* CLEARING 16 BYTES FROM CURRENT EOT                                            
         XC    0(16,R5),0(R5)                                                   
*                                                                               
         ST    R5,ALSTNTRY         SAVE A(NEXT SLOT IN DATASPACE)               
         AR    R8,R1               INCREMENT RECORD POINTER                     
         J     DNAD14              COPY ELEMENTS TO END OF RECORD               
*                                                                               
DNAD18   BRAS  RE,NAD5OFF          XA & AR5 SET OFF                             
*                                                                               
         LA    R8,KEY2                                                          
         USING PRKEY,R8                                                         
         XC    KEY2,KEY2           DO REST OF MKTBRKS FOR YEAR                  
         MVC   KEY2(14),IO2                                                     
         ZIC   R1,PRBTYP           BUMP STYPE CTR OF RECDS FOR YEAR             
         LA    R1,1(R1)            NEXT RECD                                    
         STC   R1,PRBTYP           R8 PTS TO KEY2                               
         XC    IO2(L'KEY2),IO2                                                  
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),READDIR,KEY2,IO2                       
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         SAC   0                                                                
         CLI   8(R1),0                                                          
         JNE   DNAD20              EOF?                                         
         CLC   KEY2(13),IO2        SAME KEY INCL SAME BOOK?                     
         JNE   DNAD20              NO - DONE WITH THIS YEAR                     
*                                                                               
         LA    R8,IO2              GET UNIV RECD FROM NTIFIL                    
         MVC   KEY2(PRSTYP-PRKEY),IO2                                           
         MVC   SVDA,PRNDXDA                                                     
         MVC   SVSTATUS,PRKSTAT                                                 
         MVC   IO2(L'KEY2),KEY2                                                 
         MVC   IO2+PRRSTAT-PRKEY(1),SVSTATUS                                    
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),READFIL,SVDA,IO2                       
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         SAC   0                                                                
         CLI   8(R1),0                                                          
         JNE   DNAD32                                                           
         CLC   KEY2(18),IO2        SAME KEY?                                    
         JNE   DNAD32              NO - ERROR                                   
         J     DNAD12                                                           
         DROP  R8                                                               
*                                                                               
DNAD20   BRAS  RE,NAD5ON           XA & AR5 SET ON                              
         ICM   R5,15,ALSTNTRY                                                   
         MVI   0(R5),X'00'         PUT '00' EOR MARKER                          
         AHI   R5,1                                                             
         STCM  R5,15,ALSTNTRY                                                   
*                                                                               
         ICM   R2,15,PRVDEMS       CMP THIS BOOKS DEMOS TO LAST IN TBL          
         JZ    DNAD28              NO PREVIOUS                                  
         ICM   R3,15,CURDEMS                                                    
         CR    R2,R3               PREVIOUS AND CURRENT SAME = IGNORE           
         JE    DNAD28                                                           
         CPYA  AR2,AR5                                                          
         CPYA  AR3,AR2                                                          
         XR    RF,RF                                                            
*                                                                               
DNAD22   CLI   0(R3),0                                                          
         JE    DNAD26              BOTH BOOKS MATCH->DELETE LATEST              
         ICM   RF,1,1(R3)          COMPARE WK TO WHAT WAS LAST SAVED            
         CLI   0(R3),X'5E'         DON'T CMP X'5E' ELEMS- BKS DIFFER            
         JE    DNAD24                                                           
         BCTR  RF,0                                                             
         EX    RF,DNADCLC                                                       
         JNE   DNAD28              DIFFERENT DATA -- SAVE THIS BK TOO           
         LA    RF,1(RF)                                                         
*                                                                               
DNAD24   AR    R3,RF               SAME DATA, BUMP AND TEST NEXT ELEM           
         AR    R2,RF                                                            
         J     DNAD22                                                           
*                                                                               
DNADCLC  CLC   0(0,R2),0(R3)                                                    
*                                                                               
DNAD26   MVC   ALSTNTRY,CURDEMS    FREE UP DEMO STG                             
         MVC   CURDEMS,PRVDEMS                                                  
         ICM   R5,15,ALSTNTRY                                                   
         XC    0(3,R5),0(R5)                                                    
         SHI   R7,L'UNVENTRY                                                    
         MVC   0(2,R7),=X'FFFF'                                                 
* CLEARING 16 BYTES FROM CURRENT EOT                                            
         XC    2(16,R7),2(R7)                                                   
         STCM  R7,15,UNVLST        SET NEXT FREE BACK TO CURRENT                
*                                                                               
DNAD28   DS    0H                                                               
         LAM   AR2,AR3,=2F'0'      RESET AR2 AND 3                              
         BRAS  RE,NAD5OFF          XA & AR5 SET OFF                             
*                                                                               
         LA    R8,KEY2             LAST YEAR READ IN                            
         USING PRKEY,R8                                                         
         CLC   PRBOOK,=AL2(MAY_95) 1ST BK IN LIST?                              
         JNE   *+14                                                             
         MVC   PRBOOK,=AL2(SEP_95) READ NEXT BK IN SAME YEAR                    
         J     DNAD30                                                           
*                                                                               
         ZIC   R1,PRBOOK+1         R1=LAST MONTH PROCESSED                      
         LA    R1,1(R1)                                                         
         STC   R1,PRBOOK+1         BUMP TO NEXT YEAR                            
         CLI   PRBOOK+1,13         MONTHS 1-12                                  
         JL    DNAD30                                                           
         MVI   PRBOOK+1,1                                                       
         IC    R1,PRBOOK                                                        
         LA    R1,1(R1)                                                         
         STC   R1,PRBOOK                                                        
*                                                                               
DNAD30   XC    PRSTYP(PRRSTAT-PRSTYP),PRSTYP  CLEAR TO END OF KEY               
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),READDIR,KEY2,IO2                       
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         SAC   0                                                                
         CLI   8(R1),0             ANY READ ERRORS                              
         JNE   DNAD34                                                           
         CLC   KEY2(PRBOOK-PRKEY),IO2   DID WE READ A PNNUUUU KEY               
         JNE   DNAD34              NO, DONE READING ALL YEARS                   
         LA    R8,IO2                                                           
         MVC   KEY2(PRSTYP-PRKEY),IO2                                           
         MVC   SVDA,PRNDXDA                                                     
         MVC   SVSTATUS,PRKSTAT                                                 
         MVC   IO2(L'KEY2),KEY2                                                 
         MVC   IO2+PRRSTAT-PRKEY(1),SVSTATUS                                    
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),READFIL,SVDA,IO2                       
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         SAC   0                                                                
         CLI   8(R1),0                                                          
         JNE   DNAD32                                                           
         CLC   KEY2(18),IO2        SAME KEY?                                    
         JNE   DNAD32              NO                                           
         J     DNAD10                                                           
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* UNABLE TO BUILD TABLE. EXIT WITH A DUMMY TEMPLATE TABLE             *         
* IN DSPACE. DON'T LET OTHER TASKS TRY TO RE-BUILD TABLE.             *         
***********************************************************************         
         SPACE 1                                                                
DNAD32   L     R5,ATABLE                                                        
         BRAS  RE,NAD5ON           XA & AR5 SET ON                              
*                                                                               
         CPYA  AR6,AR5                                                          
         LR    R6,R5                                                            
         LA    RE,HNADU                                                         
         LA    RF,HNADUL                                                        
         LR    R7,RF                                                            
         MVCL  R6,RE               COPY IN DUMMY TABLE                          
         JNO   *+6                 DESTRUCTIVE MOVE?                            
         DC    H'0'                YES!                                         
*                                                                               
         LAM   AR6,AR6,=F'0'                                                    
         BRAS  RE,NAD5OFF          XA & AR5 SET OFF                             
         J     YES                                                              
         SPACE 3                                                                
* NAD UNIVERSE DUMMY TABLE IN CASE BUILD FAILS                                  
*                                                                               
HNADU    DC    CL8'DNADUNV ',AL4(20),XL4'00',X'FFFF'                            
HNADUL   EQU   *-HNADU                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* UNIVERSES READ INTO UNVTAB AND POINTERS CREATED TO DATA.            *         
* SLOT ENTRIES IN REVERSE ORDER INTO REAL TABLE IN DATASPACE.         *         
***********************************************************************         
         SPACE 1                                                                
DNAD34   DS    0H                                                               
         L     R5,ATABHDR                                                       
         BRAS  RE,NAD5ON                                                        
*                                                                               
         XR    R0,R0                                                            
         L     R7,UNVLST           PT TO LAST ENTRY SAVED                       
DNAD36   LA    R0,UNVENTRY         1ST ENTRY IN UNVTAB                          
         CR    R7,R0                                                            
         JE    DNAD38              YES, DONE COPYING ENTRIES                    
         LA    R0,L'UNVENTRY       NO, BACK UP                                  
         SR    R7,R0               R7 ORIG PTS TO EOT OF UNVTAB                 
         MVC   0(6,R5),0(R7)                                                    
         XC    0(2,R5),=X'FFFF'    REVERSE SEQ OF BK                            
         AR    R5,R0               BUMP DATASPACE TABLE ENTRY                   
         J     DNAD36                                                           
*                                                                               
DNAD38   MVC   0(2,R5),=X'FFFF'    MARK END OF BK->PTR TABLE                    
* CLEARING 16 BYTES FROM CURRENT EOT                                            
         XC    2(16,R5),2(R5)                                                   
                                                                                
         L     R5,ALSTNTRY         PT TO TOP OF TABLE                           
         MVC   0(2,R5),=X'FFFF'    MARK IT FOR EXTRA PROTECTION                 
* CLEARING 16 BYTES FROM CURRENT EOT                                            
         XC    2(16,R5),2(R5)                                                   
*                                                                               
         AHI   R5,3                                                             
         S     R5,ATABLE           TOTAL LENGTH OF TABLE                        
         STCM  R5,15,TOTLEN                                                     
         SAM24 ,                   TURN OFF XA                                  
*                                                                               
         BRAS  RE,NAD5OFF                                                       
         J     YES                                                              
*                                                                               
NAD5ON   LAM   AR5,AR5,ALET        AR5 ON/XA MODE ON                            
         SAC   512                                                              
         SAM31 ,                                                                
         BR    RE                                                               
*                                                                               
NAD5OFF  SAC   0                   AR5 OFF/XA MODE OFF                          
         LAM   AR5,AR5,=F'0'                                                    
         SAM24 ,                                                                
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DENADUNVX EQU   *                                                               
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* DEMFUSN - BUILD TABLE CONTAINING FUSION SUBSCRIBER BASE DATA       *          
**********************************************************************          
*                                                                               
DEMFUSN  NTR1  BASE=(*,DEMFUSNX),LABEL=*                                        
*                                                                               
         MVI   DSPFLAG,0                                                        
         MVC   DATADISP,=H'23'     DISPLACEMENT TO 1ST DEMFIL ELEMENT           
*                                                                               
         LA    R3,BPAR             PARAMETERS TO BINSR31                        
         XC    BPAR(28),BPAR       CLEAR ALL 7 PARAMETERS                       
         USING BSPARA,R3                                                        
         L     RF,ATABHDR          A(FUSION TABLE AREA)                         
         AHI   RF,FUSTABLQ         BUMP PAST BINSR31 PARAMETER AREA             
         ST    RF,BSPSTRT          A(TABLE)                                     
         MVC   BSPLENR,=A(FUSTABLQ) L'RECORD                                    
         MVI   BSPLENR+1,X'80'     SET USING ACCESS REGISTER                    
         MVC   BSPLENK,=A(FUSKEYLQ) L'KEY                                       
*                                  THEORETICAL MAX # OF TABLE ENTRIES =         
*                                   (MAX NO. SYSCODES (9999) TIMES...           
*                                    ...NUMBER OF YEARS OF DATA)...             
*                                    ...MINUS 1 ENTRY FOR BINSR31 PARMS         
*                                  BUT WE CALCULATE MAX # OF ENTRIES...         
*                                    ...FROM THE DSPACE HEADER SO...            
*                                    ...THAT WE CAN'T POSSIBLY OVERFLOW         
*                                                                               
         ICM   R1,15,DSPTEND       A(END OF TABLE MINUS 1)                      
         AHI   R1,1                A(END OF TABLE)                              
         ICM   R0,15,DSPTFRST      A(START OF TABLE)                            
         N     R0,=XL4'0FFFFFFF'                                                
         SR    R1,R0               R1 = MAX SIZE OF TABLE                       
         SHI   R1,(16+FUSTABLQ)    RESERVE 16 BYTES FOR TABLE HEADER...         
*                                   AND ONE ENTRY FOR BINSR31 PARMS             
         SR    R0,R0               PREPARE FOR DIVIDE                           
         D     R0,=A(FUSTABLQ)     R1 = MAX NUMBER OF ENTRIES                   
         ST    R1,BSPEND           SAVE IN BINSR31 PARAMETER LIST               
*                                                                               
         MVC   BSPARS,ALET         SET ALET                                     
         L     R5,ATABHDR                                                       
         BRAS  RE,FUS5ON           XA & AR5 SET ON                              
         MVC   0(28,R5),BPAR       SET BINSR31 PARMS IN DATASPACE               
         BRAS  RE,FUS5OFF                                                       
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=C'DTFAD'),=C'DEMFIL',0,0                          
         L     RE,12(R1)                                                        
         TM    (DTFOPEN-DTFPHD)(RE),X'20'                                       
         BZ    DFUSBAD                                                          
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=C'DMREAD'),=C'SYSFLES',0                          
         ICM   R2,15,12(R1)        GET A(LIST OF FILES)                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,2(R2)          R0=NUMBER OF FILES                           
         LA    R2,4(R2)            R2=A(1ST FILE ENTRY)                         
*                                                                               
DFUS10   CLI   3(R2),DEMDRAQ       DEMDIRA                                      
         BE    DFUS20                                                           
         LA    R2,8(R2)                                                         
         BCT   R0,DFUS10                                                        
         B     DFUSBAD             SYSTEM UNAVAIL - TABLE NOT INIT              
*                                                                               
DFUS20   TM    0(R2),X'80'         IS IT NOOP--FILE UNAVAILABLE                 
         BO    DFUSBAD              FILE UNAVAIL - EXIT W/TABLE NOT INT         
*                                                                               
         LA    R8,KEY2                                                          
         XC    KEY2,KEY2           SEE IF ANY FUSION RECORDS EXIST              
         USING DSYKEY,R8                                                        
         XC    DSYKMAJ,DSYKMAJ     READ 'BTF' KEYS                              
         MVI   DSYCODE,DSYCDEQU    C'B'                                         
         MVI   DSYMEDIA,C'T'       TV                                           
         MVI   DSYSRC,C'F'         FUSION                                       
         MVI   DSYBKEFF,YR_2008    NO NEED TO READ PAST 2008 (WE USE...         
         XI    DSYBKEFF,X'FF'      ...UE DATA, NOT CABLEZONE)                   
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),=C'DEMDIR',KEY2,IO2                    
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         B     DFUS40                                                           
*                                                                               
DFUS30   GOTO1 VDMGR,DMCB,(0,=C'DMRSEQ'),=C'DEMDIR',KEY2,IO2                    
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
*                                                                               
DFUS40   SAC   0                                                                
         CLI   8(R1),0             ANY READ ERRORS                              
         BNE   DFUSBAD             DATAMGR ERROR--JUST EXIT CLEANLY             
*                                                                               
         LA    R8,IO2                                                           
         MVC   SVSTATUS,DSYKSTAT   SAVE STATUS BYTE                             
         MVC   SVDA,DSYNDXDA       SAVE DISK ADDRESS                            
*                                                                               
         CLI   DSYCODE,DSYCDEQU    STILL LOOKING AT 'BTF' KEYS?                 
         BNE   DFUSOK                                                           
         CLI   DSYMEDIA,C'T'                                                    
         BNE   DFUSOK                                                           
         CLI   DSYSRC,C'F'                                                      
         BNE   DFUSOK                                                           
*                                                                               
         XC    DSYSYSCD,DSYSYSCD   CLEAR MINOR KEY                              
         XC    DSYRLEN,DSYRLEN                                                  
         MVC   DSYRSTAT,SVSTATUS   COPY STATUS BYTE TO I/O AREA                 
         GOTO1 VDMGR,DMCB,(0,=C'DMRDHI'),=C'DEMFIL',SVDA,IO2                    
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         B     DFUS60                                                           
*                                                                               
DFUS50   GOTO1 VDMGR,DMCB,(0,=C'DMRSEQ'),=C'DEMFIL',SVDA,IO2                    
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
*                                                                               
DFUS60   SAC   0                                                                
         CLI   8(R1),X'80'         EOF?                                         
         BE    DFUS30              YES: NO MORE SYSCODES FOR THIS YR/MN         
         CLI   8(R1),0                                                          
         BNE   DFUSBAD             CLEAN EXIT ON ANY OTHER DMGR ERROR           
*                                                                               
         XC    FORMWRK,FORMWRK                                                  
REC      USING FUSTABD,FORMWRK                                                  
         MVC   REC.FUSSYSCD,DSYSYSCD   SYSCODE                                  
         MVC   HALF,DSYBKEFF       BINARY YEAR/MONTH (BITS INVERTED)            
         XC    HALF,=X'FFFF'       NORMALIZE                                    
         MVC   REC.FUSYEAR,HALF    EXTRACT YEAR                                 
*                                                                               
         LR    R7,R8                                                            
         MVI   ELCODE,FANCODEQ     FUSION ALPHA NAMES ELEMENT                   
         USING FANELEM,R7                                                       
         BRAS  RE,GETEL                                                         
         BNE   *+10                ELEMENT ABSENT (NO LONGER CREATED)           
         MVC   REC.FUSMKT,FANMKTCD MARKET CODE                                  
         DROP  R7                                                               
*                                                                               
         LR    R7,R8                                                            
         MVI   ELCODE,FSCCODEQ     FUSION SUBSCRIBER BASE ELEMENT               
         USING FSCELEM,R7                                                       
         BRAS  RE,GETEL                                                         
         BNE   DFUS70              ELEMENT ABSENT (NO LONGER CREATED)           
*                                                                               
         ZIC   R1,HALF+1           EXTRACT MONTH NUMBER                         
         BCTR  R1,0                ZERO-BASED (NOT 1-BASED)                     
         MHI   R1,8                TIMES TWO FULLWORD VALUES PER MONTH          
         LA    R1,REC.FUSVALS(R1)  R1 POINTS TO SLOT IN MONTH ARRAY             
         USING FUSVALS,R1                                                       
         MVC   FUSSBASE,FSCSBASE   SUBSCRIBER BASE                              
         MVC   FUSUNIV,FSCUNIV     UNIVERSE                                     
         DROP  R1                                                               
         DROP  R7                                                               
*                                                                               
DFUS70   DS    0H                                                               
         LA    RF,FORMWRK                                                       
         ST    RF,BSPAREC          A(RECORD)                                    
         OI    BSPLENR,X'01'       INSERT THE RECORD IF NOT FOUND               
         GOTO1 VBINSRCH,BPAR                                                    
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         OC    0(4,R1),0(R1)       TABLE FULL?                                  
         BNZ   *+6                                                              
         DC    H'0'                YES                                          
         TM    BSPNF,X'80'         RECORD NOT FOUND?                            
         BO    DFUS50              CORRECT, BUT IT'S JUST BEEN ADDED            
*                                                                               
         L     R5,0(,R1)           A(RECORD IN TABLE)                           
*********CLC   FUSMKT,REC.FUSMKT   MARKET CODE MUST MATCH                       
*********BE    *+6                                                              
*********DC    H'0'                MARKET CODE FOR SYSCODE HAS CHANGED!         
         BRAS  RE,FUS5ON           XA & AR5 SET ON                              
         USING FUSTABD,R5                                                       
         OC    FUSVALS(12*2*4),REC.FUSVALS  'OR' IN THIS MONTH'S VALUES         
         DROP  R5                                                               
         BRAS  RE,FUS5OFF          XA & AR5 SET OFF                             
*                                                                               
         B     DFUS50              LOOK FOR NEXT MINOR KEY                      
         DROP  REC                                                              
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* UNABLE TO BUILD TABLE. EXIT WITH A DUMMY TEMPLATE TABLE             *         
* IN DSPACE. DON'T LET OTHER TASKS TRY TO RE-BUILD TABLE.             *         
* TABLE CONSISTS OF BINSR31 PARAMETERS, WITH NUMBER OF TABLE          *         
* ENTRIES SET TO ZERO.                                                *         
***********************************************************************         
*                                                                               
DFUSBAD  L     R5,ATABLE           A(16-BYTE HEADER)                            
         BRAS  RE,FUS5ON           XA & AR5 SET ON                              
         MVC   0(8,R5),=CL8'DFUSION'                                            
         BRAS  RE,FUS5OFF          XA & AR5 SET OFF                             
         MVC   TOTLEN,=AL4(FUSTABLQ+16)                                         
         J     YES                                                              
         SPACE 3                                                                
***********************************************************************         
* TABLE IS FULLY BUILT.                                               *         
***********************************************************************         
*                                                                               
DFUSOK   L     R5,ATABHDR          A(BINSR31 PARMS)                             
         BRAS  RE,FUS5ON           XA & AR5 SET ON                              
         MVC   (BSPNOR-BSPARA)(,R5),BSPNOR  SET NO. OF ENTRIES IN PARMS         
         BRAS  RE,FUS5OFF          XA & AR5 SET OFF                             
*                                                                               
         L     R0,BSPNOR           NUMBER OF RECORDS IN TABLE                   
         DROP  R3                                                               
         AHI   R0,1                ADD 1 FOR BINSR31 PARMS                      
         MHI   R0,FUSTABLQ         R0 = TOTAL LENGTH OF TABLE                   
         AHI   R0,16               ADD 16 FOR TABLE HEADER                      
         STCM  R0,15,TOTLEN                                                     
         J     YES                                                              
         SPACE 3                                                                
FUS5ON   LAM   AR5,AR5,ALET        AR5 ON/XA MODE ON                            
         SAC   512                                                              
         SAM31 ,                                                                
         BR    RE                                                               
*                                                                               
FUS5OFF  SAC   0                   AR5 OFF/XA MODE OFF                          
         LAM   AR5,AR5,=F'0'                                                    
         SAM24 ,                                                                
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DEMFUSNX EQU   *                                                                
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* DEMKNAM - BUILD TABLE CONTAINING MARKET NAMES                      *          
**********************************************************************          
DEMKNAM  NTR1  BASE=(*,DEMKNAMX),LABEL=*                                        
                                                                                
         L     RF,VCOM                                                          
         ICM   RF,15,CDEMTBOF-COMFACSD(RF)    DEMTABOF                          
         JZ    YES                 EXIT IF ONLINE                               
         LHI   R0,MRKTNAMT         MARKET NAME TABLE INDICATOR                  
         ST    R0,DMCB                                                          
         GOTO1 (RF),DMCB           GET A(MARKET NAME TABLE)                     
         ICM   R2,15,0(R1)         A(TABLE) RETURNED IN P1                      
         JZ    YES                 BAD TABLE ID PASSED                          
         ICM   R6,15,4(R1)         L(MKT TABLE ENTRY)                           
                                                                                
         LA    R3,BPAR             PARAMETERS TO BINSR31                        
         XC    BPAR(28),BPAR       CLEAR ALL 7 PARAMETERS                       
         USING BSPARA,R3                                                        
         L     RF,ATABHDR          A(MKT NAME TABLE AREA)                       
         AHI   RF,DMPARMSL         12 FULL WORDS BEFORE DATA                    
         ST    RF,BSPSTRT                                                       
         MVC   BSPLENR,=A(DMNDATL)                                              
         MVI   BSPLENR+1,X'80'     SET USING ACCESS REGISTER                    
         MVC   BSPLENK,=A(DMNKEYL) L'KEY                                        
                                                                                
* CALCULATE MAX # OF ENTRIES FROM THE DSPACE HEADER SO THAT WE CAN'T            
* POSSIBLY OVERFLOW                                                             
         ICM   R1,15,DSPTEND       A(END OF TABLE MINUS 1)                      
         AHI   R1,1                A(END OF TABLE)                              
         ICM   R0,15,DSPTFRST      A(START OF TABLE)                            
         N     R0,=XL4'0FFFFFFF'                                                
         SR    R1,R0               R1 = MAX SIZE OF TABLE                       
*RESERVE 16 BYTES FOR TABLE HEADER, FOR BINSR31 PARMS, AND EOT                  
         SHI   R1,(16+DMPARMSL+2)                                               
         SR    R0,R0               PREPARE FOR DIVIDE                           
         D     R0,=A(DMNDATL)      R1 = MAX NUMBER OF ENTRIES                   
         ST    R1,BSPEND           SAVE IN BINSR31 PARAMETER LIST               
                                                                                
         MVC   BSPARS,ALET         SET ALET                                     
         L     R5,ATABHDR                                                       
         BRAS  RE,DMK5ON                                                        
         XC    0(DMPARMSL,R5),0(R5)  CLEAR PARMS AREA AND SPARE                 
         MVC   0(28,R5),BPAR       SET BINSR31 PARMS IN DATASPACE               
         DROP  R3                                                               
                                                                                
         LA    R5,DMPARMSL(,R5)                                                 
         USING DMNDATD,R5                                                       
DEMKN10  LA    R3,5(R2)            MARKET NAME ENTRY IN OFFLINE TABLE           
         USING MKNTABD,R3                                                       
DEMKN20  MVC   DMNSRC,0(R2)        FILL IN MKT NAME ENTRIES IN DSPACE           
         MVC   DMNMED,1(R2)                                                     
         MVC   DMNDNUM,MKNNUM                                                   
         MVC   DMNDBTYP,MKNBTYP                                                 
         MVC   DMNDNAM,MKNNAME                                                  
         AR    R3,R6               NEXT ENTRY IN OFFLINE TABLE                  
         LA    R5,DMNDATL(,R5)     NEXT SLOT IN DATASPACE                       
         OC    0(2,R3),0(R3)                                                    
         BNZ   DEMKN20                                                          
                                                                                
         SR    R0,R0               NEXT MEDIA/SOURCE                            
         ICM   R0,7,2(R2)                                                       
         LR    R2,R0                                                            
         OC    0(2,R2),0(R2)                                                    
         BNZ   DEMKN10                                                          
                                                                                
         MVC   0(2,R5),=X'FFFF'                                                 
         LA    R5,2(,R5)                                                        
         DROP  R3,R5                                                            
                                                                                
         CPYA  AR1,AR5                                                          
         L     R1,ATABHDR                                                       
         SR    R5,R1                                                            
         STCM  R5,15,TOTLEN        TOTAL LENGTH OF MKT NAME TABLE               
         LAM   AR1,AR1,=F'0'                                                    
                                                                                
         LR    R1,R5               TOTAL LENGTH...                              
         SHI   R1,DMPARMSL+2       ...LESS THE PARAMS HEADER AND EOT            
         SR    R0,R0                                                            
         D     R0,=A(DMNDATL)      ...DEVIDED BY ENTRY LENGTH                   
         L     R5,ATABHDR                                                       
         USING BSPARA,R5                                                        
         ST    R1,BSPNOR           NO OF ENTRIES IN THE DSPACE TABLE            
         DROP  R5                                                               
                                                                                
         BRAS  RE,DMK5OFF                                                       
         J     YES                                                              
*                                                                               
DMK5ON   LAM   AR5,AR5,ALET        AR5 ON/XA MODE ON                            
         SAC   512                                                              
         SAM31 ,                                                                
         BR    RE                                                               
                                                                                
DMK5OFF  SAC   0                   AR5 OFF/XA MODE OFF                          
         LAM   AR5,AR5,=F'0'                                                    
         SAM24 ,                                                                
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DEMKNAMX EQU   *                                                                
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* DENFORM - BUILD TABLE CONTAINING NEW FORMULAS                      *          
**********************************************************************          
                                                                                
DENFORM  NTR1  BASE=(*,DENFORMX),LABEL=*                                        
                                                                                
         L     RF,VCOM                                                          
         ICM   RF,15,CDEMTBOF-COMFACSD(RF)    DEMTABOF                          
         JZ    YES                 EXIT IF ONLINE                               
         LHI   R0,NFORMTAB         NEW FORMULA TABLE INDICATOR                  
         ST    R0,DMCB                                                          
         GOTO1 (RF),DMCB           GET A(MARKET NAME TABLE)                     
                                                                                
         ICM   R2,15,0(R1)         A(TABLE) RETURNED IN P1                      
         JZ    YES                 BAD TABLE ID PASSED                          
         MVC   TOTLEN,8(R1)                                                     
         ICM   R7,15,8(R1)         L(TABLE) PASSED IN P2                        
         LR    R3,R7                                                            
                                                                                
         BRAS  RE,DNF5ON           AR5 ON/XA MODE ON                            
         L     R5,ATABHDR          SET R5 TO A(FIRST HEADER)                    
                                                                                
         CPYA  AR6,AR5                                                          
         LR    R6,R5                                                            
         MVCL  R6,R2               MOVE TABLE INTO DSPACE                       
         JNO   *+6                 DESTRUCTIVE MOVE?                            
         DC    H'0'                YES!                                         
         LAM   AR6,AR6,=F'0'                                                    
                                                                                
         BRAS  RE,DNF5OFF          AR5 ON/XA MODE OFF                           
         J     YES                                                              
                                                                                
DNF5ON   LAM   AR5,AR5,ALET        AR5 ON/XA MODE ON                            
         SAC   512                                                              
         SAM31 ,                                                                
         BR    RE                                                               
*                                                                               
DNF5OFF  SAC   0                   AR5 OFF/XA MODE OFF                          
         LAM   AR5,AR5,=F'0'                                                    
         SAM24 ,                                                                
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
DENFORMX EQU   *                                                                
         SPACE 2                                                                
         DROP  RB                                                               
                                                                                
**********************************************************************          
* RENTRAK - BUILD TABLE CONTAINING RENTRAK INFO                      *          
*           ONLY ALLOW A BUILD ON P1(1) = X'FF' OR X'FE'             *          
**********************************************************************          
DRENTK   NTR1  BASE=(*,DRENTKX),LABEL=*                                         
         L     R1,CALLR1                                                        
         CLI   4(R1),X'FF'         THIS CAN NOT BE A BUILD                      
         JE    NO                                                               
         ICM   R6,15,8(R1)         A(MQ BUFFER OF RENTRAK ENTRIES)              
                                                                                
         LA    R3,BPAR             PARAMETERS TO BINSR31                        
         XC    BPAR(28),BPAR       CLEAR ALL 7 PARAMETERS                       
         USING BSPARA,R3                                                        
         L     RF,ATABHDR          A(TABLE AREA)                                
         AHI   RF,DMPARMSL         12 FULL WORDS BEFORE DATA                    
         ST    RF,BSPSTRT                                                       
         MVC   BSPLENR,=A(RENLNQ)  RECORD LENGTH                                
         MVI   BSPLENR+1,X'80'     SET USING ACCESS REGISTER                    
         MVC   BSPLENK,=A(L'RENCODE)   L'KEY                                    
*----------------------------------------------------------------------         
* CALCULATE MAX # OF ENTRIES FROM THE DSPACE HEADER SO THAT WE CAN'T            
* POSSIBLY OVERFLOW                                                             
*----------------------------------------------------------------------         
         ICM   R1,15,DSPTEND       A(END OF TABLE MINUS 1)                      
         AHI   R1,1                A(END OF TABLE)                              
         ICM   R0,15,DSPTFRST      A(START OF TABLE)                            
         N     R0,=XL4'0FFFFFFF'                                                
         SR    R1,R0               R1 = MAX SIZE OF TABLE                       
*----------------------------------------------------------------------         
*RESERVE 16 BYTES FOR TABLE HEADER, FOR BINSR31 PARMS, AND EOT                  
*----------------------------------------------------------------------         
         SHI   R1,(16+DMPARMSL+2)                                               
         SR    R0,R0               PREPARE FOR DIVIDE                           
         D     R0,=A(RENLNQ)       R1 = MAX NUMBER OF ENTRIES                   
         ST    R1,BSPEND           SAVE IN BINSR31 PARAMETER LIST               
         LR    R9,R1               SAVE MAX ENTRIES                             
                                                                                
         MVC   BSPARS,ALET         SET ALET                                     
         L     R5,ATABHDR                                                       
         LAM   AR5,AR5,ALET        AR5 ON/XA MODE ON                            
         SAC   512                                                              
         SAM31 ,                   R5 NOW IN TABS DATA SPACE                    
*                                                                               
         XC    0(DMPARMSL,R5),0(R5)  CLEAR PARMS AREA AND SPARE                 
         MVC   0(28,R5),BPAR       SET BINSR31 PARMS IN DATASPACE               
         DROP  R3                                                               
                                                                                
         LA    R5,DMPARMSL(,R5)    POINT TO START OF TABLE                      
         USING RENTABD,R5                                                       
MQ       USING RENTABD,R3                                                       
*                                                                               
         LR    R3,R6               R6=A(INPUT FROM MQ MSG OR DATASET)           
         SHI   R3,DMTLNQ           MQ HEADER INFO                               
         LM    R7,R8,0(R3)         LOAD LENGTH OF ENTRY, SIZE OF MSG            
         LR    R3,R6               R6=A(INPUT FROM MQ MSG OR DATASET)           
                                                                                
         SR    R0,R0               R0=ENTRY COUNT                               
DRENT10  MVC   RENCODE,MQ.RENCODE  MOVE INFO INTO DATASPACE                     
         MVC   RENNAME,MQ.RENNAME  MOVE INFO TO DATA SPACE                      
         MVC   RENDESC,MQ.RENDESC  MOVE INFO TO DATA SPACE                      
         AR    R3,R7               NEXT ENTRY IN MQ BUFFER                      
         AHI   R5,RENLNQ           NEXT ENTRY IN TABS DATA SPACE                
         AHI   R0,1                                                             
         CR    R0,R9               HIT MAX ENTRIES                              
         JH    *+2                 [] MAKE TABLE BIGGER                         
         SR    R8,R7               LESS ONE ENTRY                               
         BP    DRENT10             KEEP GOING                                   
         DROP  MQ                                                               
         DROP  R5                                                               
                                                                                
         L     R1,ATABHDR          POINT TO PARAMETERS                          
         SR    R5,R1                                                            
         STCM  R5,15,TOTLEN        TOTAL LENGTH OF RENTRAK TABLE                
*                                                                               
         L     R5,ATABHDR          POINT TO PARAMETERS                          
         USING BSPARA,R5                                                        
         ST    R0,BSPNOR           NO OF ENTRIES IN THE DSPACE TABLE            
         DROP  R5                                                               
                                                                                
         SAC   0                   AR5 OFF/XA MODE OFF                          
         LAM   AR5,AR5,=F'0'                                                    
         SAM24 ,                                                                
         J     YES                                                              
*                                                                               
         LTORG                                                                  
DRENTKX  EQU   *                                                                
         DROP  RB                                                               
                                                                                
***********************************************************************         
* COMSCORE - BUILD TABLE CONTAINING COMSCORE LOCAL DEMOS BY NUMBER              
*            ONLY ALLOW A BUILD ON P1(1) = X'FF' OR X'FE'                       
***********************************************************************         
DCSLON   NTR1  BASE=(*,DCSLONX),LABEL=*                                         
         L     R1,CALLR1                                                        
         CLI   4(R1),X'FF'         THIS CAN NOT BE A BUILD                      
         JE    NO                                                               
         ICM   R6,15,8(R1)         A(MQ BUFFER OF RENTRAK ENTRIES)              
                                                                                
         LA    R3,BPAR             PARAMETERS TO BINSR31                        
         XC    BPAR(28),BPAR       CLEAR ALL 7 PARAMETERS                       
         USING BSPARA,R3                                                        
         L     RF,ATABHDR          A(TABLE AREA)                                
         AHI   RF,DMPARMSL         12 FULL WORDS BEFORE DATA                    
         ST    RF,BSPSTRT                                                       
         MVC   BSPLENR,=A(CSDLNQ)  RECORD LENGTH                                
         MVI   BSPLENR+1,X'80'     SET USING ACCESS REGISTER                    
         MVC   BSPLENK,=A(L'CSDDESC)   L'KEY                                    
*----------------------------------------------------------------------         
* CALCULATE MAX # OF ENTRIES FROM THE DSPACE HEADER SO THAT WE CAN'T            
* POSSIBLY OVERFLOW                                                             
*----------------------------------------------------------------------         
         ICM   R1,15,DSPTEND       A(END OF TABLE MINUS 1)                      
         AHI   R1,1                A(END OF TABLE)                              
         ICM   R0,15,DSPTFRST      A(START OF TABLE)                            
         N     R0,=XL4'0FFFFFFF'                                                
         SR    R1,R0               R1 = MAX SIZE OF TABLE                       
*----------------------------------------------------------------------         
*RESERVE 16 BYTES FOR TABLE HEADER, FOR BINSR31 PARMS, AND EOT                  
*----------------------------------------------------------------------         
         SHI   R1,(16+DMPARMSL+2)                                               
         SR    R0,R0               PREPARE FOR DIVIDE                           
         D     R0,=A(CSDLNQ)       R1 = MAX NUMBER OF ENTRIES                   
         ST    R1,BSPEND           SAVE IN BINSR31 PARAMETER LIST               
         LR    R9,R1               SAVE MAX ENTRIES                             
                                                                                
         MVC   BSPARS,ALET         SET ALET                                     
         L     R5,ATABHDR                                                       
         LAM   AR5,AR5,ALET        AR5 ON/XA MODE ON                            
         SAC   512                                                              
         SAM31 ,                   R5 NOW IN TABS DATA SPACE                    
*                                                                               
         XC    0(DMPARMSL,R5),0(R5)  CLEAR PARMS AREA AND SPARE                 
         MVC   0(28,R5),BPAR       SET BINSR31 PARMS IN DATASPACE               
         DROP  R3                                                               
                                                                                
         LA    R5,DMPARMSL(,R5)    POINT TO START OF TABLE                      
         USING CSDTABD,R5                                                       
MQ       USING CSDTABD,R3                                                       
*                                                                               
         LR    R3,R6               R6=A(INPUT FROM MQ MSG OR DATASET)           
         SHI   R3,DMTLNQ           MQ HEADER INFO                               
         LM    R7,R8,0(R3)         LOAD LENGTH OF ENTRY, SIZE OF MSG            
         LR    R3,R6               R6=A(INPUT FROM MQ MSG OR DATASET)           
                                                                                
         SR    R0,R0               R0=ENTRY COUNT                               
DCSLON10 MVC   CSDDESC,MQ.CSDDESC  MOVE INFO INTO DATASPACE                     
         MVC   CSDCODE,MQ.CSDCODE  MOVE INFO TO DATA SPACE                      
         AR    R3,R7               NEXT ENTRY IN MQ BUFFER                      
         AHI   R5,CSDLNQ           NEXT ENTRY IN TABS DATA SPACE                
         AHI   R0,1                                                             
         CR    R0,R9               HIT MAX ENTRIES                              
         JH    *+2                 [] MAKE TABLE BIGGER                         
         SR    R8,R7               LESS ONE ENTRY                               
         BP    DCSLON10            KEEP GOING                                   
         DROP  MQ                                                               
         DROP  R5                                                               
                                                                                
         L     R1,ATABHDR          POINT TO PARAMETERS                          
         SR    R5,R1                                                            
         STCM  R5,15,TOTLEN        TOTAL LENGTH OF RENTRAK TABLE                
*                                                                               
         L     R5,ATABHDR          POINT TO PARAMETERS                          
         USING BSPARA,R5                                                        
         ST    R0,BSPNOR           NO OF ENTRIES IN THE DSPACE TABLE            
         DROP  R5                                                               
                                                                                
         SAC   0                   AR5 OFF/XA MODE OFF                          
         LAM   AR5,AR5,=F'0'                                                    
         SAM24 ,                                                                
         J     YES                                                              
*                                                                               
         LTORG                                                                  
DCSLONX  EQU   *                                                                
         DROP  RB                                                               
                                                                                
**********************************************************************          
* RENTRAK - BUILD TABLE CONTAINING RENTRAK COMSCORE NETWORK INFO     *          
*           ONLY ALLOW A BUILD ON P1(1) = X'FF' OR X'FE'             *          
**********************************************************************          
DRENTN   NTR1  BASE=(*,DRENTNX),LABEL=*                                         
         L     R1,CALLR1                                                        
         CLI   4(R1),X'FF'         THIS CAN NOT BE A BUILD                      
         JE    NO                                                               
         ICM   R6,15,8(R1)         A(MQ BUFFER OF RENTRAK ENTRIES)              
                                                                                
         LA    R3,BPAR             PARAMETERS TO BINSR31                        
         XC    BPAR(28),BPAR       CLEAR ALL 7 PARAMETERS                       
         USING BSPARA,R3                                                        
         L     RF,ATABHDR          A(TABLE AREA)                                
         AHI   RF,DMPARMSL         12 FULL WORDS BEFORE DATA                    
         ST    RF,BSPSTRT                                                       
         MVC   BSPLENR,=A(RTKNLNQ) RECORD LENGTH                                
         MVI   BSPLENR+1,X'80'     SET USING ACCESS REGISTER                    
         MVC   BSPLENK,=A(L'RTKNET)  L'KEY                                      
*---------------------------------------------------------------------          
* CALCULATE MAX # OF ENTRIES FROM THE DSPACE HEADER SO THAT WE CAN'T            
* POSSIBLY OVERFLOW                                                             
*---------------------------------------------------------------------          
         ICM   R1,15,DSPTEND       A(END OF TABLE MINUS 1)                      
         AHI   R1,1                A(END OF TABLE)                              
         ICM   R0,15,DSPTFRST      A(START OF TABLE)                            
         N     R0,=XL4'0FFFFFFF'                                                
         SR    R1,R0               R1 = MAX SIZE OF TABLE                       
*---------------------------------------------------------------------          
*RESERVE 16 BYTES FOR TABLE HEADER, FOR BINSR31 PARMS, AND EOT                  
*---------------------------------------------------------------------          
         SHI   R1,(16+DMPARMSL+2)                                               
         SR    R0,R0               PREPARE FOR DIVIDE                           
         D     R0,=A(RTKNLNQ)      R1 = MAX NUMBER OF ENTRIES                   
         ST    R1,BSPEND           SAVE IN BINSR31 PARAMETER LIST               
         LR    R9,R1               SAVE MAX ENTRIES                             
                                                                                
         MVC   BSPARS,ALET         SET ALET                                     
         L     R5,ATABHDR                                                       
         LAM   AR5,AR5,ALET        AR5 ON/XA MODE ON                            
         SAC   512                                                              
         SAM31 ,                   R5 NOW IN TABS DATA SPACE                    
*                                                                               
         XC    0(DMPARMSL,R5),0(R5)  CLEAR PARMS AREA AND SPARE                 
         MVC   0(28,R5),BPAR       SET BINSR31 PARMS IN DATASPACE               
         DROP  R3                                                               
                                                                                
         LA    R5,DMPARMSL(,R5)    POINT TO START OF TABLE                      
         USING RTKNETD,R5                                                       
MQ       USING RTKNETD,R3                                                       
*                                                                               
         LR    R3,R6               R6=A(INPUT FROM MQ MSG OR DATASET)           
         SHI   R3,DMTLNQ           MQ HEADER INFO                               
         LM    R7,R8,0(R3)         LOAD LENGTH OF ENTRY, SIZE OF MSG            
         LR    R3,R6               R6=A(INPUT FROM MQ MSG OR DATASET)           
                                                                                
         SR    R0,R0               R0=ENTRY COUNT                               
DRNTN10  MVC   RTKNET,MQ.RTKNET    MOVE INFO INTO DATASPACE                     
         MVC   RTKNET#,MQ.RTKNET#  MOVE INFO TO DATA SPACE                      
         MVC   RTKNCA#,MQ.RTKNCA#  MOVE INFO TO DATA SPACE                      
         AR    R3,R7               NEXT ENTRY IN MQ BUFFER                      
         AHI   R5,RTKNLNQ          NEXT ENTRY IN TABS DATA SPACE                
         AHI   R0,1                                                             
         CR    R0,R9               HIT MAX ENTRIES                              
         JH    *+2                 [] MAKE TABLE BIGGER                         
         SR    R8,R7               LESS ONE ENTRY                               
         BP    DRNTN10             KEEP GOING                                   
         DROP  MQ                                                               
         DROP  R5                                                               
                                                                                
         L     R1,ATABHDR          POINT TO PARAMETERS                          
         SR    R5,R1                                                            
         STCM  R5,15,TOTLEN        TOTAL LENGTH OF RENTRAK TABLE                
*                                                                               
         L     R5,ATABHDR          POINT TO PARAMETERS                          
         USING BSPARA,R5                                                        
         ST    R0,BSPNOR           NO OF ENTRIES IN THE DSPACE TABLE            
         DROP  R5                                                               
                                                                                
         SAC   0                   AR5 OFF/XA MODE OFF                          
         LAM   AR5,AR5,=F'0'                                                    
         SAM24 ,                                                                
         J     YES                                                              
*                                                                               
         LTORG                                                                  
DRENTNX  EQU   *                                                                
         DROP  RB                                                               
                                                                                
**********************************************************************          
* SEND AN E-MAIL IF A DEMOS DATASPACE TABLE IS REBUILT BY ANY                   
* UNEXPECTED PROCESS. EXPECTED PROCESSES ARE:                                   
*   1. THE DAILY CLRTABS JOB                                                    
*   2. A MANUAL CLEAR OF THE TABLE VIA SPOT/SFM DMTEST ACTION                   
**********************************************************************          
SNDMAIL  NTR1  BASE=(*,SNDMAILXX),LABEL=*                                       
*                                                                               
         CLI   DELIBFLG,C'Y'       CALLER ASKED FOR REBUILD?                    
         BE    SNDMAILX            YES: DON'T SEND E-MAIL                       
*                                                                               
         MVC   WRNMSG,=C'AUTONOTE*US-MFDEMOSPROGRAMMERS:DEMADDR REBUILD+        
                OF '                                                            
         MVC   WRNMSG1,=C' CALLED BY '                                          
         LR    RF,RD                                                            
         CLC   =C'EMAD',0(RF)      LOOK FOR DEMADDR EYE-CATCHER                 
         BE    *+12                                                             
         L     RF,4(,RF)                                                        
         B     *-14                                                             
*                                                                               
         L     R1,4(,RF)           GO BACK 1 MORE LEVEL                         
         L     RE,(17-1)*4(,RF)    THIS IS THE CALLER'S RB                      
         MVC   WRNCALR,22(RE)      POSIT CALLER USED NBASE                      
         CLC   =X'90ECD00C0DB041BB0000',0(RE)                                   
         BE    SNDMAIL5            CORRECT                                      
         CLC   =X'90ECD00C0DB0A7BAFFFA47FB',0(RE)   ... OR NMOD1                
         BE    SNDMAIL5            CORRECT                                      
         CLC   =X'90ECD00C0DB0A7BAFFFAA7F4',0(RE)   OR NMOD1 WITH JUMP          
         BE    SNDMAIL5            CORRECT                                      
         MVC   WRNCALR,20(RE)      POSIT CALLER USED NTR1 BASE=*                
         CLC   =X'90ECD00C0DB0A7BAFFFA50DD',0(RE)                               
         BE    SNDMAIL5            CORRECT                                      
         MVC   WRNCALR(4),0(R1)    USE RD CHAIN EYE-CATCHER                     
         MVC   WRNCALR+4(4),=C'????'                                            
*                                                                               
SNDMAIL5 DS    0H                                                               
         GOTO1 VDMGR,DMCB,=C'OPMSG',('WRNMSGLQ',WRNMSG)                         
*                                                                               
SNDMAILX DS    0H                                                               
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
SNDMAILXX EQU   *                                                               
         SPACE 2                                                                
         DROP  RB                                                               
         EJECT                                                                  
         TITLE 'DEMADDR - LITERALS AND CONSTANTS'                               
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R7,DATADISP,ELCODE                                               
         SPACE 1                                                                
EOT      EQU   0                                                                
FIRST    EQU   4                                                                
PROCESS  EQU   8                                                                
LAST     EQU   12                                                               
*                                                                               
*                                  SYSTEM FILE #'S                              
PAVDQ    EQU   X'2C'               PAVDIR                                       
PAVFQ    EQU   X'2E'               PAVFIL                                       
DEMDRAQ  EQU   X'2F'               DEMDIRA                                      
NTIDQ    EQU   X'38'               NTIDIR                                       
NTIFQ    EQU   X'39'               NTIFIL                                       
DEMDQ    EQU   X'30'               DEMDIRR                                      
         TITLE 'DEMADDR - TABLE ENTRIES'                                        
         EJECT                                                                  
***********************************************************************         
* TABLE OF VALID DEMO TABLES                                          *         
***********************************************************************         
CTLTAB   DS    0L                                                               
         DC    CL8'DDISP   ',X'D0',C' ',AL4(0,DEMDISP)                          
         DC    AL1(CTLHDR+CTLCORE),AL2(CT00AD0-COMFACSD),11X'00'                
         DC    CL8'DBOOK   ',X'D1',C'B',AL4(DTDBOOK,DEMBOOK)                    
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DSTATION',X'D2',C'S',AL4(DTDSTN,DEMSTAT)                     
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DMASTER ',X'D3',C'G',AL4(DTDMSTR,DEMFORM)                    
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
CDFORM   DC    CL8'DFORMULA',X'D4',C' ',AL4(DTDFRMLA,0)                         
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DNAME   ',X'D5',C'D',AL4(DTDNAME,DEMNAME)                    
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DCODE   ',X'D6',C'N',AL4(DTDCODE,DEMCODE)                    
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DCONTROL',X'D7',C'R',AL4(DTDCNTRL,DEMCTRL)                   
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DADJUST ',X'D8',C'Q',AL4(DTDADJST,DEMADJT)                   
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DFMTAB  ',X'E2',C' ',AL4(DTDFMTAB,0)                         
         DC    AL1(0),AL2(0),11X'00'                                            
         DC    CL8'DALPHMKT',X'E3',C' ',AL4(DTDAMKT,DEMAMKT)                    
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DNADUNV ',X'E4',C' ',AL4(DTDNUNV,DENADUNV)                   
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DFUSION ',X'E5',C' ',AL4(DTFUSN,DEMFUSN)                     
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DMKNAME ',X'E6',C' ',AL4(DTDMKTN,DEMKNAM)                    
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DNFORMS ',X'E7',C' ',AL4(DTDNFOR,DENFORM)                    
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DRENTRK ',X'E8',C' ',AL4(DTDRNTRK,DRENTK)                    
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DRENTNE ',X'E9',C' ',AL4(DTDRNTNE,DRENTN)                    
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DCOMSNAT',X'EA',C' ',AL4(DTDCSNAT,DRENTK)                    
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    CL8'DCOMSLON',X'EB',C' ',AL4(DTDCSLON,DCSLON)                    
         DC    AL1(CTLHDR),AL2(0),11X'00'                                       
         DC    AL1(EOT)                                                         
*                                                                               
CTLTABD  DSECT ,               *** DSECT COVERING CTLTAB                        
CTLNAME  DS    CL8                 TABLE NAME                                   
CTLPHSN  DS    XL1                 TABLE PHASE NUMBER                           
CTLKLET  DS    CL1                 CONTROL FILE KEY LETTER                      
CTLID    DS    AL4                 TABSDSP IDENTIFIER FOR LOCKSPC               
CTLADDR  DS    AL4                 A(INITIALIZATION ROUTINE)                    
CTLFLAG  DS    XL1                 FLAGS                                        
CTLHDR   EQU   X'80'               TABLE HAS 16-BYTE HEADER                     
CTLCORE  EQU   X'40'               TABLE IN CORE (IN COMFACS)                   
CTLCOMF  DS    AL2                 DISPLACEMENT INTO COMFACS IF CTLCORE         
         DS    XL11                SPARE                                        
CTLLNQ   EQU   *-CTLTABD                                                        
         TITLE 'DEMADDR - WORKING STORAGE DSECTS'                               
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER WORKING STORAGE                                      *         
***********************************************************************         
WORKD    DSECT ,                                                                
DUB      DS    D                                                                
LOCKID   DS    F                    CURRENT EQUATED TABLE IDENTIFIER            
FULL     DS    F                                                                
RELO     DS    A                                                                
CALLR1   DS    A                                                                
VCOM     DS    A                    V(COMFACS)                                  
VSWITCH  DS    A                    V(SWITCH)                                   
VDMGR    DS    A                    V(DMGR)                                     
VSYSFAC  DS    A                    V(SYSFACS)                                  
AMASTC   DS    A                    V(MASTC)                                    
ASSB     DS    A                    V(SSB)                                      
ALET     DS    A                    TABSDSP ALET                                
DSPXID   DS    F                    EQU FOR 1ST ADDITIONAL TABLE                
DSPXID1  DS    F                    EQU FOR 2ND ADDITIONAL TABLE                
VBINSRCH DS    A                    V(BINSRCH)                                  
*                                                                               
P0       DS    F                    FOR LOCKSPC CALLS VIA DATAMGR               
DMCB     DS    XL24                 DMCB MUST FOLLOW FIELD P0 !!!               
*                                                                               
BPAR     DS    XL32                                                             
READDIR  DS    CL8                                                              
READFIL  DS    CL8                                                              
READEQU  DS    XL1                                                              
WORK     DS    XL64                                                             
DSPHD    DS    XL64                 SAVED DSPACE HEADER                         
FORMWRK  DS    XL256                                                            
REINIT   DS    C                                                                
BYTE     DS    X                                                                
OKAY     DS    C                                                                
DELIBFLG DS    C                    'Y' = CALLER WANTS DELIBERATE BUILD         
DSPFLG   DS    X                                                                
DSPFLCK  EQU   X'80'                1ST ADDITIONAL DATASPACE LOCKED             
DSPFLCK1 EQU   X'40'                2ND ADDITIONAL DATASPACE LOCKED             
FLAG1    DS    X                                                                
F1LAST   EQU   X'80'                LAST TIME PROCESSING                        
F1EQU    EQU   X'40'                EQUATED DEMO BOOK FOUND                     
FLAG2    DS    X                                                                
FLAG4    DS    X                                                                
F4STARTN EQU   X'80'                WENT THRU DEMRDIR ROUTINE ALREADY           
F4GTMAIN EQU   X'40'                DID "MY OWN" GETMAIN FOR STORAGE            
FLAG5    DS    X                                                                
ELCODE   DS    X                   ELEMENT CODE (FOR GETEL)                     
ARTN     DS    A                                                                
ATABLE   DS    A                                                                
ATABHDR  DS    A                                                                
ALSTNTRY DS    A                                                                
ALSTFRM  DS    A                   A(FORMULA TABLE)                             
ALSTFRM1 DS    A                   A(FIRST ENTRY FORMULA TABLE)                 
ALSTFRMX DS    A                   A(LAST IN FORMULA TABLE)                     
ALSTMST  DS    A                   A(MASTER TABLE)                              
ALSTMST1 DS    A                   A(FIRST ENTRY MASTER TABLE)                  
ALSTMSTX DS    A                   A(LAST IN MASTER TABLE)                      
ALSTPTR  DS    A                                                                
AREC     DS    A                                                                
APRKSRC  DS    A                                                                
PRKSRC   DS    CL2                                                              
SVSTATUS DS    X                   SAVED I/S STATUS BYTE                        
SVDA     DS    XL4                 SAVED I/S DISK ADDRESS                       
GETSIZE  DS    F                   AMOUNT REQUESTED FROM GETMAIN                
TOTLEN   DS    F                                                                
HALF     DS    H                                                                
TABLEN   DS    H                                                                
CURBOOK  DS    H                                                                
DATADISP DS    H                   DISPLACEMENT TO 1ST ELEM (FOR GETEL)         
PREV     DS    0XL2                                                             
PRVMED   DS    CL1                 PREVIOUS MEDIA                               
PRVSRC   DS    CL1                 PREVIOUS SOURCE                              
FILTERS  DS    XL3                                                              
KEY      DS    CL25                                                             
KEY2     DS    CL25                                                             
*                                                                               
CURDEMS  DS    F                   ADDRESS OF LAST SET OF DEMOS SAVED           
PRVDEMS  DS    F                   ADDRESS OF LAST SET OF DEMOS SAVED           
CORADDR  DS    F                   PTR TO ACTUAL CORETABLE ENTRY                
UNVLST   DS    F                   ADDRESS OF LAST ENTRY SAVED                  
*                                                                               
* THE DNADUNV DATASPACE TABLE BEGINS WITH AN INDEX AREA. THIS IS AN             
* ARRAY OF POINTERS INTO THE ACTUAL NAD UNIVERSE DATA AREA FURTHER DOWN         
* IN THE TABLE. THE ARRAY IS KEYED BY YEAR/MONTH. THE INDEX AREA IS             
* BUILT HERE IN W/S AND COPIED INTO THE DATASPACE. THE "UNVTAB" TABLE           
* MUST BE LARGE ENOUGH TO ACCOMMODATE ALL YEAR/MONTH COMBINATIONS OF            
* SURVEYS FOR WHICH WE HAVE NAD UNIVERSE DATA.                                  
UNVTAB   DS    0X                  SOME SPACE FOR A UNV WORK TABLE              
UNVHDR   DS    XL16                16-BYTE HEADER                               
UNVENTRY DS    (NADUNVMAXBOOKS)XL(UNVTABLQ) WORK TABLE                          
NADUNVMAXBOOKS EQU 200             MAXIMUM NUMBER OF UNIQUE BOOKS               
UNVTABX  DS    XL(UNVTABLQ)        END OF TABLE MARKER                          
UNVTABLQ EQU   2+4                 L'ENTRY (YM BOOK AND POINTER)                
*                                                                               
DCNBUFFQ EQU   1200                                                             
PRVKAGY  DS    CL2                                                              
ASRCPTR  DS    A                                                                
ADUMMY   DS    A                                                                
*                                                                               
WRNMSG   DS    C'AUTONOTE*US-MFDEMOSPROGRAMMERS:DEMADDR REBUILD OF '            
WRNTBLID DS    CL8                 TABLE ID                                     
WRNMSG1  DS    C' CALLED BY '                                                   
WRNCALR  DS    CL8                 EYE-CATCHER OF CALLER                        
WRNMSGLQ EQU   *-WRNMSG                                                         
*                                                                               
SOURCE   DS    C                                                                
IO       DS    1000C                                                            
IO2      DS    2000C               CHGD FROM 1000C TO 2000C FOR PAVFIL          
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* OTHER INTERNAL DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* DSECT TO COVER CTRLTAB                                                        
*                                                                               
CTRLTABD DSECT ,                                                                
CTRLNAME DS    CL8                 TABLE NAME                                   
CTRLKLET DS    CL1                 CONTROL FILE KEY LETTER                      
CTRLPHSN DS    XL1                 TABLE PHASE NUMBER                           
CTRLINDS DS    XL1                 INDICATORS - X'80' TABLE IS LOADED           
*                                   X'20' BUILD TABLE IN HIGH CORE              
CTRLDISP DS    AL2                 DISP TO TABLE ADDRESS IN COMFACS             
CTRLADDR DS    AL3                 A(INITIALIZATION ROUTINE)                    
CTRLLEN  EQU   *-CTRLTABD                                                       
         SPACE 1                                                                
MS1DTAD  DSECT ,                   DATA ELEMENT                                 
MS1MODC  DS    CL1                 MODIFIER CODE                                
MS1DEMO  DS    XL2                 DEMO NUMBER (ZERO=MACRO)                     
MS1BOOK  DS    XL2                 EFFECTIVE BOOK NUMBER                        
MS1IPRC  DS    XL1                 INPUT FIELD PRECISION                        
MS1OPRC  DS    XL1                 OUTPUT FIELD PRECISION                       
MS1INDS  DS    XL1                 DEMO INDICATORS                              
MS1INDS2 DS    XL1                 DEMO INDICATORS2                             
MS1DISP  DS    AL3                 DISPLACEMENT TO FORMULA OR TABLE             
MS1ELCD  DS    XL1                 ELEMENT CODE                                 
MS1FLDN  DS    XL1                 FIELD NUMBER                                 
MS1DTALN EQU   *-MS1DTAD                                                        
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
         PRINT ON                                                               
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
         PRINT ON                                                               
* DDTABSDEMD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDTABSDEMD                                                     
         EJECT                                                                  
         PRINT ON                                                               
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         EJECT                                                                  
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         EJECT                                                                  
         PRINT ON                                                               
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
         PRINT ON                                                               
* FATABSDEQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         EJECT                                                                  
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         EJECT                                                                  
         PRINT ON                                                               
* DDBSPARA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBSPARA                                                       
         EJECT                                                                  
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         EJECT                                                                  
         PRINT ON                                                               
* DDMONYREQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDMONYREQU                                                     
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
***********************************************************************         
* ORG TO NEXT 1024 BYTES SO THAT IF PROGRAM GETS LARGER, IT WILL STILL*         
* FIT IN THE SAME SLOT IN CORE (WITHIN REASON)                        *         
***********************************************************************         
         SPACE 1                                                                
DEMADDR  RSECT                                                                  
         ORG   DEMADDR+(((*-DEMADDR)/1024)+1)*1024                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'112DEDEMADDR 08/29/19'                                      
         END                                                                    
