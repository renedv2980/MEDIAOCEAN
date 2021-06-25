*          DATA SET CTMAD17    AT LEVEL 117 AS OF 05/01/02                      
*          DATA SET CTMAD17C   AT LEVEL 115 AS OF 06/29/93                      
*PHASE TA0C17A                                                                  
*INCLUDE GETBROAD                                                               
*INCLUDE REGETIUN                                                               
         TITLE 'TA0C17 - $MAD REP AVERAGE UNIT RATE RETRIEVAL'                  
**********************************************************************          
*   HISTORY OF CHANGES                                               *          
**********************************************************************          
*   MAY19/93   (BU ) --- ORIGINAL ENTRY                              *          
*                                                                    *          
*   JUN11/93   (BU ) --- UPGRADED VERSION FOR AUR USE.               *          
*                                                                    *          
*   JUN22/93   (BU ) --- ADD CROSS-COMPANY LOOKUP                    *          
*                                                                    *          
*   JUN28/93   (BU ) --- SET AUR USER CROSS-COMPANY                  *          
*                                                                    *          
*   JUN29/93   (BU ) --- MAKE TRULY RE-ENTRANT                       *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                        **  END TOMBSTONE  **                       *          
**********************************************************************          
TA0C17   CSECT                                                                  
         PRINT GEN                                                              
         NMOD1 1750,TA0C17,RA,RR=R2                                             
         PRINT NOGEN                                                            
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
*                                                                               
         ST    R2,RELO             SAVE RELOCATION ADDRESS                      
*                                                                               
*                                                                               
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
*     ORIGINAL ADDRESS OF NMOD'ED SPACE                                         
*                                                                               
         LR    RF,RC               SAVE ADDRESS FOR TABLE                       
*                                                                               
*        INITIALIZE OVERLAY WIDE REGISTERS                                      
*                                                                               
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
*                                                                               
         DROP  R7                  DROP 1ST APPLIC COMMON STORAGE               
*                                                                               
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RF,AWORKTAB         SET ADDRESS OF TABLE SPACE                   
         ST    RF,NXTWKTAB         SET ADDRESS OF NEXT TABLE SPACE              
         MVC   COTABLE,COTABLE2    SET UP COMPANY TABLE                         
         MVI   DTFILLER,1          INITIALIZE DATE CONVERT VALUE                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         EJECT                                                                  
*                                                                               
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     XIT                                                              
         EJECT                                                                  
*                                                                               
* 'INIT' INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                        
* INITIALIZATION.                                                               
*                                                                               
INIT     NTR1                                                                   
*                                                                               
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
* 'PROCSTRT' PROCESSES THE START MODE.  IT PROCESSES THE REQUEST FOR            
* AVERAGE UNIT RATE INFO FROM THE PC AND RETURNS THE FIRST SCREENFUL            
* OF INFORMATION.  IT ALSO SETS THE INTERNAL CONTROL TO BEGIN THE               
* SEND OF SUBSEQUENT DATA.                                                      
*                                                                               
PROCSTRT NTR1                                                                   
         GOTO1 SETSYS,DMCB,(3,=C'REP'),=CL8'REPDIR',=CL8'REPFILE'               
         BAS   RE,GETAURFL         SET AURUSER INDICATOR BIT                    
*****>   BAS   RE,LOADTEMP         LOAD TEMPORARY DATA                          
         BAS   RE,PROCINIT         INITIALIZE WORK SCRATCH AREA                 
         BAS   RE,PROCTEMP         RETURN TEMP AREA TO WORKSPACE                
         BAS   RE,PROCDATA         PRODUCE RETURN DATA, STORE IN                
*                                     TEMP FILE                                 
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* 'PROCMID' PROCESSES MIDDLE MODE.  IT PROCESSES AND SENDS THE 2ND              
*  THRU NTH SCREENS OF DATA.                                                    
*                                                                               
PROCMID  NTR1                                                                   
*                                                                               
         GOTO1 SETSYS,DMCB,(3,=C'REP'),=CL8'REPDIR',=CL8'REPFILE'               
*                                                                               
PMX      B     XIT                                                              
         EJECT                                                                  
* 'PROCEND' PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.                 
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  GETAURFL:  ACCESSES THE PROFILES.  THE AUR USER PROFILE IS THE               
*        THIRD BIT ON THE SFM PROFILE.  SFM IS USED BECAUSE THIS IS             
*        WHERE THE MAJOR REPORTING OF AUR IS DONE.                              
*                                                                               
GETAURFL NTR1                                                                   
         MVC   COTABLE(2),SIGNON2C INSERT SIGNON REP INTO TABLE                 
         XC    USRTABLE,USRTABLE   CLEAR THE AUR USER TABLE                     
         LA    R4,COTABLE          A(COMPANY TABLE)                             
GAUR0020 EQU   *                                                                
         CLI   0(R4),0             ANY ENTRY IN COMPANY TABLE?                  
         BE    GAUR0140            NO  - FINISHED                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),0(R4)     INSERT CODE FROM COMPANY TABLE               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEY         KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO REP RECORD????                            
         GOTO1 GETREC                                                           
         L     RE,AIO              SET A(REP RECORD)                            
         USING RREPRECD,RE                                                      
         ZICM  RF,RREPLEN,2        GET LENGTH                                   
         AR    RF,RE               FORCE ZERO AT END OF RECORD                  
         MVI   0(RF),0                                                          
*                                                                               
         DROP  RE                                                               
*                                                                               
         LA    RE,34(RE)           A(1ST ELEMENT)                               
         MVI   0+GRTABSIZ(R4),C'N' SET AURUSER TO 'NO'                          
GAUR0040 EQU   *                                                                
         CLI   0(RE),0             END OF RECORD?                               
         BE    GAUR0120            YES - FINISHED                               
         CLI   0(RE),X'04'         PROGRAM PROFILE ELEMENT?                     
         BE    GAUR0060            YES                                          
         ZIC   RF,1(RE)            NO  - GET NEXT ELEMENT                       
         AR    RE,RF                                                            
         B     GAUR0040                                                         
GAUR0060 EQU   *                                                                
         USING RREPPGMP,RE                                                      
         ZIC   R0,RREPPGM#         NUMBER OF PROFILE UNITS                      
         LA    RE,RREPPGM1                                                      
         DROP  RE                                                               
GAUR0080 EQU   *                                                                
         CLI   0(RE),RREPQSFM      SFM PROFILE?                                 
         BE    GAUR0100            YES                                          
         LA    RE,RREPPGML(RE)     NO  - BUMP TO NEXT ELEMENT                   
         BCT   R0,GAUR0080         GO BACK FOR NEXT                             
         B     GAUR0120            NOT FOUND - EXIT                             
GAUR0100 EQU   *                                                                
         LA    RE,2(RE)            CHECK PROFILE:  SKIP CONTROL                 
         TM    0(RE),X'20'         IS 3RD BIT SET?                              
         BNO   GAUR0120            NO  -                                        
         MVI   0+GRTABSIZ(R4),C'Y' YES                                          
GAUR0120 EQU   *                                                                
         LA    R4,2(R4)            BUMP TO NEXT COMPANY ENTRY                   
         CLC   SIGNON2C,0(R4)      COMPANY CODE = SIGNON CODE?                  
         BE    GAUR0120            YES - SKIP IT                                
         B     GAUR0020            NO  - GO BACK FOR NEXT                       
GAUR0140 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    INITIALIZES ALL VALUES IN WORK SCRATCH SPACE ON THE                        
*      START-UP PASS                                                            
*                                                                               
PROCINIT NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
*                                                                               
*   GET DATVAL ADDRESS FROM COMFACS LIST                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   DATVAL,CDATVAL                                                   
*                                                                               
         DROP  RF                                                               
*                                                                               
*   DETERMINE MAXIMUM ACCEPTABLE IO'S ALLOWED FOR THIS JOB:                     
*     SET MAXIOCTR TO 90% OF MAX IO'S SHOWN IN GETFACT                          
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         MVC   MAXIOCTR,FATMAXIO-FACTSD(R1)                                     
         SR    R2,R2                                                            
         LH    R3,MAXIOCTR                                                      
         LA    R4,9                MULTIPLE MAXIMUM IO BY 9                     
         MR    R2,R4                                                            
         LA    R4,10               DIVIDE MAXIMUM BY 10                         
         DR    R2,R4               TO PRODUCE 90%                               
         STH   R3,MAXIOCTR         SAVE 90% OF MAX COUNT                        
*                                                                               
         XC    AUROPT,AUROPT       SET AUR $$$ OPTION TO ZERO                   
*                                     FOR NOW.....                              
PIN0099  B     XIT                                                              
         EJECT                                                                  
*                                                                               
* THIS ROUTINE:                                                                 
*      OPENS THE TEMP FILE                                                      
*      RETURNS ALL TEMP FILE ELEMENTS                                           
*      TRANSLATES TEMP FILE ELEMENTS TO STORABLE FORMATS                        
*      MOVES TRANSLATED ELEMENTS TO NMOD STORAGE                                
*                                                                               
PROCTEMP NTR1                                                                   
         GOTO1 TMPOPEN,DMCB,=C'GET',L'TMPAREA                                   
         BNE   EXIT                ERROR EXIT                                   
PROT0010 EQU   *                   GET NEXT ITEM FROM TEMP FILE                 
         GOTO1 GETTMP,DMCB,TMPAREA                                              
         BNE   EXIT                ERROR - UNWIND TRANSACTION                   
         CLI   EOTFLAG,C'Y'        END OF TEMP FILE?                            
         BNE   PROT0020            NO  - CONTINUE PROCESSING                    
         BAS   RE,SETEOF           YES - SET EOF INDICATOR                      
         GOTO1 TMPCLOSE            CLOSE TEMP FILE                              
         BNE   EXIT                ERROR - UNWIND TRANSACTION                   
         B     XIT                                                              
*                                                                               
PROT0020 EQU   *                                                                
         MVC   TMPLEN,7(R1)        SAVE LENGTH OF ITEM                          
*                                     ONE CHARACTER ONLY                        
*                                                                               
         ICM   R1,15,TMPAREA       DETERMINE TYPE OF INPUT                      
         LA    R2,ITAMAVSC                                                      
         CR    R1,R2               BASIC SCHEDULE INFORMATION?                  
         BNE   PROT0030            NO                                           
         BAS   RE,BASHDINF         YES                                          
         B     PROT0010                                                         
PROT0030 EQU   *                                                                
         LA    R2,ITAMAVLN                                                      
         CR    R1,R2               BASIC LINE     INFORMATION?                  
         BNE   PROT0040            NO                                           
         BAS   RE,BALININF         YES                                          
         B     PROT0010                                                         
PROT0040 EQU   *                                                                
         LA    R2,ITEOD                                                         
         CR    R1,R2               END OF DATA?                                 
         BE    PROT0060                                                         
PROT0050 EQU   *                                                                
*   NO PROCESSING FOR AN UNRECOGNIZED TYPE                                      
         B     PROT0010                                                         
PROT0060 EQU   *                                                                
         B     PROT0010            EVEN THOUGH FINISHED, LET                    
*                                     'GETTMP' FIND END OF FILE                 
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*     TRANSLATES THE DATES OF THE SCHEDULE TO A USABLE FORMAT                   
*                                                                               
BASHDINF NTR1                                                                   
         LA    R2,TMPAREA          A(ITEM)                                      
         USING CT17IN01,R2                                                      
*                                                                               
         CLC   BSFLSTRT(16),MYSPACES                                            
*                                  ANY DATES ENTERED?                           
         BNE   BH0008              YES                                          
         B     EXIT                NO  - EXIT PROGRAM                           
*                                  NO DATES = NO DATA                           
*                                                                               
BH0008   EQU   *                                                                
         PRINT GEN                                                              
         GOTO1 DATVAL,DMCB,BSFLSTRT,WORKAREA                                    
         GOTO1 DATVAL,DMCB,BSFLEND,WORKAREA+6                                   
*                                                                               
*  DATES HAVE BEEN VALIDATED AT PC.  NO ERROR CHECKING IS DONE HERE.            
*                                                                               
         GOTO1 DATCON,DMCB,WORKAREA,(3,TWSTDT)                                  
         GOTO1 DATCON,DMCB,WORKAREA+6,(3,TWENDT)                                
         PRINT NOGEN                                                            
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    MOVES A LINE INFORMATION ITEM TO NMOD AREA.                                
*                                                                               
BALININF NTR1                                                                   
*                                                                               
         LA    R2,TMPAREA          A(ITEM)                                      
*                                                                               
         L     RF,NXTWKTAB         LOAD A(NEXT WK TABLE ENTRY)                  
         ZIC   RE,TMPLEN           LOAD L(THIS ENTRY)                           
         BCTR  RE,0                DECREMENT LENGTH BY 1 FOR MOVE               
         MVC   0(1,RF),TMPLEN      INSERT LENGTH OF ITEM                        
         EX    RE,BALI0090         MOVE ELEMENT TO NMOD AREA                    
         LA    RF,1+1(RE,RF)       SET A(NEXT ENTRY:                            
*                                     1 FOR L(ENTRY LENGTH) +                   
*                                     1 FOR DECR FOR EX STATEMENT +             
*                                     RE FOR L(ENTRY) +                         
*                                     RF FOR A(CURRENT ENTRY)                   
         ST    RF,NXTWKTAB         STORE A(NEXT WK TABLE ENTRY)                 
         B     XIT                                                              
*                                                                               
BALI0090 MVC   1(0,RF),0(R2)       MOVE BY LENGTH                               
*                                                                               
*                                                                               
         EJECT                                                                  
* THIS ROUTINE:                                                                 
*    INSERTS AN EOF INDICATOR INTO THE MOD AREA.                                
*                                                                               
SETEOF   NTR1                                                                   
*                                                                               
         L     RF,NXTWKTAB         LOAD A(NEXT WK TABLE ENTRY)                  
         MVI   0(RF),4             INSERT LENGTH OF ITEM: 4 CHARS               
         MVC   1(4,RF),=X'FFFFFFFF'                                             
*                                  MOVE EOF ELEMENT TO MOD AREA                 
         LA    RF,1+1(RE,RF)       SET A(NEXT ENTRY:                            
*                                     1 FOR L(ENTRY LENGTH) +                   
*                                     1 FOR DECR FOR EX STATEMENT +             
*                                     RE FOR L(ENTRY) +                         
*                                     RF FOR A(CURRENT ENTRY)                   
         ST    RF,NXTWKTAB         STORE A(NEXT WK TABLE ENTRY)                 
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* THIS ROUTINE:                                                                 
*                                                                               
PROCDATA NTR1                                                                   
         GOTO1 TMPOPEN,DMCB,=C'PUT'                                             
*                                  OPEN TEMP FILE FOR OUTPUT                    
         BNE   EXIT                ERROR EXIT                                   
PDAT0020 EQU   *                                                                
         MVC   NXTWKTAB,AWORKTAB   A(1ST TABLE ENTRY)                           
PDAT0040 EQU   *                                                                
         L     R2,NXTWKTAB         SET A(TABLE ENTRY IN PROGRESS)               
         LA    R2,1(R2)            SKIP PAST LENGTH ATTRIBUTE                   
         CLC   =X'FFFFFFFF',0(R2)  END OF FILE?                                 
         BE    PDAT0320            YES - WRAP IT UP                             
*                                                                               
         USING CT17IN02,R2                                                      
*                                                                               
         LA    R5,BLSTATN          A(1ST/ONLY STATION)                          
         NI    BLSTACTR,X'0F'      DROP ZONE BITS                               
         ZIC   R4,BLSTACTR         LOOP: INSERT # STATIONS                      
         MVI   COMBOFLG,C'N'       SET COMBO FLAG TO 'NO'                       
         CLI   BLSTACTR,1          SINGLE OR COMBO?                             
         BE    PDAT0060            SINGLE                                       
         MVI   COMBOFLG,C'Y'       SET COMBO FLAG TO 'YES'                      
         ZIC   R6,BLSTACTR         PASS # STATIONS THROUGH                      
         GOTO1 CHEKCMBO,DMCB,(R5),(R6)                                          
*                                  CHECK OUT COMBO STATIONS                     
         BZ    PDAT0300            ERROR - SKIP ITEM                            
PDAT0060 EQU   *                                                                
         CLC   LASTSTAT,0(R5)      SAME STATION?                                
         BE    PDAT0100            YES                                          
         MVC   LASTSTAT,0(R5)      NO  - SAVE STATION                           
         BAS   RE,GETSTAT          GET GROUP/SUBGRP                             
         BZ    PDAT0100            STATION NOT FOUND ANYWHERE                   
         GOTO1 FORMDUMY,DMCB,(R2)  NOT FOUND - PUT OUT DUMMY                    
         B     PDAT0300            SKIP THIS ELEMENT NOW                        
PDAT0100 EQU   *                                                                
         LA    R1,COTABLE          A(COMPANY TABLE)                             
PDAT0104 EQU   *                                                                
         CLI   0(R1),0             END OF TABLE?                                
         BE    PDAT0210            YES - NOTHING FOUND                          
         CLI   0+COTABSIZ(R1),0    ANY GROUP/SUBGROUP?                          
         BNZ   PDAT0106            YES                                          
         LA    R1,2(R1)            NO  - BUMP TO NEXT ENTRY IN TABLE            
         B     PDAT0104                                                         
PDAT0106 EQU   *                                                                
         MVI   AURUSER,C'Y'        SET AUR USER TO 'YES'                        
         CLI   0+GRTABSIZ(R1),C'Y' AUR USER?                                    
         BE    PDAT0140            YES                                          
         MVI   AURUSER,C'N'        SET AUR USER TO 'NO'                         
         XC    KEY,KEY             NO  - SET UP ATHENA KEY                      
         LA    R3,KEY                                                           
         USING RATNRECD,R3                                                      
         MVI   RATNKTYP,X'27'                                                   
         MVC   RATNKREP,0(R1)      INSERT REP FROM TABLE                        
         MVC   RATNKGRP,0+COTABSIZ(R1)                                          
*                                  INSERT CORRESPONDING GROUP/SUBGROUP          
         MVC   RATNKSTA,0(R5)      INSERT STATION                               
         MVI   RATNKTPE,1          INSERT 'STATION TOTALS' INDICATOR            
         MVC   RATNKDPT(2),BLDYPART                                             
*                                  INSERT DAYPART                               
         CLI   BLDYPART+1,C' '     2ND CHAR OF DAYPART = SPACE?                 
         BNE   PDAT0120            NO                                           
         MVI   RATNKDPT+1,X'00'    YES - SET TO BINARY ZERO                     
PDAT0120 EQU   *                                                                
         PACK  DUB(8),BLSPTLEN(3)  PACK SPOT LENGTH                             
         CVB   RF,DUB              CONVERT TO BINARY                            
         STH   RF,DUB                                                           
         MVC   RATNKSLN,DUB        INSERT SPOT LENGTH                           
         MVC   RATNKYM,TWSTDT      INSERT FLIGHT START DATE                     
         B     PDAT0180                                                         
*                                                                               
         DROP R3                                                                
*                                                                               
PDAT0140 EQU   *                                                                
         XC    KEY,KEY             NO  - SET UP A.U.R. KEY                      
         LA    R3,KEY                                                           
         USING RAURRECD,R3                                                      
         MVI   RAURKTYP,X'2C'                                                   
         MVC   RAURKREP,0(R1)      INSERT REP FROM TABLE                        
         MVC   RAURKGRP,0+COTABSIZ(R1)                                          
*                                  INSERT CORRESPONDING GROUP/SUBGROUP          
         MVC   RAURKSTA,0(R5)      INSERT STATION                               
         MVI   RAURKTPE,1          INSERT 'STATION TOTALS' INDICATOR            
         MVC   RAURKDPT(2),BLDYPART                                             
*                                  INSERT DAYPART                               
         CLI   BLDYPART+1,C' '     2ND CHAR OF DAYPART = SPACE?                 
         BNE   PDAT0160            NO                                           
         MVI   RAURKDPT+1,X'00'    YES - SET TO BINARY ZERO                     
PDAT0160 EQU   *                                                                
         PACK  DUB(8),BLSPTLEN(3)  PACK SPOT LENGTH                             
         CVB   RF,DUB              CONVERT TO BINARY                            
         STH   RF,DUB                                                           
         MVC   RAURKSLN,DUB        INSERT SPOT LENGTH                           
         MVC   RAURKYM,TWSTDT      INSERT FLIGHT START DATE                     
*                                                                               
         DROP  R3                                                               
*                                                                               
PDAT0180 EQU   *                                                                
         GOTO1 HIGH                READ FIRST KEY                               
*                                                                               
*   IF FIRST RECORD DOES NOT PRODUCE A HIT, A ZERO-VALUE RECORD                 
*      MUST BE RETURNED TO THE PC.  THEREFORE, THESE TESTS ARE                  
*      DUPLICATED TO ISOLATE THIS UNIQUE SITUATION.                             
*                                                                               
         CLC   KEY(25),KEYSAVE     CHECK KEY THRU SPOT LENGTH                   
         BNE   PDAT0200            NOT FOUND -                                  
         CLC   KEY+25(2),TWENDT    DATE WITHIN FLIGHT?                          
*                                     NO NEED TO CHECK START DATE -             
*                                       FIRST READ DID THAT                     
         BNH   PDAT0260            YES - CONTINUE TO PROCESS ITEM               
PDAT0200 EQU   *                                                                
         LA    R1,2(R1)            BUMP TO NEXT COMPANY TABLE ENTRY             
         CLC   SIGNON2C,0(R1)      SIGNON = TABLE ENTRY?                        
         BE    PDAT0200            YES                                          
         CLI   0(R1),0             END OF TABLE?                                
         BNE   PDAT0104            NO  - GO BACK AND CHECK NEXT ITEM            
PDAT0210 EQU   *                                                                
         GOTO1 FORMDUMY,DMCB,(R2)                                               
         B     PDAT0280                                                         
*                                                                               
PDAT0220 EQU   *                                                                
         GOTO1 SEQ                                                              
PDAT0240 EQU   *                                                                
         CLC   KEY(25),KEYSAVE     CHECK KEY THRU SPOT LENGTH                   
         BNE   PDAT0280            NOT FOUND -                                  
         CLC   KEY+25(2),TWENDT    DATE WITHIN FLIGHT?                          
*                                     NO NEED TO CHECK START DATE -             
*                                       FIRST READ DID THAT                     
         BH    PDAT0280            NO  - FINISHED WITH THIS ITEM                
PDAT0260 EQU   *                                                                
         GOTO1 GETREC              RETRIEVE AUR RECORD                          
         GOTO1 FORMRETN,DMCB,(R2)                                               
*                                  YES - FORMAT RETURN ELEMENT                  
         B     PDAT0220            GO BACK FOR NEXT RECORD                      
*                                     WITHIN FLIGHT                             
PDAT0280 EQU   *                                                                
         LA    R5,5(R5)            BUMP TO NEXT STATION                         
*                                     MAY ONLY BE A SINGLE STATION              
         BCT   R4,PDAT0060         GO BACK FOR NEXT, IF NEEDED                  
PDAT0300 EQU   *                                                                
         L     R2,NXTWKTAB         RESET A(TABLE ENTRY)                         
         ZIC   RF,0(R2)            GET L(ENTRY)                                 
         LA    R2,1(RF,R2)         BUMP TO NEXT ENTRY =                         
*                                     CURRENT LOCATION +                        
*                                     1 FOR LENGTH ATTRIBUTE +                  
*                                     LENGTH OF DATA                            
         ST    R2,NXTWKTAB         SAVE A(NEXT ENTRY)                           
         B     PDAT0040            GO BACK FOR NEXT                             
PDAT0320 EQU   *                                                                
*                                                                               
         XC    ELTAREA,ELTAREA     CLEAR ELEMENT BUILD AREA                     
         LA    RF,ITEOD                                                         
         ST    RF,ELTAREA          INSERT END OF DATA TYPE                      
         GOTO1 PUTTMP,DMCB,ELTAREA,4                                            
*                                  WRITE END OF DATA ELEMENT                    
         GOTO1 TMPCLOSE            CLOSE TEMP FILE                              
         BNE   EXIT                                                             
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
         B     XIT                 FINAL ROUTINE EXIT                           
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*  CHEKCMBO: 1. READS STATION RECORD FOR FIRST RECORD IN SET                    
*            2. SAVES GROUP/SUBGROUP AND SETS LASTSTAT                          
*            3. FINDS PARENT STATION.  IF NONE, ERROR EXISTS                    
*            4. READS PARENT STATION RECORD.  VALIDATES THAT                    
*               OTHER STATIONS PARTICIPATE IN COMBO.  IF ANY                    
*               DON'T, ERROR EXISTS.                                            
*            RETURN OF ZERO INDICATES ERROR.                                    
*                                                                               
CHEKCMBO NTR1                                                                   
         LA    R3,COTABLE          A(COMPANY TABLE)                             
CHCM0000 EQU   *                                                                
         CLI   0(R3),0             END OF TABLE?                                
         BE    CHCM0090            YES - EXIT NO GOOD                           
         L     R5,0(R1)            RESET A(1ST STATION IN LIST)                 
         L     R7,4(R1)            SET # STATIONS IN LIST                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           INSERT TYPE                                  
         MVC   KEY+20(2),0(R3)     INSERT REP FROM TABLE                        
         MVC   KEY+22(5),0(R5)     INSERT STATION CALLS                         
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CHCM0075            NOT FOUND - CHECK NEXT TABLE ENTRY           
         GOTO1 GETREC              RETRIEVE RECORD                              
         L     R6,AIO                                                           
         USING RSTARECD,R6                                                      
         LA    R1,RSTAELEM         A(DESCRIPT ELEMENT)                          
CHCM0010 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    CHCM0090            YES - ERROR                                  
         CLI   0(R1),X'0A'         COMBO STATION ELEMENT?                       
         BE    CHCM0020            YES - PROCESS IT                             
         ZIC   RF,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,RF                                                            
         B     CHCM0010            GO BACK FOR NEXT                             
CHCM0020 EQU   *                                                                
*                                  ELEMENT FOUND MUST BE PARENT                 
         MVC   KEY+22(5),2(R1)     INSERT PARENT STATION INTO KEY               
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CHCM0075            NOT FOUND - CHECK NEXT TABLE ENTRY           
         GOTO1 GETREC              RETRIEVE RECORD                              
         XC    CMBOTABL,CMBOTABL   CLEAR COMBO STATIONS TABLE                   
         LA    R2,CMBOTABL                                                      
         LA    R1,RSTAELEM         A(DESCRIPT ELEMENT)                          
*                                                                               
         DROP  R6                                                               
*                                                                               
CHCM0030 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    CHCM0050            YES - ERROR                                  
         CLI   0(R1),X'0A'         COMBO STATION ELEMENT?                       
         BNE   CHCM0040            NO  - BUMP TO NEXT ELEMENT                   
         MVC   0(5,R2),2(R1)       SAVE STATION CALLS IN TABLE                  
         LA    R2,5(R2)            NEXT TABLE ENTRY                             
CHCM0040 EQU   *                                                                
         ZIC   RF,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,RF                                                            
         B     CHCM0030            GO BACK FOR NEXT                             
CHCM0050 EQU   *                                                                
         OC    CMBOTABL,CMBOTABL   ANY ENTRIES?                                 
         BZ    CHCM0075            NO  - CHECK NEXT TABLE ENTRY                 
         LR    RF,R7               LOOP CONTROL: # STATIONS IN LIST             
CHCM0060 EQU   *                                                                
         LA    R2,CMBOTABL         A(STATIONS IN TABLE)                         
         LA    R1,4                MAX STATIONS IN TABLE                        
CHCM0070 EQU   *                                                                
         CLC   0(5,R5),0(R2)       STATION IN TABLE?                            
         BE    CHCM0080            YES                                          
         LA    R2,5(R2)            NO  - BUMP TO NEXT TABLE ENTRY               
         BCT   R1,CHCM0070         GO BACK FOR NEXT                             
*                                  NOT FOUND IN TABLE - ERROR                   
CHCM0075 EQU   *                                                                
         LA    R3,2(R3)            BUMP TO NEXT TABLE ENTRY                     
         CLC   SIGNON2C,0(R3)      SIGNON = TABLE ENTRY?                        
         BE    CHCM0075            YES - SKIP IT                                
         B     CHCM0000            GO BACK AND CHECK NEXT ENTRY                 
CHCM0080 EQU   *                                                                
         LA    R5,5(R5)            FOUND - BUMP TO NEXT STATION                 
         BCT   RF,CHCM0060         CHECK IT AGAINST TABLE                       
         LTR   RB,RB               FINISHED - EXIT OKAY                         
         B     CHCM0100                                                         
CHCM0090 EQU   *                                                                
         SR    R0,R0               ERROR EXIT                                   
CHCM0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   GETSTAT:  RETRIEVE STATION RECORD, SAVE GROUP/SUBGROUP                      
*                                                                               
GETSTAT  NTR1                                                                   
         XC    GRPTABLE,GRPTABLE   CLEAR GROUP/SUBGROUP TABLE                   
         LA    R1,COTABLE          A(COMPANY TABLE)                             
         LA    R2,GRPTABLE                                                      
         XC    SAVEGRP,SAVEGRP     CLEAR GROUP/SUBGROUP                         
GSTA00020 EQU  *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           INSERT TYPE                                  
         MVC   KEY+20(2),0(R1)     INSERT REP                                   
         MVC   KEY+22(5),0(R5)     INSERT STATION CALLS                         
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GSTA00040           NOT FOUND - ERROR                            
         GOTO1 GETREC              RETRIEVE RECORD                              
         L     R6,AIO                                                           
         USING RSTARECD,R6                                                      
         MVC   0(2,R2),RSTAGRUP    SAVE OFF GROUP/SUBGROUP                      
GSTA00040 EQU  *                                                                
         LA    R1,2(R1)            BUMP TABLE ENTRY                             
         LA    R2,2(R2)                                                         
         CLC   SIGNON2C,0(R1)      SIGNON = TABLE ENTRY?                        
         BE    GSTA00040           YES - SKIP IT                                
         CLI   0(R1),0             END OF TABLE?                                
         BNE   GSTA00020           NO                                           
         OC    GRPTABLE,GRPTABLE   ANY ENTRIES?                                 
         BZ    GSTA00080           NO  - STATION NOT FOUND ANYWHERE             
GSTA00060 EQU  *                                                                
         SR    R0,R0               SET CC = ZERO:  GOOD RETURN                  
         B     GSTA00100                                                        
GSTA00080 EQU  *                                                                
         LTR   RB,RB               SET CC NOTE = ZERO:  BAD RETURN              
GSTA00100 EQU  *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*   FORMRETN:  1.  BUILDS ELEMENT TO SEND BACK TO PC                            
*              2.  ANALYZES AUR RECORD, AND ACCUMULATES VALUES                  
*              3.  INSERTS RECORD INTO TEMP AREA                                
*                                                                               
FORMRETN NTR1                                                                   
         L     R2,0(R1)            RESET A(ITEM IN PROGRESS)                    
         USING CT17IN02,R2                                                      
         XC    ELTAREA,ELTAREA     CLEAR ELEMENT BUILD AREA                     
         LA    R3,ELTAREA          SET A(ELEMENT BUILD AREA)                    
         USING CT17OU01,R3                                                      
         MVC   AVAMFMKY,BLAMFMKY   RETURN KEY SENT UP                           
         MVC   AVSPTLEN,BLSPTLEN   INSERT SPOT LENGTH                           
         MVI   AVCOMBO,C'N'        SET TO 'SINGLE STATION'                      
         CLI   BLSTACTR,1          SINGLE STATION?                              
         BE    FORM0010            YES                                          
         MVI   AVCOMBO,C'Y'        SET TO 'COMBO STATIONS'                      
FORM0010 EQU   *                                                                
         MVC   AVDYPART,BLDYPART   INSERT DAYPART                               
         MVC   AVSTATN,LASTSTAT    INSERT STATION                               
         CLI   AURUSER,C'Y'        AUR USER?                                    
         BE    FORM0040            YES                                          
         L     R4,AIO              NO  - PROCESS ATHENA DATA                    
         USING RATNRECD,R4                                                      
         MVC   DATEFLD,RATNKYM     INTERPRET DATE                               
         GOTO1 DATCON,DMCB,(3,DATEFLD),(0,WORKAREA)                             
         MVC   AVDATE,WORKAREA     INSERT YYMM OF DATE                          
         LA    R5,RATNELEM         A(DESCRIPT ELEMENT)                          
*                                                                               
         DROP  R4                                                               
*                                                                               
         XC    TOTSPOTS(16),TOTSPOTS                                            
*                                  CLEAR ACCUMULATORS                           
FORM0020 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    FORM0110            YES                                          
         CLI   0(R5),2             SPOT/COST ELEMENT?                           
         BNE   FORM0030            NO  - GO TO NEXT ELEMENT                     
*                                  YES - ACCUMULATE VALUE FOR RECORD            
         USING RATNSCDT,R5                                                      
         XC    DUB,DUB             CLEAR ACCUMULATORS                           
         MVC   DUB+2(2),RATNSCSP   LOAD TOTAL SPOTS                             
         MVC   DUB+4(4),RATNSCCS   LOAD TOTAL COST                              
*                                                                               
         DROP  R5                                                               
*                                                                               
         L     RF,DUB              ACCUMULATE TOTAL SPOTS                       
         L     RE,TOTSPOTS                                                      
         AR    RE,RF                                                            
         ST    RE,TOTSPOTS         SAVE NEW TOTAL SPOTS                         
         L     RF,DUB+4            ACCUMULATE TOTAL COST                        
         L     RE,TOTCOST                                                       
         AR    RE,RF                                                            
         ST    RE,TOTCOST          SAVE NEW TOTAL COST                          
FORM0030 EQU   *                                                                
         ZIC   RF,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
         B     FORM0020            GO BACK FOR NEXT                             
FORM0040 EQU   *                                                                
         L     R4,AIO              PROCESS A.U.R. DATA                          
         USING RAURRECD,R4                                                      
         MVC   DATEFLD,RAURKYM     INTERPRET DATE                               
         GOTO1 DATCON,DMCB,(3,DATEFLD),(0,WORKAREA)                             
         MVC   AVDATE,WORKAREA     INSERT YYMM OF DATE                          
         LA    R5,RAURELEM         A(DESCRIPT ELEMENT)                          
*                                                                               
         DROP  R4                                                               
*                                                                               
         XC    TOTSPOTS(16),TOTSPOTS                                            
*                                  CLEAR ACCUMULATORS                           
FORM0050 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    FORM0110            YES                                          
         CLI   0(R5),2             SPOT/COST ELEMENT?                           
         BNE   FORM0100            NO  - GO TO NEXT ELEMENT                     
*                                  YES - ACCUMULATE VALUE FOR RECORD            
         USING RAURSCDT,R5                                                      
         CLI   COMBOFLG,C' '       COMBO+REGULAR $$$/UNITS?                     
         BNE   FORM0070            NO                                           
*                                                                               
*    ADD COMBO + REGULAR $$$/UNITS FROM ELEMENTS                                
*                                                                               
         TM    RAURSCTL,X'80'      REGULAR $$$ PRESENT?                         
         BNO   FORM0060            NO  - CHECK COMBO ONLY                       
*                                  YES - USE FIRST SET OF BUCKETS               
         XC    DUB,DUB             CLEAR ACCUMULATORS                           
         MVC   DUB+2(2),RAURSCS1   LOAD REGULAR TOTAL SPOTS                     
         MVC   DUB+4(4),RAURSCC1   LOAD REGULAR TOTAL COST                      
*                                                                               
         BAS   RE,ADDUP$$$         ACCUMULATE $$$/UNITS                         
         TM    RAURSCTL,X'40'      COMBO   $$$ PRESENT?                         
         BNO   FORM0100            NO  - FINISHED WITH ELEMENT                  
*                                  YES - USE SECOND SET OF BUCKETS              
         XC    DUB,DUB             CLEAR ACCUMULATORS                           
         MVC   DUB+2(2),RAURSCS2   LOAD REGULAR TOTAL SPOTS                     
         MVC   DUB+4(4),RAURSCC2   LOAD REGULAR TOTAL COST                      
         BAS   RE,ADDUP$$$         ACCUMULATE $$$/UNITS                         
         B     FORM0100            FINISHED WITH ELEMENT                        
*                                                                               
FORM0060 EQU   *                   COMBO + REGULAR $$$/UNITS                    
*                                     NO REGULAR $$$ IN ELEMENT                 
         TM    RAURSCTL,X'40'      COMBO   $$$ PRESENT?                         
         BNO   FORM0100            NO  - FINISHED WITH ELEMENT                  
*                                  YES - USE FIRST  SET OF BUCKETS              
         XC    DUB,DUB             CLEAR ACCUMULATORS                           
         MVC   DUB+2(2),RAURSCS1   LOAD REGULAR TOTAL SPOTS                     
         MVC   DUB+4(4),RAURSCC1   LOAD REGULAR TOTAL COST                      
         BAS   RE,ADDUP$$$         ACCUMULATE $$$/UNITS                         
         B     FORM0100            FINISHED WITH ELEMENT                        
*                                                                               
FORM0070 EQU   *                                                                
         CLI   COMBOFLG,C'Y'       COMBO $$$/UNITS ONLY?                        
         BNE   FORM0090            NO                                           
*                                                                               
*    ADD COMBO $$$/UNITS ONLY FROM ELEMENTS                                     
*                                                                               
         TM    RAURSCTL,X'80'      REGULAR $$$ PRESENT?                         
         BNO   FORM0080            NO  - CHECK COMBO ONLY                       
*                                  YES - CHECK COMBO PRESENCE ANYWAY.           
         TM    RAURSCTL,X'40'      COMBO   $$$ PRESENT?                         
         BNO   FORM0100            NO  - FINISHED WITH ELEMENT                  
*                                  YES - USE SECOND SET OF BUCKETS              
         XC    DUB,DUB             CLEAR ACCUMULATORS                           
         MVC   DUB+2(2),RAURSCS2   LOAD REGULAR TOTAL SPOTS                     
         MVC   DUB+4(4),RAURSCC2   LOAD REGULAR TOTAL COST                      
*                                                                               
         BAS   RE,ADDUP$$$         ACCUMULATE $$$/UNITS                         
         B     FORM0100            FINISHED WITH ELEMENT                        
*                                                                               
FORM0080 EQU   *                                                                
*                                  NO REGULAR $$$ - CHECK COMBO                 
         TM    RAURSCTL,X'40'      COMBO   $$$ PRESENT?                         
         BNO   FORM0100            NO  - FINISHED WITH ELEMENT                  
*                                  YES - USE FIRST  SET OF BUCKETS              
         XC    DUB,DUB             CLEAR ACCUMULATORS                           
         MVC   DUB+2(2),RAURSCS1   LOAD REGULAR TOTAL SPOTS                     
         MVC   DUB+4(4),RAURSCC1   LOAD REGULAR TOTAL COST                      
*                                                                               
         BAS   RE,ADDUP$$$         ACCUMULATE $$$/UNITS                         
         B     FORM0100            FINISHED WITH ELEMENT                        
*                                                                               
FORM0090 EQU   *                                                                
         CLI   COMBOFLG,C'N'       REGULAR $$$/UNITS ONLY?                      
         BE    *+6                 YES                                          
         DC    H'0'                NOT RECOGNIZED....                           
*                                                                               
*    ADD REGULAR $$$/UNITS ONLY FROM ELEMENTS                                   
*                                                                               
         TM    RAURSCTL,X'80'      REGULAR $$$ PRESENT?                         
         BNO   FORM0100            NO  - FINISHED - NO DATA IN ELT              
*                                  YES - USE FIRST SET OF BUCKETS               
         XC    DUB,DUB             CLEAR ACCUMULATORS                           
         MVC   DUB+2(2),RAURSCS1   LOAD REGULAR TOTAL SPOTS                     
         MVC   DUB+4(4),RAURSCC1   LOAD REGULAR TOTAL COST                      
*                                                                               
         BAS   RE,ADDUP$$$         ACCUMULATE $$$/UNITS                         
         B     FORM0100            FINISHED WITH ELEMENT                        
*                                                                               
         DROP  R5                                                               
*                                                                               
FORM0100 EQU   *                                                                
         ZIC   RF,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
         B     FORM0050            GO BACK FOR NEXT                             
FORM0110 EQU   *                                                                
         GOTO1 HEXOUT,DMCB,TOTSPOTS,WORKAREA,4,=C'TOG'                          
         MVC   AVTOTLSP,WORKAREA+4                                              
*                                  INSERT # SPOTS INTO ELEMENT                  
         GOTO1 HEXOUT,DMCB,TOTCOST,WORKAREA,4,=C'TOG'                           
         MVC   AVTOTL$$,WORKAREA+2                                              
*                                  INSERT TOTAL DOLLARS INTO ELEMENT            
         MVC   AVAMFMID+2(2),=X'0BCB'                                           
*                                  INSERT ELEMENT TYPE                          
         PRINT GEN                                                              
         GOTO1 PUTTMP,DMCB,ELTAREA,37                                           
         PRINT NOGEN                                                            
         BNE   EXIT                                                             
         XIT1                                                                   
*                                                                               
         DROP R2,R3                                                             
         EJECT                                                                  
*                                                                               
*   ADDUP$$$:  ACCUMULATE ROUTINE FOR REGULAR/COMBO $$$/UNITS                   
*                                                                               
ADDUP$$$ NTR1                                                                   
         L     RF,DUB              ACCUMULATE TOTAL SPOTS                       
         L     RE,TOTSPOTS                                                      
         AR    RE,RF                                                            
         ST    RE,TOTSPOTS         SAVE NEW TOTAL SPOTS                         
         L     RF,DUB+4            ACCUMULATE TOTAL COST                        
         L     RE,TOTCOST                                                       
         AR    RE,RF                                                            
         ST    RE,TOTCOST          SAVE NEW TOTAL COST                          
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   FORMDUMY:  1.  BUILDS DUMMY ELEMENT TO SEND BACK TO PC                      
*              2.  PLUGS IN ZERO VALUE INTO RECORD                              
*              3.  INSERTS RECORD INTO TEMP AREA                                
*                                                                               
FORMDUMY NTR1                                                                   
         L     R2,0(R1)            RESET A(ITEM IN PROGRESS)                    
         USING CT17IN02,R2                                                      
         XC    ELTAREA,ELTAREA     CLEAR ELEMENT BUILD AREA                     
         LA    R3,ELTAREA          SET A(ELEMENT BUILD AREA)                    
         USING CT17OU01,R3                                                      
         MVC   AVAMFMKY,BLAMFMKY   RETURN KEY SENT UP                           
         MVC   AVSPTLEN,BLSPTLEN   INSERT SPOT LENGTH                           
         MVI   AVCOMBO,C'X'        SET TO 'DUMMY DATA BACK'                     
         CLI   BLSTACTR,1          SINGLE STATION?                              
         BE    FORD0010            YES                                          
FORD0010 EQU   *                                                                
         MVC   AVDYPART,BLDYPART   INSERT DAYPART                               
         MVC   AVSTATN,LASTSTAT    INSERT STATION                               
         MVC   DATEFLD,TWSTDT      INTERPRET FLIGHT START DATE                  
         GOTO1 DATCON,DMCB,(3,DATEFLD),(0,WORKAREA)                             
         MVC   AVDATE,WORKAREA     INSERT YYMM OF DATE                          
*                                                                               
         XC    TOTSPOTS(16),TOTSPOTS                                            
*                                  CLEAR ACCUMULATORS                           
         GOTO1 HEXOUT,DMCB,TOTSPOTS,WORKAREA,4,=C'TOG'                          
         MVC   AVTOTLSP,WORKAREA+4                                              
*                                  INSERT # SPOTS INTO ELEMENT                  
         GOTO1 HEXOUT,DMCB,TOTCOST,WORKAREA,4,=C'TOG'                           
         MVC   AVTOTL$$,WORKAREA+2                                              
*                                  INSERT TOTAL DOLLARS INTO ELEMENT            
         MVC   AVAMFMID+2(2),=X'0BCB'                                           
*                                  INSERT ELEMENT TYPE                          
         PRINT GEN                                                              
         GOTO1 PUTTMP,DMCB,ELTAREA,37                                           
         PRINT NOGEN                                                            
         BNE   EXIT                                                             
         XIT1                                                                   
*                                                                               
         DROP R2,R3                                                             
         EJECT                                                                  
*                                                                               
*   LOADTEMP:  STUFFS 'UPLOAD DATA' INTO TEMP FILE.  TO BE REMOVED              
*       FOR LIVE TESTING.                                                       
*                                                                               
LOADTEMP NTR1                                                                   
         GOTO1 TMPOPEN,DMCB,=C'PUT'                                             
         BNE   EXIT                                                             
         LA    R2,TEMPDATA                                                      
LTEM0010 EQU   *                                                                
         CLI   0(R2),X'FF'         END OF DATA?                                 
         BE    LTEM0030            YES                                          
         ZIC   R3,0(R2)            DATA LENGTH                                  
         BCTR  R3,0                   MINUS LENGTH ATTRIBUTE                    
         BCTR  R3,0                   MINUS 1 FOR EX STATEMENT                  
         XC    ELTAREA,ELTAREA                                                  
         EX    R3,LTEMEX                                                        
         B     LTEM0020                                                         
LTEMEX   MVC   ELTAREA(0),1(R2)                                                 
LTEM0020 EQU   *                                                                
         LA    R3,1(R3)            RESET TO DATA LENGTH                         
         GOTO1 PUTTMP,DMCB,ELTAREA,(R3)                                         
         BNE   EXIT                                                             
*                                  SEND DATA TO TEMP FILE                       
         LA    R2,1(R3,R2)         BUMP TO NEXT ENTRY                           
         B     LTEM0010            GO BACK FOR NEXT                             
LTEM0030 EQU   *                                                                
         GOTO1 TMPCLOSE            CLOSE TEMP FILE                              
         BNE   EXIT                                                             
         XIT1                                                                   
*                                                                               
*                                                                               
TEMPDATA EQU   *                                                                
         DC    XL1'18'    DECIMAL 24: DATA LEN + LEN ATTRIBUTE                  
         DC    XL2'0000'                                                        
         DC    XL2'0BC9'                                                        
         DC    CL19'01/01/9312/31/93R  '                                        
         DC    XL1'18'                                                          
         DC    XL2'0000'                                                        
         DC    XL2'0BCA'                                                        
         DC    CL19'00000001060CA1KIISF'                                        
         DC    XL1'18'                                                          
         DC    XL2'0000'                                                        
         DC    XL2'0BCA'                                                        
         DC    CL19'00000002060CA1WNEWF'                                        
*        DC    XL1'18'                                                          
*        DC    XL2'0000'                                                        
*        DC    XL2'0BCA'                                                        
*        DC    CL19'12345678060CB1METLA'                                        
*        DC    XL1'18'                                                          
*        DC    XL2'0000'                                                        
*        DC    XL2'0BCA'                                                        
*        DC    CL19'12345678060CA1METLA'                                        
*        DC    XL1'18'                                                          
*        DC    XL2'0000'                                                        
*        DC    XL2'0BCA'                                                        
*        DC    CL19'12345678030D 1WFAXF'                                        
*        DC    XL1'18'                                                          
*        DC    XL2'0000'                                                        
*        DC    XL2'0BCA'                                                        
*        DC    CL19'00000001060LD1WMXVF'                                        
*        DC    XL1'18'                                                          
*        DC    XL2'0000'                                                        
*        DC    XL2'0BCA'                                                        
*        DC    CL19'00000001060CB1METLF'                                        
*        DC    XL1'1D'                                                          
*        DC    XL2'0000'                                                        
*        DC    XL2'0BCA'                                                        
*        DC    CL24'00000002060CA2METLFMETLF'                                   
*        DC    XL1'22'                                                          
*        DC    XL2'0000'                                                        
*        DC    XL2'0BCA'                                                        
*        DC    CL29'00000333060CA3KATZAKATFAKHOWA'                              
*        DC    XL1'1D'                                                          
*        DC    XL2'0000'                                                        
*        DC    XL2'0BCA'                                                        
*        DC    CL24'00000222060CA2KATZFKATZA'                                   
         DC    XL1'FF'             END DELIMITER                                
         EJECT                                                                  
EXIT     L     RD,SAVEDRD          RESTORE RD                                   
XIT      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*     LOCAL WORKSPACE FOR THIS MODULE                                           
*                                                                               
MYSPACES DC    CL20'                    '                                       
RELO     DS    A                                                                
COTABLE2 DC    CL22'00IRTODIMGGPHNI1I2I8I9'                                     
SAVER12  DS    A                                                                
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
CT17IN01 DSECT                                                                  
*                                                                               
*        $MAD I/P ITEM TYPE 3017   (BASIC SCHEDULE INFORMATION: BS)             
*                                                                               
BSAMFMID DS    CL4                 ELEMENT ID FROM TEMP FILE                    
BSFLSTRT DS    CL8                 FLIGHT START DATE:  MMMDD/YY                 
BSFLEND  DS    CL8                 FLIGHT END   DATE:  MMMDD/YY                 
BSMEDIA  DS    CL1                 R = RADIO, N = RADIO NETWORK                 
BSCOMPNY DS    CL2                 COMPANY IDENTIFICATION                       
*                                                                               
         SPACE 4                                                                
CT17IN02 DSECT                                                                  
*                                                                               
*        $MAD I/P ITEM TYPE 3018   (BASIC LINE INFORMATION: BL)                 
*                                                                               
BLAMFMID DS    CL4                 ELEMENT ID FROM TEMP FILE                    
BLAMFMKY DS    CL8                 AM/FM KEY: TO BE RETURNED                    
BLSPTLEN DS    CL3                 SPOT LENGTH IN SECONDS                       
BLDYPART DS    CL2                 DAYPART CODE                                 
BLSTACTR DS    CL1                 STATION COUNTER:  MAX 4.                     
*                                     IF > 1, COMBO PROCESSING NEEDED           
BLSTATN  DS    CL5                 4-CHAR STATION CALL LETTERS, THEN            
*                                     MEDIA.  WILL OCCUR UP TO                  
*                                     'BLSTACTR' TIMES.                         
*                                                                               
         SPACE 4                                                                
CT17OU01 DSECT                                                                  
*                                                                               
*        $MAD O/P ITEM TYPE 3019   (AVERAGE UNIT RATE DATA: AV)                 
*                                                                               
AVAMFMID DS    CL4                 ELEMENT ID FOR TEMP FILE                     
AVAMFMKY DS    CL8                 AM/FM KEY: TO BE RETURNED                    
AVSPTLEN DS    CL3                 SPOT LENGTH IN SECONDS                       
AVDYPART DS    CL2                 DAYPART CODE                                 
AVCOMBO  DS    CL1                 COMBO FLAG - SET TO 'Y' IF                   
*                                     STATION IS IN A COMBO                     
AVSTATN  DS    CL5                 4-CHAR STATION CALL LETTERS, THEN            
*                                     MEDIA.                                    
AVDATE   DS    CL4                 YYMM - DATE WITHIN FLIGHT                    
AVTOTL$$ DS    CL6                 HEX VALUE OF TOTAL COST.                     
AVTOTLSP DS    CL4                 HEX VALUE OF TOTAL SPOTS                     
*                                                                               
         SPACE 4                                                                
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
*                                                                               
*     FOLLOWING FIELDS ARE DEVELOPED FROM THE REQUEST SENT                      
*     FROM THE PC.                                                              
*                                                                               
SCRATCH  DS    0CL1                SCRATCH SPACE                                
WORKAREA DS    CL64                WORK SPACE                                   
TWSTDT   DS    CL3                 FLIGHT START DATE : YMD BINARY               
TWENDT   DS    CL3                 FLIGHT END DATE   : YMD BINARY               
TMPCTR   DS    XL1                 TEMPORARY COUNTER                            
AURUSER  DS    CL1                 AUR USER:  N= NO, Y= YES                     
AUROPT   DS    CL1                 C=COMBO DOLLARS/UNITS ONLY                   
*                                  R=REGULAR $$$/UNITS ONLY                     
*                                  0=COMBO+REGULAR $$$/UNITS                    
         DS    0F                  FULL WORD ALIGNMENT                          
TMPAREA  DS    CL100               TEMP FILE AREA                               
TMPLEN   DS    XL1                 TEMP ITEM LENGTH                             
TMPTYPE  DS    XL4                 TEMP OBJECT TYPE                             
MAXIOCTR DS    H                   90% OF MAXIMUM IO'S                          
TESTDUMP DS    CL1                 COUNTER FOR SERIAL DUMPS                     
EXTRAKEY DS    CL48                EXTRA KEY STORAGE                            
LASTSTAT DS    CL5                 LAST STATION READ                            
SAVEGRP  DS    CL2                 GROUP/SUBGRP OF STATION                      
SAVECOMP DS    CL2                 COMPANY OF LAST STATION                      
DATEFLD  DS    CL2                 DATE CONVERSION FIELD                        
DATEFLD2 DC    XL1'01'             INSERT DAY FIELD                             
CMBOTABL DS    CL20                                                             
COMBOFLG DS    CL1                                                              
TOTSPOTS DS    F                   REGULAR SPOTS                                
TOTCOST  DS    F                   REGULAR COST                                 
TOTSPCMB DS    F                   COMBO SPOTS                                  
TOTCOCMB DS    F                   COMBO COST                                   
         DS    0D                  ALIGNMENT                                    
HEXOUTWK DS    CL8                 WORK SPACE FOR HEXOUT                        
ELTAREA  DS    CL160               ELEMENT BUILD AREA                           
STORE16  DS    4F                  16 BYTES CONTIGUOUS STORAGE                  
CROSSKEY DS    CL27                CROSS-COMPANY KEY SAVE                       
*                                                                               
AWORKTAB DS    A                   A(WORKTABLE)                                 
NXTWKTAB DS    A                   A(NEXT ENTRY IN WORKTABLE)                   
DATVAL   DS    A                   A(DATVAL ROUTINE)                            
*                                                                               
*        WORKAREA FOR TABLES, ETC                                               
*                                                                               
DATECONV DS    CL2                 THREE-BYTE DATE FIELD WITH                   
DTFILLER DC    XL1'1'                 DAY ALWAYS SET TO 1                       
COTABLE  DC    CL22'00IRTODIMGGPHNI1I2I8I9'                                     
COTABSTP DC    XL1'00'             DELIMITER                                    
COTABSIZ EQU   *-COTABLE           SIZE OF COTABLE                              
GRPTABLE DS    CL22                                                             
GRPDELIM DS    XL1'00'             DELIMITER                                    
GRTABSIZ EQU   *-COTABLE           SIZE OF COTABLE+GRPTABLE                     
USRTABLE DS    CL22                                                             
USRDELIM DS    XL1'00'             DELIMITER                                    
*                                                                               
LSCRATCH EQU   *-SCRATCH                                                        
*                                                                               
*        DSECT TO COVER PROFYL17                                                
*                                                                               
RSTARECD DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
*                                                                               
RREPRECD DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
*                                                                               
       ++INCLUDE REGENATNA                                                      
*                                                                               
       ++INCLUDE REGENAUR                                                       
*                                                                               
         EJECT                                                                  
*      END OF PC FIELDS       *                                                 
*                                                                               
*  DDCOMFACS: ADDRESS ROUTINE DSECT                                             
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'117CTMAD17   05/01/02'                                      
         END                                                                    
