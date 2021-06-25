*          DATA SET RECNT72    AT LEVEL 067 AS OF 02/20/09                      
*PHASE T80272A,+0                                                               
*INCLUDE RECONDAT                                                               
*INCLUDE REVALDAT                                                               
         TITLE 'T80272 - REP SAR/SPL COMBINED DISPLAY/EDIT'                     
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT72 (T80272) --- SAR/SPL COMBINED DISPLAY/EDIT       *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 22JUN92 (BU ) --- ORIGINAL ENTRY                                *             
*                                                                 *             
* 14JUN93 (BU ) --- ALL BUDGET $$$ NOW MARKET $$$.                *             
*                                                                 *             
* 29JUL93 (BU ) --- PROHIBIT BUDGET,SHR GOAL,SPL ENTRY FOR TYPE N *             
*                   CONTRACTS                                     *             
*                                                                 *             
* 03AUG93 (BU ) --- PROHIBIT BUDGET,SHR GOAL FOR GEN/ZZ CONTRACTS *             
*                                                                 *             
* 05AUG93 (BU ) --- INSTALL TRAP FOR EC KEY W/ZERO D/A            *             
*                                                                 *             
* 17AUG93 (BU ) --- RELOCATE DBLOCK:  WAS STEPPING ON END OF      *             
*                   LONG CONTRACTS                                *             
*                                                                 *             
* 22SEP93 (BU ) --- REMOVE REQUIREMENT FOR COMMENT CHANGE FOR     *             
*                   UPDATE TO TRUE ACTIVITY DATE FOR SPL          *             
*                                                                 *             
* 01OCT93 (BU ) --- 'SPREDFOR' CHANGED TO BROADCAST MONTH BASIS   *             
*                                                                 *             
* 21OCT93 (BU ) --- DON'T ROUND PENNIES FOR 'SPREDFOR'            *             
*                                                                 *             
* 07JAN94 (BU ) --- ADD 'FORECAST?' FLAG                          *             
*                                                                 *             
* 22APR94 (SKU) --- ADD FORECASTING SUPPORT                       *             
*                                                                 *             
* 17MAY94 (SKU) --- FIX MARKET BUDGET DIVISION BY ZERO BUG        *             
*                                                                 *             
* 12SEP94 (BU ) --- REQUIRE REP STA SPL > $0 IF STA HAS MONEY     *             
*                                                                 *             
* 09DEC94 (SKU) --- FIX DISPLAY BUGS                              *             
*                                                                 *             
* 08MAR95 (BU ) --- FIX STATION LIST PROBLEM                      *             
*                                                                 *             
* 04APR95 (BU ) --- REQUIRE BOTH BUDGET $ AND SHARE GOAL FOR      *             
*                   PENDING...                                    *             
*                                                                 *             
* 18JUL95 (SKU) --- FIX DEMO/BOOK DR VALIDATION BUG               *             
*                                                                 *             
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                           *             
*                                                                 *             
* 23JAN96 (BU ) --- CHANGE DAYPART TABLE LOOKUP TO NOT ABORT IF   *             
*                   UNRECOGNIZED DAYPART ENCOUNTERED.             *             
*                                                                 *             
* 22FEB96 (SKU) --- IF PROPOSAL EXPANSION USED, DON'T ALLOW EDIT  *             
*                                                                 *             
* 07MAR96 (JRD) --- FULLSCREEN SPL                                *             
*                                                                 *             
* 16APR96 (SKU) --- FIX DIVISION BY ZERO BUG IN CALCTOT           *             
*                                                                 *             
* 13JUN96 RHV REMOVE CONTYPE B RESTRICTION                        *             
*                                                                 *             
* 04OCT96 SKU SUPPORT LOW POWER STATION                           *             
*                                                                 *             
* 28FEB97 DBU NEW CLEAR ACTION FROM REP SIDE                      *             
*                                                                 *             
* 23JUL97 SKU 4K CONTRACT SUPPORT                                 *             
*                                                                 *             
* 30DEC97 RHV SPL=NNNNNNNN                                        *             
*                                                                 *             
* 06MAR98 SKU 4K CONTRACT SUPPORT                                 *             
*                                                                 *             
* 09FEB20 SKU FIX SPL SCREEN BUG                                  *             
*                   **  END TOMBSTONE  **                         *             
*******************************************************************             
*                                                                               
T80272   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80272,R9,RR=R5                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         ST    R5,TEMP                                                          
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)     4K                                           
         ST    R7,SAVUSING         SAVE FOR LATER 'USING' NEEDS                 
         SPACE 1                                                                
         MVC   BUYACT(6),MYSPACES  BLANK OUT BUYACT,BUYNUM                      
*                                                                               
*                                                                               
         MVC   LOCALFLT,RCONDATE                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   MAIN010                                                          
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT                                                
         BZ    MAIN010                                                          
         MVC   LOCALFLT,RCONRFLT   USE REVISED FLIGHT DATE                      
         DROP  R6                                                               
*                                                                               
MAIN010  DS    0H                                                               
         L     R2,4(R1)                                                         
         CLC   =C'DISP',0(R2)      DISPLAY REQUEST?                             
         BE    DISPLY                                                           
         CLC   =C'EDIT',0(R2)      UPDATE REQUEST?                              
         BE    EDIT                                                             
         CLC   =C'CLRC',0(R2)      CLEAR REQUEST?                               
         BE    CONCLR                                                           
         DC    H'0'                NO  - SHOULDN'T HAPPEN                       
         EJECT                                                                  
*                                                                               
*   BEGIN DISPLAY OF SCREEN                                                     
*                                                                               
DISPLY   EQU   *                                                                
         GOTO1 VLOAD,DMCB,(X'80',0)                                             
DISSPL   DS    0H                                                               
*                                                                               
*   FOUTBLK CALL CLEARS NON-SPACE FIELDS, TURNS ON TRANSMIT BITS                
*      FOR THOSE FIELDS, BUT DOES NOT TURN ON TRANSMIT BITS FOR                 
*      FIELDS WHICH ARE BLANK, BUT MAY BE FILLED IN BY NEXT DISPLAY             
*      LOGIC.                                                                   
*                                                                               
         GOTO1 VFOUTBLK,DMCB,SPLSMTHH,SPLLAST,0                                 
*                                                                               
*   TRANOUT TURNS ON TRANSMIT BITS FOR ALL UNPROTECTED FIELDS ON                
*      SCREEN.  THIS IS A BIT PROFLIGATE, BUT....                               
*                                                                               
         GOTO1 =A(TRANOUT),DMCB,(RC),SPLSMTHH,SPLLAST,RR=Y                      
*                                                                               
*   CLEAR PROTECTED FIELDS BETWEEN DISPLAYS                                     
*                                                                               
         XC    SPLSTRU,SPLSTRU     TRUE ACTIVITY DATE                           
         FOUT  SPLSTRUH                                                         
         XC    SPLSMTH,SPLSMTH     SPL MONTH                                    
         FOUT  SPLSMTHH                                                         
         SPACE 2                                                                
*                                                                               
*   BEGIN DISPLAY OF SPL PORTION OF SCREEN...                                   
*                                                                               
DSPL0000 DS    0H                                                               
         GOTO1 =A(SETPROTS),DMCB,(RC),RR=Y                                      
         BAS   RE,UNPROTAL         UNPROTECT ALL STATION $ FIELDS               
         LA    R2,CONCACTH                                                      
         LA    R3,BACERR                                                        
         TM    RCONREC+29,X'01'    COMPRESSED                                   
         BO    ERROR                                                            
*                                  GET STATION RECORD                           
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         CLC   KEY(27),RSTAKEY                                                  
         BE    DSPL0020                                                         
         GOTO1 VHIGH                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
DSPL0020 EQU   *                                                                
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   DSPL0028                                                         
         LA    R3,55               ERROR, SECURITY LOCKOUT                      
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   DSPL0026                                                         
         SPACE 1                                                                
         USING RSTASOEL,R6         STATION'S CONTRACTS                          
DSPL0024 CLC   TWAUSRID,RSTASID                                                 
         BE    DSPL0028                                                         
         BAS   RE,NEXTEL                                                        
         BE    DSPL0024                                                         
DSPL0026 EQU   *                                                                
         B     ERROR                                                            
*                                                                               
DSPL0028 EQU   *                                                                
         DROP  R6                                                               
         MVC   WORK(5),RSTAKSTA    INSERT ORIGINAL STATION                      
         MVC   WORK+5(3),RSTAAFFL  INSERT AFFILIATE                             
         LA    R2,WORK                                                          
         LA    R3,SCRSTA01         1ST (REPPED) STATION IN LIST                 
         LA    R4,SPLSST1          1ST STATION FIELD ON SCREEN                  
         BAS   RE,SETSTA           SET REPPED STATION ON SCREEN                 
         LA    R3,LSCRNTRY(R3)     SET TO 2ND STALIST ENTRY                     
*                                  NOW GO THRU THE ELEMENTS                     
         LA    R5,SCRFLD02         A(NEXT SCREEN FIELD)                         
         LA    R6,RSTAELEM                                                      
DSPL0030 CLI   0(R6),0                                                          
         BE    DSPL0060                                                         
         CLI   0(R6),2             'OTHER STATION' ELEMENT?                     
         BE    DSPL0050            YES - SET IT ON SCREEN                       
DSPL0040 ZIC   R0,1(R6)            NO  - SCAN RECORD FOR MORE                   
         AR    R6,R0                                                            
         B     DSPL0030                                                         
*                                                                               
DSPL0050 EQU   *                                                                
         L     R4,0(R5)            LOAD SCREEN ADDR FROM TABLE                  
         AR    R4,RA               SET ADDRESSABILITY                           
*                                                                               
         LA    R2,2(R6)            SET TO CALL LTRS IN ELEMENT                  
         BAS   RE,SETSTA           SET STATION ON SCREEN                        
         LA    R3,LSCRNTRY(R3)     NEXT STALIST ENTRY                           
         LA    R5,LSCRNTRY(R5)     NEXT SCREEN FIELD                            
         B     DSPL0040                                                         
*                                  TURN ON PROTECT BIT FOR UNUSED FIELD         
DSPL0060 EQU   *                                                                
         BAS   RE,PROTUNUS         PROTECT UNUSED STA $ FIELDS                  
         B     DSPL0080                                                         
         EJECT                                                                  
SCRTABLE DS    0H                                                               
*                                                                               
*   TABLE IS SET UP IN ORDER IN WHICH STATIONS ARE TO BE INSERTED               
*     ONTO THE SCREEN.  ADDITIONALLY, THE STATION THAT APPEARS IN               
*     A FIELD IS ALSO CONTAINED IN THE TABLE FOR MATCHING MINI-                 
*     ELEMENTS.  THESE STATIONS WERE PREVIOUSLY LISTED IN A TABLE               
*     CALLED 'STALIST'.  BILL UHR.  JUN30/92.                                   
*                                                                               
LSCRNTRY EQU   SCRFLD02-SCRFLD01                                                
STADISP  EQU   SCRSTA01-SCRFLD01                                                
CSTAT1   EQU   SPLSST1-TWAD        DISPLACEMENTS OF FIELDS WITHIN               
CSTAT2   EQU   SPLSST2-TWAD          SCREEN FROM BEGINNING OF TWA               
CSTAT3   EQU   SPLSST3-TWAD                                                     
CSTAT4   EQU   SPLSST4-TWAD                                                     
CSTAT5   EQU   SPLSST5-TWAD                                                     
CSTAT6   EQU   SPLSST6-TWAD                                                     
CSTAT7   EQU   SPLSST7-TWAD                                                     
CSTAT8   EQU   SPLSST8-TWAD                                                     
CSTAT9   EQU   SPLSST9-TWAD                                                     
CSTATA   EQU   SPLSSTA-TWAD                                                     
CSTATB   EQU   SPLSSTB-TWAD                                                     
CSTATC   EQU   SPLSSTC-TWAD                                                     
*                                                                               
CSTAT1H  EQU   SPLSST1H-TWAD                                                    
CSTAT2H  EQU   SPLSST2H-TWAD                                                    
CSTAT3H  EQU   SPLSST3H-TWAD                                                    
CSTAT4H  EQU   SPLSST4H-TWAD                                                    
CSTAT5H  EQU   SPLSST5H-TWAD                                                    
CSTAT6H  EQU   SPLSST6H-TWAD                                                    
CSTAT7H  EQU   SPLSST7H-TWAD                                                    
CSTAT8H  EQU   SPLSST8H-TWAD                                                    
CSTAT9H  EQU   SPLSST9H-TWAD                                                    
CSTATAH  EQU   SPLSSTAH-TWAD                                                    
CSTATBH  EQU   SPLSSTBH-TWAD                                                    
CSTATCH  EQU   SPLSSTCH-TWAD                                                    
*                                                                               
CDOLS1H  EQU   SPLSDL1H-TWAD                                                    
CDOLS2H  EQU   SPLSDL2H-TWAD                                                    
CDOLS3H  EQU   SPLSDL3H-TWAD                                                    
CDOLS4H  EQU   SPLSDL4H-TWAD                                                    
CDOLS5H  EQU   SPLSDL5H-TWAD                                                    
CDOLS6H  EQU   SPLSDL6H-TWAD                                                    
CDOLS7H  EQU   SPLSDL7H-TWAD                                                    
CDOLS8H  EQU   SPLSDL8H-TWAD                                                    
CDOLS9H  EQU   SPLSDL9H-TWAD                                                    
CDOLSAH  EQU   SPLSDLAH-TWAD                                                    
CDOLSBH  EQU   SPLSDLBH-TWAD                                                    
CDOLSCH  EQU   SPLSDLCH-TWAD                                                    
*                                                                               
SCRFLD01 DC    AL4(CSTAT1)         A(ACTUAL FIELD ON SCREEN)                    
SCRFLD1H DC    AL4(CSTAT1H)        A(SCREEN FIELD HEADER: STATION)              
SCRSTA01 DS    CL8                 STATION CALL LETTERS IN FIELD                
SCRDOL1H DC    AL4(CDOLS1H)        A(SCREEN FIELD HEADER: DOLLARS)              
SCRFLD02 DC    AL4(CSTAT3)                                                      
SCRFLD2H DC    AL4(CSTAT3H)                                                     
SCRSTA02 DS    CL8                                                              
SCRDOL2H DC    AL4(CDOLS3H)                                                     
         DC    AL4(CSTAT5)                                                      
         DC    AL4(CSTAT5H)                                                     
         DS    CL8                                                              
         DC    AL4(CDOLS5H)                                                     
         DC    AL4(CSTAT7)                                                      
         DC    AL4(CSTAT7H)                                                     
         DS    CL8                                                              
         DC    AL4(CDOLS7H)                                                     
         DC    AL4(CSTAT9)                                                      
         DC    AL4(CSTAT9H)                                                     
         DS    CL8                                                              
         DC    AL4(CDOLS9H)                                                     
         DC    AL4(CSTATB)                                                      
         DC    AL4(CSTATBH)                                                     
         DS    CL8                                                              
         DC    AL4(CDOLSBH)                                                     
         DC    AL4(CSTAT2)                                                      
         DC    AL4(CSTAT2H)                                                     
         DS    CL8                                                              
         DC    AL4(CDOLS2H)                                                     
         DC    AL4(CSTAT4)                                                      
         DC    AL4(CSTAT4H)                                                     
         DS    CL8                                                              
         DC    AL4(CDOLS4H)                                                     
         DC    AL4(CSTAT6)                                                      
         DC    AL4(CSTAT6H)                                                     
         DS    CL8                                                              
         DC    AL4(CDOLS6H)                                                     
         DC    AL4(CSTAT8)                                                      
         DC    AL4(CSTAT8H)                                                     
         DS    CL8                                                              
         DC    AL4(CDOLS8H)                                                     
         DC    AL4(CSTATA)                                                      
         DC    AL4(CSTATAH)                                                     
         DS    CL8                                                              
         DC    AL4(CDOLSAH)                                                     
SCRFLD0C DC    AL4(CSTATC)                                                      
SCRFLDCH DC    AL4(CSTATCH)                                                     
SCRSTA0C DS    CL8                                                              
SCRDOLCH DC    AL4(CDOLSCH)                                                     
         EJECT                                                                  
SETSTA   DS    0H                  R2 AT STA/AFFL                               
*                                  R3 AT STALIST ENTRY                          
*                                  R4 AT FIELD HEADER                           
*                                                                               
         MVC   0(5,R3),0(R2)       STALIST ENTRY                                
         MVC   0(10,R4),MYSPACES   SPACE FILL SCREEN FIELD                      
         MVC   0(4,R4),0(R2)       MOVE IN CALL LETTERS                         
         CLI   4(R2),C' '          CHECK MEDIA = TV?                            
         BE    SS4                 YES                                          
         CLI   4(R2),C'L'          CHECK MEDIA = TV?                            
         BE    SS4                 YES                                          
         LA    R1,2(R4)            NO  - RADIO                                  
         CLI   0(R1),C' '          FIND LAST CHARACTER                          
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
*                                                                               
         MVI   0(R1),C'-'          INSERT 'M'                                   
         MVC   1(1,R1),4(R2)                                                    
         MVI   2(R1),C'M'                                                       
*                                                                               
SS4      MVI   8(R4),C'('          INSERT PARENS                                
         MVC   9(3,R4),5(R2)       AFFL                                         
         MVI   12(R4),C')'                                                      
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*  FOR ALL STATION $ FIELDS WHERE THERE ARE NO STATION CALL LETTERS,            
*    TURN ON PROTECT BIT TO FACILITATE TABBING                                  
*                                                                               
NEXTSTA  EQU   SPLSST2-SPLSST1                                                  
NEXTDOL  EQU   SPLSDL2H-SPLSDL1H                                                
*                                                                               
PROTUNUS NTR1                                                                   
         LA    R0,12               12 STATION FIELDS ON SCREEN                  
         LA    R5,SCRFLD01         A(FIRST STATION FIELD)                       
         LA    R6,SCRDOL1H         A(FIRST DOLLAR FIELD HEADER)                 
PUNU0010 EQU   *                                                                
         L     R1,0(R5)            LOAD STATION SCREEN ADDRESS                  
         AR    R1,RA               SET ADDRESSABILITY                           
         L     R2,0(R6)            LOAD DOLLARS SCREEN ADDRESS                  
         AR    R2,RA               SET ADDRESSABILITY                           
         OC    0(13,R1),0(R1)      ANY STATION CALL LETTERS?                    
         BZ    PUNU0020            YES - DON'T SET BIT                          
         CLC   0(13,R1),MYSPACES                                                
         BNE   PUNU0030            NOT SPACES, EITHER                           
PUNU0020 EQU   *                                                                
         OI    1(R2),X'20'         EMPTY  - SET PROTECT BIT                     
         OI    6(R2),X'80'         SET TRANSMIT BIT                             
PUNU0030 EQU   *                                                                
         LA    R5,LSCRNTRY(R5)     BUMP TO NEXT STATION FIELD                   
         LA    R6,LSCRNTRY(R6)     BUMP TO NEXT STA $ FIELD                     
         BCT   R0,PUNU0010         GO BACK FOR NEXT                             
         B     PROTEXIT            GO TO COMMON EXIT                            
         SPACE 4                                                                
*                                                                               
UNPROTAL NTR1                                                                   
         LA    R0,12               12 STATION FIELDS ON SCREEN                  
         LA    R5,SCRDOL1H         A(FIRST DOLLAR FIELD HEADER)                 
UNPR0010 EQU   *                                                                
         L     R2,0(R5)            LOAD SCREEN ADDRESS FROM TABLE               
         AR    R2,RA               SET ADDRESSABILITY                           
         NI    1(R2),X'FF'-X'20'   TURN OFF PROTECT BIT                         
         OI    6(R2),X'80'         SET TRANSMIT BIT                             
         LA    R5,LSCRNTRY(R5)     BUMP TO NEXT STA $ FIELD                     
         BCT   R0,UNPR0010         GO BACK FOR NEXT                             
PROTEXIT EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
DSPL0080 LA    R2,RCONELEM         ACCUMULATE ESTIMATE DOLLARS                  
         SR    R3,R3                  FROM X'03' ELEMENTS FOR ALL               
         SR    R7,R7                  DISPLAYS                                  
*                                                                               
DSPL0090 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    DSPL0120            YES                                          
         CLI   0(R2),3             ESTIMATE BUCKET?                             
         BE    DSPL0110            YES                                          
DSPL0100 EQU   *                                                                
         IC    R3,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,R3                                                            
         B     DSPL0090            GO BACK FOR NEXT                             
DSPL0110 EQU   *                                                                
         A     R7,6(R2)            ACCUMULATE ESTIMATE DOLLARS                  
         B     DSPL0100                                                         
DSPL0120 EQU   *                                                                
         SR    R6,R6                                                            
         D     R6,=F'100'          GET RID OF PENNIES                           
         ST    R7,ESTDOLS          STORE ESTIMATE DOLLARS                       
         LA    R2,RCONELEM         TEST IF ANY X'06' ELEMENT                    
         SR    R3,R3                                                            
*                                                                               
*   FIND THE X'08' ELEMENT FOR AUTOCLOSE DATE                                   
*                                                                               
DSPL0130 CLI   0(R2),0             END OF RECORD?                               
         BE    DSPL0150            YES - LOOK FOR X'08' ELEMENT                 
         CLI   0(R2),8             NO  - AUTOCLOSE/ACT DATE ELEMENT?            
         BE    DSPL0140            YES                                          
         IC    R3,1(R2)            NO  - SCAN REST OF RECORD                    
         AR    R2,R3                                                            
         B     DSPL0130                                                         
         SPACE 2                                                                
DSPL0140 EQU   *                                                                
         OC    RCONACAC-RCONACEL(3,R2),RCONACAC-RCONACEL(R2)                    
*                                  ANY AUTOCLOSE DATE?                          
         BZ    DSPL0145            NO                                           
         GOTO1 DATCON,DMCB,(3,RCONACAC-RCONACEL(R2)),(5,SPLSACL)                
DSPL0145 EQU   *                                                                
         OC    RCONACTA-RCONACEL(3,R2),RCONACTA-RCONACEL(R2)                    
*                                  ANY TRUE ACTIVITY DATE?                      
         BZ    DSPL0150            NO                                           
         GOTO1 DATCON,DMCB,(3,RCONACTA-RCONACEL(R2)),(5,SPLSTRU)                
DSPL0150 EQU   *                                                                
         LA    R2,RCONELEM         TEST IF ANY X'06' ELEMENT                    
         SR    R3,R3                                                            
*                                                                               
*   FIND THE X'06' ELEMENT FOR SPL                                              
*                                                                               
DSPL0160 CLI   0(R2),0             END OF RECORD?                               
         BE    DSPL0170            YES - CHECK ESTIMATE BUCKETS                 
         CLI   0(R2),6             NO  - SPL DATA FOUND?                        
         BE    DSPL0180            YES                                          
         IC    R3,1(R2)            NO  - SCAN REST OF RECORD                    
         AR    R2,R3                                                            
         B     DSPL0160                                                         
         SPACE 2                                                                
DSPL0170 EQU   *                                                                
         LA    R8,SPLSDL1          REPPED STATION AMOUNT                        
         L     R7,ESTDOLS                                                       
         GOTO1 EDITAMT,DMCB,(R7)                                                
         B     DSPL0210                                                         
         EJECT                                                                  
*                                                                               
*   FOLLOWING CODE DISPLAYS THE SPL ELEMENT, IF FOUND                           
*                                                                               
DSPL0180 EQU   *                                                                
*                                                                               
         TM    4(R2),X'01'                                                      
         BNO   DSPL0190                                                         
         MVI   SPLSEST,C'E'                                                     
         FOUT  SPLSESTH                                                         
DSPL0190 EQU   *                                                                
*                                                                               
*  R2 CONTAINS A(X'06' ELEMENT) IN CONTRACT RECORD                              
*                                                                               
         TM    RCONSPES-RCONSPEL(R2),X'04'                                      
*                                  BUCKETS = PERCENTS?                          
         BNO   DSPL0200            NO  - BUCKETS = DOLLARS                      
         GOTO1 PERCENTS,DMCB,(R2)  YES - BUCKETS = PERCENTS                     
         B     DSPL0210                                                         
DSPL0200 EQU   *                                                                
         GOTO1 ABSOLUT$,DMCB,(R2)  PROCESS BUCKETS AS DOLLARS                   
DSPL0210 EQU   *                                                                
*                                                                               
*   NOW DISPLAY THE COMMENTS CONTAINED IN THE X'07' ELEMENTS                    
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BNE   DSPL0230                                                         
         LA    R2,SPLSCM1H                                                      
         SPACE 1                                                                
DSPL0220 MVI   WORK3,C' '                                                       
         MVC   WORK3+1(L'WORK3-1),WORK3                                         
         ZIC   R3,1(R6)            GET L(COMMENT ELEMENT)                       
         SH    R3,=H'3'            MINUS CONTROL +1 FOR 'EX'                    
         EX    R3,*+8              MOVE IT BY LENGTH TO WORK3                   
         B     *+10                                                             
         MVC   WORK3(0),2(R6)                                                   
         SPACE 1                                                                
         ZIC   R1,0(R2)            GET L(SCREEN FIELD)                          
         TM    1(R2),X'02'         EXTENDED FIELD HEADER PRESENT?               
         BZ    *+8                 NO                                           
         SH    R1,=H'8'            YES - SUBTRACT 8 MORE                        
         SH    R1,=H'9'            MINUS FIELD HDR +1 FOR 'EX'                  
         BNP   DSPL0230            NEGATIVE?  DON'T MOVE IT                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK3                                                    
         L     R8,AIO4             CHECK FOR STANDARD COMMENT                   
         GOTO1 VREGENSC,DMCB,(1,(R2)),(R8),DATAMGR,RCONREC,GETTXT               
         BZ    *+10                                                             
         MVC   22(24,R2),=C'***CMNT REC NOT FOUND***'                           
*                                                                               
         LA    R2,NEXTCOMT(R2)     BUMP TO NEXT SCREEN FIELD                    
         LA    R0,SPLSLCMH                                                      
         CR    R2,R0               PAST LAST COMMENT?                           
         BH    DSPL0230            YES                                          
*                                                                               
         BAS   RE,NEXTEL           GO BACK AND CHECK IT                         
         BE    DSPL0220                                                         
*                                                                               
NEXTCOMT EQU   SPLSCM2-SPLSCM1                                                  
*                                                                               
DSPL0230 EQU   *                                                                
*                                                                               
*   FOLLOWING CALL TO PREVALID WILL SET PREVIOUSLY VALID BITS FOR               
*     ALL UNPROTECTED FIELDS.  THE RETURN CODE IS IGNORED.                      
*                                                                               
         GOTO1 =A(PREVALID),DMCB,(RC),SPLSMTHH,SPLLAST,0,0,RR=Y                 
*                                                                               
         BAS   RE,TESTDISP         DISPLAY CONTROL INFORMATION                  
*JRD     MVC   CONCACT,MYSPACES    TURN OFF DISPLAY ACTION                      
         SR    R3,R3               TURN OFF ERROR PASS-BACK                     
         B     EXXMOD                                                           
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
*   ABSOLUT$:  ORIGINAL ROUTINE WHICH DISPLAYS CONTENTS OF THE X'06'            
*     ELEMENT AS CONTAINING DOLLARS                                             
*                                                                               
ABSOLUT$ NTR1                                                                   
         L     R2,0(R1)            RELOAD A(X'06' ELEMENT)                      
*                                                                               
*   LOAD MONTH (MMM/DD) FROM OLD-STYLE ENTRY                                    
*                                                                               
         MVC   FULL,2(R2)          MONTH                                        
         MVI   FULL+2,1                                                         
         MVC   SPLSMTH,MYSPACES    SPACE-FILL DATE FIELD                        
         GOTO1 DATCON,DMCB,(3,FULL),(9,SPLSMTH)                                 
         FOUT  SPLSMTHH                                                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,8(R2)            NUMBER OF MINI-ELEMENTS                      
         LA    RF,9(R2)            POINT TO FIRST                               
         SR    R0,R0                                                            
*                                                                               
ABSO0010 EQU   *                                                                
         MVC   FULL,5(RF)          LOOP THRU MINI-ELTS AND                      
         CLC   FULL,NAVALUE           ACCUMULATE THE TOTAL                      
         BE    ABSO0020                                                         
         A     R0,FULL                                                          
         LA    RF,9(RF)                                                         
ABSO0020 EQU   *                                                                
         BCT   RE,ABSO0010                                                      
         ST    R0,TOTAMT           GET TOTAL FOR PERCENTAGE CALC                
*                                                                               
         SR    R3,R3                                                            
         IC    R3,8(R2)            NUMBER OF MINI-ELEMENTS                      
         LA    R2,9(R2)            POINT TO FIRST                               
ABSO0030 EQU   *                                                                
         OC    0(5,R2),MYSPACES                                                 
         LA    R5,SCRSTA01         1ST STA IN SCREEN LIST                       
         LA    R4,SCRFLD1H         TABLE OF A(SCREEN HEADERS)                   
         SR    R0,R0                                                            
*                                                                               
ABSO0040 EQU   *                                                                
         L     R6,0(R4)            A(FIELD ON SCREEN)                           
         AR    R6,RA               SET ADDRESSABILITY                           
         ZIC   R0,0(R6)            SKIP OVER STATION CALL FIELD                 
         AR    R6,R0                  SET A(AMOUNT FIELD)                       
         CLC   0(5,R5),0(R2)       COMPARE ON STATION                           
         BE    ABSO0050                                                         
         LA    R5,LSCRNTRY(R5)     NEXT STA IN SCREEN LIST                      
         LA    R4,LSCRNTRY(R4)     NEXT SCREEN FIELD IN TABLE                   
         B     ABSO0040                                                         
*                                                                               
ABSO0050 LA    R8,8(R6)                                                         
         MVC   FULL,5(R2)                                                       
         L     RF,FULL                                                          
         GOTO1 EDITAMT,DMCB,(RF)   LOAD AMOUNT                                  
         FOUT  (R6)                                                             
         ZIC   R0,0(R6)                                                         
         AR    R6,R0               NOW POINT AT PERCENT FILED                   
         LA    R8,8(R6)            POINT AT DATA AREA                           
         BAS   RE,EDITPRCT         LOAD PERCENTAGE                              
         FOUT  (R6)                                                             
         OI    1(R6),X'20'                                                      
*                                                                               
         LA    R2,9(R2)                                                         
         BCT   R3,ABSO0030         LOOP                                         
*                                                                               
         L     RF,TOTAMT           PUT OUT CONTRACT TOTAL                       
         LA    R8,SPLSTTL                                                       
         GOTO1 EDITAMT,DMCB,(RF)                                                
         FOUT  SPLSTTLH                                                         
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PERCENTS:  NEW ROUTINE WHICH DISPLAYS CONTENTS OF THE X'06'                 
*     ELEMENT AS CONTAINING PERCENTS, AND APPLIES THEM AGAINST                  
*     APPROPRIATE 'BASE' VALUES.                                                
*                                                                               
PERCENTS NTR1                                                                   
         L     R2,0(R1)            RELOAD A(X'06' ELEMENT)                      
*                                                                               
*   TEST FOR DATE FORMAT                                                        
*                                                                               
         MVC   FULL,2(R2)          UNLOAD DATE TO COMMON STORAGE                
         TM    RCONSPES-RCONSPEL(R2),X'08'                                      
         BO    PERC0010            ON = COMPRESSED FORMAT                       
*                                                                               
         MVI   FULL+2,1            OFF = ORIGINAL  FORMAT                       
         GOTO1 DATCON,DMCB,(3,FULL),(9,SPLSMTH)                                 
         B     PERC0015                                                         
PERC0010 EQU   *                                                                
*                                                                               
         GOTO1 DATCON,DMCB,(2,FULL),(5,SPLSMTH)                                 
PERC0015 EQU   *                                                                
         FOUT  SPLSMTHH                                                         
*                                                                               
*   DISPLAY TRUE ACTIVITY DATE                                                  
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL            RETRIEVE X'08' ELEMENT                       
         BNE   PERC0018            FOUND                                        
*        DC    H'0'                MUST BE PRESENT                              
         GOTO1 DATCON,DMCB,(3,5(R6)),(5,SPLSTRU)                                
         FOUT  SPLSTRUH                                                         
*                                                                               
PERC0018 EQU   *                                                                
         TM    RCONSPES-RCONSPEL(R2),X'40'                                      
*                                  NO REP'D STATION:  VALUE = TOTAL?            
         BNO   PERC0020            NO                                           
         BAS   RE,PERCDOLS         YES - RETRIEVE $$ FROM X'08' ELT             
         B     PERC0040              THIS IS >>TOTAL DOLLARS<<                  
PERC0020 EQU   *                                                                
         TM    RCONSPES-RCONSPEL(R2),X'20'                                      
*                                  REP STATION OVERRIDE $$?                     
         BNO   PERC0030            NO                                           
         BAS   RE,PERCDOLS         YES - RETRIEVE $$ FROM X'08' ELT             
PERC0030 EQU   *                                                                
         GOTO1 CALCTOT$,DMCB,(R2)  CALC TOTAL $ FROM COMPONENTS                 
PERC0040 EQU   *                                                                
*                                                                               
*   AT THIS POINT, ESTDOLS CONTAINS TOTAL MARKET BUDGET, EITHER                 
*      CALCULATED FROM REP'D STATION PERCENT AND $$, OR AS A FIXED              
*      FIGURE BECAUSE REP'D STATION HAS NO PERCENT VALUE.                       
*                                                                               
*                                                                               
         SR    R3,R3                                                            
         IC    R3,8(R2)            NUMBER OF MINI-ELEMENTS (STATIONS)           
         LA    R2,9(R2)            POINT TO FIRST                               
PERC0050 EQU   *                                                                
         OC    0(5,R2),MYSPACES                                                 
         LA    R5,SCRSTA01         1ST STA IN SCREEN LIST                       
         LA    R4,SCRFLD1H         TABLE OF A(SCREEN HEADERS)                   
         SR    R0,R0                                                            
*                                                                               
PERC0060 L     R6,0(R4)            A(FIELD ON SCREEN)                           
         AR    R6,RA               SET ADDRESSABILITY                           
         ZIC   R0,0(R6)            SKIP OVER STATION CALL FIELD                 
         AR    R6,R0                  SET A(AMOUNT FIELD)                       
         CLC   0(5,R5),0(R2)       COMPARE ON STATION                           
         BE    PERC0070                                                         
         LA    R5,LSCRNTRY(R5)     NEXT STA IN SCREEN LIST                      
         LA    R4,LSCRNTRY(R4)     NEXT SCREEN FIELD IN TABLE                   
         OC    0(5,R5),0(R5)       ANY STATION IN FIELD?                        
         BZ    PERC0100            NO  - STATION NOT FOUND IN LIST              
*                                     SKIP IT                                   
         LA    RF,SCRFLDCH                                                      
         CR    R4,RF               END OF LIST?                                 
         BH    PERC0100                                                         
*                                                                               
         B     PERC0060            GO BACK FOR NEXT                             
*                                                                               
PERC0070 LA    R8,8(R6)            A(SCREEN FIELD)                              
         MVC   FULL,5(R2)          UNLOAD STATION PERCENT                       
         L     R7,FULL             LOAD TO REGISTER                             
         LTR   R7,R7               ANY VALUE AT ALL?                            
         BZ    PERC0080            NO  - ZERO                                   
         CL    R7,NAVALUE          IS VALUE N/A?                                
         BE    PERC0080            YES - DON'T CALC/ACCUM ANYTHING              
         GOTO1 STADOLL$,DMCB,(R7)  CALCULATE STATION DOLLARS                    
         L     R7,FULL             STATION DOLLARS PASSED BACK                  
         A     R7,TOTAMT           ACCUMULATE TOTAL                             
         ST    R7,TOTAMT           STORE IT BACK                                
         L     R7,FULL             RELOAD STATION DOLLARS                       
PERC0080 EQU   *                                                                
         GOTO1 EDITAMT,DMCB,(R7)   LOAD AMOUNT                                  
         FOUT  (R6)                                                             
         ZIC   R4,0(R6)                                                         
         AR    R6,R4               NOW POINT AT PERCENT FIELD                   
         LA    R8,8(R6)            POINT AT DATA AREA                           
         MVC   FULL,5(R2)          RELOAD STATION PERCENT                       
         L     R7,FULL             LOAD TO REGISTER                             
         BAS   RE,EDITPERC         LOAD PERCENTAGE                              
         FOUT  (R6)                                                             
         OI    1(R6),X'20'                                                      
*                                                                               
PERC0100 EQU   *                                                                
         LA    R2,9(R2)            INCREMENT TO NEXT STATION                    
         BCT   R3,PERC0050         LOOP                                         
*                                                                               
         L     R7,TOTAMT           PUT OUT CONTRACT TOTAL                       
         LA    R8,SPLSTTL                                                       
         GOTO1 EDITAMT,DMCB,(R7)                                                
         FOUT  SPLSTTLH                                                         
         ST    R2,FULL             PASS R2 BACK                                 
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PERCDOLS:  RETRIEVE X'08' ELEMENT, GET DOLLARS FROM IT                      
*                                                                               
PERCDOLS NTR1                                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'08'        SET FOR X'08' ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    PDOL0010            FOUND                                        
         DC    H'0'                MUST BE FOUND!!                              
PDOL0010 EQU   *                                                                
         MVC   ESTDOLS,RCONAC$$-RCONACEL(R6)                                    
*                                  UNLOAD DOLLARS                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CALCTOT$:  CALCULATE THE TOTAL MARKET DOLLARS FROM STATION                  
*      DOLLARS AND PERCENT                                                      
*                                                                               
CALCTOT$ NTR1                                                                   
         L     R2,0(R1)            RELOAD A(X'06' ELEMENT)                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
* RETURN 0 IF DIVISION BY ZERO                                                  
         OC    RCONSPAM-RCONSPEL(4,R2),RCONSPAM-RCONSPEL(R2)                    
         BZ    CALCTOTX                                                         
*                                                                               
         L     RF,ESTDOLS          LOAD ESTIMATE DOLLARS                        
         M     RE,=F'10000'                                                     
*                                                                               
*   10,000 PROPERLY DECIMAL-ALIGNS A PERCENT VALUE OF FORMAT X.XX%              
*                                                                               
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         A     RF,RCONSPAM-RCONSPEL(R2)                                         
*                                  PERCENT FROM 1ST MINI-ELEMENT IN             
*                                     X'06' ELT FOR ROUNDING                    
         D     RE,RCONSPAM-RCONSPEL(R2) DIVIDE BY PERCENT                       
         SRA   RF,1                DIVIDE BY 2                                  
*                                                                               
CALCTOTX DS    0H                                                               
         ST    RF,ESTDOLS          STORE IT BACK                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   STADOLL$:  CALCULATE STATION DOLLARS FROM ESTDOLS AND                       
*      STATION PERCENT                                                          
*                                                                               
STADOLL$ NTR1                                                                   
         L     R7,0(R1)            RELOAD PERCENT                               
         SR    RE,RE                                                            
         L     RF,ESTDOLS          LOAD TOTAL MARKET DOLLARS                    
         MR    RE,R7               MULTIPLY ESTDOLS BY PERCENT                  
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         AH    RF,=H'10000'        ADD FOR ROUNDING                             
         D     RE,=F'10000'        DIVIDE FOR DEC'L SCALING                     
*                                                                               
*   10,000 PROPERLY DECIMAL-ALIGNS A PERCENT VALUE OF FORMAT X.XX%              
*                                                                               
         SRA   RF,1                DIVIDE BY 2                                  
         ST    RF,FULL             STORE FOR PASSING BACK                       
         XIT1                                                                   
         EJECT                                                                  
*      DISPLAYS VALUE IN CORRECT FIELD ON SCREEN.                               
EDITPERC EQU   *                                                                
*                                                                               
         MVC   0(8,R8),MYSPACES                                                 
*                                                                               
         LTR   R7,R7                                                            
         BZ    EPER0010                                                         
         CL    R7,NAVALUE                                                       
         BE    EPER0010                                                         
*                                                                               
         EDIT  (R7),(6,0(R8)),2    EDIT VALUE INTO SCREEN                       
         MVI   6(R8),C'%'          SLIDE IN PERCENT SIGN                        
EPER0010 EQU   *                                                                
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*   TESTDISP:   IF DDS TERMINAL, DISPLAY ADDITIONAL CONTRACT INFO               
*        IN HIDDEN FIELD 'SPLFLG'.                                              
*                                                                               
TESTDISP NTR1                                                                   
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BNE   TDIS0050            NO  - DON'T DISPLAY ANYTHING                 
         MVC   SPLSFLG,=C'.....'   SET FILLER                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'        LOOK FOR X'06' ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   TDIS0050            NO X'06' - EXIT                              
         USING RCONSPEL,R6                                                      
         TM    RCONSPES,X'08'      IS DATE COMPRESSED?                          
         BNO   TDIS0010            NO  - DON'T FLAG                             
         MVI   SPLSFLG,C'C'        YES - FLAG                                   
TDIS0010 EQU   *                                                                
         MVI   SPLSFLG+1,C'$'      FLAG  DATA AS DOLLARS                        
         TM    RCONSPES,X'04'      DATA = PERCENTS?                             
         BNO   TDIS0020            NO  - DON'T FLAG                             
         MVI   SPLSFLG+1,C'%'      FLAG  DATA AS PERCENT                        
TDIS0020 EQU   *                                                                
         TM    RCONSPES,X'20'      REPD DOLLARS OVERRIDDEN?                     
         BNO   TDIS0030            NO  - DON'T FLAG                             
         MVI   SPLSFLG+2,C'O'      FLAG OVERRIDE                                
TDIS0030 EQU   *                                                                
         TM    RCONSPES,X'40'      TOTAL DOLLARS?                               
         BNO   TDIS0040            NO  - DON'T FLAG                             
         MVI   SPLSFLG+2,C'T'      FLAG TOTAL DOLLARS                           
TDIS0040 EQU   *                                                                
         FOUT  SPLSFLGH                                                         
TDIS0050 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP R6                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
*   BEGIN EDIT OF SCREEN                                                        
*                                                                               
EDIT     DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'80',0)                                             
*                                                                               
*   ABOVE VLOAD CALLS THE T80280 MODULE, WHICH RESOLVES THE VADDRESSES          
*       OF COMMONLY-USED ROUTINES.                                              
*                                                                               
         L     R7,SAVUSING                                                      
         USING TWAWORK,R7                                                       
         MVC   KEY+28(4),TWAKADDR                                               
*                                                                               
         DROP  R7                                                               
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
*    FOLLOWING CALL TO PREVALID CHECKS TO SEE IF SPL SECTION OF                 
*      SCREEN MUST BE PROCESSED.  IF ALL PREVIOUSLY VALID BITS                  
*      ARE STILL SET, VALIDATION OF THIS SECTION IS SKIPPED.                    
*                                                                               
         GOTO1 =A(PREVALID),DMCB,(RC),SPLSMTHH,SPLSLCMH,1,0,RR=Y                
         BZ    EDTSPL5             ALL SET - SKIP SPL EDIT                      
         CLI   RCONTYPE,C'N'       TYPE N CONTRACT?                             
         BNE   EDTSPL1             NO                                           
*                                  YES - TEST FOR DATA                          
*   FOLLOWING TEST IS NECESSARY IN EVENT DATA IS ENTERED, REJECTED,             
*     THEN DELETED.  AS THE PREVALID BIT IS NOT RESET, A CONTINUOUS             
*     REJECTION RESULTS UNLESS EACH FIELD IS CHECKED.                           
*                                                                               
         GOTO1 =A(PREVALID),DMCB,(RC),SPLSMTHH,SPLSLCMH,1,1,RR=Y                
         BZ    EDTSPL5             NO DATA - SKIP SPL EDIT                      
         LA    R3,NODATAIN         SET ERROR MESSAGE                            
         LA    R2,SPLSDL1H         SET A(CURSOR)                                
         B     ERROR               ERROR OUT                                    
*                                                                               
EDTSPL1  MVI   UPDATSPL,C'Y'       SET 'UPDATE SPL' TO YES                      
         GOTO1 =A(SPLEDIT),DMCB,(RC),SCRTABLE,RR=Y                              
         BZ    EDTSPL5             CLEAN RETURN                                 
         L     R2,DUB              ERROR RETURN: RESTORE ERROR ADDR             
         L     R3,DUB+4            ERROR RETURN: RESTORE ERROR CODE             
         B     ERROR               ERROR RETURN                                 
         SPACE 3                                                                
EDTSPL5  DS    0H                                                               
         L     R7,SAVUSING         ESTABLISH ADDRESSABILITY FOR                 
         USING TWAWORK,R7             TWA FOR THIS SET OF ROUTINES              
*                                                                               
*  IS IT NECESSARY TO DELETE THE EC (SAR) KEY?                                  
*                                                                               
         CLI   DELSAR,C'Y'         FLAG SET TO DELETE?                          
         BNE   EDTSPL10            NO  -                                        
         USING RCONATYP,R5                                                      
         LA    R5,WORK                                                          
         XC    WORK(40),WORK                                                    
         MVI   RCONATYP,X'EC'                                                   
*                                                                               
         MVC   RCONACON,TWACNUM    CONTRACT NUMBER 9'S COMP.                    
         MVC   RCONAREP,REPALPHA                                                
         MVC   KEY,RCONATYP                                                     
*                                                                               
         DROP  R5                                                               
*                                                                               
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,0                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EDTSPL20                                                         
         SPACE 1                                                                
         OI    KEY+27,X'80'                                                     
         GOTO1 VWRITE                                                           
         BAS   RE,CHECK                                                         
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,58     SAR DATA DELETED                             
         LA    R2,CONBACTH                                                      
         B     EXIT                                                             
*                                                                               
* NEXT ROUTINE WRITES/REWRITES CONTRACT EC (SAR) KEY                            
*                                                                               
EDTSPL10 EQU   *                                                                
         CLI   UPDATSAR,C'Y'       WAS SAR UPDATED?                             
         BNE   EDTSPL30            NO  - DON'T REDO KEYS                        
         USING RCONATYP,R5                                                      
         LA    R5,WORK                                                          
         XC    WORK(40),WORK       CREATE POINTER                               
         MVI   RCONATYP,X'EC'                                                   
*                                                                               
         MVC   RCONACON,TWACNUM    CONTRACT NUMBER 9'S COMP.                    
         MVC   RCONAREP,REPALPHA                                                
         MVC   KEY,RCONATYP                                                     
*                                                                               
         DROP  R5                                                               
*                                                                               
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,0                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BE    EDTSPL15            ALREADY THERE                                
         SPACE 1                                                                
         MVC   KEY,KEYSAVE                                                      
*                                                                               
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VADD                                                             
         BAS   RE,CHECK                                                         
         B     EDTSPL20                                                         
         SPACE 1                                                                
EDTSPL15 CLI   KEY+27,0                                                         
         BE    EDTSPL20            NOT DELETED                                  
         MVI   KEY+27,0                                                         
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VWRITE                                                           
         BAS   RE,CHECK                                                         
         SPACE 1                                                                
EDTSPL20 DC    0H'0'                                                            
         NI    TWASTAT,X'FD'       TURN OFF 02 - DISPLAYED                      
         NI    DMINBTS,X'F7'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
EDTSPL30 EQU   *                                                                
         MVC   CONCACT,MYSPACES    TURN OFF CHANGE ACTION                       
         FOUT  CONCACTH                                                         
         CLC   DELSAR(3),=C'NNN'   ANY UPDATES AT ALL?                          
         BNE   EDTSPL40            NO  - DON'T REWRITE RECORD                   
         LA    R2,CONCACTH         NO CHANGES - SEND MESSAGE                    
         LA    R3,367              NO CHANGE TO SCREEN ERROR                    
         B     ERROR                                                            
*                                                                               
EDTSPL40 EQU   *                                                                
         MVC   KEY+28(4),TWAKADDR  RESET DISK ADDRESS OF RECORD                 
*                                                                               
         MVI   UPDATE,C'Y'         RETRIEVE FOR UPDATE                          
         GOTO1 VGETREC,DMCB,AIO4      INTO ALTERNATE AREA                       
         GOTO1 VPUTREC,DMCB,RCONREC   REWRITE FROM NEW RECORD                   
*                                                                               
*   RETRANSMIT SCREEN FIELDS                                                    
*                                                                               
         B     DISSPL              GO BACK AND REPENPLAY RECORD                 
         SPACE 1                                                                
         DROP  R7                                                               
********************************************************************            
* CLEARING SPL ELEMENT AND UPDATING 08 ELEMENT                                  
********************************************************************            
CONCLR   DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
         CLI   RCONKSTA+4,C'F'                                                  
         BE    CLRBAD                                                           
         CLI   RCONKSTA+4,C'A'                                                  
         BE    CLRBAD                                                           
         GOTO1 VDELELEM,DMCB,(6,RCONREC)                                        
         GOTO1 =A(TRUDATE),DMCB,(RC),RR=Y                                       
         GOTO1 VPUTREC,DMCB,RCONREC   REWRITE                                   
         LA    R2,CONBACTH                                                      
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDISMSG,DMCB,89     COMPETITIVE DATA HAS BEEN REMOVED            
         B     DISSPL              GO BACK AND REDISPLAY RECORD                 
CLRBAD   EQU   *                                                                
         LA    R3,679              CAN'T REMOVE COMPETITIVE DATA                
         B     ERROR               FOR RADIO STATION RECORD                     
         EJECT                                                                  
********************************************************************            
CHECK    TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
DPUNKNWN DC    CL4'UNK?'           UNKNOWN DAYPART FILLER                       
*                                                                               
GRPENTRY DS    XL1                                                              
CPPENTRY DS    XL1                                                              
         EJECT                                                                  
EDITAMT  NTR1                                                                   
         L     R3,0(R1)            RELOAD AMOUNT                                
         LTR   R3,R3                                                            
         BNZ   EAMT0010                                                         
*        MVC   0(7,R8),MYSPACES                                                 
*        B     EAMT0030                                                         
         B     EAMT0020                                                         
*                                                                               
EAMT0010 EQU   *                                                                
         CL    R3,NAVALUE                                                       
         BNE   EAMT0020                                                         
         MVC   0(7,R8),=C'N/A    '                                              
         B     EAMT0030                                                         
*                                                                               
EAMT0020 EDIT  (R3),(7,0(R8)),ALIGN=LEFT,ZERO=NOBLANK                           
EAMT0030 EQU   *                                                                
         XIT1                                                                   
         SPACE 3                                                                
EDITPRCT NTR1                                                                   
*                                                                               
         L     R3,0(R1)            RELOAD AMOUNT                                
         MVC   0(4,R8),MYSPACES                                                 
*                                                                               
         LTR   R3,R3               ANY VALUE?                                   
         BZ    EPRC0040            NO  - EXIT                                   
         CL    R3,NAVALUE          VALUE EQUAL N/A?                             
         BE    EPRC0040            YES - EXIT                                   
*                                                                               
         SR    R6,R6               CLEAR R6 FOR R6/R7 REG PAIR                  
         L     R7,FULL             RELOAD STATION $                             
         LA    R0,100              MULT STA $ BY 100                            
         MR    R6,R0                                                            
         L     R1,TOTAMT           GET MARKET TOTAL                             
         SRL   R1,1                DIVIDE TOTAMT BY 2 FOR ROUNDING              
         AR    R7,R1               HALF-ADD FOR ROUNDING                        
         SR    R0,R0               SET R0 = ZERO                                
         D     R6,TOTAMT           DIVIDE SOMETHING BY SOMETHING ELSE.          
         LTR   R7,R7                                                            
         BZ    EPRC0030                                                         
*                                                                               
         EDIT  (R7),(3,0(R8)),ALIGN=LEFT                                        
         LA    R0,3                                                             
EPRC0010 EQU   *                                                                
         CLI   0(R8),0                                                          
         BE    EPRC0020                                                         
         CLI   0(R8),C' '                                                       
         BE    EPRC0020                                                         
         LA    R8,1(R8)                                                         
         BCT   R0,EPRC0010                                                      
EPRC0020 EQU   *                                                                
         MVI   0(R8),C'%'                                                       
         B     EPRC0040                                                         
*                                                                               
EPRC0030 EQU   *                                                                
         MVI   0(R8),C'<'                                                       
*                                                                               
EPRC0040 EQU   *                                                                
         B     EAMT0030                                                         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*        LOCAL VARIABLES - MUST BE OFF SECOND BASE REGISTER                     
*           THE MACRO BELOW IS USED TO MAKE SURE THE VARIBLES END               
*            UP WHERE THEY SHOULD                                               
*                                                                               
LCHERE   EQU   *-T80272                                                         
LCTHERE  EQU   X'1000'                                                          
         AIF   (LCTHERE LT LCHERE).NOORG                                        
         ORG   T80272+X'1000'                                                   
.NOORG   ANOP                                                                   
*                                                                               
NAVALUE  DC    F'-1'                                                            
NAPACKED DC    PL8'-1'                                                          
TOTAMT   DC    F'0'                                                             
SAVASCRN DS    A                   SAVE FOR A(SCREEN FIELD)                     
SAVUSING DS    A                   SAVE FOR TWA 'USING' REGISTER                
ESTDOLS  DC    F'0'                                                             
ATWAWORK DS    A                                                                
COMMFLAG DC    C'N'                COMMENT FLAG                                 
DELSAR   DC    C'N'                DELETE SAR FLAG                              
UPDATSAR DC    C'N'                UPDATE SAR FLAG                              
UPDATSPL DC    C'N'                UPDATE SPL FLAG                              
FORSPRED DC    C'N'                SPREAD FORECAST DOLLARS                      
LOCALFLT DS    CL6                 REVISED HEADER FLIGHT DATES                  
*                                                                               
*  DON'T SEPARATE THREE FIELDS:  DELSAR, UPDATSAR, UPDATSPL                     
*                                                                               
         DS    0F                                                               
*                   .0.0.0.0.0.0.0.0.0.1.1.1                                    
*                   .1.2.3.4.5.6.7.8.9.0.1.2                                    
NEW08ELT DC    XL12'080C00000000000000000000'                                   
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
*                                                                               
*  THE SCREEN FOR COMBINED SAR/SPL OVERLAYS THE ENTIRE CONTRACT                 
*    SCREEN, AND IS ORG'D OVER THE CONTRACT ACTION INPUT FIELD                  
*                                                                               
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTD4D                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       EJECT                                                                    
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    XL3                 BINARY MONTH START DATE                      
BRDEND   DS    XL3                 BINARY MONTH END   DATE                      
BRDWEEKS DS    XL1                 NUM WEEKS IN PERIOD                          
BRDLEN   EQU   *-BRDSTART          LENGTH OF ENTRY                              
         EJECT                                                                  
*                                                                               
* IMPORT COMMENT TXT FROM OTHER CONTRACT                                        
*                                                                               
         CSECT                                                                  
GETCMT   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'SPL=',8(R2)       HAVE 'SPL=NNNNNNNN' SYNTAX?                 
         BE    GCMT010              YES                                         
         SR    R0,R0                NO - SET CC                                 
         B     EXXMOD               BYE                                         
*                                                                               
GCMT010  DS    0H                                                               
         GOTO1 SCANNER,DMCB,(R2),AIO4                                           
         LA    R3,2                                                             
         CLI   4(R1),1                                                          
         BNE   ERROR                                                            
         L     R4,AIO4              SCANNER OUTPUT                              
         TM    3(R4),X'80'         NUMERIC?                                     
         BZ    ERROR                                                            
*                                                                               
         XC    KEY,KEY              LOOKUP CONTRACT                             
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),REPALPHA                                               
         GOTOX (RFCONNUM,VREPFACS),DMCB,(7,8(R4)),(2,KEY+23)                    
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,AIO4                                                
*                                                                               
         L     R6,AIO4                                                          
         CLC   RCONKCON,23(R6)     SAME K?                                      
         BE    ERROR                                                            
*                                                                               
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BE    GCMT015                                                          
         LA    R3,758              NO CMT ON SOURCE K                           
         B     ERROR                                                            
GCMT015  DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(7,RCONREC)                                        
GCMT020  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,0(R6)                                      
         BAS   RE,NEXTEL                                                        
         BE    GCMT020                                                          
         LTR   RB,RB                                                            
         B     EXXMOD                                                           
         LTORG                                                                  
*                                                                               
*   BEGIN EDIT OF SPL PORTION OF SCREEN                                         
*                                                                               
         CSECT                                                                  
SPLEDIT  NMOD1 0,*SPED*                                                         
         L     RC,0(R1)            RELOAD A(WORKSPACE)                          
         MVC   SAVSTATB,4(R1)      SAVE A(STATION TABLE)                        
*                                                                               
*   'CLOSE-AFTER' DATE IS INTERNAL ON SCREEN.  IF IT IS THE ONLY                
*     ITEM ON THE SPL SCREEN TO HAVE BEEN CHANGED, IT MUST BE TREATED           
*     ALONE.  THEREFORE, FIELDS BEFORE/AFTER 'CLOSE-AFTER' ARE CHECKED          
*     FOR 'PREV VALID'.                                                         
         GOTO1 =A(PREVALID),DMCB,(RC),SPLSMTHH,SPLSTRUH,1,0,RR=Y                
         BNZ   ESPL0010            ONE OF FIELDS CHANGED                        
         GOTO1 =A(PREVALID),DMCB,(RC),SPLSST1H,SPLSFLGH,1,0,RR=Y                
         BNZ   ESPL0010            ONE OF FIELDS CHANGED                        
         GOTO1 =A(PREVALID),DMCB,(RC),SPLSCM1H,SPLSLCMH,1,0,RR=Y                
         BNZ   ESPL0500            ONE OF COMMENT FIELDS CHANGED                
         BAS   RE,CLOSAFTR         PROCESS 'CLOSE-AFTER' ONLY                   
         BZ    SPLEXMOD            CLEAN RETURN - PASS BACK CC=ZERO             
         LA    R2,SPLSACLH         ERROR - PASS BACK A(CLOSE AFTER)             
         B     ERRX0370                                                         
*                                                                               
ESPL0010 DS    0H                                                               
*        TEST IF RECORD CAN BE CHANGED                                          
         MVC   NEW08ELT+5(3),TODAY                                              
*                                  INITIALIZE NEW X'08' ELEMENT                 
         SR    R3,R3                                                            
         LA    R2,RCONELEM                                                      
ESPL0020 IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    ESPL0030            YES - NO SPL ELEMENT                         
         CLI   0(R2),6             SPL ELEMENT?                                 
         BNE   ESPL0020            NO  - GO BACK FOR NEXT                       
*                                                                               
         L     R7,SAVUSING                                                      
         USING TWAWORK,R7                                                       
*                                                                               
         CLC   2(2,R2),TWASPLMN    DATE CHECK: ONLY DDS TERMINAL                
*                                    CAN UPDATE EARLIER MONTH                   
         DROP  R7                                                               
*                                                                               
         BNL   ESPL0030                                                         
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    ESPL0030            YES                                          
*                                                                               
         LA    R3,SPLERR1          NO  - ERROR                                  
         LA    R2,CONBACTH                                                      
         B     SPLERROR                                                         
*                                  GET STATION RECORD                           
ESPL0030 EQU   *                                                                
*                                                                               
ESPL0040 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         GOTO1 VHIGH                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                  SET UP STATION LIST                          
         L     R5,SAVSTATB                                                      
         USING SCRFLD01,R5                                                      
*                                                                               
         MVC   SCRSTA01(5),RSTAKSTA    LOAD REPPED STATION                      
         LA    R3,SCRSTA02         A(1ST COMPETITIVE STATION)                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         SR    R2,R2                                                            
         LA    R4,RSTAELEM         SCAN FOR X'02' ELEMENTS                      
         LA    R5,1                SET NUMBER OF STATIONS TO 1                  
ESPL0050 CLI   0(R4),0             END OF RECORD?                               
         BE    ESPL0080            YES                                          
         CLI   0(R4),2             COMPETING STA ELEMENT?                       
         BE    ESPL0070            YES                                          
ESPL0060 IC    R2,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,R2                                                            
         B     ESPL0050                                                         
*                                                                               
ESPL0070 MVC   0(5,R3),2(R4)       INSERT COMPETING STATION IN TABLE            
         LA    R3,LSCRNTRY(R3)     BUMP TABLE                                   
         LA    R5,1(R5)            ADD TO NUMBER OF STATIONS                    
         B     ESPL0060            GO BACK FOR NEXT ELEMENT                     
         SPACE 1                                                                
ESPL0080 XC    WORK2,WORK2         BUILD NEW ELEMENT IN WORK2                   
         OI    WORK2+RCONSPES-RCONSPEL,X'04'                                    
*                                  FLAG AS 'PERCENT FORMAT'                     
         STC   R5,STLNUM           SAVE NUMBER OF STATIONS                      
         MVI   WORK2,6             INSERT ELEMENT CODE                          
         STC   R5,WORK2+8          INSERT # STATIONS (MINI-ELTS)                
         MH    R5,=H'9'            # X SIZE                                     
         LA    R5,9(R5)            ADD L(REMAINING ELEMENT)                     
         STC   R5,WORK2+1          INSERT TOTAL LENGTH                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(2,WORK2+2)                                 
*                                  INSERT TODAY'S DATE (COMPRESSED)             
*                                     INTO SPL DATE                             
         OI    WORK2+4,X'08'       TURN ON NEW DATE FORMAT FLAG                 
         MVC   WORK2+5(3),TODAY    INITIALIZE ENTRY DATE W TODAY                
*                                                                               
         LA    R2,SPLSACLH         AUTOCLOSE DATE FIELD                         
         CLI   5(R2),0             ANY INPUT?                                   
         BE    ESPL0090            NO  -                                        
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB        ACCEPTED?                                    
         BZ    ERRX0370            NO  - ERROR IN DATE INPUT                    
         GOTO1 DATCON,DMCB,WORK,(3,NEW08ELT+2)                                  
*                                  INSERT AUTOCLOSE INTO X'08' ELEMENT          
         LA    R6,RCONREC          LOOK FOR X'08' ELEMENT                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   ESPL0090            NOT FOUND                                    
*                                                                               
         USING RCONACEL,R6                                                      
         MVC   RCONACAC,NEW08ELT+2 INSERT AUTOCLOSE DATE                        
         DROP  R6                                                               
ESPL0090 EQU   *                                                                
         XC    PCTAMT,PCTAMT                                                    
         XC    WORK3(28),WORK3     AMOUNTS                                      
         MVI   EMPBIT,C'N'                                                      
         ZIC   R1,STLNUM                                                        
         L     RF,SAVSTATB         POINT TO FIRST FIELD IN ADDRLIST             
         ST    RF,SAVASCRN         SAVE A(ADDRLIST)                             
         LA    R3,WORK3                                                         
         SPACE 2                                                                
ESPL0100 EQU   *                                                                
         L     RF,SAVASCRN         RELOAD A(ADDRLIST)                           
         L     R2,4(RF)            A(HDR OF FIELD ON SCREEN)                    
         AR    R2,RA               SET ADDRESSABILITY                           
         ZIC   R0,0(R2)            SKIP OVER STATION CALL FIELD                 
         AR    R2,R0                  SET A(AMOUNT FIELD)                       
ESPL0110 EQU   *                   RE-ENTRY FOR TOTAL FIELD BRANCH              
         SR    R4,R4                                                            
         IC    R4,5(R2)            INPUT LENGTH                                 
         BCTR  R4,0                                                             
         CLI   8(R2),C'P'          'P'ERCENT?                                   
         BE    ESPL0140            YES - PROCESS PERCENT INPUT                  
         CLI   5(R2),0             ANY INPUT IN FIELD?                          
         BE    ESPL0160            NO  - ALLOW 1 CHARACTER                      
         CLC   8(3,R2),=C'N/A'     'N/A' ?                                      
         BNE   ESPL0130            NO  - CONTINUE                               
         L     RF,SAVSTATB         YES - IT CAN'T BE ON THE FIRST               
*                                     ENTRY, WHICH IS OUR STATION               
         L     R0,0(RF)            A(FIELD WITH N/A)                            
         AR    R0,RA               SET ADDRESSABILITY                           
         CR    R0,R2               IS IT FIRST FIELD?                           
         BE    ERRX0380            YES - ERROR                                  
         USING COMFACSD,R3                                                      
         L     R3,ACOMFACS                                                      
         XC    WORK,WORK           CHECK CONTROL PROFILE FOR NA USE             
         MVC   WORK+16(4),=C'R0CO'                                              
         MVC   WORK+20(2),REPALPHA                                              
         GOTO1 CGETPROF,DMCB,WORK+16,WORK,CDATAMGR                              
         DROP  R3                                                               
         CLI   WORK+1,C'Y'                                                      
         BNE   ERRX0390            REP DOES NOT ALLOW USE OF NA                 
         LA    R3,WORK3            LOOP THRU REST OF THE STATION                
         SR    R1,R1                VALUES AND SET TO -1                        
         IC    R1,STLNUM                                                        
         BCTR  R1,0                                                             
ESPL0120 EQU   *                                                                
         LA    R3,4(R3)            INSERT 'NA VALUE' INTO ALL                   
         MVC   0(4,R3),NAVALUE        OTHER COMPETITIVE STATIONS                
         BCT   R1,ESPL0120                                                      
         LA    R3,4(R3)                                                         
         LA    R2,SPLSTTLH         SET TO TOTAL FIELD                           
         B     ESPL0110                                                         
ESPL0130 EQU   *                                                                
         TM    4(R2),X'08'         VALID NUMERIC                                
         BNO   ERRX0370            NOT NUMERIC: ERROR                           
         EX    R4,*+8              MOVE VALUE BY LENGTH                         
         B     *+10                                                             
*                                                                               
         MVC   WORK(0),8(R2)       EXECUTED                                     
*                                                                               
         EX    R4,*+8              PACK FIELD                                   
         B     *+10                                                             
*                                                                               
         PACK  DUB,WORK(0)         EXECUTED                                     
*                                                                               
         CVB   R0,DUB              CONVERT VALUE TO BINARY                      
         ST    R0,0(R3)            STORE VALUE IN WORK AREA                     
         B     ESPL0170            GO TO GET NEXT FIELD                         
         SPACE 1                                                                
ESPL0140 EQU   *                   EDIT 'PERCENT' INPUT                         
         CLI   5(R2),1             INPUT = ONLY 'P'?                            
         BE    ERRX0370            YES - ERROR                                  
         CLI   5(R2),3             INPUT = 'P' + > 2 CHARS?                     
         BH    ERRX0370            YES - ERROR                                  
*                                                                               
         LR    R8,R4               LOAD INPUT LENGTH                            
         LA    R6,9(R2)            A('P'-CHAR +1)                               
         EX    R8,*+12             INPUT = 'P00'?                               
         BE    ERRX0370            YES - ERROR                                  
         B     *+10                                                             
*                                                                               
         CLC   8(0,R2),=C'P00'     EXECUTED                                     
*                                                                               
ESPL0150 CLI   0(R6),X'F0'         TEST NUMERIC                                 
         BL    ERRX0370                                                         
         CLI   0(R6),X'F9'                                                      
         BH    ERRX0370                                                         
*                                                                               
         LA    R6,1(R6)            A(NUMERIC VALUE OF PERCENT)                  
         BCT   R8,ESPL0150         GO BACK FOR NEXT CHARACTER                   
         BCTR  R4,0                SUBTRACT 1 MORE FROM LENGTH                  
         EX    R4,*+8              MOVE NUMERIC PORTION OUT                     
         B     *+10                                                             
*                                                                               
         MVC   HALF(0),9(R2)       EXECUTED                                     
*                                                                               
         EX    R4,*+8              PACK FIELD                                   
         B     *+10                                                             
*                                                                               
         PACK  DUB,HALF(0)         EXECUTED                                     
*                                                                               
         CVB   R5,DUB              CONVERT VALUE TO BINARY                      
         A     R5,PCTAMT           ADD PERCENT ACCUMULATOR                      
         CH    R5,=H'100'          CHECK FOR 100%                               
         BH    ERRX0340            EXCEEDED - ERROR                             
         ST    R5,PCTAMT           STORE IT BACK                                
         MVC   0(4,R3),8(R2)       MOVE SCREEN VALUE TO WORK AREA               
         B     ESPL0170            GO GET NEXT FIELD                            
         SPACE 2                                                                
ESPL0160 MVI   0(R3),C'X'          INDICATE FIELD IS BLANK                      
         CLI   EMPBIT,C'Y'         ONLY ONE FIELD CAN BE BLANK                  
         BE    ERRX0350            BLANK FIELD ALREADY ENTERED: ERROR           
         MVI   EMPBIT,C'Y'         SET 'BLANK FIELD ENTERED'                    
         SPACE 2                                                                
ESPL0170 LTR   R1,R1               ANY STATIONS?                                
         BZ    ESPL0180            NO  -                                        
         LA    R3,4(R3)            NEXT AMT IN WORK AREA                        
         L     RF,SAVASCRN         RELOAD A(A(SCREEN FIELD))                    
         LA    RF,LSCRNTRY(RF)     ADD L(ONE SET OF SCREEN FLDS)                
         ST    RF,SAVASCRN         RESAVE IT                                    
         BCT   R1,ESPL0100         GO BACK FOR NEXT FIELD                       
*                                                                               
         LA    R2,SPLSTTLH         PROCESS VALUE IN TOTAL FIELD                 
         CLI   8(R2),C'P'          ANY 'P'ERCENT IN TOTAL?                      
         BE    ERRX0400            YES - ERROR                                  
         B     ESPL0110            NO  - EDIT IT                                
         EJECT                                                                  
ESPL0180 LA    R2,SPLSTTLH         A(TOTAL HEADER)                              
         XC    SAVETOT,SAVETOT     INITIALIZE WORK AREA                         
         CLI   5(R2),0             ANY INPUT?                                   
         BE    ESPL0190            NO                                           
         MVC   SAVETOT,0(R3)       STILL POINTING TO TOT                        
*                                                                               
ESPL0190 OC    PCTAMT,PCTAMT       ANY PERCENTAGES ENTERED?                     
         BNZ   ESPL0270            YES                                          
         IC    R1,STLNUM           NO  - RESET STATION COUNT                    
         CLI   0(R3),C'X'          ANY TOTAL ENTERED?                           
         BE    ESPL0240            NO                                           
*                                  TOTALS ENTERED WITH NO PERCENTS              
         LA    R3,WORK3            RESET A(1ST BUCKET)                          
         SR    R5,R5                                                            
         XR    R6,R6                                                            
ESPL0200 EQU   *                                                                
         CLI   0(R3),C'X'          FIELD ENTERED AS BLANK?                      
         BNE   ESPL0210            NO  - TEST FOR N/A VALUE                     
         LR    R6,R3               SAVE ADDRESS OF BLANK FIELD                  
         B     ESPL0220                                                         
ESPL0210 EQU   *                                                                
         CLC   0(4,R3),NAVALUE     FIELD SET TO N/A?                            
         BE    ESPL0220            YES                                          
         A     R5,0(R3)            NO  - ACCUMULATE FOR TOTAL                   
ESPL0220 EQU   *                                                                
         LA    R3,4(R3)            BUMP TO NEXT BUCKET                          
         BCT   R1,ESPL0200         GO BACK FOR NEXT                             
         L     RF,0(R3)            LOAD TOTAL                                   
         SR    RF,R5               SUBTRACT CALC'D VALUE FROM TOTAL             
         LTR   R6,R6               ANY BLANK FIELD?                             
         BZ    ESPL0230            NO FIELD BLANK                               
         ST    RF,0(R6)            STORE DIFFERENCE AT BLANK FIELD              
ESPL0230 EQU   *                                                                
         LTR   RF,RF               IS DIFFERENCE NEGATIVE?                      
         BM    ERRX0360            NEGATIVE = ERROR                             
         B     ESPL0340                                                         
         SPACE 2                                                                
ESPL0240 EQU   *                                                                
         SR    R5,R5               NO TOTAL OR PERCENTS ENTERED                 
         LA    R3,WORK3            A(1ST BUCKET)                                
         SR    R1,R1                                                            
         IC    R1,STLNUM           SET LOOP CONTROL = # STATIONS                
*                                                                               
ESPL0250 EQU   *                                                                
         CLC   0(4,R3),NAVALUE     BUCKET SET TO N/A VALUE?                     
         BE    ESPL0260            YES - DON'T ACCUMULATE                       
         A     R5,0(R3)            ACCUMULATE                                   
ESPL0260 EQU   *                                                                
         LA    R3,4(R3)            BUMP TO NEXT BUCKET                          
         BCT   R1,ESPL0250         GO BACK FOR NEXT                             
         ST    R5,0(R3)            STORE TOTAL IN BUCKET                        
         B     ESPL0340                                                         
         SPACE 2                                                                
ESPL0270 EQU   *                   PERCENTAGES HAVE BEEN ENTERED                
         IC    R1,STLNUM           SET LOOP CONTROL = # BUCKETS                 
         CLI   0(R3),C'X'          IS THERE A TOTAL?                            
         BE    ESPL0310            NO  - 'X' INDICATES BLANK FIELD              
*                                  YES TOTAL/ YES PCT                           
         L     R6,0(R3)            LOAD TOTAL FIELD                             
         LA    R3,WORK3            SET A(1ST FIELD                              
*                                  CHECK EACH FIELD FOR PERCENT!                
*                                     AND CONVERT IT TO DOLLARS                 
ESPL0280 CLI   0(R3),C'P'          FIELD CONTAINS 'P'ERCENT?                    
         BNE   ESPL0290            NO  -                                        
         MVC   HALF,1(R3)          YES - MOVE OUT NUMERIC                       
         CLI   HALF+1,X'40'        2ND BYTE = SPACE?                            
         BH    *+14                NO                                           
         MVC   HALF+1(1),HALF      YES - SHIFT 1ST BYTE DOWN                    
         MVI   HALF,X'F0'          STUFF C'0' INTO 1ST BYTE                     
         PACK  DUB,HALF            PACK PERCENT                                 
         CVB   R5,DUB              CONVERT PERCENT TO BINARY                    
         MR    R4,R6               MULTIPLY TOTAL BY PERCENT                    
         SLDA  R4,1                DOUBLE VALUE FOR ROUNDING                    
         D     R4,=F'100'                                                       
         AH    R5,=H'1'                                                         
         SRL   R5,1                                                             
         ST    R5,0(R3)            STORE CALC'D VALUE BACK                      
*                                                                               
ESPL0290 LA    R3,4(R3)            BUMP TO NEXT BUCKET                          
         BCT   R1,ESPL0280         DO EACH STATION                              
*                                                                               
         LA    R3,WORK3            RESET A(1ST BUCKET)                          
         SR    R5,R5                                                            
         XR    RF,RF                                                            
         IC    R1,STLNUM           RESET LOOP CONTROL = # STATIONS              
ESPL0300 CLI   0(R3),C'X'          FIELD CONTAINS BLANK?                        
         BNE   *+10                NO                                           
         LR    RF,R3               YES - SET BLANK FIELD FLAG                   
         B     *+8                 SKIP ADD OF VALUE                            
         A     R5,0(R3)            ADD VALUE IN BUCKET                          
         LA    R3,4(R3)            BUMP BUCKET                                  
         BCT   R1,ESPL0300         ACCUMULATE EACH BUCKET                       
*                                                                               
         L     R3,0(R3)            LOAD TOTAL                                   
         SR    R3,R5               SUBTRACT SUM OOF VALUES FROM TOTAL           
         LTR   RF,RF               NO FIELD BLANK                               
         BZ    *+8                                                              
         ST    R3,0(RF)            STORE DIFFERENCE IN BLANK                    
         LTR   R3,R3               CHECK DIFFERENCE FOR NEGATIVE                
         BM    ERRX0360            NEGATIVE = ERROR                             
         B     ESPL0340                                                         
*        SPACE 2                   PERCENTAGES HAVE BEEN ENTERED,               
*                                     BUT NO TOTAL                              
ESPL0310 LA    R3,WORK3            A(1ST BUCKET)                                
         SR    R7,R7                                                            
         IC    R1,STLNUM           SET LOOP CONTROL = # STATIONS                
*                                                                               
         CLI   0(R3),C'P'          FIELD CONTAINS 'P'ERCENT?                    
         BE    *+8                 YES                                          
         A     R7,0(R3)            NO  - ACCUMULATE VALUE                       
         LA    R3,4(R3)            BUMP TO NEXT BUCKET                          
         BCT   R1,*-16             GO BACK FOR NEXT                             
         LTR   R7,R7               NEED AT LEAST ONE DOLLAR FIELD               
         BZ    ERRX0360            NO DOLLARS = ERROR                           
*                                                                               
         L     R4,=F'100'          CALCULATE DOLLARS OF REMAINDER               
         S     R4,PCTAMT           100% - % ENTERED                             
         LTR   R4,R4               ANY VALUE?                                   
         BZ    ERRX0360            NO  - ERROR                                  
         M     R6,=F'100'          MULTIPLY DOLLARS FOR ROUNDING                
         SLDL  R6,1                DOUBLE                                       
         DR    R6,R4               DIVIDE BY PERCENT                            
         AH    R7,=H'1'            HALF-ROUND                                   
         SRL   R7,1                DIVIDE BY TWO                                
*                                                                               
         IC    R1,STLNUM           RESET LOOP CONTROL = # STATIONS              
         LA    R3,WORK3            SET A(1ST BUCKET)                            
         SR    R8,R8                                                            
*                                                                               
ESPL0320 CLI   0(R3),C'P'                                                       
         BE    *+12                                                             
         A     R8,0(R3)                                                         
         B     ESPL0330                                                         
         MVC   HALF,1(R3)                                                       
         CLI   HALF+1,X'40'                                                     
         BH    *+14                                                             
         MVC   HALF+1(1),HALF                                                   
         MVI   HALF,X'F0'                                                       
         PACK  DUB,HALF                                                         
         CVB   R5,DUB                                                           
         MR    R4,R7                                                            
         SLDA  R4,1                                                             
         D     R4,=F'100'                                                       
         AH    R5,=H'1'                                                         
         SRL   R5,1                                                             
         AR    R8,R5                                                            
         ST    R5,0(R3)                                                         
*                                                                               
ESPL0330 LA    R3,4(R3)                                                         
         BCT   R1,ESPL0320                                                      
         ST    R8,0(R3)                                                         
         B     ESPL0340                                                         
         SPACE 3                                                                
*                                                                               
ESPL0340 EQU   *                                                                
         OC    SAVETOT,SAVETOT     ANY TOTAL ON SCREEN?                         
         BZ    ESPL0380            NO                                           
         SR    R1,R1               YES - ENSURE NEW = OLD                       
         IC    R1,STLNUM           SET LOOP CONTROL = # STATIONS                
         SR    R5,R5                                                            
         LA    R3,WORK3            A(1ST BUCKET)                                
ESPL0350 EQU   *                                                                
         CLC   0(4,R3),NAVALUE     BUCKET SET TO N/A VALUE?                     
         BE    ESPL0360            YES - DON'T ACCUMULATE                       
         A     R5,0(R3)            ACCUMULATE TOTAL                             
ESPL0360 EQU   *                                                                
         LA    R3,4(R3)            BUMP TO NEXT BUCKET                          
         BCT   R1,ESPL0350         GO BACK FOR NEXT                             
         C     R5,SAVETOT          SUM OF BUCKETS = TOTAL?                      
         BE    ESPL0380            YES                                          
         LA    R3,BALERR           NO  - ERROR                                  
         LA    R2,SPLSDL1H                                                      
ESPL0370 EQU   *                                                                
         B     SPLERROR                                                         
ESPL0380 EQU   *                                                                
         OC    WORK3(4),WORK3      ANY DOLLARS IN REP'D STATION?                
         BNZ   ESPL0390            YES                                          
         BAS   RE,NOREPD$$         NO                                           
         BZ    ESPL0400            NO ERROR                                     
         LA    R3,ORDRAWIN         ERROR:  ORDER A WIN                          
         LA    R2,SPLSDL1H                                                      
         B     SPLERROR                                                         
ESPL0390 EQU   *                                                                
         BAS   RE,TESTREPD         COMPARE REP'D STATION DOLLARS                
ESPL0400 EQU   *                                                                
         L     R5,SAVSTATB                                                      
         USING SCRFLD01,R5                                                      
         LA    R5,SCRSTA01         BUILD NEW WORK ELEMENT                       
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R3,WORK3                                                         
         IC    R1,STLNUM           SET LOOP CONTROL = # STATIONS                
         LA    R4,WORK2+9                                                       
         BAS   RE,ADDTOTAL         CALCULATE TOTAL: RETURNED IN                 
*                                     'FULL'                                    
*                                                                               
ESPL0410 MVC   0(5,R4),0(R5)       STATION                                      
         OC    0(5,R4),MYSPACES    MAKE SURE BAND IS BLANK                      
         BAS   RE,CALCPERC         CALCULATE PERCENTAGE: RETURNED               
*                                     IN 'DUB'                                  
         MVC   5(4,R4),DUB         AMOUNT                                       
         LA    R3,4(R3)                                                         
         LA    R4,9(R4)                                                         
         LA    R5,LSCRNTRY(R5)                                                  
         BCT   R1,ESPL0410                                                      
*                                                                               
         LA    R2,SPLSESTH         VALIDATE 'ESTIMATE' FLAG                     
         LA    R3,INVINP                                                        
         OI    8(R2),C' '          'OR' IN SPACES                               
         CLI   8(R2),C' '          ANYTHING IN FIELD?                           
         BE    ESPL0420            NO  - SKIP IT                                
         CLI   8(R2),C'E'          YES - IS IT AN ESTIMATE?                     
         BNE   SPLERROR            NO  - IT'S AN ERROR, THEN                    
         OI    WORK2+4,X'01'       TURN ON 'ESTIMATE' FLAG                      
*                                                                               
ESPL0420 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   ESPL0480                                                         
         NI    WORK2+4,X'F7'       TURN OFF DATE FORMAT BIT                     
         MVC   WORK2+2(2),2(R6)    LOAD SPL DATE FROM                           
*                                     OLD SPL ELEMENT                           
         TM    4(R6),X'08'         TEST DATE FORMAT BIT                         
         BNO   ESPL0430            NOT ON = OLD FORMAT                          
         OI    WORK2+4,X'08'       ON  - NEW FORMAT                             
ESPL0430 EQU   *                                                                
         USING RCONSPEL,R7                                                      
         LA    R7,WORK2            NEW ELEMENT                                  
         OC    5(3,R6),5(R6)                                                    
         BZ    ESPL0470            NO OLD SPL ENTRY DATE                        
         TM    4(R6),X'10'                                                      
         BO    ESPL0440            SPL DATA PREVIOUSLY UPDATED                  
         GOTO1 DATCON,DMCB,(3,5(R6)),DUB    FIND THE MONDAY AT THE              
         GOTO1 GETDAY,(R1),DUB,FULL         BEGINNING OF THE OLD SPL            
         CLC   FULL(3),MYSPACES             ENTRY DATE WEEK                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R4,DMCB                                                          
         BCTR  R4,0                                                             
         LNR   R4,R4                                                            
         GOTO1 ADDAY,(R1),DUB,WORK,(R4)                                         
         GOTO1 DATCON,(R1),WORK,(2,FULL)                                        
         CLC   MONDATE,FULL        COMPARE MONDAYS                              
         BE    ESPL0470            EQUAL - STILL IN FIRST WEEK, ELSE...         
*                                                                               
ESPL0440 EQU   *                                                                
         LA    R2,SPLSCM1H         CHECK FOR MANDATORY NEW COMMENT              
         CLI   5(R2),0                                                          
         BE    ERRX0330            ERROR - NEW COMMENT REQUIRED                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BNE   ESPL0460                                                         
         SR    R3,R3                                                            
*                                                                               
ESPL0450 IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),2(R6)       COMPARE NEW COMMENT TO OLD COMMENT           
         BNE   ESPL0460            NOT EQUAL - OK                               
         LA    R2,NEXTCOMT(R2)     BUMP TO NEXT COMMENT LINE                    
         CLI   0(R2),0                                                          
         BE    ERRX0330                                                         
         CLI   5(R2),0                                                          
         BE    ERRX0330                                                         
         BAS   RE,NEXTEL                                                        
         BE    ESPL0450                                                         
*                                                                               
ESPL0460 OI    RCONSPES,X'10'      FLAG SPL DATA UPDATED                        
         DROP  R7                                                               
         L     R7,SAVUSING                                                      
         USING TWAWORK,R7                                                       
*                                                                               
         XC    KEY,KEY        ADD PASSIVE POINTER FOR SAR TO DIRECTORY          
         MVI   KEY,X'EC'                                                        
         MVC   KEY+21(2),RCONKREP                                               
         MVC   KEY+23(4),TWACNUM                                                
         MVC   KEY+28(4),TWAKADDR                                               
         OC    TWAKADDR,TWAKADDR   DISK ADDRESS OKAY?                           
         BNZ   *+6                 YES                                          
         DC    H'0'                NO  - DUMP IT OUT                            
*                                                                               
         DROP  R7                                                               
*                                                                               
         MVI   DMOUTBTS,X'DF'                                                   
         GOTO1 VADD                                                             
*                                  DELETE SPL ELEMENT                           
ESPL0470 GOTO1 VDELELEM,DMCB,(X'06',RCONREC)                                    
*                                  ADD    SPL ELEMENT                           
ESPL0480 GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         SPACE 1                                                                
         CLI   RCONKSTA+4,C'F'     RADIO COMMENT - MUST KEEP                    
         BE    ESPL0490                                                         
         CLI   RCONKSTA+4,C'A'     RADIO COMMENT - MUST KEEP                    
         BE    ESPL0490                                                         
         CLI   RCONKSTA+4,C'C'     RADIO COMMENT - MUST KEEP                    
         BE    ESPL0490                                                         
ESPL0490 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   ESPL0500                                                         
         OI    4(R6),X'04'         SPL UPDATED                                  
         EJECT                                                                  
ESPL0500 LA    R2,SPLSCM1H         COMMENTS                                     
         OC    SPLSCM1(L'MOVESTR),=CL8' '                                       
         CLC   SPLSCM1,MOVESTR     COMMENT MOVE REQUEST?                        
         BNE   ESPLCMVX            NO  SKIP IT                                  
         EJECT                                                                  
**************************************                                          
** MOVE THE PENDING COMMENTS TO SPL **                                          
**************************************                                          
ESPLCMV  DS    0H                                                               
         LA    R2,SPLSCM1H         CLEAR                                        
*                                                                               
ESPLCMV2 MVI   5(R2),0                                                          
         XC    8(L'SPLSCM1,R2),8(R2)                                            
         LA    R2,NEXTCOMT(R2)     YES - BUMP TO NEXT HEADER                    
         LA    R0,SPLSLCMH         CHECK EACH HEADER                            
         CR    R2,R0               PAST LAST COMMENT?                           
         BNH   ESPLCMV2            NO - CONTINUE                                
*                                                                               
         LA    R6,RCONREC          GET PENDING COMMENTS                         
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   ESPLCMVX                                                         
*                                                                               
******************************                                                  
** DISPLAY PENDING COMMENTS **                                                  
******************************                                                  
         LA    R2,SPLSCM1H         COMMENTS                                     
ESPLCMV4 MVI   WORK3,C' '                                                       
         MVC   WORK3+1(L'WORK3-1),WORK3                                         
         ZIC   RE,1(R6)            GET L(COMMENT ELEMENT)                       
         SH    RE,=H'3'            MINUS CONTROL +1 FOR 'EX'                    
         EX    RE,*+8              MOVE IT BY LENGTH TO WORK3                   
         B     *+10                                                             
         MVC   WORK3(0),2(R6)                                                   
         LA    RE,1(RE)                                                         
         STC   RE,5(R2)                                                         
         SPACE 1                                                                
         ZIC   R1,0(R2)            GET L(SCREEN FIELD)                          
         TM    1(R2),X'02'         EXTENDED FIELD HEADER PRESENT?               
         BZ    *+8                                                              
         SH    R1,=H'8'            YES - SUBTRACT 8                             
         SH    R1,=H'9'            MINUS FIELD HDR +1 FOR 'EX'                  
         BNP   ESPLCMVX            NEGATIVE?  DON'T MOVE IT                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK3                                                    
*                                                                               
         LA    R2,SPLSCM2H-SPLSCM1H(R2)      NEXT COMMENT                       
         LA    R0,SPLSLCMH                   LAST COMMENT ON SCREEN             
         CR    R2,R0                         PAST IT?                           
         BH    *+12                          YES - ALL DONE                     
         BAS   RE,NEXTEL                     NO - READ NEXT                     
         BE    ESPLCMV4                                                         
*                                                                               
ESPLCMVX DS    0H                                                               
         LA    R2,SPLSCM1H         COMMENTS                                     
         EJECT                                                                  
*                                                                               
* GET BOP COMMENT - ( DOUBLES AS SAR COMMENT IF TV CONTRACT)                    
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   ESPL0501                                                         
         GOTO1 VDELELEM,DMCB,(X'11',RCONREC)                                    
*                                                                               
ESPL0501 CLI   5(R2),0          COMMENT REQUIRED IF REP STATION IS $0           
         BNE   ESPL0510                                                         
         LA    R2,SPLSDL1H        REP STATION AMOUNT FIELD                      
         ZIC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'0000000'                                              
         BE    ERRX0320            COMMENT REQUIRED WHEN STATION=$0             
         LA    R2,SPLSCM1H         COMMENTS                                     
*                                                                               
ESPL0510 DS    0H                                                               
         GOTO1 =A(GETCMT),RR=YES   COPY CMT FROM OTHER K?                       
         BNZ   ESPL0575                                                         
*                                                                               
         SR    R4,R4                                                            
*                                                                               
ESPL0520 TM    4(R2),X'20'         PREVIOUSLY VALID?                            
         BNO   ESPL0530            NO  - COMMENT CHANGED                        
         LA    R2,NEXTCOMT(R2)     YES - BUMP TO NEXT HEADER                    
         LA    R0,SPLSLCMH         CHECK EACH HEADER                            
         CR    R2,R0               PAST LAST COMMENT?                           
         BNH   ESPL0520            NO - CONTINUE                                
         LA    R2,SPLSLCMH                                                      
         B     ESPL0580                                                         
         SPACE 1                                                                
ESPL0530 DS    0H                                                               
         L     R8,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(0,(R2)),(R8),DATAMGR,RCONREC,GETTXT               
         BZ    ESPL0540                                                         
         L     RD,BASERD           ERROR, RETURN CONTROL TO USER                
         DC    H'0'                ABORT ON THIS ERROR ---                      
ESPL0540 EQU   *                                                                
         GOTO1 VDELELEM,DMCB,(7,RCONREC)                                        
         LA    R2,SPLSCM1H                                                      
ESPL0550 DS    0H                                                               
         TM    4(R2),X'20'         LINE PREVIOUSLY VALID?                       
         BO    ESPL0560            YES - NO CHANGE IN COMMENTS                  
         MVI   COMMFLAG,C'Y'       NO  - COMMENTS CHANGED                       
ESPL0560 EQU   *                                                                
         CLI   5(R2),0             NO INPUT, KEEP CHECKING                      
         BE    ESPL0570                                                         
         ZIC   R3,5(R2)                                                         
         XC    WORK2(67),WORK2                                                  
         MVI   WORK2,7                                                          
         LA    R4,2(R3)                                                         
         STC   R4,WORK2+1                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+2(0),8(R2)                                                 
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         SPACE 1                                                                
ESPL0570 LA    R2,NEXTCOMT(R2)     BUMP TO NEXT COMMENT                         
         LA    R0,SPLSLCMH                                                      
         CR    R2,R0               PAST LAST COMMENT?                           
         BNH   ESPL0550            NO - CONTINUE                                
ESPL0575 DS    0H                                                               
         LA    R2,SPLSLCMH                                                      
         SPACE 1                                                                
ESPL0580 DS    0H                                                               
*                                                                               
*   REMOVE COMMENT REQUIREMENT FOR TRUE ACTIVITY DATE CHANGE.                   
*                                                                               
****>>>  CLI   COMMFLAG,C'N'       COMMENTS CHANGED?                            
****>>>  BE    ESPL0590            NO  - NO DATE CHANGE FOR THIS                
         GOTO1 =A(TRUDATE),DMCB,(RC),RR=Y                                       
ESPL0590 EQU   *                                                                
*                                                                               
         BAS   RE,TESTDIS2         DISPLAY CONTROL INFORMATION                  
         SR    R0,R0               SET CC = ZERO FOR RETURN                     
         B     SPLEXMOD            EXIT WITH NO ERROR                           
SPLERROR EQU   *                                                                
         ST    R2,DUB              SAVE ERROR ADDRESS FOR PASSBACK              
         ST    R3,DUB+4            SAVE ERROR MESSAGE FOR PASSBACK              
         LTR   RB,RB               SET CC NOT = ZERO: EXIT W/ERROR              
SPLEXMOD EQU   *                                                                
         XMOD1                                                                  
MOVESTR  DC    C'PD=Y'             MOVE REQUES STRING                           
         DC    XL(L'SPLSCM1-L'MOVESTR)'0'                                       
         EJECT                                                                  
*                                                                               
*   TESTDIS2:   IF DDS TERMINAL, DISPLAY ADDITIONAL CONTRACT INFO               
*        IN HIDDEN FIELD 'SPLFLG'.                                              
*                                                                               
TESTDIS2 NTR1                                                                   
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BNE   TESD0050            NO  - DON'T DISPLAY ANYTHING                 
         MVC   SPLSFLG,=C'.....'   SET FILLER                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'        LOOK FOR X'06' ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   TESD0050            NO X'06' - EXIT                              
         USING RCONSPEL,R6                                                      
         TM    RCONSPES,X'08'      IS DATE COMPRESSED?                          
         BNO   TESD0010            NO  - DON'T FLAG                             
         MVI   SPLSFLG,C'C'        YES - FLAG                                   
TESD0010 EQU   *                                                                
         MVI   SPLSFLG+1,C'$'      FLAG  DATA AS DOLLARS                        
         TM    RCONSPES,X'04'      DATA = PERCENTS?                             
         BNO   TESD0020            NO  - DON'T FLAG                             
         MVI   SPLSFLG+1,C'%'      FLAG  DATA AS PERCENT                        
TESD0020 EQU   *                                                                
         TM    RCONSPES,X'20'      REPD DOLLARS OVERRIDDEN?                     
         BNO   TESD0030            NO  - DON'T FLAG                             
         MVI   SPLSFLG+2,C'O'      FLAG OVERRIDE                                
TESD0030 EQU   *                                                                
         TM    RCONSPES,X'40'      TOTAL DOLLARS?                               
         BNO   TESD0040            NO  - DON'T FLAG                             
         MVI   SPLSFLG+2,C'T'      FLAG TOTAL DOLLARS                           
TESD0040 EQU   *                                                                
         FOUT  SPLSFLGH                                                         
TESD0050 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ADDTOTAL:  CYCLES THROUGH BUCKETS IN WORK AREA AND ACCUMULATES              
*      THEIR TOTAL VALUE FOR PERCENT CALCULATIONS                               
*          R1     =  LOOP CONTROL SET TO # STATIONS                             
*          R3     =  ADDRESS OF BUCKETS                                         
*          FULL   =  RETURN VALUE                                               
*                                                                               
ADDTOTAL NTR1                                                                   
         SR    RF,RF               INITIALIZE STORAGE                           
ADDT0010 EQU   *                                                                
         CLC   0(4,R3),NAVALUE     BUCKET VALUE = N/A?                          
         BE    ADDT0020            YES - DON'T ADD IT                           
         A     RF,0(R3)            ADD BUCKET                                   
ADDT0020 EQU   *                                                                
         LA    R3,4(R3)            BUMP TO NEXT BUCKET                          
         BCT   R1,ADDT0010         GO BACK FOR NEXT                             
         ST    RF,FULL             SAVE TOTAL                                   
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CALCPERC:  CALCULATES THE PERCENT REPRESENTED BY THE BUCKET VS              
*      THE TOTAL AMOUNT.                                                        
*          R3     =  ADDRESS OF BUCKET IN PROGRESS                              
*          FULL   =  TOTAL VALUE                                                
*          DUB    =  RETURN VALUE (PERCENT)                                     
*                                                                               
CALCPERC NTR1                                                                   
         SR    RE,RE                                                            
         L     RF,0(R3)            INSERT BUCKET VALUE                          
         CLC   0(4,R3),NAVALUE     BUCKET VALUE = N/A?                          
         BE    CPER0060            YES - PASS IT THROUGH                        
         OC    FULL,FULL           ANY VALUE IN FULL?                           
         BZ    CPER0060            NO  - DON'T DIVIDE BY ZERO                   
         CLC   0(4,R3),=F'128000'  ADJUSTMENT LIMIT FOR CALC                    
         BL    CPER0020            USE AS IS                                    
         SRDA  RE,3                TOO BIG:  DIVIDE VALUE BY 8                  
*                                     THIS WILL RESULT IN A SMALL LOSS          
*                                     OF PRECISION, BUT THE NUMBERS             
*                                     ARE SO BIT IT WILL BE MEANINGLESS         
CPER0020 EQU   *                                                                
         MH    RF,=H'10000'        MULTIPLY BUCKET BY 10 THOUSAND               
*                                                                               
*   10,000 WILL LEAVE DECIMAL PRECISION TO X.XX%                                
*                                                                               
         SLDA  RE,1                DOUBLE THE VALUE                             
         A     RF,FULL             ADD TOTAL FOR ROUNDING                       
         D     RE,FULL             DIVIDE BY TOTAL                              
         CLC   0(4,R3),=F'128000'  RESET FINAL VALUE?                           
         BL    CPER0040            USE AS IS                                    
         SLA   RF,3                YES - MULTIPLY VALUE BY 8                    
CPER0040 EQU   *                                                                
         SRA   RF,1                DIVIDE BY 2                                  
CPER0060 EQU   *                                                                
         C     RF,=F'10000'        CHECK FOR MAX VALUE                          
         BNH   CPER0080                                                         
         L     RF,=F'10000'                                                     
CPER0080 EQU   *                                                                
         ST    RF,DUB              STORE FOR RETURN                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   TESTREPD:  COMPARE REP'D STATION DOLLARS WITH TOTAL OF ESTIMATE             
*      BUCKETS.  IF NOT THE SAME, SET OVERRIDE DOLLAR FLAG, INSERT              
*      OVERRIDE DOLLARS INTO X'08' ELEMENT, CREATING THE ELEMENT IF             
*      NECESSARY.                                                               
*                                                                               
TESTREPD NTR1                                                                   
         LA    R2,RCONELEM         SCAN ESTIMATE BUCKETS                        
         SR    R3,R3                                                            
         SR    R7,R7                                                            
TREP0010 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    TREP0040            YES                                          
         CLI   0(R2),3             ESTIMATE BUCKET?                             
         BE    TREP0030            YES                                          
TREP0020 IC    R3,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,R3                                                            
         B     TREP0010            GO BACK FOR NEXT                             
*                                                                               
TREP0030 A     R7,6(R2)            ACCUMULATE ESTIMATE DOLLARS                  
         B     TREP0020            BUMP TO NEXT ELEMENT                         
TREP0040 EQU   *                                                                
         SR    R6,R6               GET RID OF PENNIES                           
         D     R6,=F'100'          DIVIDE BY 100                                
         C     R7,WORK3            ESTIMATE $ = REP'D STA $?                    
         BNE   TREP0050            NO                                           
         LA    R2,WORK2            YES - TURN OFF OVERRIDE $ BIT                
*                                        AND VALUE BIT                          
         NI    RCONSPES-RCONSPEL(R2),X'9F'                                      
         LA    R6,RCONREC          LOOK FOR X'08' ELEMENT                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   TREP0055            NOT FOUND - ADD IT                           
*                                                                               
         USING RCONACEL,R6         FOUND                                        
         XC    RCONAC$$,RCONAC$$   CLEAR REP'D STATION OVERRIDE $               
         DROP R6                                                                
         B     TREP0070            EXIT                                         
TREP0050 EQU   *                                                                
         LA    R2,WORK2            TURN ON OVERRIDE $ BIT                       
         OI    RCONSPES-RCONSPEL(R2),X'20'                                      
         LA    R6,RCONREC          LOOK FOR X'08' ELEMENT                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BE    TREP0060            FOUND - INSERT DOLLARS                       
         MVC   NEW08ELT+8(4),WORK3                                              
*                                  NOT FOUND - INSERT OVERRIDE $ IN             
*                                     NEW ELEMENT TO BE ADDED                   
TREP0055 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,RCONREC,NEW08ELT                                   
*                                  INSERT NEW X'08' ELEMENT INTO RECORD         
         B     TREP0070            EXIT                                         
TREP0060 EQU   *                                                                
*                                  INSERT OVERRIDE DOLLARS INTO                 
*                                     EXISTING X'08' ELEMENT                    
         MVC   RCONAC$$-RCONACEL(4,R6),WORK3                                    
TREP0070 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*   NOREPT$$:  REP'D STATION HAS NO DOLLARS.  THEREFORE, THERE IS               
*      NOTHING TO BASE THE OTHER PERCENT FIGURES ON.  THE TOTAL OF              
*      ALL NON-REP'D STATIONS MUST BE SAVED IN THE X'08' ELEMENT,               
*      AND THE FLAG SET TO INDICATE ITS MEANING.                                
*                                                                               
NOREPD$$ NTR1                                                                   
         BAS   RE,CHKAWIN          CHECK TO SEE IF ORDER A WIN                  
         BNZ   NORP0040            YES - ERROR OUT                              
         LA    R3,WORK3            TOTAL NON-REP'D STATIONS $$                  
         SR    R4,R4                                                            
         ZIC   RF,STLNUM           SET LOOP CONTROL = # STATIONS                
NORP0010 EQU   *                                                                
         A     R4,0(R3)            ADD STATION'S DOLLARS TO ACCUM.              
         LA    R3,4(R3)            BUMP TO NEXT BUCKET                          
         BCT   RF,NORP0010         GO BACK FOR NEXT                             
*                                                                               
         LA    R2,WORK2            TURN OFF OVERRIDE $ BIT                      
*                                        AND VALUE BIT                          
         NI    RCONSPES-RCONSPEL(R2),X'9F'                                      
         OI    RCONSPES-RCONSPEL(R2),X'40'                                      
*                                  TURN ON 'VALUE = TOTAL $$'                   
         LA    R6,RCONREC          LOOK FOR X'08' ELEMENT                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   NORP0020            NOT FOUND - MUST BE ADDED                    
*                                                                               
         USING RCONACEL,R6         FOUND                                        
         ST    R4,FULL             INSERT STATION TOTAL $$                      
         MVC   RCONAC$$,FULL       INSERT INTO EXISTING X'08' ELEMENT           
*                                                                               
         DROP R6                                                                
*                                                                               
         B     NORP0030            EXIT                                         
NORP0020 EQU   *                                                                
         ST    R4,NEW08ELT+8       NOT FOUND - INSERT OVERRIDE $ IN             
*                                     NEW ELEMENT TO BE ADDED                   
         GOTO1 VADDELEM,DMCB,RCONREC,NEW08ELT                                   
*                                  INSERT NEW X'08' ELEMENT INTO RECORD         
         B     NORP0030            EXIT                                         
NORP0030 EQU   *                                                                
         SR    RF,RF               NO ERROR: RETURN CC= ZERO                    
NORP0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CHKAWIN:  SCAN CONTRACT FOR DOLLARS.  IF THERE ARE, SPL CANNOT              
*     BE ENTERED WITH $0 FOR REP'D STATION.                                     
*                                                                               
CHKAWIN  NTR1                                                                   
         SR    R3,R3               ESTABLISH COUNTER                            
         LA    R6,RCONREC          LOOK FOR X'03' ELEMENTS                      
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         B     CHKW0020                                                         
CHKW0010 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
CHKW0020 EQU   *                                                                
         BNE   CHKW0080            DONE - EXIT CC = ZERO                        
         USING RCONBKEL,R6                                                      
         ZICM  R4,RCONBKAM,4       GET DOLLARS FROM ELEMENT                     
         AR    R3,R4               ACCUMULATE VALUE                             
         B     CHKW0010            GO BACK FOR NEXT ELEMENT                     
CHKW0080 EQU   *                                                                
         LTR   R3,R3               SET CC BASED ON VALUE                        
CHKW0090 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*   CLOSAFTR  HANDLES THE 'CLOSE AFTER' FIELD OF THE SPL SCREEN                 
*     WHEN THAT IS EITHER THE ONLY FIELD ON THAT PORTION OF THE                 
*     SCREEN, OR THAT IS THE ONLY PIECE TO HAVE CHANGED.                        
*                                                                               
CLOSAFTR NTR1                                                                   
         LA    R2,SPLSACLH         AUTOCLOSE DATE FIELD                         
         CLI   5(R2),0             ANY INPUT?                                   
         BE    CLOS0020            NO  -                                        
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB        ACCEPTED?                                    
         BZ    CLOS0030            NO  - ERROR IN DATE INPUT                    
         GOTO1 DATCON,DMCB,WORK,(3,NEW08ELT+2)                                  
*                                  INSERT AUTOCLOSE INTO X'08' ELEMENT          
         LA    R6,RCONREC          LOOK FOR X'08' ELEMENT                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BE    CLOS0010            FOUND                                        
         GOTO1 VADDELEM,DMCB,RCONREC,NEW08ELT                                   
         B     CLOS0020            EXIT W/CC=ZERO                               
CLOS0010 EQU   *                                                                
         USING RCONACEL,R6         FOUND - INSERT DATE INTO IT                  
         MVC   RCONACAC,NEW08ELT+2 INSERT AUTOCLOSE DATE                        
         DROP  R6                                                               
CLOS0020 EQU   *                                                                
         SR    R0,R0                                                            
         B     CLOS0040                                                         
CLOS0030 EQU   *                                                                
         LTR   RC,RC               SET CC NOT= ZERO                             
CLOS0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ERROR DISPLAYS AND EXIT ROUTINES                                            
*                                                                               
ERRX0320 LA    R3,193              COMMENT REQUIRED WHEN STATION=$0             
         LA    R2,SPLSCM1H         COMMENTS                                     
         B     ESPL0370                                                         
*                                                                               
ERRX0330 LA    R3,194    ADDITIONAL COMMENT REQUIRED WHEN ALTERING SPL          
         LA    R2,SPLSCM1H         COMMENT                                      
         B     ESPL0370                                                         
*                                                                               
ERRX0340 LA    R3,PCTERR                                                        
         B     ESPL0370                                                         
*                                                                               
ERRX0350 LA    R3,BLNKERR                                                       
         B     ESPL0370                                                         
*                                                                               
ERRX0360 LA    R3,BALERR                                                        
         LA    R2,SPLSDL1H                                                      
         B     ESPL0370                                                         
*                                                                               
ERRX0370 LA    R3,INVINP                                                        
         B     ESPL0370                                                         
*                                                                               
ERRX0380 EQU   *                                                                
         LA    R3,350              N/A NOT ALLOWED ON STA                       
         B     SPLERROR                                                         
*                                                                               
ERRX0390 EQU   *                                                                
         LA    R3,351              N/A NOT ALLOWED BY REP                       
         B     SPLERROR                                                         
*                                                                               
ERRX0400 LA    R3,PTOTERR                                                       
         B     ESPL0370                                                         
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
*   LOCAL STORAGE FOR THIS NMOD                                                 
*                                                                               
SAVSTATB DS    A                   A(SCRTABLE)                                  
         EJECT                                                                  
*                                                                               
*    PREVALID:  TURNS ON PREVIOUSLY-VALID BITS IN ALL OUTGOING                  
*        FIELDS.                                                                
*              PARAMETER  1        A(WORKSPACE)                                 
*              PARAMETER  2        A(FIRST FIELD)                               
*              PARAMETER  3        A(END-ADDR)                                  
*              PARAMETER  4        0=SET PREVIOUSLY-VALID BIT                   
*                                  1=SCAN FOR PREVIOUSLY-VALID                  
*                                    BIT:  IF NOT FOUND, EXIT                   
*                                    WITH CC NOT = ZERO                         
*              PARAMETER  5        1=SCAN FOR FIELD WITH DATA:                  
*                                    LENGTH = NON-ZERO                          
         DS    0F                                                               
PREVALID NMOD1 0,*VALI*                                                         
         L     RC,0(R1)            RELOAD A(WORKSPACE)                          
         LM    R2,R5,4(R1)         LOAD OPTIONS                                 
PREV0010 EQU   *                                                                
         ZIC   RE,0(R2)            GET FIELD LENGTH                             
         TM    1(R2),X'20'         PROTECTED?                                   
         BNO   PREV0020            NO  -                                        
         LTR   R4,R4               YES - SKIP IT:  'SET' OR 'TEST'?             
         BNZ   PREV0050            NOT ZERO = TEST                              
         B     PREV0030            ZERO = SET PREVIOUSLY VALID BIT              
PREV0020 EQU   *                                                                
         LTR   R4,R4               NO  - IS REQUEST SET OR TEST?                
         BNZ   PREV0040            NOT ZERO = TEST                              
         OI    4(R2),X'20'         ZERO = SET PREVIOUSLY VALID BIT              
PREV0030 EQU   *                                                                
         LA    R2,0(RE,R2)         BUMP TO NEXT FIELD                           
         CR    R2,R3               A('LAST' FIELD) FROM SCREEN GEN              
         BL    PREV0010            STOP AT 'LAST' FIELD                         
         SR    R0,R0               SET CC = ZERO                                
         B     PREV0070                                                         
PREV0040 EQU   *                                                                
         LTR   R5,R5               LOOK FOR NON-ZERO LENGTH?                    
         BZ    PREV0045            NO  -                                        
         ZIC   RF,5(R2)            YES - DATA IN FIELD?                         
         LTR   RF,RF                                                            
         BNZ   PREV0060            YES - RETURN CC NOT = ZERO                   
         B     PREV0050                                                         
PREV0045 EQU   *                                                                
         TM    4(R2),X'20'         IS FIELD PREVIOUSLY VALID?                   
         BNO   PREV0060            NO  - RETURN CC NOT = ZERO                   
PREV0050 EQU   *                                                                
         LA    R2,0(RE,R2)         BUMP TO NEXT FIELD                           
         CR    R2,R3               LAST FIELD TO LOOK AT                        
         BNH   PREV0010            LOOK AT ALL FIELDS                           
         SR    R0,R0               SET CC = ZERO                                
         B     PREV0070                                                         
PREV0060 EQU   *                                                                
         LTR   RC,RC               SET CC NOT = ZERO                            
PREV0070 EQU   *                                                                
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FOUT ENTIRE SCREEN                                    
*                                                                               
*              PARAMETER  1        A(FIRST FIELD)                               
*              PARAMETER  2        A(END-ADDR) (FIELD NOT CHECKED!)             
*                                                                               
         DS    0F                                                               
TRANOUT  NMOD1 0,*TOUT*                                                         
         L     RC,0(R1)            RELOAD A(WORKSPACE)                          
         LM    R2,R3,4(R1)         LOAD OPTIONS                                 
         SPACE 1                                                                
TOUT0010 ZIC   RE,0(R2)            LENGTH(SCREEN FIELD)                         
         TM    1(R2),X'20'         IS FIELD PROTECTED?                          
         BO    TOUT0020            YES - SKIP IT                                
*                                                                               
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
TOUT0020 LA    R2,0(RE,R2)         BUMP TO NEXT FIELD                           
         CR    R2,R3               HAVE WE REACHED LAST FIELD?                  
         BL    TOUT0010            NO  - GO BACK FOR NEXT                       
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ROUTINE TO CLEAR/FOUT STATION CALL LETTER AND PERCENTAGE FIELDS             
*                                                                               
         DS    0F                                                               
SETPROTS NMOD1 0,*PROT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,PROTFLDS         SET A(SCREEN FIELD TABLE)                    
SPRO0010 EQU   *                                                                
         OC    0(4,R2),0(R2)       END OF TABLE?                                
         BZ    SPRO0020            YES                                          
         L     R3,0(R2)            UNLOAD A(HEADER FIELD)                       
         AR    R3,RA               SET ADDRESSABILITY                           
         ZIC   RF,0(R3)            TAKE LENGTH OF FIELD                         
         SH    RF,=H'9'            SET FOR LENGTH OF SPACE-FILL                 
         TM    1(R3),X'02'         EXTENDED FIELD HEADER PRESENT?               
         BZ    *+8                 NO                                           
         SH    RF,=H'8'            YES - SUBTRACT 8 MORE                        
         EX    RF,MOVESPAC         CLEAR THE FIELD                              
         FOUT  (R3)                                                             
         LA    R2,LPROT(R2)        BUMP TO NEXT ENTRY                           
         B     SPRO0010                                                         
SPRO0020 EQU   *                                                                
         XMOD1                                                                  
MOVESPAC MVC   8(0,R3),MYSPACES                                                 
         SPACE 5                                                                
PROTFLDS DS    0H                                                               
         DC    AL4(SPLSST1H-TWAD)   1ST STATION CALL LETTERS                    
LPROT    EQU   *-PROTFLDS                                                       
         DC    AL4(SPLSPC1H-TWAD)   1ST PERCENT FIELD                           
         DC    AL4(SPLSST2H-TWAD)   2ND STATION CALL LETTERS                    
         DC    AL4(SPLSPC2H-TWAD)   ETC                                         
         DC    AL4(SPLSST3H-TWAD)                                               
         DC    AL4(SPLSPC3H-TWAD)                                               
         DC    AL4(SPLSST4H-TWAD)                                               
         DC    AL4(SPLSPC4H-TWAD)                                               
         DC    AL4(SPLSST5H-TWAD)                                               
         DC    AL4(SPLSPC5H-TWAD)                                               
         DC    AL4(SPLSST6H-TWAD)                                               
         DC    AL4(SPLSPC6H-TWAD)                                               
         DC    AL4(SPLSST7H-TWAD)                                               
         DC    AL4(SPLSPC7H-TWAD)                                               
         DC    AL4(SPLSST8H-TWAD)                                               
         DC    AL4(SPLSPC8H-TWAD)                                               
         DC    AL4(SPLSST9H-TWAD)                                               
         DC    AL4(SPLSPC9H-TWAD)                                               
         DC    AL4(SPLSSTAH-TWAD)                                               
         DC    AL4(SPLSPCAH-TWAD)                                               
         DC    AL4(SPLSSTBH-TWAD)                                               
         DC    AL4(SPLSPCBH-TWAD)                                               
         DC    AL4(SPLSSTCH-TWAD)                                               
         DC    AL4(SPLSPCCH-TWAD)                                               
         DC    XL4'00000000'       DELIMITER                                    
         EJECT                                                                  
*                                                                               
*                                                                               
*   ROUTINE TO SPREAD FORECAST DOLLARS FROM SAR ELEMENT OVER FLIGHT             
*        OF ORDER.  CONTRACT FLIGHT DATES ARE USED, NOT SAR FLIGHT              
*        DATES.                                                                 
*                                                                               
         DS    0F                                                               
SPREDFOR NMOD1 0,*FORE*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,4(R1)            RESET A(SAR ELEMENT)                         
         USING RSARXEL,R4                                                       
*                                                                               
*   INITIALIZE WORKSPACE FOR FORECAST SPREADING....                             
*                                                                               
         XC    NEW23ELT,NEW23ELT   SET NEW ELEMENT                              
         MVC   NEW23ELT(2),=X'230A'                                             
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'23',RCONREC)                                    
*                                  DELETE EXISTING FORECAST BUCKETS             
         GOTO1 DATCON,DMCB,(5,WORK),(0,WORK)                                    
*                                  GET TODAY'S DATE EBCDIC                      
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
*                                  GET DAY OF WEEK OF TODAY'S DATE              
         ZIC   R2,DMCB             SAVE DAY OF WEEK RETURNED                    
         BCTR  R2,0                MAKE DAY OF WEEK ZERO/MONDAY REL             
         LNR   R2,R2               NEGATE THE VALUE                             
         SR    RF,R2               SUBTRACT MONDAY ADJUSTMENT                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,NEW23ELT+4)                            
*                                  INSERT IT INTO NEW 23 ELEMENT                
         BAS   RE,GENDAYS          GENERATE DAYTABLE                            
         BAS   RE,SPREDAYS         GENERATE DAYS WITHIN TABLE                   
SFOR0020 EQU   *                                                                
         SR    RF,RF                                                            
         LA    R2,DAYTABLE         ACCUMULATE TOTAL DAYS                        
SFOR0030 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SFOR0040            YES                                          
         ZIC   RE,3(R2)            TAKE DAYS FROM TABLE                         
         AR    RF,RE               ACCUMULATE                                   
         LA    R2,4(R2)            BUMP TO NEXT ENTRY                           
         B     SFOR0030            GO BACK FOR NEXT                             
SFOR0040 EQU   *                                                                
         ST    RF,TOTDAYS          TOTAL DAYS AT THIS POINT                     
         MVC   FULL,RSARXBGT       LOAD MARKET $$ BUDGET FIGURE                 
         L     RF,FULL                                                          
         ZIC   R2,RSARXSHG         ADJUST WITH SHARE GOAL                       
         MR    RE,R2               MULTIPLY MARKET $ BY SHARE GOAL              
*                                     GIVING STATION $$                         
*                                                                               
*   NOW MULTIPLY BY 10 FOR PROPER DECIMAL ALIGNMENT                             
*                                                                               
         M     RE,=F'10'           MULTIPLY BY 10                               
         L     R2,TOTDAYS          DIV STA $$ BY TOTAL DAYS                     
*                                     GIVING $$ PER DAY                         
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         AR    RF,R2               ADD TOTDAYS FOR ROUNDING                     
         DR    RE,R2               DIVIDE BY TOTDAYS                            
         SRA   RF,1                DIVIDE BY 2                                  
         ST    RF,TOTDAYS          SAVE $$ PER DAY                              
         LA    R2,DAYTABLE                                                      
SFOR0050 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SFOR0060            YES - FINISHED                               
         BAS   RE,GENBUCKS         NO  - GEN X'23' FORECAST BUCKET              
         LA    R2,4(R2)            BUMP TO NEXT BUCKET                          
         B     SFOR0050            GO BACK FOR NEXT                             
SFOR0060 EQU   *                                                                
         XC    MYP,MYP             CLEAR PRINT AREA FOR OTHER USERS             
         XMOD1                                                                  
         EJECT                                                                  
GENDAYS  NTR1                                                                   
         MVC   CYCLEDAT,LOCALFLT   CONTRACT FLIGHT DATES                        
*                                                                               
*   EXTRA PRINT LINE IS USED TO SET UP BROADCAST MONTH ARRAY.                   
*                                                                               
         LA    R2,MYP              A(DAYTABLE)                                  
         XC    MYP,MYP             INITIALIZE TABLE                             
         USING BROADTBL,R2                                                      
GDAY0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,CYCLEDAT),(0,DAYTABLE)                            
*                                  CONVERT START DATE TO EBCDIC                 
GDAY0040 EQU   *                                                                
         GOTO1 VGTBROAD,DMCB,(1,DAYTABLE),DAYTABLE+6,GETDAY,ADDAY               
         MVC   BRDWEEKS,DMCB       INSERT NUMBER OF WEEKS                       
*                                  GET BROADCAST DATES FOR MONTH                
         CLI   DMCB,X'FF'          ERROR?                                       
         BNE   *+6                 NO                                           
         DC    H'0'                SHOULDN'T HAPPEN!!                           
         GOTO1 DATCON,DMCB,(0,DAYTABLE+6),(3,BRDSTART)                          
*                                  INSERT START DATE IN TABLE                   
         GOTO1 DATCON,DMCB,(0,DAYTABLE+12),(3,BRDEND)                           
*                                  INSERT END   DATE IN TABLE                   
         CLC   CYCLEDAT+3(3),BRDEND                                             
*                                  CONTRACT FLIGHT END REACHED?                 
         BNH   GDAY0060            YES                                          
         GOTO1 DATCON,DMCB,(3,BRDEND),(0,DAYTABLE+6)                            
*                                  CONVERT END DATE TO EBCDIC                   
         LA    RF,1                DATE INCREMENT                               
         GOTO1 ADDAY,DMCB,DAYTABLE+6,DAYTABLE,(RF)                              
*                                  GET NEXT DAY, WHICH IS FIRST                 
*                                     DAY OF NEXT BDCST MONTH                   
         LA    R2,BRDLEN(R2)       BUMP TO NEXT TABLE ENTRY                     
         B     GDAY0040            GO BACK, SET NEXT MONTH                      
GDAY0060 EQU   *                                                                
         XC    DAYTABLE(56),DAYTABLE     CLEAR THE WORKAREA                     
         LA    R2,MYP              RESET A(BDCST MONTH TABLE)                   
         LA    R3,DAYTABLE                                                      
GDAY0080 EQU   *                                                                
         CLI   BRDEND,0            ANY ENTRY?                                   
         BZ    GDAY0100            NO  - FINISHED                               
         MVC   0(2,R3),BRDEND      MOVE BDCST MON END (YM) TO TABLE             
         LA    R2,BRDLEN(R2)       BUMP TO NEXT BDCST MONTH                     
         LA    R3,4(R3)            BUMP TO NEXT DAYTABLE                        
         B     GDAY0080            GO BACK FOR NEXT                             
GDAY0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
SPREDAYS NTR1                                                                   
         LA    R2,DAYTABLE         A(DAYTABLE)                                  
         LA    R3,MYP              A(BDCST MONTH TABLE)                         
         USING BROADTBL,R3                                                      
         CLC   BRDSTART,LOCALFLT   IS FLIGHT START FIRST DAY                    
*                                     OF FIRST BROADCAST MONTH?                 
         BE    SPDA0040            YES                                          
         GOTO1 DATCON,DMCB,(3,LOCALFLT),(0,WORK)                                
*                                  CONVERT FLIGHT START DATE                    
         CLC   LOCALFLT+3(3),BRDEND                                             
*                                  IS FLIGHT END DATE EARLIER                   
*                                     THAN BROADCAST MONTH END DATE?            
         BNL   SPDA0020            NO  -                                        
         GOTO1 DATCON,DMCB,(3,LOCALFLT+3),(0,WORK+6)                            
*                                  CONVERT FLIGHT END   DATE                    
*                                                                               
*   AT THIS POINT, BOTH FLIGHT START AND END ARE WITHIN THE FIRST               
*     BROADCAST MONTH, SO THAT THE NUMBER OF DAYS CALCULATION IS                
*     DONE FROM FLIGHT START TO FLIGHT END .                                    
*                                                                               
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   DAYTABLE+2(2),DMCB+8                                             
*                                  MOVE NUM DAYS TO 1ST TABLE NTRY              
         B     SPDA0100            EXIT ROUTINE                                 
*                                                                               
*   AT THIS POINT, FLIGHT START IS OTHER THAN BEGINNING OF BDCST                
*     MONTH, AND FLIGHT END IS EITHER AFTER THE BDCST MONTH, OR                 
*     THE LAST DAY OF THE MONTH.  NUMBER OF DAYS IS CALCULATED                  
*     FROM FLIGHT START TO BDCST MONTH END.                                     
*                                                                               
SPDA0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,BRDEND),(0,WORK+6)                                
*                                  CONVERT FLIGHT END   DATE                    
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   DAYTABLE+2(2),DMCB+8                                             
*                                  MOVE NUM DAYS TO 1ST TABLE NTRY              
         B     SPDA0080            FIRST ENTRY DONE                             
SPDA0040 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SPDA0100            YES                                          
         CLC   LOCALFLT+3(3),BRDEND                                             
*                                  END OF FLIGHT REACHED?                       
         BL    SPDA0060            YES - PARTIAL MONTH TO DO                    
         ZIC   RF,BRDWEEKS         NO  - CALCULATE DAYS FROM WEEKS              
         SR    RE,RE                                                            
         LA    R1,7                                                             
         MR    RE,R1               MULT WEEKS BY 7                              
         STC   RF,3(R2)            INSERT # DAYS INTO TABLE                     
         B     SPDA0080            GO TO NEXT SLOT                              
SPDA0060 EQU   *                                                                
*                                                                               
*   AT THIS POINT, FLIGHT END IS OTHER THAN END OF BROADCAST                    
*     MONTH.  NUMBER OF DAYS IS CALCULATED FROM BROADCAST MONTH                 
*     START DATE THROUGH FLIGHT END.                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,BRDSTART),(0,WORK)                                
*                                  CONVERT BROADCAST MONTH START                
         GOTO1 DATCON,DMCB,(3,LOCALFLT+3),(0,WORK+6)                            
*                                  CONVERT FLIGHT END   DATE                    
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   2(2,R2),DMCB+8                                                   
*                                  MOVE NUM DAYS TO LAST TABLE NTRY             
         B     SPDA0100            FINISHED                                     
*                                                                               
SPDA0080 EQU   *                                                                
         LA    R2,4(R2)            BUMP DAYTABLE                                
         LA    R3,BRDLEN(R3)       BUMP BDCST MONTH TABLE                       
         B     SPDA0040            GO BACK FOR NEXT                             
SPDA0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
GENBUCKS NTR1                                                                   
         MVC   NEW23ELT+2(2),0(R2) INSERT MONTH INTO 23 ELT                     
         SR    RE,RE                                                            
         ZIC   RF,3(R2)            NUMBER OF DAYS FOR MONTH *                   
         M     RE,TOTDAYS             $$ PER DAY = $$ FOR MONTH                 
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         AH    RF,=H'10'           ADD FOR ROUNDING                             
         D     RE,=F'10'           DIVIDE FOR DECIMAL SCALING                   
         SRA   RF,1                DIVIDE BY 2                                  
         ST    RF,FULL                                                          
         MVC   NEW23ELT+6(4),FULL  INSERT INTO X'23' ELEMENT                    
         GOTO1 VADDELEM,DMCB,RCONREC,NEW23ELT                                   
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*  TRUDATE:  FOR CONTRACTS WHERE THE BUYLINES HAVE RESULTED IN BUCKET           
*     CHANGES, FIND (ADD IF NOT PRESENT) THE X'08' ELEMENT, AND                 
*     UPDATE THE TRUE ACTIVITY DATE FOR SAR REPORTING                           
*                                                                               
TRUDATE  NMOD1 0,*TDATE*                                                        
         L     RC,0(R1)                                                         
         LA    R2,RCONELEM         A(1ST ELEMENT)                               
TDAT0010 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    TDAT0030            YES - NO X'08' FOUND - ADD IT                
         CLI   0(R2),X'08'         X'08'?                                       
         BNE   TDAT0010            NO  - GO BACK FOR NEXT                       
TDAT0020 EQU   *                   YES - ADD TODAYS DATE                        
         USING RCONACEL,R2                                                      
         GOTO1 DATCON,DMCB,(5,RCONACTA),(3,RCONACTA)                            
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         DROP  R2                                                               
*                                                                               
         B     TDAT0040            FINISHED                                     
TDAT0030 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,LOCALFLT),(3,TDATELT+5)                           
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         GOTO1 VADDELEM,DMCB,RCONREC,TDATELT                                    
TDAT0040 EQU   *                                                                
         XIT1                                                                   
*                   .1.2.3.4.5.6.7.8.9.0.1.2                                    
TDATELT  DC    XL12'080C00000000000000000000'                                   
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067RECNT72   02/20/09'                                      
         END                                                                    
