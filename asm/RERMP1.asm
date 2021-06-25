*          DATA SET RERMP1     AT LEVEL 067 AS OF 05/01/02                      
*PHASE T81015A,+0                                                               
*INCLUDE INVDAY                                                                 
*INCLUDE REBKLST                                                                
         TITLE 'T81015 - REPPAK FILE MAINT - MULT INV ADD'                      
********************************************************************            
T81015   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81015,RR=R5                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T81015+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
         ST    R5,RELO                                                          
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC AND PUTREC             
*  MOVE PROFILE TO LOCAL WORKING STORAGE                                        
         LR    R3,RA                                                            
         AH    R3,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,R3                                                       
         MVC   RMPPROFS,SVPGPBIT                                                
         DROP  R3                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
*                                                                               
         EJECT                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
VKEY     DS    0H                                                               
*   TAKE THIS CODE OUT TEMP                                                     
         OI    GENSTAT1,OKADDEL                                                 
*                                                                               
*                                                                               
*        VALIDATE THE ACTION                                                    
*                                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CONACTH                                                       
         CLI   ACTNUM,ACTADD                                                    
         BNE   ERREND                                                           
*                                                                               
*              INIT WORK AREA                                                   
*                                                                               
         XC    KEY,KEY                                                          
         XC    STAHLD(15),STAHLD                                                
*                                                                               
*              VALIDATE THE STATION                                             
*                                                                               
         LA    R2,MINSSTAH                                                      
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0             REQUIRED                                     
         BE    ERREND                                                           
         GOTO1 VALISTA                                                          
         MVC   STAHLD,WORK                                                      
         MVI   STAHLD+4,C'T'                                                    
         CLI   WORK+4,C' '                                                      
         BE    *+10                                                             
         MVC   STAHLD+4(1),WORK+4                                               
         CLI   WORK+40,C' '                                                     
         BE    *+10                                                             
         MVC   STAHLD+4(1),WORK+40 CHECK SATTELITE                              
         MVC   CSTAT,STAHLD                                                     
         MVC   CCOSCRST,8(R2)                                                   
         SPACE 1                                                                
*                                                                               
VKXIT    MVC   CCONKSTA,STAHLD                                                  
         XC    KEY,KEY                                                          
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
DKEY     DS    0H                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         LA    R2,MINSSTAH         STATION                                      
         MVC   8(4,R2),RINVKSTA                                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         MVC   CCONKSTA,RINVKSTA                                                
         MVC   CCONINV,RINVKINV                                                 
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(5,CCONEFF)                             
*                                                                               
DKXIT    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
VREC     DS    0H                                                               
         GOTO1 CHKLOCK                                                          
         L     RF,AIO3                                                          
         MVI   0(RF),X'FF'                                                      
         MVI   RECCNT,0                                                         
*                                                                               
         LA    R2,MININV1H                                                      
         BAS   RE,TRANSCRN         SET TRANSMIT BIT ON SCREEN                   
*                                                                               
VR100    LA    RF,MINTAGH          LAST FIELD                                   
         CR    R2,RF                                                            
         BNL   VR200                                                            
         CLI   12(R2),C'*'         HAS LINE BEEN PREVIUSLY VALIDATED            
         BE    VR140                                                            
         BAS   RE,BLDLINE                                                       
VR140    LA    R2,LINELEN(R2)                                                   
         B     VR100                                                            
*                                                                               
VR200    BAS   RE,ADDRECS                                                       
         BAS   RE,SAVINVS          SAVE INVENTORY NUMBERS FOR PFKEYS            
         BAS   RE,GOLTRANS         CREATE LTRANS REQUEST                        
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              BUILD RECORD ENTRIES FROM SCREEN                *                
****************************************************************                
****************************************************************                
BLDLINE  NTR1                                                                   
         ST    R2,THISLINE                                                      
         LA    R6,LINEINP                                                       
         USING INVLINE,R6                                                       
*                                                                               
*-- MOVE INVENTORY NUMBER OUT                                                   
*                                                                               
BLDL050  XC    INVOINV,INVOINV                                                  
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         XC    INVLNUM,INVLNUM                                                  
         B     BLDL070                                                          
*                                                                               
         CLI   5(R2),1                                                          
         BNE   BLDL060                                                          
         CLI   8(R2),C'='          CHECK IF DROP DOWN                           
         BNE   BLDL060                                                          
         MVI   ERROR,INVALID                                                    
         OC    INVLNUM,INVLNUM                                                  
         BZ    ERREND                                                           
         MVC   8(4,R2),INVLNUM                                                  
         MVI   5(R2),4                                                          
         B     BLDL070                                                          
*                                                                               
BLDL060  MVC   INVLNUM(4),8(R2)                                                 
         OC    INVLNUM(4),=4X'40'                                               
*                                                                               
*-- MOVE EFFECTIVE DATES                                                        
*                                                                               
BLDL070  L     R2,THISLINE                                                      
         LA    R2,EFFDH(R2)                                                     
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         XC    INVLEFFS(4),INVLEFFS                                             
         B     BLDL160                                                          
*                                                                               
         CLI   5(R2),1                                                          
         BNE   BLDL120                                                          
         CLI   8(R2),C'='          CHECK IF DROP DOWN                           
         BNE   BLDL120                                                          
*  DISPLAY DROP DOWN DATE                                                       
         MVI   ERROR,INVALID                                                    
         OC    INVLEFFS,INVLEFFS                                                
         BZ    ERREND                                                           
         LR    R5,R2                                                            
         LA    R5,8(R5)                                                         
         GOTO1 DATCON,DMCB,(2,INVLEFFS),(5,0(R5))                               
         MVI   5(R2),8                                                          
         SPACE 1                                                                
         OC    INVLEFFE,INVLEFFE                                                
         BZ    BLDL080             NO END                                       
         LA    R5,8(R5)                                                         
         MVI   0(R5),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,INVLEFFE),(5,1(R5))                               
         MVI   5(R2),17                                                         
BLDL080  B     BLDL160                                                          
*                                                                               
BLDL120  XC    INVLEFFS(4),INVLEFFS                                             
         GOTO1 ANY                                                              
         GOTO1 SCANNER,DMCB,(R2),(2,WORK2),C',=,-'                              
         MVI   ERROR,INVALID                                                    
         CLI   DMCB+4,1                                                         
         BNE   ERREND              THEY INPUT A ,                               
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(0,WORK2+12),WORK      START DATE                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(2,INVLEFFS)                                
         CLI   WORK2+1,0           NO END                                       
         BE    BLDL160                                                          
         MVC   INVLEFFE,WORK2+10                                                
         CLI   WORK2+1,1                                                        
         BNE   *+16                SHOULD BE A DATE                             
         TM    WORK2+3,X'80'       TEST FOR 1 POSITION NUMERIC                  
         BO    BLDL160                                                          
         B     ERREND                                                           
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(0,WORK2+22),WORK                                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(2,INVLEFFE)                                
         CLC   INVLEFFS,INVLEFFE                                                
         BH    ERREND                                                           
         SPACE                                                                  
*                                                                               
*-- MOVE DAY                                                                    
*                                                                               
BLDL160  L     R2,THISLINE                                                      
         LA    R2,DAYH(R2)                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         XC    INVLDAY,INVLDAY                                                  
         B     BLDL220                                                          
*                                                                               
         CLI   5(R2),1                                                          
         BNE   BLDL180                                                          
         CLI   8(R2),C'='          CHECK IF DROP DOWN                           
         BNE   BLDL180                                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         OC    INVLDAY,INVLDAY                                                  
         BZ    ERREND                                                           
         XC    WORK,WORK                                                        
         GOTO1 UNDAY,DMCB,INVLDAY,WORK              DAY                         
         MVC   8(11,R2),WORK                                                    
         MVI   5(R2),11                                                         
         B     BLDL220                                                          
*                                                                               
BLDL180  XR    R5,R5                                                            
         IC    R5,5(R2)            LENGTH OF EXPRESSION                         
         GOTO1 DAYVAL,DMCB,((R5),8(R2)),INVLDAY,WORK                            
         CLI   INVLDAY,0                                                        
         BE    ERREND                                                           
         GOTO1 =V(INVDAY),DMCB,((R5),8(R2)),GINVDAY,WORK,DAYVAL,       X        
               RR=RELO                                                          
         CLI   GINVDAY,0                                                        
         BE    ERREND                                                           
*                                                                               
*-- MOVE TIME                                                                   
*                                                                               
BLDL220  L     R2,THISLINE                                                      
         LA    R2,TIMEH(R2)                                                     
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         XC    INVLTIME,INVLTIME                                                
         B     BLDL340                                                          
*                                                                               
         CLI   5(R2),1                                                          
         BNE   BLDL240                                                          
         CLI   8(R2),C'='          CHECK IF DROP DOWN                           
         BNE   BLDL240                                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         OC    INVLTIME,INVLTIME                                                
         BZ    ERREND                                                           
         GOTO1 UNTIME,DMCB,INVLTIME,(0,8(R2))       TIME                        
         MVI   5(R2),11            OUTPUT LENGTH                                
         B     BLDL340                                                          
*                                                                               
BLDL240  XC    INVLTIME,INVLTIME                                                
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'N'          NONE                                         
         BE    ERREND                                                           
         CLI   8(R2),C'V'            VARIOUS ARE NOT VALID                      
         BE    ERREND                                                           
         SPACE 1                                                                
         XR    R5,R5                                                            
         IC    R5,5(R2)            LENGTH OF EXPRESSION                         
         LA    R3,6(R5,R2)                                                      
         CLC   0(2,R3),=C',B'                                                   
         BNE   *+8                                                              
         SH    R5,=H'2'                                                         
         GOTO1 TIMVAL,DMCB,((R5),8(R2)),INVLTIME                                
         CLI   DMCB,X'FF'                                                       
         BE    ERREND                                                           
         SPACE 1                                                                
         BAS   RE,GETQTR           GET QTR HOUR FOR KEY                         
         CLC   0(2,R3),=C',B'                                                   
         BNE   BLDL260                                                          
         OC    INVLTIME+2(2),INVLTIME+2                                         
         BNZ   ERREND                                                           
         MVI   GINVLEN,C'0'                                                     
         B     BLDL340                                                          
         SPACE 1                                                                
BLDL260  CLC   INVLTIME+2(2),=C'CC'                                             
         BNE   BLDL280                                                          
         MVI   GINVLEN,C'9'                                                     
         B     BLDL340                                                          
         SPACE 1                                                                
BLDL280  MVC   HALF,INVLTIME       START TIME TO MINUTES                        
         BAS   RE,TOMIN                                                         
         LH    R5,HALF             START MINUTE TO R5                           
         SPACE 1                                                                
         MVC   HALF,INVLTIME+2     END TIME TO MINUTES                          
         BAS   RE,TOMIN                                                         
         LH    R3,HALF             END MINUTES                                  
         SPACE 1                                                                
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,30(R5)           IF NO END / ADD 30 TO START                  
         LR    RF,R3               END TIME MINUTES                             
         SPACE 1                                                                
         CH    RF,=H'1440'                                                      
         BNH   *+8                                                              
         SH    RF,=H'1440'         PAST MIDNIGHT                                
         XR    RE,RE                                                            
         SPACE 1                                                                
         D     RE,=F'60'           GET MILITARY END FROM MINUTES                
         MH    RF,=H'100'                                                       
         AR    RF,RE                                                            
         STH   RF,HALF                                                          
         MVC   INVLTIME+2(2),HALF                                               
         SPACE 1                                                                
         CR    R5,R3               START/END MINUTES                            
         BNH   *+8                                                              
         AH    R3,=H'1440'         ADD 24 X 60 TO END                           
         SPACE 1                                                                
         SR    R3,R5               END - START                                  
         LR    RF,R3                                                            
         XR    RE,RE                                                            
         D     RE,=F'30'           GET NUMBER 1/2 HOURS                         
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         AH    RF,=H'1'            ADD 1 TO HALF HOURS                          
         STC   RF,GINVLEN                                                       
         SPACE 1                                                                
         LA    R3,10               GET CODE FROM LENGTH TABLE                   
         LA    R5,LENGTH                                                        
         CLC   GINVLEN,0(R5)                                                    
         BNH   *+14                                                             
         LA    R5,2(R5)                                                         
         BCT   R3,*-14                                                          
         DC    H'0'                                                             
         MVC   GINVLEN,1(R5)                                                    
*                                                                               
*-- MOVE PROGRAM                                                                
*                                                                               
BLDL340  L     R2,THISLINE                                                      
         LA    R2,PROGH(R2)                                                     
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         XC    INVLPROG,INVLPROG                                                
         B     BLDL380                                                          
*                                                                               
         CLI   5(R2),1                                                          
         BNE   BLDL360                                                          
         CLI   8(R2),C'='          CHECK IF DROP DOWN                           
         BNE   BLDL360                                                          
         MVI   ERROR,INVALID                                                    
         OC    INVLPROG,INVLPROG                                                
         BZ    ERREND                                                           
         MVC   8(22,R2),INVLPROG                                                
         MVI   5(R2),22                                                         
         B     BLDL380                                                          
*                                                                               
BLDL360  MVC   INVLPROG,8(R2)                                                   
         OC    INVLPROG,=22X'40'                                                
*                                                                               
*-- MOVE DAYPART                                                                
*                                                                               
BLDL380  L     R2,THISLINE                                                      
         LA    R2,DAYPTH(R2)                                                    
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         XC    INVLDYPT,INVLDYPT                                                
         B     BLDL500                                                          
*                                                                               
         CLI   5(R2),1                                                          
         BNE   BLDL440                                                          
         CLI   8(R2),C'='          CHECK IF DROP DOWN                           
         BNE   BLDL440                                                          
         MVI   ERROR,INVALID                                                    
         OC    INVLDYPT,INVLDYPT                                                
         BZ    ERREND                                                           
         MVC   8(6,R2),INVLDYPT                                                 
         MVI   5(R2),6                                                          
         B     BLDL500                                                          
*                                                                               
*  VALIDATE THE DAYPART FIELD                                                   
*                                                                               
BLDL440  MVI   ERROR,INVALID                                                    
         MVC   INVLDYPT,=6X'40'    SPACE FILL                                   
*                                                                               
         GOTO1 VALDPTM,DMCB,(0,0(R2))                                           
*  CHECK DUPLICATES                                                             
         LA    R1,8(R2)            POINTS TO DAYPARTS                           
         ZIC   RF,5(R2)            NUMBER OF DAYPARTS                           
BLDL450  LA    R5,INVLDYPT         DAYPART FIELD                                
         LA    RE,6                MAXIMUM NUMBER OF DAYPARTS                   
*                                                                               
BLDL460  CLI   0(R5),X'40'         DAYPARTS TO RECORD                           
         BE    BLDL480                                                          
         CLC   0(1,R5),0(R1)       HAS ENTRY BEEN ADDED ALREADY                 
         BE    ERREND                                                           
         LA    R5,1(R5)                                                         
         BCT   RE,BLDL460                                                       
         DC    H'0'                IMPOSSIBLE                                   
BLDL480  MVC   0(1,R5),0(R1)       MOVE DAYPART TO RECORD                       
         LA    R1,1(R1)            GET NEXT DAYPART                             
         BCT   RF,BLDL450                                                       
         SPACE 1                                                                
*******************************************                                     
         SPACE 3                                                                
BLDL500  OC    LINEINP,LINEINP     ANY DATA ON THIS LINE                        
         BZ    BLDLEX                                                           
*                                                                               
*  CHECK ALL REQUIRED DATA HAS BEEN INPUTTED                                    
*                                                                               
         L     R2,THISLINE                                                      
         MVI   ERROR,INVALID                                                    
*                                                                               
*  INVENTORY NUMBER                                                             
*                                                                               
         MVC   INVOINV(1),GINVBQTR  SAVE OLD FORMAT INVENTORY NUMBER            
         MVC   INVOINV+1(2),GINVDAY                                             
*                                                                               
         TM    RMPPROFS,X'80'      IS INPUT REQUIRED                            
         BO    BLDL540             BIT ON FIELD REQUIRED                        
         CLI   5(R2),0             NO INPUT ALLOWED SELF GENERATING NBR         
         BNE   ERREND                                                           
         MVC   INVLNUM,GINVNUM     GET DEFAULT NUMBER                           
         B     BLDL580                                                          
*                                                                               
BLDL540  CLI   5(R2),0             INPUT REQUIRED                               
         BE    ERREND                                                           
         CLI   5(R2),4             MAX LENGTH IS 4                              
         BH    ERREND                                                           
*                                                                               
*  EFFECTIVE DATES                                                              
*                                                                               
BLDL580  L     R2,THISLINE                                                      
         LA    R2,EFFDH(R2)                                                     
         OC    INVLEFFS,INVLEFFS                                                
         BZ    ERREND                                                           
*                                                                               
*  DAY                                                                          
*                                                                               
         L     R2,THISLINE                                                      
         LA    R2,DAYH(R2)                                                      
         OC    INVLDAY,INVLDAY                                                  
         BZ    ERREND                                                           
*                                                                               
*  TIME                                                                         
*                                                                               
         L     R2,THISLINE                                                      
         LA    R2,TIMEH(R2)                                                     
         OC    INVLTIME,INVLTIME                                                
         BZ    ERREND                                                           
*                                                                               
*  PROGRAM                                                                      
*                                                                               
         L     R2,THISLINE                                                      
         LA    R2,PROGH(R2)                                                     
         OC    INVLPROG,INVLPROG                                                
         BZ    ERREND                                                           
*                                                                               
*  DAYPART                                                                      
*                                                                               
         L     R2,THISLINE                                                      
         LA    R2,DAYPTH(R2)                                                    
         OC    INVLDYPT,INVLDYPT                                                
         BZ    ERREND                                                           
*                                                                               
*  CHECK FOR DUPLICATE LINE                                                     
*                                                                               
         MVI   ERROR,DUPLICAT                                                   
*                                                                               
         L     RF,AIO3                                                          
BLDL660  CLI   0(RF),X'FF'                                                      
         BE    BLDL700                                                          
         CLC   0(6,RF),INVLNUM     COMPARE INV,EFF DATES                        
         BE    ERREND                                                           
         LA    RF,INVLEN(RF)                                                    
         B     BLDL660                                                          
BLDL700  MVC   0(INVLEN,RF),INVLNUM   ADD THE ENTRY                             
         MVI   INVLEN(RF),X'FF'    END OF TABLE MARKER                          
         ZIC   RF,RECCNT                                                        
         LA    RF,1(RF)                                                         
         STCM  RF,1,RECCNT         ADD TO RECORD COUNT                          
*                                                                               
BLDLEX   B     EXIT                                                             
         DROP  R6                                                               
         SPACE 3                                                                
*              BYTE 1  = NUMBER HALF HOURS, BYTE 2 CODE                         
*                                                                               
LENGTH   DC    AL1(01),C'0'        UP TO 1/2 HOUR                               
         DC    AL1(02),C'1'        FROM MORE THAN 1/2  TO     1 HOUR            
         DC    AL1(03),C'2'                         1     1 1/2                 
         DC    AL1(04),C'3'                     1 1/2         2                 
         DC    AL1(05),C'4'                         2     2 1/2                 
         DC    AL1(06),C'5'                     2 1/2         3                 
         DC    AL1(08),C'6'                         3         4                 
         DC    AL1(12),C'7'                         4         6                 
         DC    AL1(16),C'8'                         6         8                 
         DC    AL1(99),C'9'        OVER 8 HOURS                                 
         SPACE 3                                                                
TOMIN    NTR1                                                                   
         XR    RE,RE                                                            
         LH    RF,HALF                                                          
         D     RE,=F'100'                                                       
         MH    RF,=H'60'                                                        
         AR    RE,RF                                                            
         STH   RE,HALF                                                          
         B     EXIT                                                             
         SPACE 3                                                                
* SET TRANSMIT BIT ON ALL SCREEN FIELDS                                         
* R2 POINTS TO FIRST FIELD                                                      
TRANSCRN NTR1                                                                   
*                                                                               
TRNSCR20 LA    RF,MINTAGH                                                       
         CR    R2,RF                                                            
         BNL   TRNSCREX                                                         
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     TRNSCR20                                                         
*                                                                               
TRNSCREX B     EXIT                                                             
         SPACE 3                                                                
* SUB-ROUTINE TO CONVERT MILITARY TIME TO START QUARTER HOUR                    
*                                                                               
GETQTR   NTR1                                                                   
         LA    R6,LINEINP                                                       
         USING INVLINE,R6                                                       
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,INVLTIME       START TIME                                   
         SR    R0,R0                                                            
         D     R0,=F'100'          R1=HOURS, R0=REMAINDER MINUTES               
         MH    R1,=H'60'           CONVERT HOURS TO MINUTES                     
         AR    R1,R0               SUM TOTAL MINUTES                            
         CH    R1,=H'360'          TEST FOR LESS THAN 6 AM                      
         BNL   *+8                                                              
         AH    R1,=Y(60*24)        ADD MINUTES OF 24 HOURS                      
         SH    R1,=H'360'          SUBTRACT 6 HOURS TO BASE OFF 6AM             
         SR    R0,R0                                                            
         D     R0,=F'15'           DIVIDE BY MINUTES IN A QUARTER HOUR          
         STC   R1,BYTE                                                          
         PRINT GEN                                                              
         MVC   GINVBQTR,BYTE                                                    
         EDIT  (B1,BYTE),(2,GINVQTR),FILL=0                                     
         PRINT NOGEN                                                            
GETQTEX  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              ADD THE RECORDS                                 *                
****************************************************************                
****************************************************************                
ADDRECS  NTR1                                                                   
         L     R3,AIO1                                                          
         USING REINVREC,R3                                                      
         L     R4,AIO3                                                          
         USING INVLINE,R4                                                       
         LA    R2,MININV1H                                                      
         BAS   RE,SETLINE                                                       
         BNL   EXIT                END OF SCREEN                                
*                                                                               
ADDR050  CLI   0(R4),X'FF'                                                      
         BE    ADDREX                                                           
*                                                                               
         BAS   RE,CHKDUPS          CHECK DUPLICATE AND OVERLAPPING KEYS         
*  CLEAR THE IO AREA                                                            
         MVC   AIO,AIO1                                                         
         L     RE,AIO1                                                          
         LA    RF,1000                                                          
         XCEF                                                                   
*  SET UP THE KEY                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVLNUM                                                 
         GOTO1 DATCON,DMCB,(2,INVLEFFS),(3,RINVKSTD)                            
*  01 ELEMENT                                                                   
         MVC   RINVPEL(2),=XL2'0128'                                            
*  IF SELF ASSIGNED SET INDICATOR                                               
         NI    RINVSTAT,X'7F'                                                   
         TM    RMPPROFS,X'80'      ADD LOGIC                                    
         BZ    *+8                 BIT OFF COMPUTER ASSIGNED                    
         OI    RINVSTAT,X'80'      SET SELF ASSIGNED BIT                        
*                                                                               
         MVC   RINVPEFF(2),INVLEFFS                                             
         CLI   INVLEFFE,0          IS END DATE INPUTTED                         
         BE    *+10                                                             
         MVC   RINVPEFF+2(2),INVLEFFE                                           
         MVC   RINVOINV,INVOINV    OLD FORMAT INVENTORY NUMBER                  
         MVC   RINVDP,INVLDYPT                                                  
         OI    RINVATD,X'80'                                                    
*  02 ELEMENT (DAY TIME)                                                        
         XC    WORK2,WORK2                                                      
         LA    R5,WORK2                                                         
         USING RIDTELEM,R5                                                      
         MVC   RIDTCODE(2),=XL2'0207'                                           
         MVC   RIDTDAY,INVLDAY                                                  
         MVC   RIDTTIME,INVLTIME                                                
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),(R5),=C'ADD=CODE'              
         DROP  R5                                                               
*  03 ELEMENT (PROGRAM)                                                         
         XC    WORK2,WORK2                                                      
         LA    R5,WORK2                                                         
         USING RIPGELEM,R5                                                      
         MVC   RIPGCODE(2),=XL2'0318'                                           
         MVC   RIPGNAME(22),INVLPROG                                            
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),(R5),=C'ADD=CODE'              
         DROP  R5                                                               
*                                                                               
         BAS   RE,ACTIVITY         CREATE ACTIVITY ELEMENT                      
         BAS   RE,MAINTREC         WRITE THE RECORD OUT                         
         BAS   RE,ENDOLD           UPDATE PRIOR INVENTORY                       
*--MOVE INV INFO TO SCREEN                                                      
         MVC   8(4,R2),INVLNUM                                                  
         MVI   12(R2),C'*'                                                      
         MVI   5(R2),5                                                          
         OI    6(R2),X'80'                                                      
         BAS   RE,SETLINE          BUMP TO NEXT QUALIFIED LINE                  
         BNL   EXIT                END OF SCREEN                                
*                                                                               
         LA    R4,INVLEN(R4)       BUMP TO NEXT RECORD                          
         B     ADDR050                                                          
*                                                                               
ADDREX   B     EXIT                                                             
         SPACE 1                                                                
DPTBL    DC    C'MDERATLWKNPVSJOXYUZ'                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
BADFLT   DC    C'1234'                                                          
         DC    X'FF'                                                            
         SPACE 4                                                                
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              SAVE INVENTORY NUMBERS                          *                
****************************************************************                
****************************************************************                
SAVINVS  NTR1                                                                   
         LA    R3,BUFF                                                          
         XC    0(2,R3),0(R3)                                                    
         LA    R3,2(R3)                                                         
         MVI   0(R3),X'FF'                                                      
*                                                                               
         LA    R2,MININV1H                                                      
*                                                                               
SAVI050  CLI   5(R2),0                                                          
         BE    SAVI100                                                          
         CLI   12(R2),C'*'         WAS LINE VALIDATED                           
         BNE   SAVI100                                                          
         MVC   0(4,R3),8(R2)       SAVE INVENTORY                               
         LA    R3,4(R3)                                                         
         MVI   0(R3),X'FF'                                                      
SAVI100  LA    R2,LINELEN(R2)      BUMP TO NEXT LINE                            
         LA    RF,MINTAGH                                                       
         CR    R2,RF                                                            
         BNL   EXIT                END OF SCREEN EXIT                           
         B     SAVI050                                                          
*                                                                               
*  BUMP TO NEXT QUALIFIED LINE                                                  
*                                                                               
SETLINE  CLI   12(R2),C'*'                                                      
         BNER  RE                                                               
         LA    R2,LINELEN(R2)                                                   
         LA    RF,MINTAGH                                                       
         CR    R2,RF                                                            
         BNLR  RE                  END OF SCREEN EXIT                           
         B     SETLINE                                                          
         SPACE 4                                                                
****************************************************************                
****************************************************************                
*              CHECK FOR DUPLICATES/AND OVERLAPS               *                
****************************************************************                
****************************************************************                
CHKDUPS  NTR1                                                                   
         MVC   AIO,AIO2                                                         
*                                                                               
CHKD050  LA    R3,KEY                                                           
         CLI   0(R4),X'FF'                                                      
         BE    CHKDEX                                                           
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVLNUM                                                 
         GOTO1 DATCON,DMCB,(2,INVLEFFS),(3,RINVKSTD)                            
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         L     R3,AIO                                                           
         GOTO1 GETINV                                                           
         CLC   RERROR,=AL2(NOTFOUND)                                            
         BE    CHKDEX                                                           
*                                                                               
*  CHECK DUPLICATE KEY                                                          
         MVI   ERROR,DUPLICAT                                                   
         CLC   SAVEKEY,0(R3)                                                    
         BE    ERREND                                                           
*                                                                               
         MVI   ERROR,OVERLAP                                                    
         CLC   SAVEKEY(21),0(R3)   DOES RETURNED RECORD QUALIFY                 
         BNE   CHKDEX              NO EXIT                                      
         OC    INVLEFFE,INVLEFFE   DOES NEW RECORD HAVE END DATE                
         BNZ   CHKDEX              IF YES DONT CHECK OVERLAP                    
*  IF THE PRIOR INVOICE RECORDS END DATE EXCEEDS OR EQUALS THE START            
*  DATE OF THE NEW INVOICE RECORD THE DATES OVERLAP CAUSING ERROR.              
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    ERREND                                                           
*        CLC   RINVPEFF+2,INVLEFFS                                              
*        BNL   ERREND                                                           
*                                                                               
CHKDEX   B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO CREATE ACTIVITY ELEMENT                                        
*                                                                               
ACTIVITY NTR1                                                                   
         USING RINVAEL,R3                                                       
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'EF',AIO1),0                         
         CLI   12(R1),0                                                         
         BE    *+8                                                              
         B     ACTV100                                                          
         L     R3,12(R1)                                                        
* !!!!                                                                          
         MVC   SVALST,RINVALST                                                  
*                                                                               
         MVI   RINVAWHY,C'C'                                                    
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
* !!!!                                                                          
         CLC   SVALST,RINVALST     LAST ACTIVITY DATE = TODAY'S DATE            
         BE    *+8                                                              
         OI    MYFLAG,DOTRANS                                                   
         B     ACTV200                                                          
*-- ADD NEW ACTIVITY ELEMENT                                                    
ACTV100  LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   RINVACOD(2),=XL2'EF0C'                                           
         MVI   RINVAWHY,C'A'                                                    
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVAFST)                                 
* !!!!                                                                          
         OI    MYFLAG,DOTRANS                                                   
*                                                                               
ACTV200  GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),(R3),0                         
         B     ACTVEX                                                           
         PRINT NOGEN                                                            
ACTVEX   B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DO FILE AND DIRECTORY MAINTENENCE                              
*                                                                               
MAINTREC NTR1                                                                   
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
*                                                                               
         BAS   RE,MYFILADD                                                      
*  ADD THE PASSIVE POINTERS                                                     
         GOTO1 INVPTR,DMCB,0(R3),WORK2                                          
         GOTO1 NWPT,DMCB,WORK2                                                  
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO ADD PASSIVE POINTERS                                  
         SPACE 1                                                                
*              PARAM 1   BYTES 1-3 A(LIST OF POINTERS)                          
         SPACE 1                                                                
NWPT     NTR1                                                                   
         L     R2,0(R1)                                                         
NWPT1    CLI   0(R2),0                                                          
         BE    EXIT                END OF LIST                                  
         MVC   KEY(27),0(R2)                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY                                                  
         BE    NWPT3                                                            
         MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,MYDIRADD                                                      
         B     NWPT4                                                            
         SPACE 1                                                                
NWPT3    MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,MYDIRWRT                                                      
         SPACE 1                                                                
NWPT4    LA    R2,32(R2)                                                        
         B     NWPT1                                                            
         SPACE 2                                                                
*              ROUTINE TO DELETE POINTERS                                       
         SPACE 1                                                                
DELPT    NTR1                                                                   
         L     R2,0(R1)                                                         
DELPT1   CLI   0(R2),0                                                          
         BE    EXIT                                                             
         MVC   KEY(27),0(R2)                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DELPT4                                                           
         OI    KEY+27,X'80'                                                     
         BAS   RE,MYDIRWRT                                                      
         SPACE 1                                                                
DELPT4   LA    R2,32(R2)                                                        
         B     DELPT1                                                           
         EJECT                                                                  
         SPACE 2                                                                
*              ROUTINE TO RESTORE POINTERS                                      
         SPACE 1                                                                
RSTPT    NTR1                                                                   
         L     R2,0(R1)                                                         
RSTPT1   CLI   0(R2),0                                                          
         BE    EXIT                                                             
         MVC   KEY(27),0(R2)                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY                                                  
         BNE   RSTPT4                                                           
         NI    KEY+27,X'7F'                                                     
         BAS   RE,MYDIRWRT                                                      
         SPACE 1                                                                
RSTPT4   LA    R2,32(R2)                                                        
         B     RSTPT1                                                           
         EJECT                                                                  
*              CREATE NEW PASSIVE POINTER                                       
         SPACE 1                                                                
*              PARAM 1   BYTES 1-3 A(INVENTORY RECORD)                          
*              PARAM 2   BYTES 1-3 A(200 BYTE OUTPUT AREA)                      
         SPACE 1                                                                
         USING RINVREC,R2                                                       
         USING RIDPKEY,R4                                                       
INVPTR   NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R4,4(R1)                                                         
         XC    0(200,R4),0(R4)                                                  
         LA    R6,6                                                             
         LA    R3,RINVDP                                                        
         SPACE 1                                                                
INVPTR1  MVI   RIDPKTYP,X'92'                                                   
         MVC   RIDPKREP,RINVKREP                                                
         MVC   RIDPKSTA,RINVKSTA                                                
         MVC   RIDPKDPT,0(R3)                                                   
         MVC   RIDPKINV,RINVKINV                                                
         MVC   RIDPKSTD,RINVKSTD                                                
         SPACE 1                                                                
*                                                                               
*  IF SELF ASSIGNED GET NEXT DAYPART                                            
*  ONLY COMPUTER GENERATED NUMBERS GET THE DAY,QTR HOUR                         
*  AND THE LENGTH FILLED IN.                                                    
*                                                                               
         TM    RMPPROFS,X'80'                                                   
         BO    INVPTR20            BIT ON SELF ASSIGNED                         
*                                                                               
         MVC   RIDPKDAY,RINVOINV+1   MOVE DAY CODE                              
         MVC   RIDPKQTR,RINVOINV     QUARTER HOUR,                              
         MVC   RIDPKLEN,RINVOINV+2   AND PROGRAM LENGTH TO KEY                  
*                                                                               
         LA    RE,EFFDAT           SPECIAL DAYPARTS                             
INVPTR10 CLI   0(RE),X'FF'                                                      
         BE    INVPTR20                                                         
         CLC   0(1,R3),0(RE)                                                    
         BE    INVPTR15                                                         
         LA    RE,1(RE)                                                         
         B     INVPTR10                                                         
*                                                                               
INVPTR15 XC    RIDPKDAY,RIDPKDAY                                                
         MVC   RIDPKDTE,RINVPEFF                                                
         SPACE                                                                  
INVPTR20 LA    R3,1(R3)            NEXT DAYPART CODE                            
         CLI   0(R3),X'40'                                                      
         BNH   INVPTX                                                           
         LA    R4,32(R4)                                                        
         BCT   R6,INVPTR1          DO NEXT POINTER                              
         SPACE 1                                                                
INVPTX   B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2,R4                                                            
         SPACE 1                                                                
*  THESE DAYPARTS GET A DAY CODE, QUARTER HOUR, AND PROGRAM LENGTH              
DAYCOD   DC    C'MDKNPOUXYWZ',X'FF'                                             
         SPACE 1                                                                
*  THESE DAYPARTS GET EFFECTIVE DATE, QUARTER HOUR, AND PROGRAM LENGTH          
EFFDAT   DC    C'VSJ',X'FF'                                                     
         SPACE 1                                                                
*  THESE DAYPARTS ONLY GET INVENTORY NUMBER AND START DATE                      
*       ERATLF - THEY ARE THE FRINGE "SUB-DAYPARTS"                             
* (W-WEEKEND IS NOT TREATED AS FRINGE FOR PASSIVE POINTERS, BUT                 
*    IS GROUPED WITH FRINGE EVERYWHERE ELSE)                                    
         EJECT                                                                  
*              END OLD INVENTORY THAT HAS NO END DATE                           
*    AIO2-OLD REORD TO BE CHANGED                                               
*    AIO1-NEW RECORD JUST ADDED                                                 
*    R4-TABLE ENTRY OF CURRENT INVENTORY RECORD                                 
         SPACE 1                                                                
ENDOLD   NTR1                                                                   
         USING INVLINE,R4                                                       
*                                                                               
         CLI   INVLEFFE,0                                                       
         BNE   EXIT                NEW RECORD HAS END                           
         OC    INVLEFFE,INVLEFFE                                                
         BZ    EXIT                NO DATA EXIT                                 
         SPACE 1                                                                
         L     R2,AIO1             RECORD JUST ADDED                            
         L     R3,AIO2             RECORD TO BE CHANGED                         
         USING REINVREC,R3                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(21),0(R2)                                                    
         GOTO1 HIGH                                                             
         SPACE 1                                                                
ENDOLDA  CLC   KEYSAVE(21),0(R2)                                                
         BNE   ENDOLDX             NO MATCHING KEY                              
         SPACE 1                                                                
         OC    KEY+24(3),KEY+24    MUST BE A HEADER                             
         BNZ   ENDOLDN                                                          
         SPACE 1                                                                
         CLC   KEY(27),0(R2)       THIS IS THE RECORD                           
         BE    ENDOLDN             I JUST ADDED                                 
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BNZ   ENDOLDN             THIS ONE HAS END DATE                        
         L     R3,AIO1             SET USING TO NEW RECORD                      
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(0,WORK)  START OF NEW ITEM             
         L     R3,AIO2             SET USING TO CHANGE RECORD                   
         SPACE 1                                                                
         MVC   HALF,INVLEFFE       NUMBER OF DAYS TO DECREASE                   
         LH    R2,HALF                                                          
         LCR   R2,R2                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,RINVPEFF+2)                            
         SPACE 1                                                                
         CLC   RINVPEFF(2),RINVPEFF+2   THE END CAN NOT BE LOWER                
         BNH   *+10                     THAN THE START                          
         MVC   RINVPEFF+2(2),RINVPEFF                                           
         MVC   AIO,AIO2                                                         
         BAS   RE,MYFILWRT         WRITE RECORD OUT                             
         B     ENDOLDX                                                          
         SPACE 1                                                                
ENDOLDN  GOTO1 SEQ                                                              
         B     ENDOLDA                                                          
         SPACE 1                                                                
ENDOLDX  MVC   AIO,AIO1            RESET AIO                                    
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ISSUE LTRANS REQUEST?                                                         
***********************************************************************         
GOLTRANS NTR1                                                                   
         TM    MYFLAG,DOTRANS      ISSUE LTRANS REQUEST?                        
         BZ    GOLTRANX            NO                                           
*                                                                               
         CLI   CSTAT+4,C'T'        TELEVISION?                                  
         BNE   *+8                                                              
         MVI   CSTAT+4,C' '        MOVE IN SPACE FOR LTRANS                     
*                                                                               
         NI    DMINBTS,X'FF'-X'80' TURN OFF DELETED REC READ                    
         GOTO1 VLTRANS             YES- ISSUE LTRANS REQUEST                    
         NI    MYFLAG,X'FF'-DOTRANS                                             
*                                                                               
         CLI   CSTAT+4,C' '        TELEVISION?                                  
         BNE   *+8                                                              
         MVI   CSTAT+4,C'T'        MOVE BACK 'T' FOR TELEVISION                 
*                                                                               
GOLTRANX B     EXIT                                                             
***********************************************************************         
* DATAMGR INTERFACE                                                             
***********************************************************************         
MYFILADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,DMCHECK                                                       
         MVC   BSVDA,KEY+28     SAVE DISK ADDRESS                               
         B     YES                                                              
*                                                                               
MYFILWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'REPDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REPDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'20'                                                      
         BO    DUPERR                                                           
         TM    8(R1),X'90'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1  REGS=(R0,R1)                                                     
*                                                                               
DUPERR   MVI   ERROR,DUPLICAT                                                   
         B     ERREND                                                           
         EJECT                                                                  
*  BUMP TO NEXT SCREEN FIELD                                                    
NEXTFLD  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
ERREND   GOTO1 ERREX                                                            
*                                                                               
RELO     DS    A                                                                
         LTORG                                                                  
*                                                                               
REPFILE  DC    CL8'REPFILE'                                                     
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RERMPFFD                                                                      
* DDGENTWA                                                                      
* RERMPWTWA                                                                     
* RERMPD7D                                                                      
* REGENMKT                                                                      
* REGENREP(A)                                                                   
* RERMPWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPDCD                                                       
         EJECT                                                                  
       ++INCLUDE RERMPWTWA                                                      
         EJECT                                                                  
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
       ++INCLUDE RERMPWRKAD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*              WORK AREA                                                        
*                                                                               
STAHLD   DS    CL5                 STATION HOLD AREA                            
INVHLD   DS    CL4                 INVENTORY HOLD AREA                          
DTEHLD   DS    CL3                 DATE HOLD AREA                               
DTEHLD2  DS    CL2                 2 BYTE DATE HOLD AREA                        
DTEHLDE2 DS    CL2                 2 BYTE END DATE HOLD                         
*                                                                               
*  PRINT ELEMENT ADDRESS STORAGE LOCATIONS                                      
DYTMPTR  DS    F                   DAY/TIME ELEMENT                             
PROGPTR  DS    F                   PROGRAM ELEMENT                              
AVPRPTR  DS    F                   AVAIL PROGRAM ELEMENT                        
*                                                                               
         DS    0F                                                               
LINEINP  DS    CL44                INPUT LINE                                   
OVFLSW   DS    CL1                 TOO MANY LINES TO PRINT                      
*                                                                               
WORK2    DS    CL200               EXTRA WORK AREA                              
SAVEKEY  DS    CL27                                                             
CHNGLEN  DS    CL1                                                              
RECCNT   DS    CL1                 RECORD COUNT                                 
*                                                                               
MYFLAG   DS    XL1                                                              
DOTRANS  EQU   X'01'               ISSUE LTRANS REQUEST                         
*                                                                               
SVALST   DS    CL3                 SAVE LAST ACTIVITY DATE (Y/M/D BIN)          
*                                                                               
FIRSTSW  DS    CL1                                                              
DAYINP   DS    CL1                                                              
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
BSVDA    DS    CL4                 SAVED DISK ADDRESS                           
*                                                                               
THISLINE DS    A                   CURRENT LINE ADDRESS                         
*                                                                               
GINVNUM  DS    0CL4                GENERATED INVENTORY NUMBER                   
GINVQTR  DS    CL2                                                              
GINVDAY  DS    CL1                                                              
GINVLEN  DS    CL1                                                              
GINVBQTR DS    CL1                 QTR HOUR BINARY                              
         SPACE 2                                                                
*                                                                               
EFFDH    EQU   MINEFF1H-MININV1H                                                
EFFD     EQU   MINEFF1-MININV1H                                                 
DAYH     EQU   MINDAY1H-MININV1H                                                
DAY      EQU   MINDAY1-MININV1H                                                 
TIMEH    EQU   MINTIM1H-MININV1H                                                
TIME     EQU   MINTIM1-MININV1H                                                 
PROGH    EQU   MINPRG1H-MININV1H                                                
PROG     EQU   MINPRG1-MININV1H                                                 
DAYPTH   EQU   MINDPT1H-MININV1H                                                
DAYPT    EQU   MINDPT1-MININV1H                                                 
LINELEN  EQU   MININV2H-MININV1H                                                
         EJECT                                                                  
* INVENTORY LIST ENTRY DSECT                                                    
*                                                                               
INVLINE  DSECT                                                                  
INVLNUM  DS    CL4                 INVENTORY NUMBER                             
INVLEFFS DS    CL2                 EFFECTIVE DATE START                         
INVLEFFE DS    CL2                 EFFECTIVE DATE END                           
INVLDAY  DS    CL1                 DAY                                          
INVLTIME DS    CL4                 TIME                                         
INVLPROG DS    CL22                PROGRAM                                      
INVLDYPT DS    CL6                 DAYPARTS                                     
INVOINV  DS    CL3                 OLD 3 BYTE INVENTORY NNUMBER                 
*                                                                               
INVLEN   EQU   *-INVLINE                                                        
         SPACE 2                                                                
         EJECT                                                                  
RINVD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDCOMFACS                                                      
         SPACE 5                                                                
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T810FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T810FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067RERMP1    05/01/02'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
