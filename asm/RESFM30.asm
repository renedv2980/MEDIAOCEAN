*          DATA SET RESFM30    AT LEVEL 049 AS OF 08/01/05                      
*PHASE T81830A                                                                  
         TITLE 'T81830 - RESFM30 - EOP RECORDS'                                 
***********************************************************************         
*                                                                     *         
*  RESFM30 (T81830) --- EOP MAINTENANCE/LIST                          *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 30AUG93 (SKU) DATE OF BIRTH                                         *         
*                                                                     *         
* 19JAN95 (SKU) ALLOW FORMAT W FOR BIAS                               *         
*                                                                     *         
* 18APR96 (SKU) ADD FORMAT K/ENTERPRISE AND C/COLUMBINE               *         
*                                                                     *         
* 20JUN97 (SKU) ALLOW NONNUMERIC INPUT FOR AGENCY EOP                 *         
*                                                                     *         
* 11SEP00 (BU ) COLUMBINE TO FOLLOW JDS FORMAT                        *         
*                                                                     *         
* 17NOV00 (BU ) TRADE ALTERNATE OFFICE CODE DISPLAY                   *         
*                                                                     *         
* 24JAN01 (BU ) COLUMBINE S/P EOP ALIGNMENT                           *         
*                                                                     *         
* 12FEB01 (BU ) COLUMBINE ADVERTISER TRADE PROCESSING                 *         
*                                                                     *         
* 10SEP01 (BU ) BIAS OFFICE EOP CODE LENGTH FIX                       *         
*                                                                     *         
* 19SEP03 (BU ) DISPLAY DELETED CODES                                 *         
*                                                                     *         
* 01AUG05 (BU ) ESTABLISH 'W' AS A BIAS CODE                          *         
*                                                                     *         
*                                                                     *         
*HERE******************************************************************         
T81830   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81830*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
                                                                                
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVI   ACTELOPT,C'N'       DON'T WANT ACTIVITIES ELEMENT                
         MVI   IOOPT,C'Y'          DO MY OWN I/O'S                              
         OI    GENSTAT1,RDUPAPPL   ALLOW READ FOR UPDATE WITH                   
*                                   ACTION LIST                                 
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
                                                                                
         CLI   MODE,LISTRECS       LIST REC?                                    
         BNE   EXIT                                                             
                                                                                
         TM    SCRNFLAG,ACTPROCD   IF AN ACTION WAS PROCESSED LAST,             
         BZ    LIST                DON'T REFRESH SCREEN.  LET USER SEE          
         MVI   SCRNFLAG,NEXTSCRN   WHAT WAS JUST ENTERED                        
         NI    EOPTRAFH+4,X'FF'-X'20'  JUST ENSURES DISPLAY FROM TOP            
         B     ACTCHGD                                                          
                                                                                
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         MVI   SCRNFLAG,SAMESCRN   WE'RE NOT SCROLLING                          
         MVI   ADDFLAG,C'N'        AND WE ARE NOT ADDING                        
                                                                                
         TM    EOPTRAFH+4,X'20'    IF NO FIELDS WERE CHANGED                    
         BZ    VK30                  USER WANTS TO PAGE TO THE NEXT             
         TM    EOPSTATH+4,X'20'      SCREEN                                     
         BZ    VK30                                                             
         TM    EOPKADVH+4,X'20'                                                 
         BZ    VK30                                                             
         TM    EOPKAGYH+4,X'20'                                                 
         BZ    VK30                                                             
         TM    EOPKOFFH+4,X'20'                                                 
         BZ    VK30                                                             
         TM    EOPKSALH+4,X'20'                                                 
         BZ    VK30                                                             
                                                                                
         TM    EOPTRAFH+4,X'80'    IF FIELDS WERE CHANGED BUT CHANGED           
         BO    VK30                  TO THE SAME DATA, USER WANTS TO            
         TM    EOPSTATH+4,X'80'      UPDT FROM THE BEGINNING                    
         BO    VK30                                                             
         TM    EOPKADVH+4,X'80'                                                 
         BO    VK30                                                             
         TM    EOPKAGYH+4,X'80'                                                 
         BO    VK30                                                             
         TM    EOPKOFFH+4,X'80'                                                 
         BO    VK30                                                             
         TM    EOPKSALH+4,X'80'                                                 
         BO    VK30                                                             
                                                                                
         MVI   SCRNFLAG,NEXTSCRN                                                
         LA    R2,EOPACTH          CHECK IF ACTION COLUMN HAS INPUTS            
                                                                                
VK10     DS    0H                                                               
         CLI   5(R2),0             FIELD HAS INPUT                              
         BE    VK20                                                             
         CLI   8(R2),C'*'          WAS THIS LINE PROCESSED?                     
         BE    VK20                YES, THEN SKIP IT                            
                                                                                
         GOTO1 PROCACT,DMCB,(R2)   GO PROCESS ACTION                            
         BNZ   INVLFLD                                                          
         MVI   SCRNFLAG,ACTPROCD                                                
         MVC   8(3,R2),=C'*  '     THIS MEANS THIS ROW WAS SUCCESSFULLY         
         OI    6(R2),X'80'          PROCESSED                                   
                                                                                
VK20     DS    0H                                                               
         LA    RF,EOPLACTH-EOPACTH                                              
         AR    R2,RF               BUMP TO NEXT ACTION                          
         LA    R1,EOPFINH                                                       
         CR    R2,R1                                                            
         BL    VK10                                                             
         B     VKX                                                              
                                                                                
VK30     DS    0H                                                               
         OI    EOPTRAFH+4,X'20'    SET VALIDATED                                
         OI    EOPSTATH+4,X'20'    SET VALIDATED                                
         OI    EOPKADVH+4,X'20'    SET VALIDATED                                
         OI    EOPKAGYH+4,X'20'    SET VALIDATED                                
         OI    EOPKOFFH+4,X'20'    SET VALIDATED                                
         OI    EOPKSALH+4,X'20'    SET VALIDATED                                
                                                                                
         XC    EOPKEY,EOPKEY                                                    
         XC    FIRSTKEY,FIRSTKEY                                                
         MVI   FILTER,0                                                         
                                                                                
         LA    R2,EOPTRAFH         TRAFFIC SYSTEM                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
                                                                                
         MVI   TRAFFSYS,1                                                       
         CLI   8(R2),C'B'          BIAS?                                        
         BE    VK40                                                             
         CLI   8(R2),C'W'          BIAS/ALTERNATE WORKSHEET?                    
         BE    VK40                                                             
*                                                                               
         MVI   TRAFFSYS,2                                                       
         CLI   8(R2),C'J'          JDS/2000?                                    
         BE    VK40                                                             
*                                                                               
         MVI   TRAFFSYS,3                                                       
         CLI   8(R2),C'K'          ENTERPRISE                                   
         BE    VK40                                                             
         CLI   8(R2),C'E'          ENTERPRISE                                   
         BE    VK40                                                             
*                                                                               
         MVI   TRAFFSYS,4                                                       
         CLI   8(R2),C'C'          COLUMBINE                                    
         BNE   INVLFLD                                                          
                                                                                
VK40     DS    0H                                                               
         LA    R2,EOPSTATH         STATION                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
                                                                                
         GOTO1 VALISTA             WORK WILL HAVE STATION+MEDIA                 
                                                                                
         BAS   RE,CKACCESS         CHECK SIGNON ACCESS                          
         BNZ   SLOCKOUT                                                         
                                                                                
         L     R6,AIO                                                           
         USING RSTAREC,R6                                                       
                                                                                
         CLI   EOPTRAF,C'J'        CHECK TRAFFIC CODE                           
         BNE   VK50                                                             
         CLI   RSTATRAF,C'J'                                                    
         BNE   NOTJDS              STATION NOT JDS/2000                         
         B     VK90                                                             
                                                                                
VK50     DS    0H                                                               
         CLI   EOPTRAF,C'B'        CHECK TRAFFIC CODE                           
         BNE   VK55                                                             
         CLI   RSTATRAF,C'B'                                                    
         BNE   NOTBIAS             STATION NOT BIAS                             
         B     VK90                                                             
VK55     DS    0H                                                               
         CLI   EOPTRAF,C'W'        CHECK TRAFFIC CODE                           
         BNE   VK60                                                             
         CLI   RSTATRAF,C'W'       ALLOW W FOR BIAS                             
         BNE   NOTBIAS             STATION NOT BIAS                             
         B     VK90                                                             
*                                                                               
VK60     DS    0H                                                               
         CLI   EOPTRAF,C'K'        CHECK TRAFFIC CODE                           
         BE    VK70                                                             
         CLI   EOPTRAF,C'E'        CHECK TRAFFIC CODE                           
         BNE   VK80                                                             
VK70     DS    0H                                                               
         CLI   RSTATRAF,C'K'                                                    
         BNE   NOTENTER            STATION NOT ENTERPRISE                       
         B     VK90                                                             
*                                                                               
VK80     DS    0H                                                               
         CLI   RSTATRAF,C'C'                                                    
         BNE   NOTCOLM             STATION NOT COLUMBINE                        
         DROP  R6                                                               
                                                                                
VK90     DS    0H                                                               
         MVC   KCODE,SPACES                                                     
         LA    R2,EOPKADVH         POINT HERE IF ERROR                          
                                                                                
         CLI   EOPKADVH+5,0        ONE AND ONLY ONE OF THESE FIELDS             
         BE    VK100               MUST HAVE AN INPUT                           
                                                                                
         CLI   EOPKADV,C'+'                                                     
         BNE   *+12                                                             
         MVI   ADDFLAG,C'Y'                                                     
         MVI   EOPKADV,C'A'                                                     
                                                                                
         ZIC   RF,FILTER                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FILTER                                                        
                                                                                
         MVC   EOPHFLD(3),=C'ADV'  SOFT SCREEN HEADING                          
                                                                                
         MVI   EOPKEY,X'1B'                                                     
         MVC   EOPKEY+15(2),AGENCY                                              
         MVC   EOPKEY+17(1),TRAFFSYS                                            
         MVC   EOPKEY+18(5),WORK                                                
         MVC   EOPKEY+23(4),EOPKADV                                             
         MVC   KCODE(4),EOPKADV                                                 
         MVI   CODESIZE,L'EOPKADV                                               
         MVI   CODEOFFS,23                                                      
         MVI   TRFFLDLN,4          JDS ADV EOP CODE LENGTH                      
         CLI   TRAFFSYS,4          COLUMBINE?                                   
         BNE   VK100               NO                                           
         MVI   TRFFLDLN,5          YES - SET COL EOP LEN                        
                                                                                
VK100    DS    0H                                                               
         CLI   EOPKAGYH+5,0                                                     
         BE    VK110                                                            
                                                                                
         CLI   EOPKAGY,C'+'                                                     
         BNE   *+12                                                             
         MVI   ADDFLAG,C'Y'                                                     
         MVI   EOPKAGY,C'A'                                                     
                                                                                
         LA    R2,EOPKAGYH         POINT HERE IF ERROR                          
         ZIC   RF,FILTER                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FILTER                                                        
                                                                                
         MVC   EOPHFLD(3),=C'AGY'  SOFT SCREEN HEADING                          
                                                                                
         MVI   EOPKEY,X'1C'                                                     
         MVC   EOPKEY+13(2),AGENCY                                              
         MVC   EOPKEY+15(1),TRAFFSYS                                            
         MVC   EOPKEY+16(5),WORK                                                
         MVC   EOPKEY+21(6),EOPKAGY                                             
         MVC   KCODE(6),EOPKAGY                                                 
         MVI   CODESIZE,L'EOPKAGY                                               
         MVI   CODEOFFS,21                                                      
         MVI   TRFFLDLN,6          JDS AGY EOP CODE LENGTH                      
         CLI   TRAFFSYS,4          COLUMBINE?                                   
         BNE   VK110               NO                                           
         MVI   TRFFLDLN,5          YES - SET COL EOP LEN                        
VK110    DS    0H                                                               
         CLI   EOPKOFFH+5,0                                                     
         BE    VK120                                                            
                                                                                
         CLI   EOPKOFF,C'+'                                                     
         BNE   *+12                                                             
         MVI   ADDFLAG,C'Y'                                                     
         MVI   EOPKOFF,C'A'                                                     
                                                                                
         LA    R2,EOPKOFFH         POINT HERE IF ERROR                          
         ZIC   RF,FILTER                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FILTER                                                        
                                                                                
         MVC   EOPHFLD(3),=C'OFF'  SOFT SCREEN HEADING                          
                                                                                
         MVI   EOPKEY,X'1D'                                                     
         MVC   EOPKEY+17(2),AGENCY                                              
         MVC   EOPKEY+19(1),TRAFFSYS                                            
         MVC   EOPKEY+20(5),WORK                                                
         MVC   EOPKEY+25(2),EOPKOFF                                             
         MVC   KCODE(2),EOPKOFF                                                 
         MVI   CODESIZE,L'EOPKOFF                                               
         MVI   CODEOFFS,25                                                      
         MVI   TRFFLDLN,3          JDS OFF EOP CODE LENGTH                      
         CLI   TRAFFSYS,1          BIAS?                                        
         BNE   VK120               NO                                           
         MVI   TRFFLDLN,6          YES - SET BIAS EOP LEN                       
                                                                                
VK120    DS    0H                                                               
         CLI   EOPKSALH+5,0                                                     
         BE    VKX                                                              
                                                                                
         CLI   EOPKSAL,C'+'                                                     
         BNE   *+12                                                             
         MVI   ADDFLAG,C'Y'                                                     
         MVI   EOPKSAL,C'A'                                                     
                                                                                
         LA    R2,EOPKSALH         POINT HERE IF ERROR                          
         ZIC   RF,FILTER                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FILTER                                                        
                                                                                
         MVC   EOPHFLD(3),=C'SAL'  SOFT SCREEN HEADING                          
                                                                                
         MVI   EOPKEY,X'1E'                                                     
         MVC   EOPKEY+16(2),AGENCY                                              
         MVC   EOPKEY+18(1),TRAFFSYS                                            
         MVC   EOPKEY+19(5),WORK                                                
         MVC   EOPKEY+24(3),EOPKSAL                                             
         MVC   KCODE(3),EOPKSAL                                                 
         MVI   CODESIZE,L'EOPKSAL                                               
         MVI   CODEOFFS,24                                                      
         MVI   TRFFLDLN,4          JDS SAL EOP CODE LENGTH                      
         CLI   TRAFFSYS,4          COLUMBINE?                                   
         BNE   VKX                 NO                                           
         MVI   TRFFLDLN,3          YES - SET COL EOP LEN                        
                                                                                
VKX      DS    0H                                                               
         CLI   FILTER,0            MUST HAVE AT LEAST ONE FIELD                 
         BE    MISSFLD                                                          
         CLI   FILTER,1            CAN ONLY FILTER ON ONE FIELD                 
         BH    INVLFLD                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LIST     DS    0H                                                               
         TWAXC EOPACTH,PROT=Y                                                   
                                                                                
         LA    RF,EOPCODEH         SET ALL DDS CODE FIELDS                      
                                                                                
LIST10   DS    0H                                                               
         XC    8(L'EOPCODE,RF),8(RF)                                            
         NI    6(RF),X'FF'-X'20'   DEFAULT TO UNPROTECT                         
         LA    RE,EOPLCDEH-EOPCODEH                                             
         AR    RF,RE                                                            
         LA    RE,EOPFINH                                                       
         CR    RF,RE                                                            
         BNH   LIST10                                                           
                                                                                
         CLI   ADDFLAG,C'Y'        IF USER WANTS TO ADD                         
         BNE   LIST20               CLEAR SCREEN AND EXIT W/MSG                 
         LA    R2,EOPACTH                                                       
         MVI   ADDFLAG,C'N'                                                     
         B     GOADD                                                            
                                                                                
LIST20   DS    0H                                                               
         LA    R2,EOPSTRTH                                                      
                                                                                
         TM    SCRNFLAG,NEXTSCRN   USER JUST PRESSED ENTER?                     
         BZ    LIST30              W/O CHANGING THE KEY FIELDS                  
         MVC   KEY,SAVEKEY         YES, DISPLAY NEXT PAGE OF CONTRACTS          
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'REOPKEY),FIRSTKEY                                      
         BE    LIST40              DON'T DO A SEQ IF DISPLAY FROM BEG           
         B     LIST150                                                          
                                                                                
LIST30   DS    0H                                                               
         MVC   KEY,EOPKEY                                                       
         MVC   FIRSTKEY,EOPKEY                                                  
         OI    DMINBTS,X'08'       RETURN DELETED KEYS                          
         GOTO1 HIGH                                                             
                                                                                
LIST40   DS    0H                                                               
         CLI   EOPKEY,X'1B'                                                     
         BNE   LIST50                                                           
         CLI   KEY,X'1B'           1B KEY IN PROGRESS?                          
         BNE   LIST200             NO  - FINISHED                               
         MVC   KEY1BSAV,KEY        YES - SAVE 1B00/1B01                         
         CLC   KEY+2(21),EOPKEY+2                                               
*                                  PROCESS 1B00/1B01 KEYS, BREAK                
*                                     ONLY ON END OF 1B KEYS                    
         BE    LIST100             SAME KEY                                     
         CLI   KEY,X'1B'           1B KEY FOUND?                                
         BE    LIST150             YES - SKIP IT                                
         BH    LIST200             NO  - FINISHED                               
         DC    H'0'                THIS IS A READ ERROR                         
LIST50   DS    0H                                                               
         CLI   EOPKEY,X'1C'                                                     
         BNE   LIST60                                                           
         CLC   KEY(21),KEYSAVE                                                  
         BNE   LIST200                                                          
         B     LIST100                                                          
                                                                                
LIST60   DS    0H                                                               
         CLI   EOPKEY,X'1D'                                                     
         BNE   LIST70                                                           
         CLC   KEY(25),KEYSAVE                                                  
         BNE   LIST200                                                          
         B     LIST100                                                          
                                                                                
LIST70   DS    0H                                                               
         CLI   EOPKEY,X'1E'                                                     
         BNE   LIST200                                                          
         CLC   KEY(24),KEYSAVE                                                  
         BNE   LIST200                                                          
                                                                                
LIST100  DS    0H                                                               
         OI    DMINBTS,X'08'       RETURN DELETED RECORDS                       
         GOTO1 GETREC                                                           
         BAS   RE,DISLINE                                                       
         LA    RF,EOPFINH          STOP IF WE'RE AT END OF SCREEN               
         CR    R2,RF                                                            
         BNL   LISTX                                                            
                                                                                
LIST150  DS    0H                                                               
         GOTO1 SEQ                                                              
         B     LIST40                                                           
                                                                                
LIST200  DS    0H                  WE'VE HIT THE LAST RECORD                    
         MVC   SAVEKEY,FIRSTKEY                                                 
         MVC   EOPLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         LA    R2,EOPTRAFH         PUT CURSOR HERE                              
         B     ENDLIST                                                          
                                                                                
LISTX    DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         MVC   EOPLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         B     NEXTLIST                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A LINE OF RECORD                                                      
***********************************************************************         
DISLINE  NTR1                                                                   
         MVC   TEMPKEY,KEY         SO WE CAN RESTORE AT END OF ROUTINE          
                                                                                
         L     R6,AIO                                                           
         USING REOPREC,R6                                                       
         MVC   8(6,R2),REOPEQUV                                                 
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         ZIC   RF,CODEOFFS                                                      
         AR    RF,R6               FIELD OFFSET IN KEY                          
         ZIC   R1,CODESIZE                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(RF)                                                    
         OI    6(R2),X'20'         PROTECT THIS FIELD, USER CAN'T CHG           
                                                                                
         MVC   WORK(20),=C'* RECORD NOT FOUND *'                                
                                                                                
         CLI   EOPKEY,X'1B'                                                     
         BNE   DISL10                                                           
         GOTO1 VADV                EXPANDED ADVERTISER NAME                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(20,R2),WORK                                                    
         B     DISL50                                                           
                                                                                
DISL10   DS    0H                                                               
         CLI   EOPKEY,X'1C'                                                     
         BNE   DISL20                                                           
         GOTO1 VAGY                EXPANDED AGENCY NAME                         
         BZ    DISL15              FOUND                                        
         GOTO1 VAGY2                                                            
DISL15   EQU   *                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(20,R2),WORK                                                    
         B     DISL50                                                           
                                                                                
DISL20   DS    0H                                                               
         CLI   EOPKEY,X'1D'                                                     
         BNE   DISL30                                                           
         GOTO1 VOFF                EXPANDED OFFICE NAME                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(20,R2),WORK                                                    
         B     DISL50                                                           
                                                                                
DISL30   DS    0H                                                               
         CLI   EOPKEY,X'1E'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VSAL                EXPANDED SALESPERSON NAME                    
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(20,R2),WORK                                                    
                                                                                
DISL50   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 DATCON,DMCB,(3,REOPDATE),(5,8(R2))                               
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(7,R2),=C'DELETED'                                              
         TM    TEMPKEY+27,X'80'    KEY DELETED?                                 
         BO    DISL60              YES                                          
         MVC   8(7,R2),=C'ACTIVE '                                              
         TM    REOPFLAG,X'80'                                                   
         BO    *+10                                                             
         MVC   8(8,R2),=C'INACTIVE'                                             
DISL60   EQU   *                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         MVC   KEY,TEMPKEY         RESTORE LIST SEQ ORDER                       
         GOTO1 HIGH                                                             
                                                                                
         XIT1  REGS=(R2)           R2 POINTS TO THE NEXT LINE                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS INPUT IN ACTION COLUMN                                                
* P1 HAS FIELD HEADER                                                           
***********************************************************************         
PROCACT  NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         CLC   =C'ADD',8(R2)       ACTION ADD                                   
         BE    PROCADD                                                          
         CLI   8(R2),C'A'                                                       
         BE    PROCADD                                                          
                                                                                
         CLC   =C'CHA',8(R2)       ACTION CHANGE                                
         BE    PROCGET                                                          
         CLI   8(R2),C'C'                                                       
         BE    PROCGET                                                          
                                                                                
         CLC   =C'DEL',8(R2)       ACTION DELETE                                
         BE    PROCGET                                                          
                                                                                
         CLC   =C'RES',8(R2)       ACTION RESTORE                               
         BE    PROCGET                                                          
         B     NO                  INVALID ACTION                               
         EJECT                                                                  
PROCADD  DS    0H                  PROCESS ACTION ADD                           
         L     R6,AIO2             USE IO2                                      
         XC    0(256,R6),0(R6)                                                  
         MVC   0(L'REOPKEY,R6),EOPKEY                                           
         MVC   27(L'REOPLEN,R6),=X'0022'                                        
                                                                                
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING REOPELEM,R5                                                      
         MVI   REOPCODE,X'01'                                                   
         MVI   REOPELLN,16                                                      
         GOTO1 DATCON,DMCB,(5,0),(3,REOPDATE)                                   
                                                                                
         LR    R3,R2                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO THE EOP CODE FIELD                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO THE DDS FIELD                       
                                                                                
         CLI   EOPKEY,X'1B'                                                     
         BNE   PADD10                                                           
         GOTO1 VADV                                                             
         BNZ   INVLFLD                                                          
         B     PADD40                                                           
                                                                                
PADD10   DS    0H                                                               
         CLI   EOPKEY,X'1C'                                                     
         BNE   PADD20                                                           
         GOTO1 VAGY                                                             
         BNZ   INVLFLD                                                          
         B     PADD40                                                           
                                                                                
PADD20   DS    0H                                                               
         CLI   EOPKEY,X'1D'                                                     
         BNE   PADD30                                                           
         GOTO1 VOFF                                                             
         BNZ   INVLFLD                                                          
         B     PADD40                                                           
                                                                                
PADD30   DS    0H                                                               
         GOTO1 VSAL                                                             
         BNZ   INVLFLD                                                          
                                                                                
PADD40   DS    0H                                                               
         MVC   AIO,AIO2            VADV/VAGY/VOFF/VSAL USES AIO3                
*                                    SO CHANGE IT BACK TO AIO2                  
         ZIC   RF,CODEOFFS         BUILD LAST FIELD IN KEY                      
         AR    RF,R6               FIELD OFFSET IN KEY                          
         ZIC   R1,CODESIZE                                                      
         BCTR  R1,0                                                             
         EX    R1,PADD50                                                        
         EX    R1,PADD60                                                        
         B     PADD70                                                           
PADD50   MVC   0(0,RF),8(R2)       ADV/AGY/OFF/SAL                              
PADD60   OC    0(0,RF),SPACES      SPACE PAD                                    
                                                                                
PADD70   DS    0H                                                               
         MVC   KEY,0(R6)                                                        
         OI    DMINBTS,X'08'       READ DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PADD80                                                           
         TM    KEY+27,X'80'                                                     
         BO    DELRECEX            DELETED RECORD EXISTS                        
         B     RECONFIL            RECORD ALREADY ON FILE                       
                                                                                
PADD80   DS    0H                                                               
         LR    R2,R3                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO THE EOP CODE FIELD                  
                                                                                
         CLI   EOPTRAF,C'J'        JDS/2000 -> JDS/2000 FORMAT                  
         BE    PADD90                                                           
                                                                                
         CLI   EOPTRAF,C'C'        COLUMBINE                                    
         BE    PACO90                                                           
                                                                                
         CLI   5(R2),6             UP TO LENGTH OF 6                            
         BH    INVLFLD                                                          
         TM    4(R2),X'08'         NUMERIC INPUTS ONLY                          
         BZ    INVLFLD                                                          
                                                                                
         XC    REOPEQUV,REOPEQUV                                                
         MVC   REOPEQUV(6),=6C'0'                                               
         LA    RE,REOPEQUV+6                                                    
         ZIC   RF,5(R2)                                                         
         SR    RE,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)                                                    
         B     PADD140                                                          
                                                                                
PADD90   DS    0H                  FOR JDS                                      
         CLC   TRFFLDLN,5(R2)      CHECK LENGTH                                 
         BL    INVLFLD                                                          
                                                                                
         CLI   EOPKEY,X'1C'        IF AGENCY                                    
         BE    PADD120                                                          
         CLI   EOPKEY,X'1E'        IF SALESPERSON                               
         BE    PADD120                                                          
                                                                                
         ZIC   R1,TRFFLDLN                                                      
         BCTR  R1,0                                                             
         EX    R1,PADD100                                                       
         EX    R1,PADD110                                                       
         B     PADD140                                                          
PADD100  MVC   REOPEQUV(0),8(R2)                                                
PADD110  OC    REOPEQUV(0),SPACES                                               
                                                                                
PADD120  DS    0H                                                               
         TM    4(R2),X'08'         NUMERIC??                                    
         BNZ   PADD125                                                          
         MVC   REOPEQUV(6),8(R2)                                                
         B     PADD140                                                          
                                                                                
PADD125  DS    0H                                                               
         ZIC   R1,5(R2)                                                         
         ZIC   RF,TRFFLDLN                                                      
         LA    RE,REOPEQUV                                                      
         AR    RE,RF                                                            
         SR    RE,R1                                                            
         BCTR  R1,0                                                             
         BCTR  RF,0                                                             
         EX    RF,PADD130                                                       
         EX    R1,PADD135                                                       
         B     PADD140                                                          
PADD130  MVC   REOPEQUV(0),=6C'0'                                               
PADD135  MVC   0(0,RE),8(R2)                                                    
                                                                                
***>>>   COLUMBINE CODE PROCESSING                                              
PACO90   DS    0H                  FOR COLUMBINE                                
         CLC   TRFFLDLN,5(R2)      CHECK LENGTH                                 
         BL    INVLFLD                                                          
                                                                                
         CLI   EOPKEY,X'1E'        SALESPERSON IS A 3-CHAR FIELD                
         BE    PACO120                                                          
*                                  PROCESS AGENCY AND ADVERTISER                
         LA    R5,REOPEQUV         SET A(EQUIV FIELD)                           
         LA    RF,5                SET MAX LENGTH                               
         ZIC   RE,5(R2)            SET LENGTH OF INPUT                          
         SR    RF,RE               SUBTRACT LENGTH FROM MAX                     
         AR    R5,RF               ADD DISPLACEMENT                             
         ZIC   R1,TRFFLDLN                                                      
         BCTR  R1,0                                                             
         EX    R1,PACO100                                                       
**       EX    R1,PACO110                                                       
         B     PADD140                                                          
PACO100  MVC   0(0,R5),8(R2)                                                    
PACO110  OC    REOPEQUV(0),SPACES                                               
                                                                                
PACO120  DS    0H                                                               
         TM    4(R2),X'08'         NUMERIC??                                    
         BNZ   PACO125             YES                                          
         MVC   REOPEQUV+3(3),8(R2)                                              
         B     PADD140                                                          
                                                                                
PACO125  DS    0H                                                               
         ZIC   R1,5(R2)                                                         
         ZIC   RF,TRFFLDLN                                                      
         LA    RE,REOPEQUV+3                                                    
         AR    RE,RF                                                            
         SR    RE,R1                                                            
         BCTR  R1,0                                                             
         BCTR  RF,0                                                             
         EX    RF,PACO130                                                       
         EX    R1,PACO135                                                       
         B     PADD140                                                          
PACO130  MVC   REOPEQUV(0),=6C'0'                                               
PACO135  MVC   0(0,RE),8(R2)                                                    
***>>>   COLUMBINE CODE PROCESSING                                              
PADD140  DS    0H                                                               
         MVC   AIO,AIO2                                                         
                                                                                
         GOTO1 ADDELEM                                                          
         GOTO1 ADDREC                                                           
         TM    DMCB+8,X'20'        RECORD ALREADY EXISTS                        
         BO    INVLFLD                                                          
                                                                                
         MVC   AIO,AIO1            RESTORE IO AREA                              
                                                                                
         B     YES                                                              
         DROP  R5                                                               
         EJECT                                                                  
PROCGET  DS    0H                  GET RECORD FOR PROCESSING                    
         MVC   KEY,EOPKEY          BUILD KEY FROM CURRENT LINE                  
                                                                                
         LR    R3,R2               BUMP TO DDS CODES FIELD                      
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
                                                                                
         LA    R6,KEY                                                           
         ZIC   RF,CODEOFFS                                                      
         AR    RF,R6               FIELD OFFSET IN KEY                          
         ZIC   R1,CODESIZE                                                      
         BCTR  R1,0                                                             
         EX    R1,PGET10                                                        
         EX    R1,PGET20                                                        
         B     PGET30                                                           
PGET10   MVC   0(0,RF),8(R3)                                                    
PGET20   OC    0(0,RF),SPACES                                                   
                                                                                
PGET30   DS    0H                                                               
         CLC   =C'RES',8(R2)       ACTION RESTORE                               
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       PASS BACK DELETED                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   INVLFLD                                                          
                                                                                
         CLC   =C'RES',8(R2)       ACTION RESTORE                               
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       PASS BACK DELETED                            
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
         CLC   =C'CHA',8(R2)       ACTION CHANGED                               
         BE    PROCCHA                                                          
         CLI   8(R2),C'C'                                                       
         BE    PROCCHA                                                          
                                                                                
         CLC   =C'DEL',8(R2)       ACTION DELETE                                
         BE    PROCDEL                                                          
                                                                                
         CLC   =C'RES',8(R2)       ACTION RESTORE                               
         BE    PROCRES                                                          
         DC    H'0'                                                             
         EJECT                                                                  
PROCCHA  DS    0H                                                               
         L     R6,AIO                                                           
         USING REOPREC,R6                                                       
                                                                                
         GOTO1 DATCON,DMCB,(5,0),(3,REOPDATE)                                   
                                                                                
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         CLC   TRFFLDLN,5(R2)      CHECK LENGTH                                 
         BL    INVLFLD                                                          
                                                                                
         CLI   EOPTRAF,C'J'        JDS/2000 -> JDS/2000 FORMAT                  
         BE    PCHA10                                                           
                                                                                
         CLI   EOPTRAF,C'C'        COLUMBINE                                    
         BE    PCHAC10                                                          
                                                                                
         CLI   5(R2),6             LENGTH OF 6 OR LOWER                         
         BH    INVLFLD                                                          
         TM    4(R2),X'08'         NUMERIC INPUTS ONLY                          
         BZ    INVLFLD                                                          
                                                                                
         XC    REOPEQUV,REOPEQUV                                                
         MVC   REOPEQUV(6),=6C'0'  LEADING ZEROES                               
         LA    RE,REOPEQUV+6                                                    
         ZIC   RF,5(R2)                                                         
         SR    RE,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)                                                    
         B     PCHA50                                                           
                                                                                
PCHA10   DS    0H                  IF JDS                                       
         CLI   EOPKEY,X'1C'        AGENCY                                       
         BE    PCHA30                                                           
         CLI   EOPKEY,X'1E'        AND SALESPERSON ARE NUMERIC                  
         BE    PCHA30                                                           
                                                                                
         ZIC   R1,TRFFLDLN                                                      
         BCTR  R1,0                                                             
         EX    R1,PCHA15                                                        
         EX    R1,PCHA20                                                        
         B     PCHA50                                                           
PCHA15   MVC   REOPEQUV(0),8(R2)                                                
PCHA20   OC    REOPEQUV(0),SPACES                                               
                                                                                
PCHA30   DS    0H                                                               
         TM    4(R2),X'08'         NUMERIC??                                    
         BNZ   PCHA35                                                           
         MVC   REOPEQUV(6),8(R2)                                                
         B     PCHA50                                                           
                                                                                
PCHA35   DS    0H                                                               
         ZIC   R1,5(R2)                                                         
         ZIC   RF,TRFFLDLN                                                      
         LA    RE,REOPEQUV                                                      
         AR    RE,RF                                                            
         SR    RE,R1                                                            
         BCTR  R1,0                                                             
         BCTR  RF,0                                                             
         EX    RF,PCHA40                                                        
         EX    R1,PCHA45                                                        
         B     PCHA50                                                           
PCHA40   MVC   REOPEQUV(0),=6C'0'                                               
PCHA45   MVC   0(0,RE),8(R2)                                                    
***>>>  COLUMBINE CHANGE                                                        
PCHAC10  DS    0H                  PROCESS COLUMBINE                            
         MVC   REOPEQUV(6),=6C'0'                                               
         CLI   EOPKEY,X'1E'        PROCESS COLUMBINE SALESPERSON                
         BE    PCHAC30                                                          
         MVI   REOPEQUV+5,0        SET LAST POSITION TO ZERO                    
                                                                                
         LA    R5,REOPEQUV         SET A(EQUIV FIELD)                           
         LA    RF,5                SET MAX LENGTH                               
         ZIC   RE,5(R2)            SET LENGTH OF INPUT                          
         SR    RF,RE               SUBTRACT LENGTH FROM MAX                     
         AR    R5,RF               ADD DISPLACEMENT                             
         ZIC   R1,TRFFLDLN                                                      
         BCTR  R1,0                                                             
         EX    R1,PCHAC15                                                       
         EX    R1,PCHAC20                                                       
         B     PCHA50                                                           
PCHAC15  MVC   0(0,R5),8(R2)       LOAD RIGHT JUSTIFIED                         
*                                     BASED ON LENGTH OF INPUT                  
PCHAC20  OC    REOPEQUV+1(0),SPACES                                             
                                                                                
PCHAC30  DS    0H                  PROCESS SALESPERSON                          
         TM    4(R2),X'08'         NUMERIC??                                    
         BNZ   PCHAC35             YES                                          
         MVC   REOPEQUV+3(3),8(R2) NO                                           
         B     PCHA50                                                           
                                                                                
PCHAC35  DS    0H                                                               
         ZIC   R1,5(R2)                                                         
         ZIC   RF,TRFFLDLN                                                      
         LA    RE,REOPEQUV+3                                                    
         AR    RE,RF                                                            
         SR    RE,R1                                                            
         BCTR  R1,0                                                             
         BCTR  RF,0                                                             
         EX    RF,PCHAC40                                                       
         EX    R1,PCHAC45                                                       
         B     PCHA50                                                           
PCHAC40  MVC   REOPEQUV(0),=6C'0'                                               
PCHAC45  MVC   0(0,RE),8(R2)                                                    
         DROP  R6                                                               
***>>>  COLUMBINE CHANGE                                                        
PCHA50   DS    0H                                                               
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO1                                                         
         B     YES                                                              
         EJECT                                                                  
PROCDEL  DS    0H                  PROCESS ACTION DELETE                        
         L     R6,AIO                                                           
         USING REOPREC,R6                                                       
         OI    REOPCNTL,X'80'      MARK FOR DELETION                            
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         OI    KEY+27,X'80'        MARK FOR DELETION                            
         GOTO1 WRITE                                                            
         B     YES                                                              
         EJECT                                                                  
PROCRES  DS    0H                  PROCESS ACTION RESTORE                       
         L     R6,AIO                                                           
         USING REOPREC,R6                                                       
         NI    REOPCNTL,X'FF'-X'80' RESTORE                                     
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         NI    KEY+27,X'FF'-X'80'  RESTORE                                      
         GOTO1 WRITE                                                            
         B     YES                                                              
         EJECT                                                                  
*********************************************************************           
* RETRIEVES ADVERTISER NAME TO WORK                                             
* R2 IS FIELD HEADER                                                            
* USES AIO3 FOR TEMP IOAREA                                                     
*********************************************************************           
VADV     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RADVKEY,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKREP,AGENCY     REP                                          
         MVC   RADVKADV,8(R2)      ADV                                          
         OC    RADVKADV,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NO                                                               
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         USING RADVREC,R6                                                       
                                                                                
         GOTO1 GETREC                                                           
         MVC   WORK(20),RADVNAME                                                
         CLC   KEY1BSAV,=X'1B01'   TRADE KEY DISPLAYED?                         
         BNE   VADV0020            NO                                           
         MVC   WORK+16(4),=C'/TRD' YES - SET TRADE INDICATOR                    
VADV0020 EQU   *                                                                
         MVC   AIO,AIO1                                                         
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
* RETRIEVES AGENCY NAME TO WORK                                                 
* R2 IS FIELD HEADER                                                            
* USES AIO3 FOR TEMP IOAREA                                                     
*********************************************************************           
VAGY     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAGYKEY,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKREP,AGENCY     REP                                          
         MVC   RAGYKAGY(6),8(R2)   AGENCY+OFF                                   
         OC    RAGYKAGY(6),SPACES                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VAGY0020                                                         
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         USING RAGYREC,R6                                                       
                                                                                
         GOTO1 GETREC                                                           
         MVC   WORK(20),RAGYNAM1                                                
         MVC   AIO,AIO1                                                         
         SR    R0,R0               SET CC ZERO                                  
         B     EXIT                                                             
VAGY0020 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*   VAGY2:  WHEN AGENCY NOT FOUND, CYCLE THROUGH ALL OFFICES,                   
*        LOOKING FOR OFFICE OF #N AGENCY OFFICE.                                
*                                                                               
VAGY2    NTR1                                                                   
         CLI   12(R2),C'#'         ALTERNATE TRADE AGENCY CODE?                 
         BNE   VAG20100            NO  - EXIT ROUTINE                           
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAGY2KEY,R6                                                      
         MVI   RAGK2TYP,X'1A'                                                   
         MVC   RAGK2REP,AGENCY     REP                                          
         MVC   RAGK2AGY(4),8(R2)   AGENCY                                       
         MVC   RAGK2AOF(2),=C'  '  SPACE-FILL AGENCY OFFICE                     
         OC    RAGK2AGY(6),SPACES                                               
*                                                                               
*   NOTE:  THE CORPORATE RECORD TRADE ALTERNATE FIELD CONTAINS THE              
*        HIGHEST ALTERNATE OFFICE VALUE IN USE.  WHEN THE CORPORATE             
*        IS READ, IT WILL AGREE WITH THE SCREEN VALUE OF THE HIGHEST            
*        OFFICE.  THE CORPORATE OFFICE, WHICH IS **SPACES**, WILL BE            
*        SHOWN.                                                                 
*        TO PREVENT THIS ERROR, IF THE SCREEN VALUE (IE, #3) IS GREATER         
*        THAN #0, THE FIRST READ WILL BE SET TO BYPASS THE CORPORATE            
*        AGENCY2 RECORD.                                                        
*                                                                               
         CLI   13(R2),C'0'         SCREEN VALUE INDICATES CORPORATE?            
         BE    VAG20005            YES - PROCESS THE CORPORATE RECORD           
         MVI   RAGK2AOF,X'41'      NO  - SKIP THE CORPORATE RECORD              
*                                     BY SETTING HIGH ORDER BYTE 1              
*                                     BIT PAST SPACE (X'40')                    
VAG20005 EQU   *                                                                
         GOTO1 HIGH                                                             
         B     VAG20020                                                         
VAG20010 EQU   *                                                                
         GOTO1 SEQ                                                              
VAG20020 EQU   *                                                                
         CLC   KEY(23),KEYSAVE     SAME KEY THROUGH AGENCY CODE?                
         BNE   VAG20120            NO  - ITEM NOT FOUND                         
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         USING RAGY2REC,R6                                                      
                                                                                
         GOTO1 GETREC                                                           
         LA    RF,RAGY2FXE         FIRST ELEMENT IN RECORD                      
VAG20060 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    VAG20010            YES - SKIP THIS RECORD                       
         CLI   0(RF),X'1F'         AGENCY ELEMENT?                              
         BE    VAG20080            YES - CHECK TRADE ALTERNATE AGY              
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     VAG20060            GO BACK FOR NEXT                             
VAG20080 EQU   *                                                                
         CLC   13(1,R2),RAG2TRAD-RAG2ELEM(RF)                                   
*                                  TRADE ALT VS AGY2 REC ALT                    
         BNE   VAG20010            GO BACK FOR NEXT                             
*                                  EQUAL                                        
         MVC   WORK(20),RAG2NAM1-RAG2ELEM(RF)                                   
         MVI   WORK+17,C'/'                                                     
         MVC   WORK+18(2),RAGK2AOF                                              
*                                  INSERT OFFICE OF RECORD INTO DISPLAY         
         MVC   AIO,AIO1                                                         
VAG20100 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
         B     EXIT                                                             
VAG20120 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
* RETRIEVES OFFICE NAME TO WORK                                                 
* R2 IS FIELD HEADER                                                            
* USES IO3 FOR TEMP IOAREA                                                      
*********************************************************************           
VOFF     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ROFFKEY,R6                                                       
         MVI   ROFFKTYP,X'04'                                                   
         MVC   ROFFKREP,AGENCY     REP                                          
         MVC   ROFFKOFF,8(R2)      OFFICE CODE                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EXIT                                                             
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         USING ROFFREC,R6                                                       
         GOTO1 GETREC                                                           
         MVC   WORK(20),ROFFNAME                                                
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
* RETRIEVES SALESPERSON NAME TO WORK                                            
* R2 IS FIELD HEADER                                                            
* USES AIO3 FOR TEMP IOAREA                                                     
*********************************************************************           
VSAL     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSALKEY,R6                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY     REP                                          
         MVC   RSALKSAL,8(R2)      SALESMAN INITIALS                            
         OC    RSALKSAL,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   EXIT                                                             
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         USING RSALREC,R6                                                       
         GOTO1 GETREC                                                           
         MVC   WORK(20),RSALNAME                                                
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
* IF STATION SIGN-ON, CHECK IF IT'S A VALID SIGN ON ID                          
*********************************************************************           
CKACCESS NTR1                                                                   
         L     R6,AIO                                                           
         USING RSTAREC,R6                                                       
                                                                                
         CLI   TWAACCS,C'$'                                                     
         BNE   YES                                                              
         L     R6,AIO                                                           
         USING RSTASOEL,R6                                                      
         MVI   ELCODE,6            GET VALID SIGN ON ID ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   YES                                                              
                                                                                
CKACC10  DS    0H                                                               
         CLC   RSTASID,TWAORIG     VALID SIGN-ON?                               
         BE    YES                 YES, PROCEED                                 
         BAS   RE,NEXTEL           NOPE, CHECK NEXT ELEMENT                     
         BE    CKACC10                                                          
         B     NO                  ALL DONE, NO MATCH, NOT VALID                
         DROP  R6                                                               
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
                                                                                
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
                                                                                
NUMERIC  MVC   RERROR,=AL2(NOTNUM) MUST BE NUMERIC                              
         B     ERREND                                                           
                                                                                
RECONFIL MVC   RERROR,=AL2(RECEXIST) RECORD ALREADY ON FILE                     
         B     ERREND                                                           
                                                                                
DELRECEX MVC   RERROR,=AL2(DELEXIST) DELETED RECORD EXISTS                      
         B     ERREND                                                           
                                                                                
RECACTIV MVC   RERROR,=AL2(290)    CANNOT DELETE ACTIVE RECORD.                 
         B     ERREND                                                           
                                                                                
NOTJDS   MVC   RERROR,=AL2(410)    STATION IS NOT A JDS/2000 STATION.           
         B     ERREND                                                           
                                                                                
NOTBIAS  MVC   RERROR,=AL2(411)    STATION IS NOT A BIAS STATION.               
         B     ERREND                                                           
                                                                                
NOTENTER MVC   RERROR,=AL2(586)    STATION IS NOT A ENTERPRISE STATION.         
         B     ERREND                                                           
                                                                                
NOTCOLM  MVC   RERROR,=AL2(587)    STATION IS NOT A COLUMBINE STATION.          
         B     ERREND                                                           
                                                                                
SLOCKOUT MVC   RERROR,=AL2(55)                                                  
         B     ERREND                                                           
                                                                                
ENDLIST  MVC   RERROR,=AL2(16)     END OF LIST                                  
         B     INFEND                                                           
                                                                                
NEXTLIST MVC   RERROR,=AL2(15)     PRESS ENTER FOR NEXT                         
         B     INFEND                                                           
                                                                                
ACTCHGD  MVC   RERROR,=AL2(106)    RECORDS CHANGED.  PRESS ENTER TO             
         LA    R2,EOPTRAFH           PROCEED                                    
         B     RINFEND                                                          
                                                                                
GOADD    MVC   RERROR,=AL2(107)    PLEASE ADD RECORDS.                          
         LA    R2,EOPACTH                                                       
         B     RINFEND                                                          
                                                                                
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'       DO A GETTXT CALL                             
         GOTO1 MYERROR                                                          
                                                                                
INFEND   DS    0H                                                               
         LA    R2,EOPTRAFH         PUT CURSOR HERE                              
         MVI   RMSGTYPE,C'I'       DO A GETTXT CALL                             
         GOTO1 MYERROR                                                          
                                                                                
RINFEND  DS    0H                  USE REP INFO FILE                            
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSYS,X'08'                                                     
         DROP  RF                                                               
                                                                                
         MVI   RMSGTYPE,C'I'       DO A GETTXT CALL                             
         GOTO1 MYERROR                                                          
                                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* LOCAL STORAGE AREA                                                            
*                                                                               
TEMPKEY  DS    CL(L'KEY)                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMBAD          (OUR MAINTENANCE SCREEN OVERLAY)             
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
CONTRACT DSECT                                                                  
       ++INCLUDE REGENCON                                                       
EOPREC   DSECT                                                                  
       ++INCLUDE REGENEOP                                                       
         DSECT                                                                  
       ++INCLUDE REGENADV                                                       
         DSECT                                                                  
       ++INCLUDE REGENAGY                                                       
         DSECT                                                                  
       ++INCLUDE REGENAGY2                                                      
         DSECT                                                                  
       ++INCLUDE REGENOFF                                                       
         DSECT                                                                  
       ++INCLUDE REGENSAL                                                       
         DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
EOPKEY   DS    CL(L'KEY)                                                        
SAVEKEY  DS    CL(L'KEY)                                                        
FIRSTKEY DS    CL(L'KEY)                                                        
SCRNFLAG DS    X                                                                
KEY1BSAV DS    XL2                 SAVE AREA FOR 1B KEYS                        
NEXTSCRN EQU   X'10'                                                            
SAMESCRN EQU   X'20'                                                            
ACTPROCD EQU   X'40'               ACTION WAS PROCESSED                         
ADDFLAG  DS    C                   Y=USER WANTS TO ADD, BLANK SCREEN            
FILTER   DS    X                                                                
TRAFFSYS DS    X                                                                
KCODE    DS    CL6                 K FIELD CODE (ADV/AGY/OFF/SAL)               
CODESIZE DS    X                   LEN OF CONTRACT FIELD                        
CODEOFFS DS    X                   OFFSET OF CONTRACT FIELD FROM KEY            
TRFFLDLN DS    X                   JDS EOP CODE FIELD LENGTH                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049RESFM30   08/01/05'                                      
         END                                                                    
