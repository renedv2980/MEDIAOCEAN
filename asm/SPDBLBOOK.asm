*          DATA SET SPDBLBOOK  AT LEVEL 039 AS OF 05/01/02                      
*PHASE T00A78                                                                   
*====================================================================*          
*                                                                    *          
*  PARAM 1     A(DBLBOOKD)                                           *          
*                                                                    *          
*====================================================================*          
DBLBOOK  TITLE 'MAINTAIN SPOTPAK DOUBLE BOOKING RECORDS'                        
DBLBOOK  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,DBLBOOK,CLEAR=YES                                    
         USING WORKD,RC                                                         
*                                                                               
         L     R9,0(R1)            SAVE PARAM POINTER                           
         USING DBLBOOKD,R9                                                      
         MVI   DBERR,0                                                          
*                                                                               
         L     RA,DBCOMFAC                                                      
         USING COMFACSD,RA                                                      
*                                                                               
         CLI   DBTYPE,0                                                         
         BNE   *+8                                                              
         MVI   DBTYPE,C'B'                                                      
*                                                                               
         CLI   DBTYPE,C'B'                                                      
         BE    DB2                                                              
         MVI   DBERR,DBERR1                                                     
         B     EXIT                                                             
*                                                                               
DB2      CLC   DBDTIN(7),DBDTOUT   TEST FOR CHANGE                              
         BE    EXIT                NO -                                         
*                                                                               
         OC    DBTIMIN,DBTIMIN     TEST NEW TIME TO LOCK                        
         BZ    DB4                 NO                                           
         LA    R4,DBDTIN           POINT TO DATE(2)/TIME(4)/DUR(1)              
         BAS   RE,ADJTIM                                                        
*                                                                               
DB4      OC    DBTIMOUT,DBTIMOUT   TEST TIME OUT                                
         BZ    DB10                NO - SO DON'T ADJUST IT                      
*                                                                               
         LA    R4,DBDTOUT          POINT TO DATE(2)/TIME(4)/DUR(1)              
         BAS   RE,ADJTIM                                                        
         B     DB10                                                             
         EJECT                                                                  
ADJTIM   SR    R0,R0                                                            
         ICM   R0,3,2(R4)          GET START TIME                               
         CH    R0,=H'600'          TEST PRIOR TO 6A                             
         BNL   *+8                                                              
         AH    R0,=H'2400'                                                      
         STCM  R0,3,2(R4)                                                       
*                                                                               
         ICM   R0,3,4(R4)          GET END TIME                                 
         BZ    ADJTIM2                                                          
         CH    R0,=H'600'          TEST PRIOR TO 6A                             
         BNL   *+8                                                              
         AH    R0,=H'2400'                                                      
         STCM  R0,3,4(R4)                                                       
         BR    RE                  EXIT SINCE END TIME PRESENT                  
*                                                                               
ADJTIM2  SR    R0,R0                                                            
         ICM   R0,1,6(R4)          GET PROGRAM LENGTH                           
         STH   R0,PROGLEN                                                       
         BNZ   ADJTIM4                                                          
         MVI   DBERR,DBERR4        SLN REQD IF NO END TIME                      
         B     EXIT                                                             
*                                                                               
ADJTIM4  SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,2(R4)          GET START TIME                               
         D     R0,=F'100'          HOURS IN R1, MINUTES IN R0                   
         MH    R1,=H'60'           CONVERT HOURS TO MINUTES                     
         AR    R1,R0                                                            
         AH    R1,PROGLEN          ADD PROGRAM DURATION                         
* CONVERT TO HHMM                                                               
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
         STCM  R1,3,4(R4)          SET END TIME                                 
         BR    RE                                                               
         EJECT                                                                  
**********************************************                                  
* BUILD KEY FOR DATE USER IS LOCKING         *                                  
**********************************************                                  
         SPACE 1                                                                
DB10     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DBLBKREC,R4         DOUBLE BOOK RECORD                           
         MVI   DBLKTYPE,X'0D'                                                   
         MVI   DBLKSTYP,X'7B'                                                   
         MVC   DBLKAGMD,DBAGYMD                                                 
         GOTO1 CDATCON,DMCB,(2,DBDTIN),(3,WORK)                                 
         MVC   DBLKYEAR,WORK       YEAR                                         
         MVC   DBLKMON,WORK+1      MONTH                                        
         MVC   DBLKSTA,DBSTA       STATION                                      
*                                                                               
         OC    DBTIMIN,DBTIMIN     TEST NEW LOCK TIME                           
         BZ    DB100               NO                                           
*                                                                               
         CLC   DBLKYEAR(2),=X'5E02' FEATURE START DATE IS FEB/94                
         BL    DB100                                                            
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 CDATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    DB40                RECORD EXISTS                                
         SPACE 1                                                                
*=====================*                                                         
* CREATE NEW RECORD   *                                                         
*=====================*                                                         
         SPACE 1                                                                
DB20     MVC   KEY,KEYSAVE         RESTORE KEY                                  
         LA    R3,IOA                                                           
         XC    0(256,R3),0(R3)                                                  
         MVC   0(13,R3),KEY                                                     
         MVC   13(2,R3),=H'24'                                                  
         MVC   20(2,R3),DBAGYA                                                  
         B     DB90                                                             
         EJECT                                                                  
*==========================*                                                    
*  GET EXISTING RECORD.    *                                                    
*==========================*                                                    
         SPACE 1                                                                
DB40     DS    0H                                                               
         GOTO1 CDATAMGR,DMCB,(X'80',=C'GETREC'),=C'SPTFILE',KEY+14,    X        
               IOA,DMWORK                                                       
         SPACE 1                                                                
*=============================================================*                 
* CHECK THAT THE DATE/TIME ARE NOT BOUGHT ALREADY.            *                 
*=============================================================*                 
         SPACE 1                                                                
DB50     DS    0H                                                               
         LA    R3,IOA                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DB60     BAS   RE,NEXTEL                                                        
         BNE   DB90                                                             
*                                                                               
         USING DBLBKEL,R3                                                       
*                                                                               
         CLC   DBELDATE,DBDTIN        SAME DATE?                                
         BNE   DB60                                                             
*                                                                               
         CLC   DBTIMIN(2),DBELNDTM    IS NEW START TIME >= END TIME?            
         BNL   DB60                   YES, NEXT ELEMENT                         
*                                                                               
         CLC   DBTIMIN+2(2),DBELSTIM  IS NEW END TIME <= START TIME?            
         BNH   DB60                   YES, NEXT ELEMENT                         
*                                                                               
* NEW DATE/TIME OVERLAPS ELEMENT. OK IF IT IS CALLER                            
*                                                                               
         CLC   DBELCLT,DBCLT       SAME CLIENT?                                 
         BNE   DB80                NO - ERROR                                   
         CLI   DBTYPE,C'B'         TEST BUY                                     
         BNE   DB70                NO                                           
         CLC   DBEST(2),DBELEST    SAME ESTIMATE/LINE                           
         BNE   DB80                                                             
         B     DB60                GO TRY AGAIN IDIOT                           
*                                                                               
DB70     DC    H'0'                FIND OUT HOW TO SUPPORT NWS RECS             
*                                                                               
DB80     ST    R3,DBERRADR         RETURN OWNER ELEMENT ADDRESS                 
         MVI   DBERR,DBERR2        SET DAY/TIME LOCKED                          
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*========================================*                                      
* ADD 05 ELEMENT - TO LOCK THE DATE/TIME *                                      
*========================================*                                      
         SPACE 1                                                                
DB90     LA    R3,ELEM                                                          
         USING DBLBKEL,R3                                                       
         MVI   DBELCODE,DBELCODQ                                                
         MVI   DBELLEN,DBELLENQ                                                 
*                                                                               
         MVC   DBELCLT,DBCLT       CLIENT                                       
         MVC   DBELTYPE,DBTYPE     TYPE                                         
         MVC   DBELEST,DBEST       ESTIMATE                                     
         MVC   DBELLINE,DBLINE     LINE NO.                                     
*                                                                               
         MVC   DBELDATE,DBDTIN     BUY DATE                                     
         MVC   DBELSTIM,DBTIMIN    START TIME                                   
         MVC   DBELNDTM,DBTIMIN+2  END TIME                                     
*                                                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFILE'),IOA,ELEM,0                        
         CLI   DM4,0                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
* UDPATE THE FILE                                                               
         TM    DBLBKOPT,DBOPT_RDONLY                                            
         BO    DB100                                                            
         LA    R0,=C'ADDREC'                                                    
         OC    KEY+14(4),KEY+14    IS RECORD ON FILE NOW                        
         BZ    *+8                                                              
         LA    R0,=C'PUTREC'                                                    
         GOTO1 CDATAMGR,DMCB,(R0),=C'SPTFILE',KEY+14,IOA,DMWORK                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*================================================================*              
*  RELEASE DATE/TIME OWNED IF REQUIRED                           *              
*================================================================*              
         SPACE 1                                                                
DB100    OC    DBDTOUT,DBDTOUT    IS THERE A DATE/TIME TO BE REMOVED?           
         BZ    EXIT                                                             
* BUILD KEY                                                                     
         GOTO1 CDATCON,DMCB,(2,DBDTOUT),(3,WORK)                                
         MVC   DBLKYEAR,WORK       YEAR                                         
         MVC   DBLKMON,WORK+1      MONTH                                        
*                                                                               
         CLC   DBLKYEAR(2),=X'5E02' FEATURE START DATE IS FEB/94                
         BL    EXIT                                                             
* SEE IF WE HAVE THE RECORD ALREADY                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DB102                                                            
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 CDATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY                      
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DB110               IF NOT FOUND, ERROR                          
*                                                                               
DB102    DS    0H                  ALWAYS REBUILD GETREC TABLE                  
         GOTO1 CDATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,IOA,DMWORK           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,IOA                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DB104    BAS   RE,NEXTEL                                                        
         BNE   DB110               ERROR IF NOT FOUND                           
*                                                                               
         CLC   DBELCLT,DBCLT       IS IT SAME CLIENT (OWNER)                    
         BNE   DB104               NEXT ELEMENT                                 
*                                                                               
         CLC   DBELDATE,DBDTOUT    IS IT SAME DATE?                             
         BNE   DB104                                                            
*                                                                               
         CLC   DBELSTIM,DBTIMOUT   ARE START TIMES THE SAME?                    
         BNE   DB104                                                            
*                                                                               
         CLC   DBELNDTM,DBTIMOUT+2 ARE END TIMES THE SAME?                      
         BNE   DB104                                                            
         SPACE 1                                                                
*=================================================================*             
* DELETE ELEMENT                                                  *             
*=================================================================*             
         SPACE 1                                                                
         CLC   0(16,R3),16(R3)     TEST NEXT IS A DUPLICATE                     
         BNE   *+8                 NO                                           
         MVI   16(R3),X'FF'        SET TO DELETE NEXT TOO                       
*                                                                               
         MVI   0(R3),X'FF'         DELETE ELEMENT                               
         GOTO1 CHELLO,DMCB,(C'D',=C'SPTFILE'),(X'FF',IOA),0,0                   
*                                                                               
         TM    DBLBKOPT,DBOPT_RDONLY                                            
         BO    DB108                                                            
         GOTO1 CDATAMGR,DMCB,=C'PUTREC',=C'SPTFILE',KEY+14,            X        
               IOA,DMWORK                                                       
DB108    B     EXIT                                                             
*                                                                               
DB110    TM    DBLBKOPT,DBOPT_NOUNLK  TEST IGNORE UNLOCK FAILURE                
         BO    *+8                                                              
         MVI   DBERR,DBERR3           ELSE RETURN UNLOCK ERROR                  
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         GETEL (R3),24,ELCODE                                                   
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*                                                                               
WORKD    DSECT                                                                  
DMCB     DS    0XL24                                                            
DM1      DS    F                                                                
DM2      DS    F                                                                
DM3      DS    F                                                                
DM4      DS    F                                                                
DM5      DS    F                                                                
DM6      DS    F                                                                
PROGLEN  DS    H                                                                
ELCODE   DS    C                                                                
         DS    XL1                 SPARE                                        
WORK     DS    CL24                                                             
KEY      DS    CL24                                                             
KEYSAVE  DS    CL24                                                             
DMWORK   DS    CL96                                                             
ELEM     DS    CL256                                                            
IOA      DS    CL2000                                                           
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPDBLBOOKD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDBLBK                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039SPDBLBOOK 05/01/02'                                      
         END                                                                    
