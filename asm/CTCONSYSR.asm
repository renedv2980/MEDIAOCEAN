*          DATA SET CTCONSYSR  AT LEVEL 061 AS OF 08/10/00                      
*PHASE CONSYSA                                                                  
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CHANGE X'21' ELS TO       *         
* DUPLICATE CON=800F FOR SEL & SWP                                    *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONSYS - CHANGE SYS AUTH (X''21'') ELEMS'                     
CONSYS   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONSYS*                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVI   DATADISP+1,28                                                    
*                                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
         USING CTTREC,R2                                                        
*                                                                               
         CLI   0(R2),X'FF'         TEST FILE TRAILER                            
**NOP    BE    DONE                 YES                                         
         BE    EXIT                                                             
*                                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'FF'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
         LA    RF,RECTAB           RECTAB HAS THE REC TYPES WE WANT             
         CLC   0(1,RF),0(R2)                                                    
         BE    *+20                                                             
         LA    RF,L'RECTAB(RF)                                                  
         CLI   0(RF),X'FF'         EOT?                                         
         BNE   *-18                 NO                                          
         B     EXIT                 YES - LEAVE IT ALONE                        
*                                                                               
         TM    27(R2),X'80'        IS THE FUCKING THING DELETED?                
         BNZ   EXIT                WHY THE FUCK AM I HERE ON SUNDAY???          
*                                                                               
M40      BAS   RE,CHA21            MAKE CHANGE IF NECESSARY                     
         B     EXIT                                                             
*                                                                               
*&&DO                                                                           
DONE     DS    0H                                                               
         LA    R3,RECTAB                                                        
DONE10   MVC   P(1),0(R3)                                                       
         EDIT  (B2,1(R3)),(5,P+3),ALIGN=RIGHT                                   
         GOTO1 VPRINTER                                                         
         LA    R3,L'RECTAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   DONE10                                                           
         B     EXIT                                                             
*&&                                                                             
         EJECT                                                                  
*                                                                               
* CHA21: SEE IF X'21' EL EXISTS FOR SYSNUM 8 (REP), AND IF SO, CLONE            
* RRG DATA FOR NRRG (IF ANY)                                                    
*                                                                               
CHA21    NTR1                                                                   
         NI    FLAGS,X'FF'-CHANGED                                              
*                                                                               
* SEE IF SYS AUTH EL EXISTS                                                     
         GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'21',(R2)),=X'08'                
         CLI   DMCB+12,X'06'       ELEM NOT FOUND?                              
         BE    EXIT                                                             
         CLI   DMCB+12,0           ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* MOVE X'21' TO WORK AREA                                                       
         XC    ELEM,ELEM                                                        
         L     R1,DMCB+12          A(ELEM)                                      
         ZIC   R3,1(R1)            L'ELEM                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ELEM,0(R1)                                                       
         LA    R6,ELEM                                                          
         USING CTSYSD,R6                                                        
*                                                                               
* SEE IF CON=800F                                                               
         LA    R4,3                BXLE INCREMENT                               
         LA    R5,0(R3,R6)         BXLE END (NOTE R3 BCTR'D)                    
         LA    R3,CTSYSPGM         BXLE START ADDR                              
*                                                                               
         CLC   =X'02800F',0(R3)    IS THIS CON=800F'                            
         BE    *+12                 YES                                         
         BXLE  R3,R4,*-10                                                       
         B     EXIT                 NONE - EXIT                                 
*                                                                               
* IF SEL=Y, CHANGE TO SEL=800F                                                  
*   IF NO SEL= AND ALL=Y, ADD SEL=800F                                          
         LA    R3,CTSYSPGM         BXLE START ADDR                              
*                                                                               
         CLI   0(R3),X'0A'         IS THIS SEL=XXXX?                            
         BE    *+12                 YES - SEE IF IT'S =Y                        
         BXLE  R3,R4,*-8                                                        
         B     CHA20                                                            
*                                                                               
         CLC   =X'000F',1(R3)      SEL=Y?                                       
         BNE   CHA30                NO                                          
         MVC   1(2,R3),=X'800F'     YES - CHANGE IT TO 800F                     
         OI    FLAGS,CHANGED                                                    
         B     CHA30                                                            
*                                                                               
* NO SEL=, IF ALL=Y, ADD SEL=800F                                               
CHA20    CLC   CTSYSALL,=X'000F'   ALL=Y?                                       
         BNE   CHA30                                                            
         OI    FLAGS,CHANGED                                                    
         MVC   1(3,R5),=X'0A800F'   ADD SEL=800F                                
         ZIC   R1,1(R6)            UPDATE ELEM LENGTH                           
         LA    R1,3(R1)                                                         
         STC   R1,1(R6)                                                         
         LA    R5,3(R5)            UPDATE TO END OF ELEM                        
*                                                                               
* IF SWP=Y, CHANGE TO SWP=800F                                                  
*   IF NO SWP= AND ALL=Y, ADD SWP=800F                                          
CHA30    LA    R3,CTSYSPGM         BXLE START ADDR                              
*                                                                               
         CLI   0(R3),X'1A'         IS THIS SWP=XXXX?                            
         BE    *+12                 YES - SEE IF IT'S =Y                        
         BXLE  R3,R4,*-8                                                        
         B     CHA40                                                            
*                                                                               
         CLC   =X'000F',1(R3)      SWP=Y?                                       
         BNE   CHA50                NO                                          
         MVC   1(2,R3),=X'800F'     YES - CHANGE IT TO 800F                     
         OI    FLAGS,CHANGED                                                    
         B     CHA50                                                            
*                                                                               
* NO SWP=, IF ALL=Y, ADD SWP=800F                                               
CHA40    CLC   =X'000F',CTSYSALL   ALL=Y?                                       
         BNE   CHA50                                                            
         OI    FLAGS,CHANGED                                                    
         MVC   1(3,R5),=X'1A800F'   ADD SWP=800F                                
         ZIC   R1,1(R6)            UPDATE ELEM LENGTH                           
         LA    R1,3(R1)                                                         
         STC   R1,1(R6)                                                         
*                                                                               
* IF HERE, DELETE OLD SYS AUTH EL, ADD BACK NEW X'21'                           
CHA50    TM    FLAGS,CHANGED                                                    
         BZ    EXIT                                                             
         CLC   =H'990',25(R2)      REC >= 990 BYTES?                            
         BNH   PRNERR                                                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE'),(X'21',(R2)),=X'08'                
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE'),(X'21',(R2)),ELEM                  
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*  UPDATE REC HDR BY USING ACTUAL LENGTH IN KEY...                              
         ZICM  R1,25(R2),2                                                      
         LA    R1,4(R1)                                                         
         L     R3,AIOAREA                                                       
         STCM  R1,3,0(R3)                                                       
*                                                                               
* PRINT KEYS OF ALTERED RECS                                                    
         GOTO1 VHEXOUT,DMCB,0(R2),P,25,=C'TOG'                                  
         MVC   P+60(25),0(R2)                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* PRINT ERROR RECS *                                                            
         SPACE                                                                  
PRNERR   DS    0H                                                               
         MVC   P(14),=C'LENGTH ERROR: '                                         
         GOTO1 VHEXOUT,DMCB,0(R2),P+14,25,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(25),0(R2)                                                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* VARIOUS TEST CODE                                                             
*                                                                               
*         MVC   P(25),0(R2)                                                     
*         GOTO1 VPRINTER                                                        
*         GOTO1 VHEXOUT,DMCB,0(R6),P,60,=C'TOG'                                 
*         GOTO1 VPRINTER                                                        
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'00'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
         ANSR  X=N                                                              
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DATADISP DS    H                                                                
ELCODE   DS    C                                                                
ELEM     DS    CL255                                                            
*                                                                               
FLAGS    DS    X                                                                
CHANGED  EQU   X'80'               21 ELEM CHANGED                              
*                                                                               
*                                                                               
* BYTES 00 - 00  REC KEY                                                        
*       01 - 02  RECS CHANGED COUNTER                                           
*                                                                               
RECTAB   DS    0CL3                                                             
         DC    C'I',XL2'0000'                                                   
         DC    C'T',XL2'0000'                                                   
         DC    C'0',XL2'0000'                                                   
         DC    X'FF'                                                            
*&&DO                                                                           
*                                                                               
* BYTES 00 - 02  PGM NAME                                                       
*       03 - 03  PGM NUMBER                                                     
*       04 - 04  PGM ADDED EQU                                                  
*       05 - 05  PGM CHANGED EQU                                                
*                                                                               
PGMTAB   DS    0CL6                                                             
         DC    C'NPO',X'254020'                                                 
         DC    C'RSR',X'211008'                                                 
         DC    C'NBU',X'110402'                                                 
PGMTABX  EQU   *                                                                
*&&                                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
PGMD     DSECT                                                                  
PGMNAM   DS    CL3                                                              
PGMNUM   DS    X                                                                
PGMADD   DS    X                                                                
PGMCHA   DS    X                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061CTCONSYSR 08/10/00'                                      
         END                                                                    
