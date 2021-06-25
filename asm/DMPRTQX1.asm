*          DATA SET DMPRTQX1   AT LEVEL 003 AS OF 08/28/08                      
*PHASE PQEXTA                                                                   
         TITLE 'PRTQEXT - PRTQXFR EXTERNAL ROUTINE'                             
         SPACE 2                                                                
***********************************************************************         
*        PRTQXFR EXTERNAL (SAMPLE CODE)                               *         
***********************************************************************         
         SPACE 2                                                                
* PARAMETER LIST LAYOUT                                                         
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IS HEADER RECORD              
*                                   X'02'= RECORD IS DATA RECORD                
*                                   X'FF'= RECORD IS EOF REC                    
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOF'=PURGE & CAUSE EOF              
* P2=N/D                                                                        
*                                                                               
*                                                                               
* P3=1ST BYTE                       C'Y' = YOU ASKED ME TO RETURN               
*                                   C'R' = RETURN BACK TO EXTERNAL              
* P4=N/D                                                                        
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMPQEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMPQEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITIALISE                     
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         CALL TO PROCESS HEADER                       
         BE    DMXHDR                                                           
         CLI   PLIST,X'02'         CALL TO PROCESS RECORD                       
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         CALL ON EOF REC                              
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 1                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED              *         
***********************************************************************         
         SPACE 1                                                                
DMXINIT  DS    0H                                                               
*                                                                               
*        INIT CODE                                                              
*                                                                               
         LA    R1,NEWLINES                                                      
         ST    R1,ANEWL                                                         
         B     DMXIT                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS HEADER LOGIC - RECORD IN AREC - NO RETURN ALLOWED           *         
***********************************************************************         
         SPACE 1                                                                
DMXHDR   L     R9,AREC             MODIFY HEADER                                
         USING QLINDEX,R9                                                       
*                                                                               
         MVC   QLDESC,=CL11'IN FROM MVS'   SET DESCRIPTION FILED                
*                                                                               
         MVI   QLCLASS,C'A'        SET CLASS TO A                               
*                                                                               
         MVC   QLRETNL,=AL2(24)    SET LIVE RETAIN 24 HRS                       
         B     DMXKEEP                                                          
         DROP  R9                                                               
***********************************************************************         
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED              *         
***********************************************************************         
         SPACE 1                                                                
DMXREC   L     R9,AREC                                                          
         CLI   8(R1),C'Y'          IS THIS A RETURN TO ME                       
         BE    INSERT02                                                         
*                                                                               
*        DATA RECORD CODE                                                       
*                                                                               
         CLC   5(6,R9),=C'REMOVE'  REMOVE ALL THESE                             
         BE    DMXPURGE                                                         
*                                                                               
         CLC   5(13,R9),=C'INSERT BEFORE'                                       
         BE    INSERT01                                                         
*                                                                               
*NOP     CLC   5(11,R9),=C'END OF DATA'                                         
*NOP     BE    DMXPGEOF            FORCE EOF HERE                               
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
INSERT01 BAS   RE,SAVELINE         SAVE THIS LINE                               
*                                                                               
         XC    0(132,R9),0(R9)                                                  
         MVC   0(INSERTL,R9),INSERT                                             
         B     DMXKERET            SAVE THIS AND RETURN TO ME                   
*                                                                               
INSERT02 BAS   RE,RESTLINE         RESTORE THIS LINE                            
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
***********************************************************************         
         SPACE                                                                  
DMXEOF   L     R9,AREC                                                          
         L     RF,ANEWL                                                         
         CLI   8(R1),C'Y'                                                       
         BE    EOF002                                                           
*                                                                               
EOF001   BAS   RE,SAVELINE         SAVE EOF LINE                                
*                                                                               
EOF002   EQU   *                   BACK AGAIN                                   
         XC    0(132,R9),0(R9)                                                  
         MVC   0(30,R9),0(RF)                                                   
         LA    RF,30(RF)                                                        
         ST    RF,ANEWL                                                         
         OC    0(2,R9),0(R9)                                                    
         BNZ   DMXKERET            WRITE HARD CODED RECS BELOW                  
*                                                                               
         BAS   RE,RESTLINE         RESTORE EOF LINE                             
         B     DMXKEEP                                                          
*                                                                               
NEWLINES DS    0CL30                                                            
         DC    X'001E0000',X'09',C'ADD TO END OF REPORT 001 '                   
         DC    X'001E0000',X'09',C'ADD TO END OF REPORT 002 '                   
         DC    X'001E0000',X'09',C'ADD TO END OF REPORT 003 '                   
         DC    X'001E0000',X'09',C'ADD TO END OF REPORT 004 '                   
         DC    X'001E0000',X'09',C'ADD TO END OF REPORT 005 '                   
         DC    X'001E0000',X'09',C'ADD TO END OF REPORT 006 '                   
         DC    X'001E0000',X'09',C'ADD TO END OF REPORT 007 '                   
         DC    H'00'                                                            
*                                                                               
ANEWL    DS    A                                                                
         EJECT                                                                  
***********************************************************************         
*        SAVE LINE FOR LATER                                          *         
***********************************************************************         
         SPACE                                                                  
SAVELINE SR    R1,R1               SAVE THIS LINE                               
         ICM   R1,3,0(R9)                                                       
         CH    R1,=Y(L'SAVELIN)                                                 
         BNH   *+6                                                              
         DC    H'0'                TOO BIG FOR SAVELIN                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAVELIN(0),0(R9)                                                 
         BR    RE                                                               
*                                                                               
RESTLINE SR    R1,R1               RESTORE THIS LINE                            
         ICM   R1,3,SAVELIN                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R9),SAVELIN                                                  
         BR    RE                                                               
*                                                                               
SAVELIN  DS    CL200               SAVE THE LINE HERE                           
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                                     
***********************************************************************         
         SPACE                                                                  
INSERT   DC    X'001E0000',X'09',C'AN INSERTED LINE OF TEXT '                   
INSERTL  EQU   *-INSERT                                                         
         EJECT                                                                  
* LITERAL POOL                                                                  
         LTORG                                                                  
         EJECT                                                                  
*DDDPRINT                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*DMPRTQL                                                                        
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
         SPACE                                                                  
* DSECT TO COVER LOCAL W/STORAGE                                                
         SPACE                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL80                                                             
APARM    DS    F                                                                
SAVERE   DS    F                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
         DS    A                                                                
         DS    A                                                                
         DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
WORKX    EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DMPRTQX1  08/28/08'                                      
         END                                                                    
