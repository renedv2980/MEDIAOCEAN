*          DATA SET CTCONAUTH  AT LEVEL 032 AS OF 05/01/02                      
*PHASE CONAUTHA                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CHANGE X'21' ELEMS FOR    *         
* ACC: FILE=0264,PROD=0464,INT=N                                      *         
* SPOT: PAY=N                                                         *         
* PRINT: PAY=N                                                        *         
* NET: NPAY=N                                                         *         
* FOR ALL LISTED ID'S, AND SAVE OFF OLD EL FOR LATER RESTORE          *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONAUTH - CHANGE SYS AUTH (X''21'') ELEMS'                    
CONAUTH  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONAUTH                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
*                                                                               
         MVI   DATADISP+1,28                                                    
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         CLI   0(R2),X'FF'         TEST FILE TRAILER                            
         BNE   M10                 YES - PRINT TOTALS                           
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVC   P(10),=C'CHANGED:  '                                             
         GOTO1 VHEXOUT,DMCB,COUNT1,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(10),=C'ALL=800F: '                                             
         GOTO1 VHEXOUT,DMCB,COUNT2,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(10),=C'RRGO=:    '                                             
         GOTO1 VHEXOUT,DMCB,COUNT3,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(10),=C'DELETED:  '                                             
         GOTO1 VHEXOUT,DMCB,COUNT4,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         DROP  R8                                                               
         B     EXIT                                                             
*                                                                               
M10      DS    0H                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'FF'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
M15      CLI   0(R2),C'I'          TEST ID RECORD                               
         BNE   EXIT                NO - LEAVE ALONE                             
         OC    1(24,R2),1(R2)      NULL KEY? (HI TERM REC?)                     
         BZ    EXIT                                                             
*                                                                               
         USING CTIREC,R2                                                        
         TM    CTISTAT,X'80'       IS THE FUCKING THING DELETED?                
         BNZ   EXIT                WHY THE FUCK AM I HERE ON SUNDAY???          
         EJECT                                                                  
*                                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXIT                TERM HAS NO VALID ID                         
         CLC   =C'YN',2(R6)                                                     
         BNE   EXIT                                                             
*                                                                               
         CLC   CTILEN,=H'1900'     REC >= 1900 BYTES?                           
         BNL   PRNERR                                                           
*                                                                               
         BAS   RE,CHA21            MAKE CHANGE IF NECESSARY                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
* CHA21: SEE IF X'21' EL EXISTS FOR SYSTEM, IF SO, CHANGE SYS TO X'FN',         
*        ADD NEW EL DENYING ACCESS TO SPECIFIC PROG                             
*                                                                               
CHA21    NTR1                                                                   
         LA    R7,SYSPRG                                                        
*                                                                               
* SEE IF SYS AUTH EL EXISTS FOR SYSTEM                                          
CHA1     GOTO1 VHELLO,DMCB,(C'G',=C'CTFBIG'),(X'21',(R2)),(1,(R7))              
         CLI   DMCB+12,X'06'       ELEM NOT FOUND?                              
         BNE   *+12                                                             
         BAS   RE,ENDSYS           IF NO SYS EL, ADVANCE TO END OF SYS          
         B     CHA20                                                            
*                                                                               
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
         MVC   ELEM(0),0(R1)                                                    
         LA    R6,ELEM                                                          
         USING CTSYSD,R6                                                        
*                                                                               
* SET SYSTEM OF CURRENT ELEM TO X'FN'                                           
         OI    2(R1),X'F0'                                                      
*                                                                               
         AHI   R7,1                R7=PROG ENTRY                                
*                                                                               
* SEE IF PGM=XXXX EXISTS.  IF SO, MAKE IT NOP BY TABLE                          
CHA3     DS    0H                                                               
         LA    R4,3                BXLE INCREMENT                               
         LA    R5,0(R3,R6)         BXLE END (NOTE R3 BCTR'D)                    
         LA    R3,CTSYSPGM         BXLE ADDR                                    
CHA5     CLC   0(1,R3),0(R7)       IS THIS THE PGM=???? WE WANT?                
         BNE   CHA6                IF YES, CHANGE IT                            
         MVC   1(2,R3),1(R7)                                                    
         B     CHA7                GET NEXT PROGRAM FOR THIS SYSTEM             
CHA6     BXLE  R3,R4,CHA5                                                       
         DROP  R6                                                               
*                                                                               
*  IF HERE, ADD PGM=....                                                        
         AHI   R5,1                R5 WAS ON LAST BYTE OF ELEM                  
         MVC   0(3,R5),0(R7)       ADD PGM=....                                 
         ZIC   R3,1(R6)            UPDATE ELEM LEN                              
         AHI   R3,3                                                             
         STC   R3,1(R6)                                                         
*                                                                               
* GET NEXT PROGRAM FOR THIS SYSTEM                                              
CHA7     ZIC   R3,1(R6)            UPDATE ELEM LEN                              
         BCTR  R3,0                                                             
         AHI   R7,3                                                             
         CLI   0(R7),0             END OF SYSTEM?                               
         BNE   CHA3                                                             
*                                                                               
* ADD BACK NEW X'21' (CHANGED OR NOT!)                                          
CHA10    GOTO1 VHELLO,DMCB,(C'P',=C'CTFBIG'),(X'21',(R2)),ELEM                  
         CLI   DMCB+12,0                                                        
         BE    *+6                 NO ERRORS, SET LENGTH AND EXIT               
         DC    H'0'                ELSE DIE                                     
*                                                                               
CHA20    AHI   R7,1                GET NEXT SYSTEM                              
         CLI   0(R7),X'FF'         NO MORE SYSTEMS                              
         BNE   CHA1                                                             
*                                                                               
*  UPDATE BY USING ACTUAL LENGTH IN KEY...                                      
         SR    R1,R1                                                            
         ICM   R1,3,CTILEN                                                      
         AHI   R1,4                                                             
         L     R3,AIOAREA                                                       
         STH   R1,0(R3)                                                         
*                                                                               
* MAKE SURE REC LT 2000 BYTES                                                   
         CHI   R1,2000                                                          
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'00'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
* PRINT KEYS OF ALTERED RECS                                                    
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         GOTO1 VHEXOUT,DMCB,0(R2),P,25,=C'TOG'                                  
         GOTO1 VPRINTER                                                         
         MVC   P(25),0(R2)                                                      
         GOTO1 VPRINTER                                                         
         DROP  R8                                                               
*                                                                               
* BUMP COUNT1  (COUNTER OF CHANGED RECS)                                        
         L     R1,COUNT1                                                        
         LA    R1,1(R1)                                                         
         ST    R1,COUNT1                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
PRNERR   DS    0H                                                               
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVC   P(14),=C'LENGTH ERROR: '                                         
         GOTO1 VHEXOUT,DMCB,0(R2),P+14,25,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         DROP  R8                                                               
*                                                                               
ENDSYS   AHI   R7,1                R7=1ST PROGRAM                               
NS2      CLI   0(R7),0             END OF SYSTEM?                               
         BER   RE                                                               
         AHI   R7,3                                                             
         B     NS2                                                              
                                                                                
*                                                                               
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
COUNT1   DC    F'0'                CHANGED RECS                                 
COUNT2   DC    F'0'                ALL=800F                                     
COUNT3   DC    F'0'                RRGO=                                        
COUNT4   DC    F'0'                DELETED C'5'S                                
*                                                                               
ELEM     DS    CL255                                                            
*                                                                               
SYSPRG   DS    0H                                                               
         DC    X'06'               ACC SYSTEM                                   
         DC    X'030264'            FILE=0264                                   
         DC    X'0B0464'            PRD=0464                                    
         DC    X'190000'            INT=N                                       
         DC    X'00'               END SYSTEM ACC                               
*                                                                               
         DC    X'02'               SPOT SYSTEM                                  
         DC    X'130000'            PAY                                         
         DC    X'00'               END SYSTEM SPOT                              
*                                                                               
         DC    X'04'               PRINT SYSTEM                                 
         DC    X'030000'            PAY                                         
         DC    X'00'               END SYSTEM PRINT                             
*                                                                               
         DC    X'03'               NET SYSTEM                                   
         DC    X'130000'            PAY                                         
         DC    X'00'               END SYSTEM NET                               
*                                                                               
         DC    X'FF'               EOFT                                         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032CTCONAUTH 05/01/02'                                      
         END                                                                    
