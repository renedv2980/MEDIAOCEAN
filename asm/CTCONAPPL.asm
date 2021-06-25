*          DATA SET CTCONAPPL  AT LEVEL 077 AS OF 08/10/00                      
*PHASE CONAPPLA                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO ADD X'24' APPL ID ELEMS   *         
* FOR ADV7 TO LISTED PRINTERS AND REMOVING ALL OTHER X'24'            *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONAPPL - ADD NEW APPL ID (X''24'') ELEMS'                    
CONAPPL  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONAPL*                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
*                                                                               
         MVI   DATADISP+1,28                                                    
*                                                                               
MAIN     DS    0H                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         CLI   0(R2),X'FF'         TEST FILE TRAILER                            
         BNE   M10                 YES - PRINT TOTALS                           
         MVC   P(10),=C'CHANGED:  '                                             
         GOTO1 VHEXOUT,DMCB,COUNT1,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
M10      DS    0H                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'FF'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
         CLI   0(R2),C'T'          TERMINAL REC?                                
         BNE   EXIT                                                             
*                                                                               
         USING CTTREC,R2                                                        
         TM    CTTSTAT,X'80'       IS THE FUCKING THING DELETED?                
         BNZ   EXIT                WHY THE FUCK AM I HERE ON SUNDAY???          
         EJECT                                                                  
*                                                                               
         CLC   CTTLEN,=H'950'      REC >= 950 BYTES?                            
         BNL   PRNERR                                                           
*                                                                               
*        LA    R1,CTTKTID                                                       
*        OC    CTTKTID,CTTKTID     THIS A PASSIVE?                              
*        BNZ   M1A                  NO                                          
*                                                                               
* FOR PASSIVES, MAKE SURE NOT PASSWORD PASSIVE                                  
*        GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'03',(R2)),0                     
*        CLI   DMCB+12,0           ANY ERRORS?                                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        L     R3,DMCB+12          A(ELEM)                                      
*        CLI   1(R3),X'0A'         PASSWORD REC                                 
*        BH    EXIT                 YES                                         
*        LA    R1,2(R3)                                                         
*        B     M1A1                                                             
*                                                                               
M1A      DS    0H                                                               
         OC    CTTKPASS,CTTKPASS   PASSWORD/PRINTER-PAGE?                       
         BNZ   EXIT                 YES - SKIP                                  
         CLI   CTTKTID+7,C'P'      PRINTER?                                     
         BE    *+12                 YES                                         
         CLI   CTTKTID+7,C'S'      SHUTTLE?                                     
         BNE   EXIT                 NO - SKIP IT                                
*                                                                               
*M1A1     LA    RF,TERMLIST                                                     
*M2       CLC   0(8,R1),0(RF)                                                   
*         BE    M14                                                             
*         LA    RF,L'TERMLIST(RF)                                               
*         CLI   0(RF),X'FF'                                                     
*         BNE   M2                                                              
*         B     EXIT                                                            
*                                                                               
M2       CLC   =C'BS',CTTKTID                                                   
         BE    M14                                                              
         CLC   =C'CA',CTTKTID                                                   
         BE    M14                                                              
         CLC   =C'DF',CTTKTID                                                   
         BE    M14                                                              
         CLC   =C'ZE',CTTKTID                                                   
         BE    M14                                                              
         CLC   =C'B4KY',CTTKTID                                                 
         BNE   EXIT                                                             
*                                                                               
M14      DS    0H                                                               
         BAS   RE,ADD24            ADD APPL ID ELEM FOR REPC                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
* ADD24: ADD X'24' ELEM FOR ADV3 FOR SELECTED TERMS.                            
*                                                                               
ADD24    NTR1                                                                   
*                                                                               
* DELETE ALL EXISTING 24'S                                                      
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE'),(X'24',(R2))                       
* ADD APPL ID ELEM FOR ADV3                                                     
         MVC   P+10(13),SPACES                                                  
         XC    ELEM,ELEM                                                        
         MVC   ELEM(3),=X'240B00'                                               
         MVC   ELEM+3(8),=C'ADV7    '                                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE'),(X'24',(R2)),ELEM                  
         CLI   DMCB+12,0                                                        
         BE    *+6                 NO ERRORS, SET LENGTH AND EXIT               
         DC    H'0'                ELSE DIE                                     
*                                                                               
*  UPDATE BY USING ACTUAL LENGTH IN KEY...                                      
         SR    R1,R1                                                            
         ICM   R1,3,CTTLEN                                                      
         AH    R1,=H'4'                                                         
         L     R3,AIOAREA                                                       
         STH   R1,0(R3)                                                         
*                                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'00'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
* PRINT KEYS OF ALTERED RECS                                                    
*         OC    CTTKTID,CTTKTID     THIS A PASSIVE?                             
*         BZ    *+14                 YES                                        
         MVC   P(8),7(R2)                                                       
         B     ADD40                                                            
*                                                                               
*         GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'03',(R2)),0                    
*         CLI   DMCB+12,0           ANY ERRORS?                                 
*         BE    *+6                                                             
*         DC    H'0'                                                            
*         L     R3,DMCB+12          A(ELEM)                                     
*         MVC   P(8),2(R3)                                                      
ADD40    GOTO1 VPRINTER                                                         
*                                                                               
* BUMP COUNT1  (COUNTER OF CHANGED RECS)                                        
         L     R1,COUNT1                                                        
         LA    R1,1(R1)                                                         
         ST    R1,COUNT1                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
PRNERR   DS    0H                                                               
         MVC   P(14),=C'LENGTH ERROR: '                                         
         GOTO1 VHEXOUT,DMCB,0(R2),P+14,25,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(25),0(R2)                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
*FNDERR   DS    0H                                                              
*         MVC   P(14),=C'REPC FOUND:   '                                        
*         GOTO1 VHEXOUT,DMCB,0(R2),P+14,25,=C'TOG'                              
*         GOTO1 VPRINTER                                                        
*         MVC   P(25),0(R2)                                                     
*         GOTO1 VPRINTER                                                        
*         B     EXIT                                                            
*         DROP  R8                                                              
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
*                                                                               
ELEM     DS    CL255                                                            
COUNT1   DS    F'0'                                                             
FLAG     DS    X                                                                
FOUND5   EQU   X'80'                                                            
*                                                                               
*TERMLIST DS    0CL8                                                            
*         DC    CL8'A041T041'                                                   
*         DC    X'FF'                                                           
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'077CTCONAPPL 08/10/00'                                      
         END                                                                    
