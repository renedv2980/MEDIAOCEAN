*          DATA SET CTCONNEW21 AT LEVEL 048 AS OF 05/01/02                      
*PHASE CON21,*                                                                  
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO ADD NEW X'21' ELEMS FOR   *         
* NEW SYSTEM TRF1 (STRAF - SYSTEM X'0D')                              *         
*                                                                     *         
* USE SPOT TRAFFIC VALUE OR SPOT ALL VALUE AS TRF1 ALL VALUE          *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONNEW21 - ADD NEW SYS AUTH (X''21'') ELEMS'                  
CON21    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CON21**                                                       
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
*         MVC   P(10),=C'CHANGED:  '                                            
*         GOTO1 VHEXOUT,DMCB,COUNT1,P+10,4,=C'TOG'                              
*         GOTO1 VPRINTER                                                        
*         MVC   P(10),=C'ALL=800F: '                                            
*         GOTO1 VHEXOUT,DMCB,COUNT2,P+10,4,=C'TOG'                              
*         GOTO1 VPRINTER                                                        
*         MVC   P(10),=C'RRGO=:    '                                            
*         GOTO1 VHEXOUT,DMCB,COUNT3,P+10,4,=C'TOG'                              
*         GOTO1 VPRINTER                                                        
*         MVC   P(10),=C'DELETED:  '                                            
*         GOTO1 VHEXOUT,DMCB,COUNT4,P+10,4,=C'TOG'                              
*         GOTO1 VPRINTER                                                        
         B     EXIT                                                             
*                                                                               
M10      DS    0H                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'FF'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
*         CLI   0(R2),C'T'          TERMINAL REC?                               
*         BE    M14                                                             
         CLI   0(R2),C'I'          ID REC?                                      
         BE    M14                                                              
*         CLI   0(R2),C'0'          AUTH REC?                                   
*         BE    M14                                                             
         CLI   0(R2),C'5'          ACCESS REC?                                  
         BNE   EXIT                                                             
*                                                                               
         USING CTTREC,R2                                                        
M14      TM    CTTSTAT,X'80'       IS THE FUCKING THING DELETED?                
         BNZ   EXIT                WHY THE FUCK AM I HERE ON SUNDAY???          
         EJECT                                                                  
*                                                                               
         BAS   RE,CHA21            MAKE CHANGE IF NECESSARY                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
* CHA21: SEE IF X'21' EL EXISTS FOR SPOT AND IF SO, ADD STRAF ELEM              
*        WITH ALL= SPOT TRAF VALUE OR SPOT ALL VALUE.                           
*                                                                               
CHA21    NTR1                                                                   
*                                                                               
* SEE IF SYS AUTH EL ALREADY EXISTS FOR STRAF                                   
         GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'21',(R2)),=X'0D'                
         CLI   DMCB+12,X'06'       FOUND?                                       
         BE    CHA1                 NO                                          
         MVC   P(25),0(R2)                                                      
         MVC   P+30(16),=C'HAD TRAFFIC ELEM'                                    
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
* SEE IF SYS AUTH EL EXISTS FOR SPOT                                            
CHA1     GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE'),(X'21',(R2)),=X'02'                
         CLI   DMCB+12,X'06'       ELEM NOT FOUND?                              
         BE    EXIT                                                             
         CLI   DMCB+12,0           ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,DMCB+12          A(ELEM)                                      
         USING CTSYSD,R6                                                        
*                                                                               
* FIND NEW TRAF SE NUM FROM SPOT SE NUM                                         
*        CLI   0(R2),C'T'          TERM RECS DON'T SEEM TO HAVE SE NUMS         
*        BE    CHA3                                                             
*        CLI   0(R2),C'0'          NEITHER DO AUTH RECS                         
*        BE    CHA3                                                             
*                                                                               
         LA    RE,SETAB                                                         
         CLC   0(1,RE),CTSYSSE                                                  
         BE    *+20                                                             
         LA    RE,L'SETAB(RE)                                                   
         CLI   0(RE),X'FF'         EOT?                                         
*        BE    BADSE                                                            
         BE    EXIT                SPOTN ONLY FOR THIS RUN                      
         B     *-22                                                             
         MVC   NEWSE,1(RE)                                                      
*                                                                               
         CLC   CTTLEN,=H'950'      REC >= 990 BYTES?                            
         BNL   PRNERR                                                           
*                                                                               
* SEE IF TRAF=XXXX EXISTS                                                       
CHA3     DS    0H                                                               
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         LA    R4,3                BXLE INCREMENT                               
         LA    R5,0(R1,R6)         BXLE END                                     
         LA    R3,CTSYSPGM         BXLE ADDR                                    
CH5      CLI   0(R3),PROGRAM       IS THIS AN TRAF=????                         
         BE    CH7                                                              
         BXLE  R3,R4,CH5                                                        
         MVC   NEWALL,CTSYSALL                                                  
         B     *+10                                                             
CH7      MVC   NEWALL,1(R3)                                                     
         MVC   NEWAGB,CTSYSAGB                                                  
*                                                                               
* BUILD NEW X'21' ELEM                                                          
         CLI   0(R2),C'5'          ACCESS REC?                                  
         BE    *+14                 YES                                         
         OC    NEWALL,NEWALL       TRAF=N?                                      
         BZ    EXIT                 YES - DON'T ADD ELEM                        
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   CTSYSEL,CTSYSELQ                                                 
         MVI   CTSYSLEN,CTSYSL1Q                                                
         MVI   CTSYSNUM,X'0D'                                                   
         MVC   CTSYSSE,NEWSE                                                    
         MVC   CTSYSAGB,NEWAGB                                                  
         OI    CTSYSIND,CTSYSINF                                                
         MVC   CTSYSALL,NEWALL                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE'),(X'21',(R2)),ELEM                  
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
         GOTO1 VHEXOUT,DMCB,0(R2),P,25,=C'TOG'                                  
         GOTO1 VPRINTER                                                         
         MVC   P(25),0(R2)                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
* BUMP COUNT1  (COUNTER OF CHANGED RECS)                                        
*         L     R1,COUNT1                                                       
*         LA    R1,1(R1)                                                        
*         ST    R1,COUNT1                                                       
          B     EXIT                                                            
         EJECT                                                                  
*                                                                               
BADSE    MVC   P(25),0(R2)                                                      
         MVC   P+30(11),=C'UNKNOWN ADV'                                         
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
PRNERR   DS    0H                                                               
         MVC   P(14),=C'LENGTH ERROR: '                                         
         GOTO1 VHEXOUT,DMCB,0(R2),P+14,25,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(25),0(R2)                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         DROP  R8                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
PROGRAM  EQU   X'16'               PROG X'16' = TRAFFIC                         
RECAGY   DS    CL2                                                              
DATADISP DS    H                                                                
ELCODE   DS    C                                                                
NEWSE    DS    X                                                                
NEWALL   DS    XL2                                                              
NEWAGB   DS    X                                                                
COUNT1   DC    F'0'                CHANGED RECS                                 
COUNT2   DC    F'0'                ALL=800F                                     
COUNT3   DC    F'0'                RRGO=                                        
COUNT4   DC    F'0'                DELETED C'5'S                                
*                                                                               
ELEM     DS    CL255                                                            
*                                                                               
AGYLIST  DS    0CL2                LIST OF 2 CHAR CODES TO RUN ON               
         DC    CL2'TO'                                                          
         DC    CL2'DI'                                                          
         DC    CL2'GP'                                                          
         DC    CL2'MM'                                                          
         DC    CL2'HN'                                                          
         DC    CL2'MG'                                                          
         DC    X'FF'               TABLE END                                    
*                                                                               
SETAB    DS    0XL2                                                             
**DONE   DC    X'0241'             SPOT1/TRAF1                                  
         DC    X'0342'             SPOT2/TRAF2                                  
         DC    X'0743'             SPOT3/TRAF3                                  
         DC    X'0D46'             SPOT4/TRAF4                                  
         DC    X'1147'             SPOT5/TRAF5                                  
         DC    X'1548'             SPOT6/TRAF6                                  
         DC    X'2249'             SPOT7/TRAF7                                  
         DC    X'274A'             SPOTB/TRAFB                                  
         DC    X'E24B'             SPOTE/TRAFE                                  
         DC    X'F24C'             SPOTF/TRAFF                                  
         DC    X'F34D'             SPOTG/TRAFG                                  
         DC    X'F44E'             SPOTH/TRAFH                                  
         DC    X'F551'             SPOTL/TRAFL                                  
         DC    X'F652'             SPOTM/TRAFM                                  
**DONE   DC    X'F753'             SPOTN/TRAFN                                  
         DC    X'3256'             SPOTQ/TRAFQ                                  
         DC    X'3357'             SPOTS/TRAFS                                  
         DC    X'FF'               EOT                                          
*                                                                               
*          DATA SET SPREPFXMH  AT LEVEL 046 AS OF 12/07/93                      
FACSYTAB DS    0D                                                               
*              X' 0 1 2 3 4 5 6 7 8 9 A B C D E F'                              
         DC    X'00000101010102010000000001010100'  00-0F                       
         DC    X'05020000010101060102000002000000'  10-0F                       
         DC    X'00000401010101010100010400000000'  20-0F                       
         DC    X'00000000020200000000000006000000'  30-0F                       
*              X' 0 1 2 3 4 5 6 7 8 9 A B C D E F'                              
         DC    X'00010000020400000000000000000000'  40-0F                       
         DC    X'00000000040000000000000000000000'  50-0F                       
         DC    X'00000000060001000000000000000000'  60-0F                       
         DC    X'00000000000605000000000000000000'  70-0F                       
*              X' 0 1 2 3 4 5 6 7 8 9 A B C D E F'                              
         DC    X'00000000000004000000000000000000'  80-0F                       
         DC    X'00000000000004000000000000000000'  90-0F                       
         DC    X'00000000000004000000000000000000'  A0-0F                       
         DC    X'00000000000006000000000000000000'  B0-0F                       
*              X' 0 1 2 3 4 5 6 7 8 9 A B C D E F'                              
         DC    X'00000000000000000000000000000000'  C0-0F                       
         DC    X'00000000000000000000000000000000'  D0-0F                       
         DC    X'00000200000000000000000000000000'  E0-0F                       
         DC    X'00000204040406000000000000000000'  F0-0F                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048CTCONNEW2105/01/02'                                      
         END                                                                    
