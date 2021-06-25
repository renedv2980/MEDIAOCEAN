*          DATA SET DDUNDAY    AT LEVEL 010 AS OF 08/22/00                      
*PHASE T00A0FA                                                                  
         TITLE '* * * CONVERT 1-BYTE DAY CODE TO DAY LITERAL * * *'             
*                                                                     *         
* CODAY - PARAMETER LIST                                              *         
*                                                                     *         
*              WORD 0 - A(1 BYTE DAY CODE)                            *         
*                                                                     *         
*              WORD 1 - A(8 BYTE DAY LTRL FIELD)                      *         
*                                                                     *         
*              FORMAT OF 1-BYTE DAY CODE -                            *         
*                                                                     *         
*              IF BYTE 0=X'FF' (REPPPAK) THEN LTRL FIELD IS 11 BYTES            
*              AND X'40' FILLED.  LENGTH OF DAYS RETURNED IN BYTE 0.            
*                        BIT 0 - ON IF OUT-OF-WEEK ROTATOR            *         
*                        BIT 1 - ON FOR MON                           *         
*                        BIT 2 - ON FOR TUE                           *         
*                             ETC                                     *         
*                        BIT 7 - ON FOR SUN                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
UNDAY    CSECT                                                                  
         NMOD1 5,**UNDAY                                                        
         B     *+12                                                             
         DC    CL8'*CODAY*'                                                     
         USING CDYWK,12                                                         
         XC    0(40,12),0(12)      CLEAR WORK AREA                              
         MVC   CDPARS,0(1)         SAVE PARAMETERS                              
         L     1,0(1)              GET BDAY ADDRESS                             
         SR    2,2                                                              
         IC    2,0(1)              INSERT DAY CDE                               
         LA    4,CDAYWK+6          POINT TO SUNDAY POSITION                     
         LA    5,7                 SET FOR SUNDAY                               
CDY01    SR    3,3                 CLEAR R3                                     
         SRDL  2,1                 SHIFT BIT TO R3                              
         LTR   3,3                 TEST IF ON                                   
         BC    8,*+8               NO                                           
         STC   5,0(4)              YES-STORE DAY NUMBER                         
         BCTR  4,0                 BUMP TO NEXT LOWER DAY                       
         BCT   5,CDY01             BUMP CDE TO NXT LWR DAY                      
         LTR   2,2                 TEST IF OUT-OF-WK ROTATOR                    
         BC    8,CDY03             NO                                           
         LA    4,CDAYWK            YES-POINT TO MONDAY                          
         LA    5,CDAYWK+7          POINT TO NEXT MONDAY                         
         LA    6,6                 SET BCT REG FOR MON THRU SAT                 
*                                                                               
CDY02    CLI   0(4),0              TEST IF END OF ROTATION                      
         BE    CDY03               YES                                          
         MVC   0(1,5),0(4)         NO-MOVE TO NEXT WEEK                         
         MVI   0(4),0              ZERO THIS WEEK                               
         LA    5,1(5)              BUMP NXT WEEK                                
         LA    4,1(4)              AND THIS WEEK POINTERS                       
         BCT   6,CDY02             TEST NEXT DAY                                
CDY03    LA    8,13                SET BCT REG FOR MAX 13 DAY SCAN              
         LA    14,DYLTL            POINT TO DAY LITERAL FORMAT AREA             
         MVI   DSCMSW,128          SET FOR 1ST TIME                             
         LA    4,CDAYWK            POINT TO THIS MONDAY                         
         LA    5,DTAB-3            BASE ADDRESS FOR DAYTAB                      
         SR    7,7                                                              
CDY04    CLI   0(4),0              TEST FOR ACTIVE DAY                          
         BNE   CDY045              YES                                          
         TM    DSCMSW,16           TEST IF 2ND TIME THRU ON ROTATION            
         BC    8,CDY041            NO                                           
         LA    14,2(14)            YES-BUMP POINTER BY 2                        
         B     CDY043              SET FOR COMMA                                
CDY041   TM    DSCMSW,1            TEST IF 3RD TIME THRU ON ROTATION            
         BC    8,CDY043+4          NO                                           
         LA    14,1(14)            BUMP OVER END DAY OF ROTATION                
         CLI   0(14),C' '          TEST IF TRUNCATED                            
         BC    8,*+8               YES                                          
         LA    14,1(14)            NO - BUMP OVER 2ND CHAR OF DAY               
CDY043   MVI   DSCMSW,2            SET FOR COMMA                                
         LA    4,1(4)              BUMP TO NEXT DAY                             
         BCT   8,CDY04                                                          
         B     CDY048              END-COMPUTE LENGTH OF LTL                    
*                                                                               
CDY045   SR    6,6                                                              
         IC    6,0(4)              INSERT DAY NUMBER                            
         MH    6,=H'3'             MULT BY DAY ABBREV LGTH                      
         LA    6,0(5,6)            POINT TO TABLE ENTRY                         
         LA    2,1                 SET MOVE LGTH                                
         TM    DSCMSW,128                                                       
         BC    8,*+12              IF 1ST TIME                                  
         MVI   DSCMSW,0                                                         
         LA    2,2                 MOVE 3 CHARS                                 
         TM    DSCMSW,16           IF LOOKING FOR NXT DAY OF ROTATION           
         BC    8,*+20                                                           
         MVI   DSCMSW,1            SET FOR SUBSEQUENT DAYS                      
         LA    14,2(14)            BUMP OVER 1ST DAY OF ROTATION                
         MVI   0(14),C'-'          MOVE IN DASH                                 
         LA    14,1(14)                                                         
         CLI   DSCMSW,1            TEST IF DASH OR COMMA WANTED                 
         BNP   CDY046                                                           
         MVI   0(14),C','                                                       
         CLI   CDPARS,X'FF'        REPPAK?                                      
         BE    *+8                                                              
         MVI   0(14),C'/'          SLASH(INSTEAD OF COMMA)                      
         LA    14,1(14)                                                         
CDY046   EX    2,MVDAY             MVC  0(0,14),0(6)                            
         CLI   0(14),C'T'          TEST IF TUE/THU                              
         BC    8,*+8                                                            
         CLI   0(14),C'S'          TEST IF SAT/SUN                              
         BE    *+22                                                             
         BCTR  14,0                1 CHARACTER IS UNIQUE                        
         CLI   DSCMSW,1            TEST IF ROTATOR                              
         BC    7,*+12                                                           
         MVI   2(14),C' '          BLANK  CHARACTER 2                           
         LA    14,1(14)            DONT MOVE R14                                
         CLI   DSCMSW,1            TEST IF LOOKINGFOR  ROTATOR END              
         BC    8,CDY043+4          YES- RETURN                                  
         MVI   DSCMSW,16           NO-SET FOR 2ND DAY OF ROTATION               
         B     CDY043+4            RETURN FOR NXT DAY                           
*                                                                               
CDY048   LA    14,DYLTL            POINT TO FORMATTED LITERAL                   
         SR    15,15               ZERO LGTH COUNTER                            
         TM    0(14),X'BF'         EXIT ON FIRST BLANK OR ZERO                  
         BC    8,CDY05                                                          
         LA    15,1(15)                                                         
         LA    14,1(14)                                                         
         B     *-16                                                             
* LITERAL FORMATTED - MOVE TO CENTER OF OUTPUT FIELD                            
*                                                                               
CDY05    EQU   *                                                                
         LA    1,DYLTL(15)         POINT TO END OF LTRL                         
         BCTR  1,0                 POINT TO 2ND LAST CHAR                       
         BCTR  1,0                                                              
         CLI   0(1),C'F'                                                        
         BC    8,*+8                                                            
         CLI   0(1),C'W'                                                        
         BC    8,*+8                                                            
         CLI   0(1),C'M'                                                        
         BNE   *+6                                                              
         BCTR  15,0                                                             
         L     1,CDPARS+4          OUTPUT AREA                                  
         CLI   CDPARS,X'FF'        REPPAK?                                      
         BNE   SPOTCD                                                           
         MVC   0(11,1),=11C' '                                                  
         STC   15,CDPARS           LENGTH                                       
         CH    15,=H'11'           MAX                                          
         BNH   CODAYX                                                           
         LA    15,11               MAX LENGTH                                   
         B     CODAYX                                                           
SPOTCD   XC    0(8,1),0(1)         SPOTPAK                                      
         CH    15,=H'8'            MAX                                          
         BNH   CODAYX                                                           
         LA    15,8                                                             
CODAYX   BCTR  15,0                                                             
         EX    15,MVLTL            MVC  0(0,1),DYLTL                            
*                                                                               
         XMOD1 1                                                                
*                                                                               
MVDAY    MVC   0(0,14),0(6)        MOVE DAY ABBRV                               
MVLTL    MVC   0(0,1),DYLTL                                                     
*                                                                               
DTAB     DC    C'MONTUEWEDTHUFRISATSUN'                                         
*                                                                               
         LTORG                                                                  
CDYWK    DSECT                                                                  
CDPARS   DS    D                                                                
CDAYWK   DS    CL14                                                             
DSCMSW   DS    C                                                                
         DS    C                                                                
DYLTL    DS    D                                                                
         DS    D                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010DDUNDAY   08/22/00'                                      
         END                                                                    
