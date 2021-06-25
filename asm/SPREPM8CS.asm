*          DATA SET SPREPM8CS  AT LEVEL 025 AS OF 06/27/01                      
*PHASE SPM8CSA                                                                  
         SPACE 1                                                                
*----------------------------------------------------------------*              
* THIS MODULE IS LOADED BY AT LEAST THE FOLLOWING PHASES         *              
* SPMEDGETBY                                                     *              
* SPREPM802                                                      *              
* SPREPM902                                                      *              
*---------                                                                      
* IF YOU FIND YET ANOTHER BUG IN THIS CODE, BE SURE TO LOOK FOR  *              
* ONE IN SPOTBUY(GETTAL) TOO.                                    *              
*----------------------------------------------------------------*              
         TITLE 'SPM8CS - ROUTINE TO FIND TALENT FACTOR'                         
GETTFACT CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TALDX-TALD,**GETT**                                              
         USING TALD,RC                                                          
         LA    R9,2048(RA)         RA IS SET TO WS                              
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LM    R2,R3,0(R1)         A(QEND)/A(TALFACT)                           
         STCM  R3,8,MYPRDCD        HOB OF TALFACT IS PRD CODE                   
         SPACE 1                                                                
* BUILD REQUEST PARAMETERS AND TEST SAME AS LAST TIME *                         
         SPACE 1                                                                
         MVC   MYSAVE(2),QAGY                                                   
         MVC   MYSAVE+2(1),QMED                                                 
         MVC   MYSAVE+3(3),QCLT                                                 
         MVC   MYSAVE+6(6),QEND                                                 
         CLC   SVQDATA(12),MYSAVE  TEST SAME AS PREVIOUS CALL                   
         BE    GETF20                                                           
*                                                                               
         MVC   SVQDATA(12),MYSAVE  ELSE SAVE THESE PARAMS                       
         LA    R0,1                INITIALIZE TALENT FACTORS TO 1               
         LA    R1,1                                                             
         STM   R0,R1,SVTALFC0                                                   
         STM   R0,R1,SVTALFC1                                                   
         STM   R0,R1,SVTALFC2                                                   
         STM   R0,R1,SVTALFC3                                                   
         SPACE 1                                                                
* BEFORE READING FILE SAVE KEY AND AREC AND RESTORE BEFORE EXIT *               
         SPACE 1                                                                
         MVC   MYSAVE(13),KEY      SAVE KEY                                     
         MVC   MYSAVE+20(4),AREC    AND REC ADDRESS                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TALKEY,R4                                                        
         MVC   TALKTYP,=X'0D27'                                                 
         MVC   TALKAGMD,BAGYMD                                                  
         GOTO1 CLPACK,DMCB,QCLT,TALKCLT                                         
         MVC   TALKDATE,0(R2)                                                   
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(5),KEYSAVE      IF NO TALENT REC, EXIT                       
         BNE   GETF10                                                           
         MVC   KEYSAVE,KEY         ELSE SAVE THE DATE WE GOT NOW                
*                                                                               
GETF2    LA    R4,MYIO                                                          
         ST    R4,AREC                                                          
         GOTO1 GET                                                              
         CLI   TALEL05,X'05'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RE,RE                                                            
         IC    RE,MYIO+11          GET TALENT FACTOR GROUP                      
         SLL   RE,3                X 8 BYTES PER GROUP                          
         LA    RE,SVTALFC0(RE)                                                  
         MVC   0(8,RE),TAL05MUL    SAVE FACTOR AND BASE                         
*                                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(10),KEYSAVE                                                  
         BE    GETF2                                                            
         SPACE 1                                                                
GETF10   MVC   AREC,MYSAVE+20          RESTORE AREC                             
         MVC   KEY(13),MYSAVE          RESTORE USER KEY                         
         GOTO1 HIGH                                                             
*                                                                               
GETF20   LA    RF,SVTALFC0                                                      
         LA    R0,1                                                             
         C     R0,0(RF)            TEST FOUND ANY FACTORS                       
         BE    GETF40              NO - JUST EXIT                               
* NOTE THIS INSTRUCTION LOOKS AT DATE IN RECORD NOT KEY                         
         CLC   SVQDATA+6(6),=C'960527' FOLLOWING LOGIC APPLIES TO '97+          
         BL    GETF40                                                           
         SPACE 1                                                                
*=============================================================                  
* FOR CLIENT PTA, TO ACCOMODATE MORE THAN 64 GMI BRANDS                         
* GMI FACTOR APPLIES TO ALL PRODUCTS 1-95, EXCEPT FOR FL AND RL                 
* WHICH GOT CODES LOWER BEFORE WE COULD DO ANYTHING ABOUT IT                    
*=============================================================                  
         SPACE 1                                                                
         CLC   QCLT,=C'PTA'        CLT PTA?                                     
         BNE   GETF30                                                           
         LA    RF,SVTALFC2                                                      
         CLI   MYPRDCD,X'9F'                                                    
         BH    GETF40                                                           
         LA    RF,SVTALFC1         USE NON-GMI FACTOR                           
         CLI   MYPRDCD,X'40'       FOR PRD FL                                   
         BE    GETF40                                                           
         CLI   MYPRDCD,X'41'       AND RL                                       
         BE    GETF40                                                           
         CLI   MYPRDCD,X'5F'       AND X'60' AND HIGHER                         
         BH    GETF40                                                           
         LA    RF,SVTALFC0         ELSE USE GMI VALUE                           
         B     GETF40                                                           
*                                                                               
* NOT CLIENT PTA                                                                
*                                                                               
GETF30   CLI   MYPRDCD,X'40'       CODES 01-3F GET GMI FACTOR                   
         BL    GETF40                                                           
         CLI   MYPRDCD,210         PRD CODES > 210 ARE SPECIAL                  
         BH    GETF40              AND GET GMI FACTOR                           
*                                                                               
GETF32   LA    RF,SVTALFC1                                                      
         CLI   MYPRDCD,X'60'                                                    
         BL    GETF40                                                           
         LA    RF,SVTALFC2                                                      
         CLI   MYPRDCD,X'80'                                                    
         BL    GETF40                                                           
         LA    RF,SVTALFC3                                                      
         CLI   MYPRDCD,X'A0'                                                    
         BL    GETF40                                                           
         DC    H'0'                THESE PRDCODES AREN'T DEFINED                
*                                                                               
GETF40   MVC   0(8,R3),0(RF)       RETURN FACTOR TO CALLER                      
         XIT1                                                                   
         SPACE 1                                                                
         DS    0D                                                               
SVQDATA  DC    XL16'00'                                                         
SVTALFC0 DC    D'0'                FOR PRD CODES X'00' - X'3F'                  
SVTALFC1 DC    D'0'                              X'40' - X'5F'                  
SVTALFC2 DC    D'0'                              X'60' - X'7F'                  
SVTALFC3 DC    D'0'                              X'80' - X'9F'                  
         SPACE 2                                                                
TALD     DSECT                                                                  
MYPRDCD  DS    XL1                                                              
         DS    XL3                                                              
MYSAVE   DS    XL24                                                             
MYIO     DS    2000C                                                            
TALDX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPGENTAL                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPREPM8CS 06/27/01'                                      
         END                                                                    
