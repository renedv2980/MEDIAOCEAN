*          DATA SET DDPAPTCRD  AT LEVEL 002 AS OF 05/07/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE PAPTCRDA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DDINFO                                                                 
*INCLUDE PANIC                                                                  
         TITLE 'GENERATE PANVALET CONTROL CARDS'                                
PAPTCRD  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,PAPTCRD,=V(REGSAVE)                                            
*                                                                               
         LR    R1,RC                                                            
         AHI   R1,-4                                                            
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM= FROM EXEC JCL CARD)                  
         CLC   0(2,R1),=H'4'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PARMUSER,2(R1)      CAPTURE THE PARM USER CODE                   
*                                                                               
         GOTO1 =V(DDINFO),DMCB,=C'UPDCARD ',=AL2(DINRTDSN)                      
         LTR   RF,RF               IS UPDCARD DD ALLOCATED?                     
         BNZ   *+8                                                              
         MVI   UPDCFLAG,C'Y'       YES                                          
*                                                                               
         USING DIRENTRY,R4         BASE FOR DIRECTORY ENTRY                     
         LA    R4,DIRECT2                                                       
*                                                                               
         OPEN  (PVIN,OUTPUT)                                                    
         OPEN  (USER,OUTPUT)                                                    
         CLI   UPDCFLAG,C'Y'       ONLY OPEN UPDCARD IF ALLOCATED               
         BNE   MAIN                                                             
         OPEN  (UPDCARD,OUTPUT)                                                 
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*   THIS IS THE MAIN PROCESSING LOOP.                                 *         
*   THERE ARE TWO POSSIBILITIES HERE:                                 *         
*   1.  IN THE RETRIEVE JOBSTREAM THE PARM IS THE LAST 4 DIGITS OF    *         
*   THE MR NUMBER, AND THE SYSIN IS ONLY ONE RECORD, WITH THE MEMBER  *         
*   NAME LEFT-JUSTIFIED.  IN THIS CASE, THE GET AFTER MAIN00 HITS     *         
*   THE EOF CONDITION, AND CONTROL PASSES TO MAIN01.                  *         
*   2.  IN THE FINAL COMPILE SEQUENCE, THE FIRST CARD IS A MEMBER     *         
*   NAME CARD, JUST AS IN CASE 1 ABOVE.  BUT IT MAY BE FOLLOWED BY    *         
*   ONE OR MORE DIRECTORY ENTRY CARDS, ONE OF WHICH WILL MATCH THE    *         
*   NAME CARD.  IN THIS CASE THE MAIN00 LOOP CONTINUES TO READ SYSIN  *         
*   UNTIL ONE OF THE DIRECTORY RECORDS MATCHES THE INITIAL NAME CARD. *         
*                                                                     *         
***********************************************************************         
*                                                                               
MAIN     DS    0H                                                               
*                                  FIRST RECORD CONTAINS MEMBER NAME            
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   BOOKNAME,CARD                                                    
         CLC   =C'/*',CARD                                                      
         BE    MAIN01                                                           
*                                                                               
         MVC   C1MEM,INMEM         SAVE IT WHERE IT WILL BE NEEDED              
         MVC   C6MEM,INMEM                                                      
         MVC   C7MEM,INMEM                                                      
*                                                                               
MAIN00   DS    0H                                                               
*                                  LOOKING FOR MATCHING DIRECTORY REC           
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    MAIN01                                                           
*                                                                               
         CLC   C1MEM(10),CARD      IS THIS IT?                                  
         BNE   MAIN00              NO: KEEP LOOKING                             
*                                                                               
* IF WE DON'T GET A MATCH, EOF ON SYSIN TAKES US TO MAIN01 BELOW.               
* THAT WILL ALWAYS BE THE CASE DURING RETRIEVE. IN FINAL COMPILE                
* WE WILL NEVER EXECUTE THE GENUSER JOB STEP IF THERE WAS NO PROD               
* MEMBER.                                                                       
*                                                                               
MAIN01   DS    0H                                                               
         LA    R5,C1MEM                                                         
         TRT   C1MEM,TRTBLANK      FIND END OF MEMBER NAME                      
         SR    R1,R5               MEMBER NAME LENGTH                           
         LR    R6,R1               SAVE THE LENGTH                              
         LA    R5,0(R6,R5)         POINT TO END OF NAME                         
         MVI   0(R5),C','          PLACE COMMA                                  
         LA    R5,1(R5)            ADVANCE OUTPUT POINTER                       
         STM   R5,R6,R56SV         SAVE POINTER AND LENGTH                      
*                                                                               
         GOTO1 =V(PANIC),DMCB,(0,=C'READ'),=C'DIR',BOOKNAME,DIRECT2             
         CLI   DMCB+8,0            PAN BOOK FOUND?                              
         BNE   CLOSE               NO: ERROR                                    
*                                                                               
*****                              YES: PROCEED                                 
         MVC   BASELVL,DLEVEL      CAPTURE THE BASE LEVEL                       
         PACK  PLVL1,DLEVEL        PACK IT TO THE INCREMENTAL FIELDS            
         PACK  PLVL2,DLEVEL                                                     
         AP    PLVL1,=PL1'1'       FORM THE BASE LEVEL + 1                      
         CP    PLVL1,=PL2'255'     DID IT ROLL OVER ?                           
         BNH   *+10                NO: BYPASS                                   
         ZAP   PLVL1,=PL2'1'       YES: RESET TO 1                              
         ZAP   PLVL2,PLVL1         COPY TO PACKED LEVEL+2                       
         AP    PLVL2,=PL1'1'       FORM THE BASE LEVEL + 2                      
         CP    PLVL2,=PL2'255'     DID IT ROLL OVER?                            
         BNH   *+10                NO: BYPASS                                   
         ZAP   PLVL2,=PL2'1'       YES: RESET TO 1                              
         UNPK  LVLPLUS1,PLVL1      CONVERT BACK TO CHARACTER                    
         OI    LVLPLUS1+2,X'F0'    ASSURE NUMERIC ZONE                          
         UNPK  LVLPLUS2,PLVL2      CONVERT BACK TO CHARACTER                    
         OI    LVLPLUS2+2,X'F0'    ASSURE NUMERIC ZONE                          
*                                                                               
* "DUSER" COMES FROM THE DIRECTORY ENTRY RETURNED BY PANIC.                     
* THIS IS THE USER CODE FROM THE PROD MEMBER THAT WAS                           
* USED AS THE BASE.                                                             
*                                                                               
* DURING RETRIEVE, THE PARM SUPPLIES THE TEST USER CODE.  BUT IN                
* FINAL COMPILE, THE PARM IS 0000 AND WE USE "INUSER", THE USER                 
* CODE FROM THE 0-UP RECORD THAT MATCHED THE INPUT MEMBER NAME.                 
* THIS IS THE TEST MEMBER'S DIRECTORY ENTRY.                                    
*                                                                               
         MVC   BASEUSER,DUSER      CAPTURE THE BASE USER CODE                   
         MVC   TESTUSER,PARMUSER   POSIT TEST USER CODE IN PARM                 
         CLC   PARMUSER,=C'0000'   WAS THE INPUT PARM 0'S ?                     
         BNE   *+10                NO: CONTINUE                                 
         MVC   TESTUSER,INUSER     YES: PICK UP THE TEST USER CODE              
*                                                                               
         LM    R5,R6,R56SV         RESTORE POINTER AND LENGTH                   
* R5 IS NOW POSITIONED AFTER THE COMMA TRAILING THE MEMBER NAME ON              
* THE ++UPDATE CARD.                                                            
         MVC   0(3,R5),BASELVL     USE BASE LEVEL FOR 1ST ++UPDATE              
         PUT   PVIN,CARD1          ++UPDATE MEMBERNM,BASELVL                    
         CLI   UPDCFLAG,C'Y'       ONLY PUT TO UPDCARD IF ALLOCATED             
         BNE   MAIN02                                                           
         PUT   UPDCARD,CARD1       ++UPDATE MEMBERNM,BASELVL                    
MAIN02   DS    0H                                                               
         PUT   PVIN,CARD2          ++C 0                                        
         PUT   PVIN,CARD3          **********                                   
         MVC   0(3,R5),LVLPLUS1                                                 
         PUT   PVIN,CARD1          ++UPDATE MEMBERNM,LVLPLUS1                   
         PUT   PVIN,CARD5          ++D 1                                        
         LA    R5,C6MEM            LOOK AT THE ++LEVEL CARD                     
         LA    R5,0(R6,R5)         R6 HAS MEMBER NAME LENGTH                    
         MVI   0(R5),C','          PLACE COMMA                                  
         MVC   1(3,R5),LVLPLUS2                                                 
         MVI   4(R5),C','                                                       
         MVC   5(3,R5),LVLPLUS1                                                 
         PUT   PVIN,CARD6          ++LEVEL MEMBERNM,LVL+2,LVL+1                 
         CLC   PARMUSER,=C'0000'   ARE WE DOING FINAL COMPILE?                  
         BE    MAIN03              YES: CONTINUE                                
*                                                                               
* WE ARE DOING RETRIEVE.  WE NEED TO DECREMENT THE LEVEL ONCE MORE              
* GET BACK TO THE LEVEL WE RETRIEVED.                                           
*                                                                               
         LA    R5,C6MEM            LOOK AT THE ++LEVEL CARD                     
         LA    R5,0(R6,R5)         R6 HAS MEMBER NAME LENGTH                    
         MVI   0(R5),C','          PLACE COMMA                                  
         MVC   1(3,R5),LVLPLUS1                                                 
         MVI   4(R5),C','                                                       
         MVC   5(3,R5),BASELVL                                                  
         PUT   PVIN,CARD6          ++LEVEL MEMBERNM,LVL+1,BASELVL               
*                                                                               
MAIN03   DS    0H                                                               
         LA    R5,C7MEM            LOOK AT ++USER CARD                          
         LA    R5,0(R6,R5)         R6 HAS MEMBER NAME LENGTH                    
         MVI   0(R5),C','          PLACE COMMA                                  
         LA    R5,1(R5)            ADVANCE OUTPUT POINTER                       
         MVC   0(4,R5),BASEUSER                                                 
         MVI   4(R5),C','                                                       
         MVC   5(4,R5),TESTUSER                                                 
         PUT   USER,CARD7                                                       
*                                                                               
CLOSE    DS    0H                                                               
         CLOSE PVIN                                                             
         CLOSE USER                                                             
         CLI   UPDCFLAG,C'Y'       ONLY CLOSE UPDCARD IF ALLOCATED              
         BNE   CLOSEPAN                                                         
         CLOSE UPDCARD                                                          
*                                                                               
CLOSEPAN GOTO1 =V(PANIC),DMCB,=C'CLOSE',=C'PAN'                                 
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
*   WORK AREAS                                                                  
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
R56SV    DS    2F                  TEMP SAVE FOR R5 AND R6                      
*                                                                               
PARMUSER DS    CL4                 USER CODE FROM PARM FIELD                    
BASEUSER DS    CL4                 USER CODE FROM BASE MEMBER                   
TESTUSER DS    CL4                 USER CODE FOR TEST MEMBER                    
*                                                                               
TRTBLANK DS    0D                                                               
         DC    XL256'00'                                                        
         ORG   TRTBLANK+X'40'                                                   
         DC    X'FF'                                                            
         ORG                                                                    
*                                                                               
BASELVL  DC    CL3'0'                                                           
LVLPLUS1 DC    CL3'0'                                                           
LVLPLUS2 DC    CL3'0'                                                           
PLVL1    DC    PL2'0'                                                           
PLVL2    DC    PL2'0'                                                           
*                                                                               
UPDCFLAG DC    C'N'                'Y': UPDCARD DD IS ALLOCATED                 
*                                                                               
CARD1    DC    CL80' '                                                          
         ORG   CARD1                                                            
         DC    CL9'++UPDATE '                                                   
C1MEM    DC    CL11' '                                                          
         ORG                                                                    
CARD2    DC    CL80'++C 0'                                                      
CARD3    DC    CL80'****************************'                               
CARD5    DC    CL80'++D 1'                                                      
CARD6    DC    CL80' '                                                          
         ORG   CARD6                                                            
         DC    CL9'++LEVEL '                                                    
C6MEM    DC    CL11' '                                                          
         ORG                                                                    
*                                                                               
CARD7    DC    CL80' '                                                          
         ORG   CARD7                                                            
         DC    CL8'++USER  '                                                    
C7MEM    DC    CL11' '                                                          
         ORG                                                                    
*                                                                               
BOOKNAME DS    CL10                                                             
DIRECT2  DS    CL80                                                             
         EJECT                                                                  
CARD     DS    CL80                                                             
         ORG   CARD                                                             
INMEM    DS    CL10                                                             
         DS    CL3                                                              
INUSER   DS    CL4                                                              
         ORG                                                                    
         SPACE 2                                                                
PVIN     DCB   DDNAME=PVIN,DSORG=PS,RECFM=FB,LRECL=80,MACRF=PM                  
USER     DCB   DDNAME=USER,DSORG=PS,RECFM=FB,LRECL=80,MACRF=PM                  
UPDCARD  DCB   DDNAME=UPDCARD,DSORG=PS,RECFM=FB,LRECL=80,MACRF=PM               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDPAN0UPD                                                      
         EJECT                                                                  
         IEFZB4D2                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDPAPTCRD 05/07/13'                                      
         END                                                                    
