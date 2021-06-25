*          DATA SET DEITNCI    AT LEVEL 003 AS OF 01/22/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEITNCIA                                                                 
                                                                                
*---------------------------------------------------------------------*         
* FAKE A DEMO CONVERSION TO GENERATE NTI CODES FOR A LIST OF                    
* NIELSEN PROGRAM NUMBERS FOR ITN.                                              
*                                                                               
* NTI CODES ARE USUALLY ASSIGNED AT CONVERSION TIME.  BUT CLIENT                
* NEEDS TO KNOW THE NTI CODES BEFORE THE NIELSEN DATA COMES IN.  SO WE          
* RUN THIS JOB TO ASSIGN NTI CODES TO THE NIELSEN PROG NUMBERS THEY             
* PROVIDE, AND THEN PRINT A REPORT THAT INCLUDES THE NEW CODES.                 
*                                                                               
* INPUT IS A CSV FILE THAT INCLUDES NIELSEN PROGRAM NUMBERS.                    
*                                                                               
* IPHASE: DEITNCI                                                               
* OPHASE: DEITNCO                                                               
*---------------------------------------------------------------------*         
                                                                                
         TITLE 'ITN CODES INPUT PHASE'                                          
                                                                                
DEITNCI  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DEITNCI,R9                                                     
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE          
                                                                                
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
                                                                                
         B     *+4(R1)             ROUTINE HOOK                                 
         B     READ                PROCESS INPUT TAPE                           
         B     CNVWR               SORT RECORD HOOK                             
         B     ENDOFFIL            E-O-F ON INPUT                               
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
READ     CLI   INTAPESW,1          TEST FOR OPENING INPUT TAPE                  
         BE    READ20                                                           
         OPEN  (FILIN,(INPUT))                                                  
         OPEN  (FILOUT,(OUTPUT))                                                
                                                                                
*BUILD MAP OF NTI CODES FOR SYNDICATION                                         
         GOTO1 VNTIPRG,DMCB,=C'BLDM',(C'S',VBITMAP1),0                          
                                                                                
         MVI   RETCODE,RETOK       INITIALIZE RETURN CODE TO OK                 
         XC    ROWNUM,ROWNUM       RECORD COUNTER                               
                                                                                
READ20   LA    RE,INREC                                                         
         LA    RF,1204                                                          
         XCEF                                                                   
         GET   FILIN,INREC                                                      
                                                                                
         SR    R0,R0               UPDATE RECORD NUMBER                         
         ICM   R0,3,ROWNUM                                                      
         AHI   R0,1                                                             
         STCM  R0,3,ROWNUM                                                      
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,INREC                                                       
         LA    R5,INREC                                                         
         LA    R6,0(R1,R5)         R6-> END OF INPUT RECORD                     
         LA    R5,INREC+4          R5-> START OF INPUT RECORD                   
                                                                                
         LA    R7,OUTREC+4         R7-> START OF OUTPUT RECORD                  
                                                                                
         XC    APREVTK,APREVTK     NO TOKEN HAS BEEN READ YET                   
                                                                                
         BAS   RE,NXTTOKEN         READ FIRST TOKEN                             
         CLC   =C'ITN',TOKEN       IF NOT 'ITN', IGNORE RECORD                  
         BE    READ40                                                           
         LA    RE,OUTREC           MOVE RECORD TO OUTPUT                        
         LA    R0,INREC                                                         
         SR    RF,RF                                                            
         ICM   RF,3,INREC                                                       
         LR    R1,R1                                                            
         MVCL  RE,R0                                                            
         B     READ100             CONTINUE TO NEXT REC                         
                                                                                
READ40   MVC   0(4,R7),=C'ITN,'    PUT FIRST TOKEN TO OUTPUT RECORD             
         LA    R7,4(R7)            UPDATE OUTREC LENGTH                         
                                                                                
         BAS   RE,NXTTOKEN         SECOND TOKEN IS THE NIELSEN PROG NUM         
         BAS   RE,VALNLNUM         VALIDATE IT SAVE IN NLSNUM                   
         BNE   BADNLNUM                                                         
         MVC   0(10,R7),NLSNUM     PUT TO OUTPUT RECORD                         
         MVI   10(R7),C','                                                      
         LA    R7,11(R7)           UPDATE OUTREC LENGTH                         
                                                                                
         BAS   RE,NXTTOKEN         NEXT COLUMN SHOULD BE BLANK                  
         BE    MISSCOL             TO LEAVE ROOM FOR THE NTI CODE               
* GET NTI CODE FOR THE NIELSEN NUMBER                                           
         GOTO1 VNTIPRG,DMCB,=C'LKUP',(0,VBITMAP1),NLSNUM                        
         MVC   WORK(2),0(R1)                                                    
         EDIT  (B2,WORK),(5,(R7)),FILL=0                                        
         LA    R7,5(R7)                                                         
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
                                                                                
READ50   BAS   RE,NXTTOKEN         COPY ANY OTHER TOKENS/COLUMNS                
         BNE   READ60              NO MORE TOKENS                               
         ZIC   R1,TOKLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),TOKEN                                                    
         ZIC   R1,TOKLEN                                                        
         LA    R7,0(R1,R7)                                                      
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
         B     READ50                                                           
READ60   LA    RE,OUTREC                                                        
         SR    R7,RE                                                            
         XC    0(4,RE),0(RE)                                                    
         STCM  R7,3,0(RE)                                                       
                                                                                
READ100  PUT   FILOUT,OUTREC       PUT RECORD TO OUTPUT FILE                    
         B     EXIT                EXIT TO DEMCNV                               
                                                                                
CNVWR    B     EXIT                                                             
                                                                                
                                                                                
ENDOFFIL MVI   INTAPESW,X'02'      EOF INDICATOR TO DEMCNV                      
CLOSEFIL CLOSE (FILIN)                                                          
         CLOSE (FILOUT)                                                         
                                                                                
EXIT     CLI   RETCODE,RETOK                                                    
         BE    *+8                                                              
         MVI   INTAPESW,X'80'      ERROR INDICATOR TO DEMCNV                    
         XMOD1 1                                                                
                                                                                
* ERRORS                                                                        
BADNLNUM MVC   P+21(22),=C'INVALID NIELSEN CODE- '                              
         MVC   P+43(L'TOKEN),TOKEN                                              
         B     PRTERR                                                           
                                                                                
MISSCOL  MVC   P+21(33),=C'MISSING BLANK COLUMN FOR DDS CODE'                   
         B     PRTERR                                                           
                                                                                
PRTERR   MVC   P(17),=C'!! ERROR !! CELL '                                      
         LA    RE,NUMTOLET         NUMBER TO LETTER CONVERSION                  
         ZIC   RF,COLUMN                                                        
         BCTR  RF,0                COLUMN NUMBERS START AT 1                    
         AR    RE,RF                                                            
         MVC   P+17(1),0(RE)       COLUMN LETTER                                
         EDIT  ROWNUM,(2,P+18),ALIGN=LEFT    ROW NUMBER                         
         MVI   P+20,C':'                                                        
PRTERR5  GOTO1 VPRINTER            PRINT INVALID ERROR                          
         MVI   RETCODE,RETERR      SET RETURN CODE                              
         B     CLOSEFIL                                                         
         EJECT                                                                  
                                                                                
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
* NXTTOKEN - ROUTINE TO RETURN NEXT COMMA SEPARATED TOKEN                       
* ON INPUT - APREVTK: A(1 BYTE PAST THE END OF PREVIOUS TOKEN)                  
*            R5: A(BEGINNING OF INPUT RECORD)                                   
*            R6: A(1 BYTE PAST END OF INPUT RECORD)                             
* ON OUTPUT- TOKEN: NEXT TOKEN                                                  
*            TOKLEN: LENGTH OF TOKEN                                            
***********************************************************************         
NXTTOKEN NTR1                                                                   
                                                                                
         MVC   TOKEN,SPACES                                                     
         SR    R3,R3               TOKEN LENGTH                                 
                                                                                
         ICM   R2,15,APREVTK       PREV ADDRESS AVAILABLE?                      
         BNZ   NXTT10              YES.                                         
         LR    R2,R5               NO. START FROM BEGINNING OF RECORD           
         ST    R2,APREVTK                                                       
         MVI   COLUMN,0            RESTART COLUMN NUMBER                        
NXTT10   ZIC   R1,COLUMN           UPDATE COLUMN NUMBER                         
         LA    R1,1(R1)                                                         
         STC   R1,COLUMN                                                        
                                                                                
NXTT20   LA    RE,TOKEN                                                         
         MVI   LEADSP,YES                                                       
NXTT25   CR    R2,R6               END OF TOKEN IS EOR                          
         BE    NXTT60                                                           
         CLI   0(R2),C','          OR COMMA                                     
         BE    NXTT60                                                           
                                                                                
         CLI   0(R2),C' '          DON'T COPY LEADING SPACES                    
         BE    NXTT30                                                           
         MVI   LEADSP,NO           HIT NON-SPACE,NO MORE LEADING SPACES         
         B     NXTT40                                                           
NXTT30   CLI   LEADSP,YES                                                       
         BE    NXTT50                                                           
                                                                                
NXTT40   MVC   0(1,RE),0(R2)       MOVE ONE CHAR AT A TIME INTO TOKEN           
         LA    R3,1(R3)            UPDATE TOKEN LENGTH                          
         CLI   0(RE),C'a'          CONVERT LOWER CASE CHARS                     
         BL    NXTT45              TO UPPER CASE                                
         CLI   0(RE),C'z'                                                       
         BH    NXTT45                                                           
         ZIC   R0,0(RE)                                                         
         AHI   R0,X'40'                                                         
         STC   R0,0(RE)                                                         
NXTT45   LA    RE,1(RE)            NEXT POSITION IN TOKEN                       
NXTT50   LA    R2,1(R2)            NEXT CHAR IN INPUT RECORD                    
         B     NXTT25                                                           
                                                                                
NXTT60   CLI   0(R2),C','          SKIP ENDING COMMA                            
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
         ST    R2,APREVTK          SAVE AS END OF PREVIOUS TOKEN                
         CLC   TOKEN,SPACES                                                     
         BE    NXTTNO              EXPECTED TOKEN NOT FOUND                     
         STC   R3,TOKLEN           RETURN TOKEN LENGTH                          
         B     NXTTYES                                                          
                                                                                
NXTTNO   CHI   RB,0                                                             
         B     NXTTX                                                            
NXTTYES  CR    RB,RB                                                            
         B     NXTTX                                                            
                                                                                
NXTTX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALNLNUM - ROUTINE TO VALIDATE THE NIELSEN PROGRAM NUMBER                     
*            NIELSEN NUMBER SHOULD BE MAX 10-DIGIT NUMERIC                      
* ON INPUT - TOKEN HAS NIELSEN NUMBER                                           
* ON OUTPUT- CC SET TO EQ IF VALID, NEQ IF INVALID                              
*            IF VALID, RETURN NIELSEN NUMBER IN NLSNUM, ZERO PADDED             
***********************************************************************         
VALNLNUM NTR1                                                                   
                                                                                
         LA    RE,TOKEN                                                         
         SR    R1,R1               DIGIT COUNTER                                
                                                                                
VNLN10   CLI   0(RE),C' '          NO MORE DIGITS                               
         BE    VNLN20                                                           
         LA    R1,1(R1)            BUMP DIGIT COUNTER                           
         TM    0(RE),X'F0'         IS THIS A DIGIT?                             
         BNO   VNLNNO              NO. EXIT AND SEND ERROR MESSAGE              
         LA    RE,1(RE)            POINT TO NEXT DIGIT                          
         B     VNLN10                                                           
VNLN20   LTR   R1,R1                                                            
         BZ    VNLNNO              NIELSEN NUMBER IS REQUIRED                   
         CHI   R1,10                                                            
         BH    VNLNNO                                                           
         MVC   NLSNUM,=10C'0'      LEFT-PAD WITH ZEROS                          
         LHI   RE,10                                                            
         SR    RE,R1                                                            
         LA    RF,NLSNUM(RE)                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),TOKEN       PUT NIELSEN NUMBER IN NLSNUM                 
                                                                                
VNLNYES  CR    RB,RB               EXIT WITH CC EQ                              
         B     VNLNX                                                            
VNLNNO   CHI   RB,0                EXIT WITH CC NEQ                             
         B     VNLNX                                                            
VNLNX    B     XIT                                                              
         EJECT                                                                  
                                                                                
                                                                                
FILIN    DCB   DDNAME=FILIN,                                           X        
               DSORG=PS,                                               X        
               EODAD=ENDOFFIL,                                         X        
               RECFM=VB,                                               X        
               LRECL=01200,                                            X        
               MACRF=GM                                                         
                                                                                
FILOUT   DCB   DDNAME=FILOUT,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=02004,                                            X        
               BLKSIZE=04008,                                          X        
               MACRF=PM                                                         
                                                                                
                                                                                
* TABLE TO CONVERT NUMBERS TO LETTERS                                           
NUMTOLET DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                                    
         LTORG                                                                  
                                                                                
         DC    CL8'**WORK**'                                                    
                                                                                
TOKEN    DS    CL(MAXTKLN)         RETURN NEXT TOKEN HERE                       
MAXTKLN  EQU   100                 MAXIMUM TOKEN LENGTH                         
TOKLEN   DS    HL1                 TOKEN LENGTH                                 
APREVTK  DS    A                   A(1 BYTE PAST END OF PREV TOKEN)             
COLUMN   DS    HL1                 COLUMN NUMBER                                
ROWNUM   DS    H                   RECORD NUMBER                                
NLSNUM   DS    CL10                                                             
                                                                                
LEADSP   DS    X                   LEADING SPACE INDICATOR                      
YES      EQU   0                                                                
NO       EQU   1                                                                
                                                                                
RETOK    EQU   0                   OK                                           
RETWRN   EQU   4                   WARNING                                      
RETERR   EQU   8                   ERROR                                        
                                                                                
         DC    CL8'*IREC*'                                                      
INREC    DS    CL1204                                                           
                                                                                
         DC    CL8'*OUTREC*'                                                    
OUTREC   DS    XL1204                                                           
         EJECT                                                                  
* DEDEMCNVD                                                                     
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DEITNCI   01/22/15'                                      
         END                                                                    
