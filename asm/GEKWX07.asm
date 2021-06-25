*          DATA SET GEKWX07    AT LEVEL 002 AS OF 05/24/96                      
*PHASE TF2007A,+0                                                               
         TITLE '$KWX MK3 - MESSAGE REPLACE ACTION'                              
         PRINT NOGEN                                                            
KWX07    CSECT                                                                  
         NMOD1 0,**KWX07*,R9                                                    
         L     RC,0(R1)                                                         
         USING KWX07+4096,R9       R9 = 2ND BASE                                
         USING TWAD,RA             RA = TWA                                     
         USING GWS,RC              RC = GWS                                     
*                                                                               
T001     ST    RB,ABASE2                                                        
         ST    R9,A2NDBAS2                                                      
         ST    RD,AREGSAV2                                                      
         B     T010                                                             
         EJECT                                                                  
*              FURTHER PARAMETER CHECKS ON PRE-PROCESSED PARAMETERS             
*                                                                               
T010     TM    MSGSTAT,ENDED       CANT REPLACE IF ENDED                        
         BZ    T015                                                             
         MVI   FERN,BKENDED                                                     
         B     ERROR                                                            
*                                                                               
T015     TM    FRMSTAT,MESSAGE     CHUNKS MUST BE DISPLAYED                     
         BZ    *+14                                                             
         OC    DISPLOM(4),DISPLOM                                               
         BNZ   T020                                                             
         MVI   FERN,NODISP                                                      
         B     ERROR                                                            
*                                                                               
T020     CLI   FXREF,0             IF REF= GIVEN, MUST BE ON DISPLAY            
         BE    T025                                                             
         CLC   REFLO,DISPLOM                                                    
         BL    *+14                                                             
         CLC   REFHI,DISPHIM                                                    
         BNH   T025                                                             
         MVC   FNDX,FXREF                                                       
         MVI   FERN,NODISP                                                      
         B     ERROR                                                            
*                                                                               
T025     CLI   FXOLD,0             OLD= MISSING                                 
         BNE   T030                                                             
         MVI   FNDX,3                                                           
T027     MVI   FERN,MISSING                                                     
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
*                                                                               
T030     CLI   FXNEW,0             NEW= MISSING                                 
         BNE   T050                                                             
         MVI   FNDX,4                                                           
         B     T027                                                             
         EJECT                                                                  
*              FIND START/END FOR SUBSTITUTION IN TWA                           
*                                                                               
T050     CLI   FXREF,0             IF NO REFS GIVEN, APPLY TO WHOLE             
         BNE   *+10                DISPLAY                                      
         MVC   REFLO(4),DISPLOM                                                 
         TM    TRMTYPE,T3270       IF NOT 3270 RESTORE TWA                      
         BO    T052                                                             
         LH    R0,DISPLOM                                                       
         LH    R1,DISPHIM                                                       
         LA    R1,1(R1)                                                         
         SR    R1,R0                                                            
         STM   R0,R1,PARAS                                                      
         GOTO1 ADISSCRN,PARAS                                                   
*                                                                               
T052     LH    R3,DISPLOM          GET START ADDR IN TWA INTO FULL              
         LA    R4,1                                                             
         LH    R5,REFLO                                                         
         GOTO1 ,PARAS,(SAVMODE,0),ADDBUFF,IO,KWXDATAH,(R5)                      
         CR    R3,R5                                                            
         BE    T059                                                             
         BCTR  R5,0                                                             
*                                                                               
T055     STH   R3,2(R1)            LOOP FOR A CHUNK                             
         GOTO1 AGETCHNK                                                         
         BZ    ERROR                                                            
         GOTO1 AVALMESS            (USED SIMPLY TO PASS BACK TWA PNTER)         
         BZ    ERROR                                                            
         BXLE  R3,R4,T055                                                       
T059     MVC   FULL,12(R1)         P4 CONTAINS START ADDR IN TWA = FULL         
*                                                                               
T060     CLC   REFHI,DISPHIM       GET END ADDR INTO R5                         
         BNE   T070                IF END IS END OF DISPLAY, FIND               
         SR    R0,R0               SCREEN TERMINATOR                            
         L     R5,FULL                                                          
         IC    R0,0(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BNE   *-10                                                             
         B     T080                                                             
*                                                                               
T070     LH    R3,REFLO            OTHERWISE USE SAME TECHNIQUE AS              
         LH    R5,REFHI            FOR START ADDR                               
         LA    RF,1(R5)                                                         
         ST    RF,16(R1)                                                        
*                                                                               
T075     STH   R3,2(R1)            LOOP FOR A CHUNK                             
         GOTO1 AGETCHNK                                                         
         BZ    ERROR                                                            
         GOTO1 AVALMESS                                                         
         BZ    ERROR                                                            
         BXLE  R3,R4,T075                                                       
T079     L     R5,12(R1)           R5 = A(NEXT BYTE AFTER END OF REFHI)         
         B     T080                                                             
         EJECT                                                                  
*              PERFORM SUBSTITUTIONS IN TWA                                     
*                                                                               
T080     L     R3,FULL             R3/4/5 ARE BXLE REGS FOR FIELD-LEVEL         
         BCTR  R5,0                BXLE                                         
         ZIC   RE,OLDL                                                          
         ZIC   RF,NEWL                                                          
         BCTR  RE,0                RE = EXECUTE L OF OLD STRING                 
         BCTR  RF,0                RF = DITTO        NEW                        
         MVI   BYTE,0              BYTE IS HIT MARKER                           
*                                                                               
T085     MVI   SCANBLCK,C' '       LOOP FOR A FIELD                             
         MVC   SCANBLCK+1(160),SCANBLCK                                         
         IC    R4,0(R3)                                                         
         TM    1(R3),X'20'         IGNORE PROTS                                 
         BO    T105                                                             
         SH    R4,=H'9'                                                         
         EX    R4,MVTOSCAN         MOVE FLD DATA TO SCANBLCK                    
         LA    R2,SCANBLCK-1                                                    
         LA    R0,1                                                             
         LA    R1,1(R4,R2)         R2/0/1 ARE BXLE REGS FOR CHARACTER-          
         SR    R1,RE               LEVEL BXLE                                   
         B     T095                                                             
*                                                                               
T090     EX    RE,COMPOLD          LOOP FOR A CHARACTER                         
         BNE   T095                                                             
         MVI   BYTE,C'H'           A HIT                                        
         OI    6(R3),X'80'                                                      
         LA    R6,1(RE,R2)                                                      
         MVC   WORK(80),0(R6)      MOVE REMAINDER OF KIELD OUT                  
         EX    RE,MVSPACE                                                       
         EX    RF,MVNEW            SWAP NEW FOR OLD                             
         LA    R2,1(RF,R2)                                                      
         MVC   0(80,R2),WORK       MOVE REMAINDER BACK                          
         BCTR  R2,0                                                             
T095     BXLE  R2,R0,T090                                                       
*                                                                               
T100     EX    R4,MVTOTWA          BUMP                                         
         LA    R4,9(R4)                                                         
T105     BXLE  R3,R4,T085                                                       
         CLI   BYTE,C'H'           AT END CHECK FOR HITS                        
         BE    T120                                                             
         MVI   FERN,NOHITS                                                      
         B     ERROR                                                            
*                                                                               
MVTOSCAN MVC   SCANBLCK(0),8(R3)                                                
MVTOTWA  MVC   8(0,R3),SCANBLCK                                                 
COMPOLD  CLC   0(0,R2),OLD                                                      
MVSPACE  MVC   0(0,R2),SPACES                                                   
MVNEW    MVC   0(0,R2),NEW                                                      
         EJECT                                                                  
*              VALIDATE AMENDED SCREEN AND UPDATE BOOK                          
*                                                                               
T120     L     R3,FULL             RESET INPUT LENGTHS IN FLD HDRS              
         SR    RF,RF               (USED BY FLD VALIDATION ROUTINES)            
T122     ICM   RF,1,0(R3)                                                       
         BZ    T124                                                             
         SH    RF,=H'8'                                                         
         LA    RE,7(R3)                                                         
         LA    R1,0(RE,RF)                                                      
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-12                                                          
         STC   RF,5(R3)                                                         
         IC    RF,0(R3)                                                         
         AR    R3,RF                                                            
         B     T122                                                             
*                                                                               
T124     LH    R3,REFLO            SET BXLE AND PARMS                           
         LA    R4,1                                                             
         LH    R5,REFHI                                                         
         L     R0,FULL                                                          
         GOTO1 ,PARAS,(SAVMODE,0),ADDBUFF,IO,(R0),(R3)                          
*                                                                               
T125     STH   R3,2(R1)            LOOP TO READ/VALIDATE/WRITE A CHUNK          
         GOTO1 AGETCHNK                                                         
         BZ    ERROR                                                            
         GOTO1 AVALMESS                                                         
         LA    R0,ADDBUFF          RESET BUMPED OUT PNTER                       
         ST    R0,4(R1)                                                         
         BZ    ERROR                                                            
         BM    T129                NO CHANGE                                    
         GOTO1 APUTCHNK                                                         
         BZ    ERROR                                                            
T129     BXLE  R3,R4,T125                                                       
*                                                                               
T130     MVC   PARAS(4),REFLO      COMPLETION MESSAGE                           
         MVC   PARAS+10(2),MSGRECHI                                             
         GOTO1 AEDITREF,PARAS,,KWXHEAD                                          
         L     RF,4(R1)                                                         
         MVC   0(27,RF),=C'CHANGED - ENTER NEXT ACTION'                         
         B     OKXIT                                                            
         EJECT                                                                  
*              EXITS BACK TO ROOT                                               
*                                                                               
ERROR    SR    R0,R0               CC = EQU FOR ERROR                           
         B     EXIT                                                             
*                                                                               
MOREXIT  LNR   RB,RB               CC = NEG FOR MORE INPUT                      
*                                                                               
OKXIT    LTR   RB,RB               CC = POS OK COMPLETED                        
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
* NESTED INCLUDES                                                               
* GEKWXDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE GEKWXDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002GEKWX07   05/24/96'                                      
         END                                                                    
