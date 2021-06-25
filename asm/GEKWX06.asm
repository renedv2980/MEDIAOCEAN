*          DATA SET GEKWX06    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TF2006A,+0                                                               
         TITLE '$KWX MK3 - MESSAGE INSERT ACTION'                               
         PRINT NOGEN                                                            
KWX06    CSECT                                                                  
         NMOD1 0,**KWX06*,R9                                                    
         L     RC,0(R1)                                                         
         USING KWX06+4096,R9       R9 = 2ND BASE                                
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
T010     TM    MSGSTAT,ENDED       CANT INSERT IF ENDED                         
         BZ    T015                                                             
         MVI   FERN,BKENDED                                                     
         B     ERROR                                                            
*                                                                               
T015     CLI   FXREF,0             REF NUM RANGE                                
         BNE   T016                                                             
         MVI   FERN,MISSING        MUST BE PRESENT                              
         MVI   FNDX,2                                                           
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
T016     CLC   REFLO,MSGRECHI      MUST BE BEFORE END OF BOOK                   
         BNH   T018                                                             
         MVI   FERN,BYONDEND                                                    
         MVC   FNDX,FXREF                                                       
         B     ERROR                                                            
T018     LH    R1,REFLO            CHUNK BEFORE 1ST INSERT (OR CHUNK 1)         
         CH    R1,=H'1'            MUST BE ON DISPLAY                           
         BE    *+6                                                              
         BCTR  R1,0                                                             
         CH    R1,DISPLOM                                                       
         BL    *+12                                                             
         CH    R1,DISPHIM                                                       
         BNH   T020                                                             
         MVI   FERN,INSNTDSP                                                    
         B     ERROR                                                            
*                                                                               
T020     CLI   FXFORMID,0          FORMID MUST BE 0 (DEFAULT FORMAT) IF         
         BE    T050                GIVEN                                        
         OC    FORMID,FORMID                                                    
         BZ    T050                                                             
         MVC   FNDX,FXFORMID                                                    
         MVI   FERN,INVALID                                                     
         MVI   SYNTAX,C'Y'                                                      
         B     ERROR                                                            
         EJECT                                                                  
*              FIND POINT OF INSERT IN TWA                                      
*                                                                               
T050     LH    RF,DISPHIM          SAVE IN NUM TOT CHUNKS DISPLAYED +           
         SH    RF,DISPLOM                      TOT CHUNKS INSERTED              
         AH    RF,REFHI                                                         
         SH    RF,REFLO                                                         
         LA    RF,2(RF)                                                         
         STH   RF,NUM                                                           
         LH    R3,DISPLOM          SET UP PARAS FOR GETCHNK/VALMESS             
         LA    R4,1                LOOP TO FIND START OF CURRENT REFLO          
         LH    R5,REFLO            IN TWA                                       
         GOTO1 ,PARAS,(SAVMODE,0),ADDBUFF,IO,KWXDATAH,(R5)                      
         CR    R3,R5                                                            
         BE    T059                CHUNK 1 INSERT                               
         BCTR  R5,0                                                             
*                                                                               
T055     STH   R3,2(R1)            LOOP FOR A CHUNK                             
         GOTO1 AGETCHNK                                                         
         BZ    ERROR                                                            
         GOTO1 AVALMESS            (USED SIMPLY TO PASS BACK TWA PNTER)         
         BZ    ERROR                                                            
         BXLE  R3,R4,T055                                                       
T059     DS    0H                  P4 CONTAINS TWA POINTER                      
*                                                                               
*              GET INSERT RECORD INTO IOB                                       
*                                                                               
T060     MVC   WORK(L'FRMBOOK),FRMBOOK                                          
         MVC   WORK+10(1),FRMSTAT  SAVE FORMAT DETAILS IN WORK                  
         CLI   FXFORMID,0                                                       
         BE    *+10                                                             
         XC    FRMBOOK,FRMBOOK                                                  
         SR    R0,R0                                                            
         ICM   R0,3,FRMRPEAT                                                    
         GOTO1 AGETFORM,PARAS,(R0)                                              
         MVC   FRMBOOK,WORK        RESTORE VALUES                               
         MVC   FRMSTAT,WORK+10                                                  
T069     DS    0H                  IOB CONTAINS INSERT CHUNK                    
*                                                                               
*              CHECK FOR ROOM IN TWA & ON SCREEN                                
*                                                                               
T070     L     R0,ATIA             SAVE TWA IN TIA                              
         L     RE,ATWA                                                          
         LA    R1,2304                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTO1 ,PARAS,0,0,AIOB     PREPARE FOR DISMESS LOOP                     
         L     RF,12(R1)           NB P4 STILL CONTAINS TWA POINTER             
         ST    RF,FULL             SAVE IT IN FULL (FOR CURSOR IF OK)           
         LH    RF,2(RF)            AND PUT LINE NUM IN P4 B0                    
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         LA    RF,1(RF)                                                         
         STC   RF,12(R1)                                                        
         LH    R3,REFLO            BXLE REGS                                    
         LH    R5,REFHI                                                         
         L     RF,ADISMESS                                                      
*                                                                               
T075     ST    R3,4(R1)            LOOP TO DISPLAY AN INSERT CHUNK              
         BASR  RE,RF                                                            
         BZ    T078                                                             
         BXLE  R3,R4,*-10                                                       
         B     T080                                                             
*                                                                               
T078     MVI   FERN,NOROOM         NOT ENOUGH ROOM                              
         L     R0,ATWA             RESTORE TWA                                  
         L     RE,ATIA                                                          
         LA    R1,2304                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         XC    ATWADIFF,ATWADIFF                                                
         B     ERROR                                                            
         EJECT                                                                  
*              INSERT CHUNK(S) INTO BOOK                                        
*                                                                               
T080     LH    R3,REFLO            RESTORE BXLE                                 
         LA    R0,IO               MOVE RECORD + TERMINATOR TO IO               
         L     RE,AIOB                                                          
         LH    RF,0(RE)                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         LA    R2,IO                                                            
         LH    RF,0(R2)                                                         
         AR    RF,R2                                                            
         MVI   0(RF),X'FF'                                                      
         GOTO1 ,PARAS,(SAVMODE,0),(C'I',(R2))                                   
         L     RF,AADDCHNK                                                      
*                                                                               
T085     STH   R3,2(R1)            LOOP TO ADD A CHUNK                          
         BASR  RE,RF                                                            
         BZ    *+12                                                             
         BXLE  R3,R4,T085                                                       
         B     T090                                                             
         BCTR  R3,0                NO ROOM FOR ALL INSERTS IN BOOK SO           
         STH   R3,REFHI            RESET REFHI                                  
*                                                                               
T090     CLC   REFLO,REFHI         ANY INSERTS                                  
         BNH   T095                                                             
         MVI   FERN,ENDFILE        NO                                           
         B     ERROR                                                            
*                                                                               
T095     LH    R0,NUM              IF SO DISPLAY AS BEFORE + INSERTS            
         LH    R2,DISPLOM                                                       
         GOTO1 ADISSCRN,PARAS,(R2),(R0)                                         
*                                                                               
T100     MVC   PARAS(4),REFLO      COMPLETION MESSAGE & NEXT ACTION             
         MVC   PARAS+10(2),MSGRECHI                                             
         GOTO1 AEDITREF,PARAS,,KWXHEAD                                          
         L     RF,4(R1)                                                         
         MVC   0(29,RF),=C'INSERTED - NOW ENTER CONTENTS'                       
         MVC   ACURSOR,FULL                                                     
         XC    KWXACT,KWXACT       RESET ACTION TO 'CHANGE,REF=N-N'             
         MVC   KWXACT(7),=C'CHANGE,'                                            
         LA    RF,KWXHEAD                                                       
         LR    RE,RF                                                            
         CLC   0(2,RE),=C'OF'                                                   
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-14                                                             
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KWXACT+7(0),KWXHEAD                                              
         MVI   KWXACT+10,C'='                                                   
         OI    KWXACTH+6,X'80'                                                  
*                                                                               
T110     DS    0H                  AMEND HEADER REC                             
         GOTO1 AGETCHNK,PARAS,(SAVMODE,0)                                       
         BZ    ERROR                                                            
         LA    R4,IO                                                            
         USING HDRD,R4                                                          
         MVC   HDMSGHI,MSGRECHI                                                 
         GOTO1 APUTCHNK,(R1),,IO                                                
         BZ    ERROR                                                            
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
**PAN#1  DC    CL21'003GEKWX06   05/01/02'                                      
         END                                                                    
