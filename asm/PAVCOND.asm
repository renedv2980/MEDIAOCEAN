*          DATA SET PAVCOND    AT LEVEL 022 AS OF 10/07/83                      
*PHASE T00A0AB,+0,NOAUTO                                                        
PAVCOND  CSECT                                                                  
BASE     DS    3000C                                                            
         ORG   BASE                                                             
         NMOD1 20,PAVCOND,RR=R9                                                 
         ST    R9,RELO                                                          
         L     RA,=V(DCWORKC)                                                   
         AR    RA,R9                                                            
         USING DCWORKC,RA                                                       
         ST    RA,VDCWORKC                                                      
         L     RE,=V(ELCWRK)                                                    
         AR    RE,R9                                                            
         ST    RE,VELCWRK                                                       
         L     R2,0(R1)                                                         
         MVI   SRCSW,C'E'                                                       
         CLI   0(R2),C'A'                                                       
         BNE   *+8                                                              
         MVI   SRCSW,C'E'                                                       
         CLI   0(R2),C'N'                                                       
         BNE   *+8                                                              
         MVI   SRCSW,C'F'                                                       
         MVC   FLTBY,1(R2)                                                      
         L     R2,4(R1)                                                         
         USING PWREC,R2                                                         
         LA    R3,PWDATA                                                        
         L     R4,=V(PWDISP1)                                                   
         AR    R4,R9                                                            
         LA    R5,OUTELAD                                                       
         ST    R1,SAVER1                                                        
         GOTO1 =V(ELCREAT),PARAMS,(1,(R3)),(R4),(R5),RR=R9                      
         L     R1,SAVER1                                                        
         L     R4,8(R1)            MOVE ELEMENTS TO OUTPUT AREA                 
         LA    R5,OUTELAD                                                       
MVOUT    CLI   0(R5),0                                                          
         BE    EXIT                                                             
         L     R6,0(R5)                                                         
         LA    R6,0(R6)                                                         
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BL    *+10                                                             
         MVC   0(0,R4),0(R6)                                                    
         ZIC   RE,1(R6)                                                         
         AR    R4,RE                                                            
         MVI   0(R4),0                                                          
         LA    R5,4(R5)                                                         
         B     MVOUT                                                            
EXIT     XMOD1 1                                                                
         LTORG                                                                  
RELO     DC    A(*)                                                             
DCWORKC  CSECT                                                                  
VDCWORKC DS    F                                                                
VELCWRK  DS    F                                                                
SRCSW    DS    C                                                                
FLTBY    DS    CL2                                                              
SAVER1   DS    F                                                                
PARAMS   DS    6F                                                               
OUTELAD  DS    20F                                                              
         TITLE 'PAV - CONDENSE WORK RECORD INTO DEMO ELEMENTS'                  
*                                                                               
*        PARAMETERS  WORD1   BYTE 0    PRECISION CONTROL 1= DIV BY 10           
*                                 1-3  A(INPUT BUCKETS)                         
*                                                                               
*                    WORD 2   BYTE 0-3  A(CONVERSION TABLE)                     
*                                                                               
*                    WORD 3   BYTE 0-3  A(OUTPUT ELEMENT ADDR TABLE)            
*                                                                               
*              INPUT BUCKETS MUST CONTAIN FULL WORD BINARY DATA                 
*                                                                               
*              CONVERSION TABLE FORMAT -A(END OF TABLE) 1 ENTRY                 
*                                       AL2(DISPLACEMENT INTO BUCKETS)          
*                                       AL1(ELEMENT CODE)                       
*                                       AL1(FIELD NUMBER-1)                     
*                                                                               
*              ELEMENT ADDR TABLE - AL1(EL CODE)                                
*                                   AL3(ADDR OF ELEMENT)                        
*                            NOTE - ONE ENTRY FOR EACH ELEMENT CREATED          
*                                                                               
ELCREAT  CSECT                                                                  
         NMOD1 0,ELCREATE                                                       
         L     RC,VELCWRK                                                       
         USING ELCWRK,RC                                                        
*                                                                               
         L     R8,VELCWRK                                                       
         LA    R9,10                                                            
CLW1     XC    4(8,R8),4(R8)                                                    
         MVI   0(R8),0                                                          
         LA    R8,12(R8)                                                        
         BCT   R9,CLW1                                                          
         LA    R8,EL1                                                           
         LA    R9,10                                                            
CLW2     XC    0(100,R8),0(R8)                                                  
         LA    R8,100(R8)                                                       
         BCT   R9,CLW2                                                          
         LM    R2,R4,0(R1)         SET PARAMETERS                               
         LA    R2,0(R2)            CLEAR HIGH ORDER BYTE                        
GT1      LA    RF,4                GET PROPER TABLE                             
         LR    RE,R3                                                            
GT1A     CLC   SRCSW,0(RE)         SOURCE OK                                    
         BE    GT2                                                              
         LA    RE,1(RE)                                                         
         BCT   RF,GT1A                                                          
         B     GT3                                                              
*                                                                               
GT2      LA    RE,4(R3)                                                         
         CLC   2(2,RE),FLTBY                                                    
         BL    GT3                                                              
         CLC   0(2,RE),FLTBY                                                    
         BH    GT3                                                              
         B     GT4                                                              
*                                                                               
GT3      L     RE,8(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BNE   GT1                                                              
         DC    H'0'                                                             
GT4      ST    R3,4(R1)                                                         
         LA    R3,12(R3)                                                        
         XC    PREVDSP,PREVDSP                                                  
CNVFLD1  LA    R6,ELCATAB          SET TO EL ADDR TABLE START                   
FNDFEL   L     R3,4(R1)            FIND FIRST ELEMENT                           
         LA    R3,12(R3)                                                        
         SR    RE,RE                                                            
FNDFEL1  CLC   2(2,R3),=X'3100'                                                 
         BE    CNVFLD2                                                          
         LA    RE,4(RE)                                                         
         LA    R3,4(R3)                                                         
         B     FNDFEL1                                                          
CNVFLD2  STH   RE,PREVDSP                                                       
*                                                                               
CNVFLD   CLC   0(1,R6),2(R3)       PROPER ELEMENT ENTRY                         
         BE    BLDEL                YES - BUILD ELEMENT                         
         CLI   0(R6),0             OPEN SLOT                                    
         BNE   SELTAB               NO - BUMP TO NEXT ENTRY                     
         MVC   0(1,R6),2(R3)        YES - SET SLOT FOR THIS ELEMENT             
         XC    4(8,R6),4(R6)                                                    
         B     BLDEL                                                            
SELTAB   LA    R6,12(R6)                                                        
         B     CNVFLD                                                           
*                                                                               
BLDEL    LH    RE,PREVDSP          SET INPUT INDEX                              
         SR    RF,RF                                                            
         IC    RF,3(R3)            SET OUTPUT INDEX                             
         SLL   RF,2                 X4                                          
         L     R9,0(R6)            SET OUTPUT ADDRESS                           
         LA    R9,4(R9)            CLEAR EL CODE - OFFSET BY 4                  
         AR    R9,RC               SET OUTPUT BASE                              
         L     RA,0(RE,R2)         MOVE INPUT TO OUTPUT                         
         ST    RA,0(RF,R9)                                                      
         TM    4(R6),X'80'                                                      
         BO    BLDEL2                                                           
         LTR   RA,RA                                                            
         BM    BLDEL1                                                           
         C     RA,4(R6)            SAVE HIGHEST NUMBER IN ELEMENT               
         BL    *+8                                                              
BLDEL1   ST    RA,4(R6)                                                         
BLDEL2   IC    RF,3(R3)            RESTORE FIELD NUMBER                         
         LA    RF,1(RF)                                                         
         LTR   RA,RA               SAVE HIGHEST NON-ZERO FIELD NO               
         BZ    *+16                                                             
         CH    RF,10(R6)                                                        
         BL    *+8                                                              
         STH   RF,10(R6)                                                        
         CH    RF,8(R6)            SAVE NUMBER OF FIELDS                        
         BL    *+8                                                              
         STH   RF,8(R6)                                                         
         MVC   PREVDSP,0(R3)       SET NEXT FIELD DISPLACEMENT                  
         CLI   0(R3),X'FF'                                                      
         BE    DIV                                                              
         CLI   2(R3),X'31'                                                      
         BE    *+14                                                             
         OC    PREVDSP,PREVDSP     END                                          
         BZ    DIV                  YES-TRY DECIMAL REDUCTION                   
         L     R3,4(R1)            GET NEXT FIELD                               
         LA    R3,12(R3)                                                        
         AH    R3,PREVDSP                                                       
         CLI   0(R3),X'FF'                                                      
         BE    DIV                                                              
         B     CNVFLD                                                           
*                                                                               
*                   DIVIDE ELEMENT FIELDS BY 10. IF A REMAINDER IS              
*                   PRESENT TURN ON DECIMAL PRECISION INDICATOR                 
*                                                                               
DIV      XC    ELDPRES,ELDPRES     CLEAR PRECISION INDICATORS                   
         CLI   0(R1),0             ANY IMPLIED DECIMALS                         
         BE    CNDNS                NO - CONDENSE ELEMENTS                      
         LA    R6,ELCATAB                                                       
         LA    R5,ELDPRES                                                       
DIV1     XC    ELDWORK(100),ELDWORK   CLEAR WORK AREA                           
         CLI   0(R6),0             END OF ELEMENTS                              
         BE    CNDNS                YES - CONDENSE ELEMENTS                     
         OC    4(4,R6),4(R6)       ANY DATA                                     
         BNZ   DIV2                 YES - DIVIDE BY 10                          
         LA    R5,1(R5)                                                         
         LA    R6,12(R6)                                                        
         B     DIV1                                                             
DIV2     LH    R7,10(R6)           SET BCT COUNT                                
         L     R8,0(R6)            SET INPUT ADDRESS                            
         LA    R8,0(R8)                                                         
         AR    R8,RC               ADD DISPLACEMANT                             
         LA    R9,ELDWORK          SET OUTPUT ADDRESS                           
         LA    RA,4                SET DISPLACEMENT                             
DIV3     SR    RE,RE                                                            
         L     RE,0(RA,R8)         DIVIDE BY 10                                 
         SRDA  RE,32                                                            
         D     RE,=F'10'                                                        
         LTR   RE,RE               ANY REMAINDER                                
         BZ    DIV4                 NO - NEXT FIELD                             
         MVI   0(R5),X'40'          YES - SET IMPLIED DECIMAL                   
         LA    R5,1(R5)                                                         
         LA    R6,12(R6)                                                        
         B     DIV1                                                             
DIV4     ST    RF,0(RA,R9)         SAVE RESULTS IN WORK AREA                    
         LA    RA,4(RA)            NEXT FIELD                                   
         BCT   R7,DIV3                                                          
         L     RE,4(R6)            REDUCE HIGHEST NUMBER                        
         SRDA  RE,32                                                            
         D     RE,=F'10'                                                        
         ST    RF,4(R6)                                                         
         LA    R5,1(R5)                                                         
         LA    R6,12(R6)                                                        
         MVC   4(96,R8),ELDWORK+4  REPLACE X10 ELEMENT                          
         B     DIV1                                                             
*                                                                               
*                   ELEMENTS HAVE BEEN BUILT NOW CONDENSE TO                    
*                    SMALLEST NUMBER OF BYTES REQUIRED                          
*                   4+N(4CHAR FIELDS)=     3+N(X CHAR FIELDS)                   
*                                                                               
CNDNS    LA    R6,ELCATAB          RESET ELEMENT ADDRESS TABLE                  
CNDNS1   CLI   0(R6),0             END OF ELEMENTS                              
         BE    PASSOUT              YES - PASS OUTPUT TO CALLER                 
         OC    4(4,R6),4(R6)       ANY DATA                                     
         BNZ   CNDNS3               YES - CONDENSE IT                           
         MVI   0(R6),0              NO - CLEAR ELEMENT ENTRY                    
         LA    R6,12(R6)                                                        
         B     CNDNS1                                                           
*                                                                               
CNDNS3   L     RF,4(R6)            COUNT NUMBER OF BYTES REQUIRED               
         LA    RE,1                                                             
CNDNS4   SRL   RF,8                                                             
         LTR   RF,RF                                                            
         BZ    CNDNS5                                                           
         LA    RE,1(RE)                                                         
         B     CNDNS4                                                           
*                                                                               
CNDNS5   LR    R9,RE               CALCULATE ELEMENT LENGTH                     
         MH    R9,10(R6)                                                        
         AH    R9,=H'3'                                                         
         L     R7,0(R6)            CREATE ELEMENT HEADER                        
         LA    R7,0(R7,RC)         SET REAL ADDRESS                             
         MVC   0(1,R7),0(R6)       EL CODE                                      
         STC   R9,1(R7)            EL CODE                                      
         STC   RE,2(R7)            FIELD LENGTH                                 
         LR    R9,RE                                                            
         BCTR  R9,R0               EXECUTE LENGTH                               
         LA    R8,4(R7)            SET MOVE FROM ADDRESS                        
         LA    R7,3(R7)            SET MOVE TO ADDRESS                          
         LH    RF,=H'4'                                                         
         SR    RF,RE               GET INITIAL DISPLACEMENT                     
         LH    RA,10(R6)           SET NUMBER OF FIELDS                         
CNDNS6   AR    R8,RF                                                            
         EX    R9,MOVE                                                          
         AR    R8,RE                                                            
         AR    R7,RE                                                            
         BCT   RA,CNDNS6                                                        
         LA    R6,12(R6)                                                        
         B     CNDNS1                                                           
MOVE     MVC   0(1,R7),0(R8)                                                    
*                                                                               
PASSOUT  LA    RA,10               PASS OUTPUT PARAMETERS TO CALLER             
         LA    R6,ELCATAB                                                       
         LA    R5,ELDPRES                                                       
PO1      CLI   0(R6),0             ANY DATA FOR ELEMENT                         
         BE    PO2                  NO-GET NEXT ELEMENT                         
         L     RE,0(R6)            A(ELEMENT)                                   
         LA    RE,0(RC,RE)                                                      
         OC    2(1,RE),0(R5)       DECIMAL PRECISION INDICATOR                  
         CLI   2(RE),1             ALL FIELDS 1 BYTE                            
         BNE   PO1A                 NO-CANNOT CONDENSE FURTHER                  
         TM    3(RE),X'80'         FIRST FIELD BELOW 128                        
         BNZ   PO1A                 NO-CANNOT CONDENSE FURTHER                  
         LA    R7,ELDWORK           YES-CONDENSE TO 1 BYTE FORMAT               
         LR    R8,RE                                                            
         SR    R9,R9                                                            
         IC    R9,1(RE)                                                         
         EX    R9,MOVE                                                          
         BCTR  R9,R0                                                            
         STC   R9,1(RE)            DECREMENT LENGTH BY 1                        
         NI    0(RE),X'FE'         SET NEW ELEMENT CODE                         
         LR    R8,R7                                                            
         AH    R8,=H'3'            SET TO DATA IN ELEMENT                       
         LA    R7,2(RE)                                                         
         EX    R9,MOVE             OVERLAY OLD DATA                             
PO1A     ST    RE,0(R4)            OUTPUT LIST A(ELEMENT)                       
         MVC   0(1,R4),0(RE)       OUTPUT LIST ELEMENT CODE                     
         LA    R4,4(R4)                                                         
PO2      LA    R6,12(R6)                                                        
         LA    R5,1(R5)                                                         
         BCT   RA,PO1                                                           
         XMOD1 1                                                                
         LTORG                                                                  
PREVDSP  DC    H'0'                PREVIOUS TABLE DISP./CURR REC DISP           
ELDPRES  DS    CL10                DECIMAL PRECISION INDICATORS                 
ELDWORK  DS    25F                 DIVIDE WORK AREA                             
ELCWRK   CSECT                                                                  
ELCATAB  DC    AL4(EL1-ELCATAB),AL4(0),AL4(0)    *AL1(ELEMENT CODE)             
         DC    AL4(EL2-ELCATAB),AL4(0),AL4(0)    *AL3(EL ADDR)                  
         DC    AL4(EL3-ELCATAB),AL4(0),AL4(0)    *AL4(HIGH NO IN EL)            
         DC    AL4(EL4-ELCATAB),AL4(0),AL4(0)    *AL2(NO OF FIELDS)             
         DC    AL4(EL5-ELCATAB),AL4(0),AL4(0)    *AL2(FNO OF LAST NON           
         DC    AL4(EL6-ELCATAB),AL4(0),AL4(0)    *     ZERO ENTRY)              
         DC    AL4(EL7-ELCATAB),AL4(0),AL4(0)                                   
         DC    AL4(EL8-ELCATAB),AL4(0),AL4(0)                                   
         DC    AL4(EL9-ELCATAB),AL4(0),AL4(0)                                   
         DC    AL4(ELA-ELCATAB),AL4(0),AL4(0)                                   
         DC    AL4(0),AL4(0),AL4(0)                                             
EL1      DS    100C                                                             
EL2      DS    100C                                                             
EL3      DS    100C                                                             
EL4      DS    100C                                                             
EL5      DS    100C                                                             
EL6      DS    100C                                                             
EL7      DS    100C                                                             
EL8      DS    100C                                                             
EL9      DS    100C                                                             
ELA      DS    100C                                                             
       ++INCLUDE DEPAVDISP                                                      
       ++INCLUDE DMPAVWORKD                                                     
         LTORG                                                                  
