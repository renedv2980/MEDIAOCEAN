*          DATA SET NELNK02    AT LEVEL 006 AS OF 03/10/09                      
*PHASE T30202A                                                                  
NELNK02  TITLE '- NETWORK SYSTEM SERVER SUPPORT ROUTINES 2'                     
NELNK02  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NL02**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,ROU2RELO         SAVE MY RELOCATION FACTOR                    
         SR    RE,RE                                                            
         SLDL  RE,8                BRANCH INDEX HELD IN HOB RF                  
         SLL   RE,2                                                             
         CHI   RE,ROUTABL          ENSURE GOOD INDEX VALUE                      
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    RE,ROUTAB(RE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                ROUTINE NOT DEFINED                          
         AR    RF,RB               RF=A(ROUTINE)                                
                                                                                
         SR    R5,R5                                                            
         ICM   R5,3,2(RE)          R5=TEMPORARY W/S AMOUNT                      
         BZR   RF                                                               
                                                                                
         AHI   R5,7                ROUND AMOUNT TO DOUBLEWORDS                  
         SRL   R5,3                                                             
         SLL   R5,3                                                             
         LR    R3,RD               ACQUIRE STORAGE FROM W/S POOL                
         AR    R3,R5                                                            
         L     R4,4(RD)                                                         
         ST    R4,4(R3)                                                         
         ST    R3,8(R4)                                                         
         LR    RC,RD                                                            
         LR    RD,R3                                                            
         LR    R4,RC               AND CLEAR IT                                 
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R4,R2                                                            
         BR    RF                                                               
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
ROUTAB   DS    0XL4                                                             
         DC    AL2(VALSRC-NELNK02),AL2(0)                                       
         DC    AL2(VALDATS-NELNK02),AL2(0)                                      
         DC    AL2(VALDYTM-NELNK02),AL2(0)                                      
         DC    AL2(VALMFID-NELNK02),AL2(0)                                      
         DC    AL2(VALPNAM-NELNK02),AL2(0)                                      
         DC    AL2(VALMIND-NELNK02),AL2(0)                                      
         DC    AL2(VALNAD-NELNK02),AL2(0)                                       
         DC    AL2(GETAGY-NELNK02),AL2(0)                                       
         DC    AL2(VALDATE-NELNK02),AL2(0)                                      
ROUTABL  EQU   *-ROUTAB                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE SOURCE                                                               
***********************************************************************         
                                                                                
VALSRC   J     *+12                                                             
         DC    C'*VALSRC*'                                                      
         LR    RB,RF                                                            
         USING VALSRC,RB                                                        
         LM    R2,R4,0(R1)                                                      
                                                                                
         CLI   0(R2),C'T'          TIME PERIOD                                  
         BNE   *+12                                                             
         MVI   0(R4),DBGETDEM                                                   
         J     EXITY                                                            
                                                                                
         CLI   0(R2),C'P'          PROGRAM AVERAGE                              
         JNE   EXITN                                                            
         MVI   0(R4),DBGETNTI                                                   
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATES: START DATE - (END DATE)                                       
* OUTPUT IS BINARY START DATE FOLLOWED BY BINARY END DATE                       
***********************************************************************         
                                                                                
VALDATS  J     *+12                                                             
         DC    C'*VALDAT*'                                                      
         LR    RB,RF                                                            
         USING VALDATS,RB                                                       
         LM    R2,R4,0(R1)                                                      
         USING DATD,R4                                                          
                                                                                
         XC    DUMMYH,DUMMYH                                                    
         XC    DUMMYFLD,DUMMYFLD                                                
         STC   R3,DUMMYH+5                                                      
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DUMMYFLD(0),0(R2)                                                
         L     R6,AIO1                                                          
         GOTOR SCANNER,DMCB,DUMMYH,(1,(R6)),C',=,-'                             
         CLI   DMCB+4,0                                                         
         JE    EXITN                                                            
         USING SCANBLKD,R6                                                      
                                                                                
         GOTOR DATVAL,DMCB,SC1STFLD,DUB      VALIDATE FOR M/D/Y                 
         OC    DMCB,DMCB                                                        
         BZ    VALDAT05                      INVALID                            
         GOTOR NETWEEK,DMCB,DUB,GETDAY,ADDAY                                    
         MVC   STBK(1),DMCB+4      START YEAR                                   
         MVC   STBK+1(1),DMCB+12   START WEEK                                   
                                                                                
         CLI   SC2NDLEN,0                                                       
         BNE   *+14                                                             
         MVC   ENDBK,STBK          END BOOK=START BOOK                          
         B     VALDAT10                                                         
         GOTOR DATVAL,DMCB,SC2NDFLD,DUB                                         
         OC    DMCB,DMCB                                                        
         BZ    EXITN                                                            
         GOTOR NETWEEK,DMCB,DUB,GETDAY,ADDAY                                    
         MVC   ENDBK(1),DMCB+4     END YEAR                                     
         MVC   ENDBK+1(1),DMCB+12  END WEEK                                     
         B     VALDAT10                                                         
                                                                                
VALDAT05 GOTOR DATVAL,DMCB,(2,SC1STFLD),DUB   VALIDATE FOR M/Y                  
         OC    DMCB,DMCB                                                        
         JZ    EXITN                                                            
         XC    DUMMYH,DUMMYH                                                    
         XC    DUMMYFLD,DUMMYFLD                                                
         ZIC   RE,SC1STLEN                                                      
         STC   RE,DUMMYH+5                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DUMMYFLD(0),SC1STFLD                                             
         GOTOR BOOKVAL,DMCB,(C'N',DUMMYH),(1,FULL1),SCANNER                     
         CLI   DMCB+4,0                                                         
         JE    EXITN                                                            
         MVC   STBK,FULL1+1        START YEAR+MONTH                             
                                                                                
         CLI   SC2NDLEN,0                                                       
         BNE   *+14                                                             
         MVC   ENDBK,STBK          END BOOK=START BOOK                          
         B     VALDAT10                                                         
         GOTOR DATVAL,DMCB,(2,SC2NDFLD),DUB   VALIDATE FOR M/Y                  
         OC    DMCB,DMCB                                                        
         JZ    EXITN                                                            
         XC    DUMMYH,DUMMYH                                                    
         XC    DUMMYFLD,DUMMYFLD                                                
         ZIC   RE,SC2NDLEN                                                      
         STC   RE,DUMMYH+5                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DUMMYFLD(0),SC2NDFLD                                             
         GOTOR BOOKVAL,DMCB,(C'N',DUMMYH),(1,FULL1),SCANNER                     
         CLI   DMCB+4,0                                                         
         JE    EXITN                                                            
         MVC   ENDBK,FULL1+1       END YEAR+MONTH                               
                                                                                
VALDAT10 CLI   STBK,84             YEAR < 84 IS INVALID                         
         JL    EXITN                                                            
         CLI   ENDBK,84                                                         
         JL    EXITN                                                            
                                                                                
         CLC   STBK(1),ENDBK       START BOOK < END BOOK?                       
         JL    EXITY                                                            
         JH    EXITN                                                            
         CLC   STBK+1(1),ENDBK+1                                                
         JH    EXITN                                                            
                                                                                
         J     EXITY                                                            
         DROP  RB,R4                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE SINGLE DATE                                                          
* OUTPUT DATE IS YYMMDD                                                         
***********************************************************************         
                                                                                
VALDATE  J     *+12                                                             
         DC    C'*VDATE**'                                                      
         LR    RB,RF                                                            
         USING VALDATE,RB                                                       
         LM    R2,R4,0(R1)                                                      
                                                                                
         GOTOR DATVAL,DMCB,(0,(R2)),(R4)                                        
         OC    DMCB(4),DMCB                                                     
         JZ    EXITN                                                            
                                                                                
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE DAY/TIME EXPRESSIONS                                                 
***********************************************************************         
                                                                                
VALDYTM  J     *+12                                                             
         DC    C'*VALDYT*'                                                      
         LR    RB,RF                                                            
         USING VALDYTM,RB                                                       
         LM    R2,R4,0(R1)                                                      
                                                                                
         CHI   R3,DTSTRLN          MAXIMUM EXPRESSION LENGTH                    
         BH    EXITN                                                            
                                                                                
         MVI   BYTE1,0                                                          
                                                                                
         LR    R7,R4                                                            
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R2)       SAVE INPUT EXPRESSION STRING                 
         LA    R7,DTSTRLN(R7)                                                   
                                                                                
         XC    DUMMYH,DUMMYH                                                    
         XC    DUMMYFLD,DUMMYFLD                                                
         STC   R3,DUMMYH+5                                                      
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DUMMYFLD(0),0(R2)                                                
         L     R6,AIO1                                                          
         GOTOR SCANNER,DMCB,(20,DUMMYH),(12,(R6)),C',=+/'                       
         ICM   R5,1,DMCB+4                                                      
         JZ    EXITN                                                            
         CHI   R5,MAXDTN                                                        
         BH    EXITN               TOO MANY DAYS/TIMES IN EXPRESSION            
                                                                                
         USING SCANBLKD,R6                                                      
         USING DYTMD,DUB                                                        
VALDT05  XC    DUB,DUB                                                          
         CLC   =C'DALL',SC1STFLD   ALL DAYS=00                                  
         BE    VALDT10                                                          
         LA    RE,SC1STFLD         VALIDATE DAY                                 
         ST    RE,DMCB                                                          
         MVC   DMCB(1),SC1STLEN                                                 
         GOTOR DAYVAL,DMCB,,DTDAYS,=X'17'                                       
         CLI   DTDAYS,0                                                         
         BNE   VALDT10                                                          
         CLI   SC2NDLEN,0                                                       
         JNE   EXITN               COULD BE JUST TIME (NO DAY)                  
         CLI   BYTE1,0                                                          
         JE    EXITN                                                            
         MVC   DTDAYS,BYTE1        USE PREVIOUS DAY                             
         B     VALDT12                                                          
                                                                                
VALDT10  CLC   =C'TALL',SC2NDFLD   ALL TIMES=00000000                           
         BE    VALDT18                                                          
         LA    RE,SC2NDFLD         VALIDATE TIME                                
         ST    RE,DMCB                                                          
         MVC   DMCB(1),SC2NDLEN                                                 
VALDT12  GOTOR TIMVAL,DMCB,,DTTIMES                                             
         CLI   0(R1),X'FF'                                                      
         JE    EXITN                                                            
         CLC   DTTIMES,=C'NONE'                                                 
         JE    EXITN                                                            
         OC    DTETIME,DTETIME                                                  
         BNZ   *+10                                                             
         MVC   DTETIME,DTSTIME                                                  
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,DTSTIME        ADJUST TIME FOR 12-6AM                       
         CH    R1,=H'600'                                                       
         BNL   *+8                                                              
         AH    R1,=H'2400'                                                      
         STCM  R1,3,DTSTIME                                                     
         ICM   R1,3,DTETIME                                                     
         CH    R1,=H'600'                                                       
         BH    *+8                                                              
         AH    R1,=H'2400'                                                      
         STCM  R1,3,DTETIME                                                     
         CLC   DTETIME,DTSTIME     ENSURE END TIME >= START TIME                
         JL    EXITN                                                            
                                                                                
VALDT18  LR    RF,R4               CHECK OVERLAPS ON DAYS/TIMES                 
         LA    RF,DTSTRLN(RF)      SKIP STRING EXPRESSION                       
VALDT20  CR    RF,R7                                                            
         BNL   VALDT30                                                          
         ZIC   RE,DTDAYS                                                        
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    DTDAYS-DYTMD(RF),0                                               
         BZ    VALDT25                                                          
         CLC   DTETIME,DTSTIME-DYTMD(RF)                                        
         BNH   VALDT25                                                          
         CLC   DTSTIME,DTETIME-DYTMD(RF)                                        
         BNL   VALDT25                                                          
         J     EXITN               NO OVERLAPS ALLOWED                          
VALDT25  LA    RF,DYTMLN(RF)                                                    
         B     VALDT20                                                          
                                                                                
VALDT30  MVC   0(DYTMLN,R7),DUB    ADD DAY/TIME TO TABLE                        
         MVC   BYTE1,DTDAYS        SAVE PREVIOUS DAY                            
                                                                                
         LA    R6,L'SCLINE+10(R6)  NEXT DAY/TIME                                
         LA    R7,DYTMLN(R7)                                                    
         BCT   R5,VALDT05                                                       
                                                                                
         MVI   0(R7),FF                                                         
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE MAINFRAME ID                                                         
***********************************************************************         
                                                                                
VALMFID  J     *+12                                                             
         DC    C'*VALMID*'                                                      
         LR    RB,RF                                                            
         USING VALMFID,RB                                                       
         LM    R2,R4,0(R1)                                                      
                                                                                
         LHI   R1,MFIDLN*2                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ZEROES      PADD WITH CHARACTER 0'S                      
                                                                                
         LR    R1,R2               INPUT MFID                                   
         LA    RE,WORK             TEMPORARY OUTPUT AREA                        
         LHI   R0,MFIDLN*2         MAXIMUM LENGTH                               
VALMF10  CLI   0(R1),0                                                          
         BE    VALMF20                                                          
         MVC   0(1,RE),0(R1)       COPY ACTUAL MFID THAT WAS SENT               
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,VALMF10                                                       
                                                                                
VALMF20  GOTOR HEXIN,DMCB,WORK,(R4),MFIDLN*2                                    
         OC    DMCB+12(4),DMCB+12                                               
         JZ    EXITN                                                            
                                                                                
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROGRAM NAME                                                         
***********************************************************************         
                                                                                
VALPNAM  J     *+12                                                             
         DC    C'*VALPNA*'                                                      
         LR    RB,RF                                                            
         USING VALPNAM,RB                                                       
         LM    R2,R4,0(R1)                                                      
                                                                                
         MVC   0(PNAMLN,R4),SPACES                                              
         GOTOR VPNAME,DMCB,((R3),(R2)),0,SCANNER                                
         CLI   DMCB,0                                                           
         JNE   EXITN                                                            
         LR    R1,R3                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
                                                                                
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE MINIMUM DURATION                                                     
***********************************************************************         
                                                                                
VALMIND  J     *+12                                                             
         DC    C'*VALMND*'                                                      
         LR    RB,RF                                                            
         USING VALMIND,RB                                                       
         LM    R2,R4,0(R1)                                                      
                                                                                
         LR    R1,R2                                                            
         LR    R0,R3                                                            
         TM    0(R1),X'F0'         TEST NUMERIC                                 
         BNO   EXITN                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
                                                                                
         LR    R1,R3                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         JZ    EXITN               MINDUR>0                                     
         CHI   R1,255                                                           
         JH    EXITN               MINDUR<256                                   
         STC   R1,0(R4)                                                         
                                                                                
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE NAD CATEGORIES                                             *         
***********************************************************************         
                                                                                
VALNAD   J     *+12                                                             
         DC    C'*VALNAD*'                                                      
         LR    RB,RF                                                            
         USING VALNAD,RB                                                        
         LM    R2,R4,0(R1)                                                      
                                                                                
         LR    R1,R3                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,X'FF'                                                         
         JH    EXITN                                                            
         STC   R1,0(R4)                                                         
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GET AGENCY RECORD - R1=A(AGENCY ALPHA ID)                           *         
***********************************************************************         
                                                                                
GETAGY   J     *+12                                                             
         DC    C'*GETAGY*'                                                      
         LR    RB,RF                                                            
         USING GETAGY,RB                                                        
         LA    R3,IOKEY                                                         
         USING AGYRECD,R3                                                       
         USING AGYKEY,IOKEY                                                     
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,AGYKTYPQ                                                
         MVC   AGYKAGY,0(R1)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+#AGYREC'                        
         JNE   EXITN                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+#AGYREC'                       
         JNE   EXITN                                                            
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
EXITN    DS    0H                  SET CC NOT EQUAL                             
EXITL    LHI   RE,0                SET CC LOW                                   
         J     EXITCC                                                           
EXITH    LHI   RE,2                SET CC HIGH                                  
         J     EXITCC                                                           
EXITY    LHI   RE,1                SET CC EQUAL                                 
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
                                                                                
         PRINT OFF                                                              
       ++INCLUDE NELNKWRK                                                       
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NELNK02   03/10/09'                                      
         END                                                                    
