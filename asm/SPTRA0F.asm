*          DATA SET SPTRA0F    AT LEVEL 005 AS OF 03/25/19                      
*PHASE T2160FA                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE TIMEOUT                                                                
         TITLE 'T2160F TRAFFIC INSTRUCTION XML MAPPING'                         
*=====================================================================*         
* THIS PROGRAM READS FLAT FILE, CREATES XMLMAP AND PUTS IT OUT ON PQ  *         
*                                                                     *         
*                                                                     *         
*  SPEC-24948  SMUR DEC04/18 BXF MO-XML INITIAL DEVELOPMENT           *         
*                                                                     *         
*=====================================================================*         
T2160F   CSECT                                                                  
         PRINT NOGEN                                                            
CXMLMAP  NMOD1 (LWSX-LWSD),*T2160F*,CLEAR=YES,R7,R8,RR=R3                       
*                                                                               
         LR    R9,RC                                                            
         USING LWSD,R9                                                          
*                                                                               
         ST    R3,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         XC    AMFDATA,AMFDATA                                                  
         L     RE,4(R1)            ADDRESS OF DATA TO PROCESS?                  
         LTR   RE,RE                                                            
         BZ    *+8                 NO, PROCESS FLAT FILE                        
         ST    RE,AMFDATA                                                       
*                                                                               
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
*                                                                               
         OC    AMFDATA,AMFDATA                                                  
         BNZ   FL00                                                             
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,TWAMASTC                                                      
         L     RF,MCSSB-MCBLOCK(RE)                                             
*                                                                               
         OPEN  (RANKWK,INPUT)                                                   
*                                                                               
FL00     L     R3,AFFROUT          FLAT FILE ROUTINE TABLE                      
         A     R3,RELO                                                          
         USING FFROUT,R3                                                        
*                                                                               
         LA    R6,FREC                                                          
         ST    R6,AFREC                                                         
         LA    RF,4(R6)                                                         
         ST    RF,AFRECP4          REC+4 IS START OF DATA                       
*                                                                               
         XC    0(L'FREC,R6),0(R6)                                               
*                                                                               
         LA    RF,MAP              SET ADDR'S OF REC AREAS                      
         ST    RF,AMAP                                                          
*                                                                               
         OC    AMFDATA,AMFDATA                                                  
         BZ    FL00C                                                            
*                                                                               
         SAM31                                                                  
         L     R2,AMFDATA          FLAT FILE LIKE DATA (31-BIT)                 
         ST    R2,AMFDFRO          ADDRESS OF FROM DATA                         
*                                                                               
* GET FIRST ENTRY                                                               
         LLC   RE,1(R2)            DATA LEN                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R2)       SAVE DATA IN FREC                            
         AR    R2,RE                                                            
         AHI   R2,1                                                             
         ST    R2,AMFDFRO          SAVE ADDRESS OF NEXT FROM DATA               
         SAM24                                                                  
         B     FL01                                                             
*                                                                               
FL00C    GET   RANKWK,(R6)                                                      
*                                                                               
FL01     L     R6,AFRECP4          POINT TO ACTUAL DATA                         
*                                                                               
         XC    SVRECTYP,SVRECTYP   INIT RECORD TYPE                             
*                                                                               
         XC    SVMSG,SVMSG         CLEAR FIELDS BELOW                           
*        XC    SVENDMSG,SVENDMSG        END MSG AREA                            
*        XC    SVSUFMSG,SVSUFMSG        SUB-FOOTER MSG AREA                     
*        XC    SVESMSG,SVESMSG          END SCHEDULE MESSAGE AREA               
*                                                                               
         XC    AROUT,AROUT              A(ROUTIN) IN MAP DATA                   
         MVI   NEWTYP,0                 NEW TYPE FLAG                           
*                                                                               
         BRAS  RE,CXHED            CREATE XML HEADING                           
*                                                                               
* FIND ROUTINE                                                                  
FL02     L     RF,AFFROUT          FLAT FILE ROUTINE TABLE                      
         A     RF,RELO                                                          
         LA    R0,FLATABLN                                                      
                                                                                
FL04     DS    0H                                                               
         CLC   0(2,R6),0(RF)                                                    
         BE    FL04A                                                            
         LA    RF,6(RF)                                                         
         BCT   R0,FL04                                                          
         DC    H'0'                                                             
*                                                                               
FL04A    DS    0H                                                               
         ICM   R2,15,2(RF)                                                      
         A     R2,RELO                                                          
         ICM   R1,15,0(R2)         ADDRESS OF MESSAGE                           
         A     R1,RELO                                                          
         BRAS  RE,XMWP                                                          
*                                                                               
         L     R6,AFREC                                                         
         XC    0(L'FREC,R6),0(R6)                                               
         XC    ANEXTPOS,ANEXTPOS                                                
*                                                                               
         OC    AMFDATA,AMFDATA                                                  
         BZ    FL04B                                                            
*                                                                               
* GET NEXT RECORD                                                               
         SAM31                                                                  
         L     R2,AMFDFRO          NEXT FROM DATA                               
         LLC   RE,1(R2)            DATA LEN                                     
         LTR   RE,RE                                                            
         BZ    ENDXML              DONE                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R2)       SAVE DATA IN FREC                            
         AR    R2,RE                                                            
         AHI   R2,1                                                             
         ST    R2,AMFDFRO          SAVE ADDRESS OF NEXT FROM DATA               
         SAM24                                                                  
         B     FL04B1                                                           
*                                                                               
FL04B    GET   RANKWK,(R6)                                                      
*                                                                               
FL04B1   L     R6,AFRECP4          POINT TO ACTUAL DATA                         
*                                                                               
* SEE IF RECORD SUB-TYPE HAS CHG AND NEED TO PRT END MSG                        
         OC    AROUT,AROUT         ANYTHING TO PUT OUT                          
         BZ    FL04C                                                            
*                                                                               
         CLC   SVSUBTY1,0(R6)      DID RECORD TYPE CHANGE                       
         BE    FL04C                NO                                          
*                                                                               
         BAS   RE,ILEQCHK          CHK FOR SIMILAR ENTRY                        
         BE    FL04C                                                            
*                                                                               
         OC    SVSUFMSG,SVSUFMSG   ANY SUB-FOOTER MSG                           
         BZ    *+8                                                              
         BRAS  RE,PSUFMSG          PUT OUT SUB-FOOTER MSG                       
*                                                                               
         OC    SVENDMSG,SVENDMSG   ANY END MSG                                  
         BZ    *+8                 NO                                           
         BRAS  RE,PENDMSG          PUT OUT END MSG                              
*                                                                               
         CLC   SVRECTYP,=C'CN'     CML REC NINS GEN                             
         BE    *+14                                                             
         CLC   SVRECTYP,=C'CM'     CML REC?                                     
         BNE   FL04B2                                                           
         CLC   SVRECTYP,0(R6)      DID RECORD TYPE CHANGE                       
         BNE   FL04B2                                                           
         XC    AROUT,AROUT         DO NOT PRINT YET                             
         B     FL04C                                                            
*                                                                               
FL04B2   OI    NEWTYP,TYPNEQ       RECORD SUB-TYPE CHANGED                      
         BRAS  RE,PROUT                                                         
         OI    NEWTYP,HEADSW       PRINT HEADING                                
*                                                                               
* SEE IF RECORD TYPE CHANGED AND WE NEED TO PRINT END MSG                       
FL04C    OC    SVRECTYP,SVRECTYP   1ST TIME IN                                  
         BZ    FL19                 YES                                         
*                                                                               
         CLC   SVRECTYP,0(R6)      PREV REC TYP TO CURR                         
         BNE   FL04E                                                            
*                                                                               
         BAS   RE,CMLCHKF          IF BOTH ARE CMLS, NO FOOTER                  
         BE    FL08                                                             
*                                                                               
         CLC   SVRECTYP,=C'CD'     BOTH RECS ARE CML DESC                       
         BNE   FL04D                                                            
         XC    SVSUFMSG,SVSUFMSG    OR SUB-FOOTER MSG                           
         NI    NEWTYP,X'FF'-(HEADSW+SUBHSW)                                     
         B     FL08                                                             
*                                                                               
* PREV=CURR BUT NOT CMLS                                                        
FL04D    BAS   RE,CMTEMSG          IF COMMENT, NEED END MSG                     
         BE    FL08                                                             
*                                                                               
         CLC   SVRECTYP,=C'TN'     BOTH RECS ARE TBA                            
         BE    *+14                                                             
         CLC   SVRECTYP,=C'TB'     BOTH RECS ARE TBA                            
         BNE   *+14                                                             
         XC    SVENDMSG,SVENDMSG   NOT READY  FOR END MSG                       
         B     FL08                                                             
*                                                                               
*NOP     CLC   SVRECTYP,=C'IN'     BOTH RECS ARE INSTRUCT LINE REC              
***      BE    *+12                                                             
         CLC   SVRECTYP,=C'IL'     BOTH RECS ARE INSTRUCT LINE REC              
         BNE   FL02                                                             
         XC    SVENDMSG(L'SVENDMSG+L'SVSUFMSG),SVENDMSG   NOT READY             
*        XC    SVSUFMSG,SVSUFMSG    FOR END MSG OR  SUB-FOOTER MSG              
         B     FL02                                                             
*                                                                               
* PREV <> CURRENT                                                               
FL04E    DS    0H                                                               
         LAY   RF,CMTTAB           COMMENTS TABLE                               
         LA    R0,CMTTYLN                                                       
FL04E0   CLC   SVRECTYP,0(RF)                                                   
         BE    FL05X               FOUND A MATCH                                
         LA    RF,2(RF)                                                         
         BCT   R0,FL04E0                                                        
* >>>>>>>>>>>>>>>>>>>>>>>>>>>>                                                  
* SEE IF I NEED CODE BELOW FOR 'RC' TO GO TO FL22                               
* >>>>>>>>>>>>>>>>>>>>>>>>>>>>                                                  
*L04E    CLC   SVRECTYP,=C'DL'     PREVIOUS TYPE DISTR LIST REC?                
*        BE    FL18                                                             
*        CLC   SVRECTYP,=C'RC'     PREVIOUS TYPE REVISION COMMENT?              
******   BE    FL22                                                             
         CLC   SVSUBTY1,0(R6)                                                   
         BE    FL04E1                                                           
         CLC   SVSUBTY1,=C'CT'     PREVIOUS SUBTYP COMTEXT?                     
         BNE   FL04E1                                                           
         MVC   SVSUBTY1,0(R6)                                                   
         CLC   =C'SS',0(R6)                                                     
         BNE   FL08                                                             
         MVC   SVRECTYP,0(R6)      CURRENT SCHEDULE SUMMARY                     
         B     FL08                                                             
*                                                                               
FL04E1   DS    0H                                                               
         MVI   BYTE,C'C'                                                        
         LAY   RF,CMLTAB           CML TABLE                                    
         LA    R0,CMLTYLN                                                       
FL04E2   CLC   SVRECTYP,0(RF)                                                   
         BE    FL04F               FOUND A MATCH                                
         LA    RF,2(RF)                                                         
         BCT   R0,FL04E2                                                        
         B     FL04H                                                            
                                                                                
* PREV RECORD=ONE OF THE CML FLAVORS                                            
FL04F    CLI   BYTE,C'U'           DOING UNIT DETAIL?                           
         BNE   FL04G                                                            
         BAS   RE,UNTTYP           STILL PROCESSING UNIT DETAILS?               
         BNE   FL20                                                             
         B     *+12                                                             
FL04G    BAS   RE,CMLTYP           STILL PROCESSING CML?                        
         BNE   FL04I                                                            
*                                                                               
         MVC   SVSUBTY1,0(6)                                                    
         TM    NEWTYP,SUBFSW       IS SUB-FOOTER FLAG ON                        
         BZ    FL02                 NO                                          
*                                                                               
         OC    SVSUFMSG,SVSUFMSG   ANY SUB-FOOTER MSG                           
         BNZ   *+12                                                             
         NI    NEWTYP,X'FF'-SUBFSW                                              
         B     FL02                                                             
*                                                                               
         BRAS  RE,PSUFMSG          PUT OUT SUB-FOOTER MSG                       
         B     FL02                                                             
*                                                                               
* NINS GEN UNIT DETAILS                                                         
FL04H    DS    0H                                                               
         CLI   BYTE,C'U'           PROCESSING UNIT DETAIL                       
         BE    FL20                YES, DONE, NO MATCH FOUND                    
*                                                                               
         MVI   BYTE,C'U'                                                        
         LAY   RF,UNTTAB           UNIT DETAIL TABLE                            
         LA    R0,UNTTYLN                                                       
         B     FL04E2                                                           
*                                                                               
FL04I    DS    0H                                                               
*NOP     CLC   SVRECTYP,=C'IN'     INSTRUCTION LINES RECORD                     
****     BE    *+14                                                             
         CLC   SVRECTYP,=C'IL'     INSTRUCTION LINES RECORD                     
         BNE   FL04M                                                            
*                                                                               
FL04J    BAS   RE,ILTYP            STILL PROCESSING INSTRUCT LINE?              
         BE    FL04K                                                            
*                                                                               
         CLC   0(2,R6),=C'TN'      TBA ENTRIES                                  
         BE    *+14                                                             
         CLC   0(2,R6),=C'TB'      TBA ENTRIES                                  
         BNE   FL20                                                             
*                                                                               
         TM    NEWTYP,SUBFSW       IS SUB-FOOTER FLAG ON                        
         BZ    FL20                NO, GO PUT OUT END MSG                       
*                                                                               
         OC    SVSUFMSG,SVSUFMSG   ANY SUB-FOOTER MSG                           
         BNZ   *+12                                                             
         NI    NEWTYP,X'FF'-SUBFSW                                              
         B     FL20                                                             
*                                                                               
         BRAS  RE,PSUFMSG          PUT OUT SUB-FOOTER MSG                       
         B     FL20                                                             
*                                                                               
FL04K    TM    NEWTYP,SUBFSW       IS SUB-FOOTER FLAG ON                        
         BZ    FL02                NO, GO PUT OUT END MSG                       
*                                                                               
         OC    SVSUFMSG,SVSUFMSG   ANY SUB-FOOTER MSG                           
         BNZ   FL02                                                             
         NI    NEWTYP,X'FF'-SUBFSW                                              
         B     FL02                                                             
*                                                                               
FL04M    CLC   SVRECTYP,=C'RT'     DONE WITH ROTATION LIST                      
         BNE   FL04P                                                            
         MVC   SVRECTYP,0(R6)                                                   
         OI    NEWTYP,SUBHSW       TURN ON SUB-HEADER                           
         B     FL02                                                             
                                                                                
FL04P    CLC   SVRECTYP,=C'TB'     TBA RECORD                                   
         BE    *+14                                                             
         CLC   SVRECTYP,=C'TB'     TBA RECORD                                   
         BNE   FL05                                                             
*                                                                               
         BAS   RE,TBATYP           STILL PROCESSING TBA RECORD?                 
         BE    FL02                                                             
         B     FL18                NO, GO PUT OUT END MSGS                      
*                                                                               
FL05     CLC   SVRECTYP,=C'CO'     PREV=HEADER COMMENT                          
         BE    FL05C                                                            
         CLC   SVRECTYP,=C'DC'     PREV=PRODUCT LEVEL COMMENT                   
         BE    FL05D                                                            
         CLC   SVRECTYP,=C'RC'     PREV=REVISION COMMENT                        
         BNE   FL15                                                             
         B     FL05M                                                            
*                                                                               
FL05C    CLC   =C'DC',0(R6)        AND CURR=PRODUCT LEVEL COMMENT               
         BE    FL05X                                                            
*                                                                               
FL05D    CLC   =C'RC',0(R6)        AND CURR=PRODUCT LEVEL COMMENT               
         BE    FL05X                                                            
*                                                                               
FL05M    CLC   =C'FC',0(R6)        AND CURR=FOOTER COMMENT                      
         BNE   FL18                                                             
*                                                                               
FL05X    MVC   SVRECTYP,0(R6)                                                   
*                                                                               
         XC    SVENDMSG,SVENDMSG   NOT READY  FOR END MSG YET                   
         OI    NEWTYP,SUBHSW       TURN ON SUB-HEADER                           
*                                                                               
FL08     OC    SVSUFMSG,SVSUFMSG   ANY SUB-FOOTER MSG                           
         BNZ   *+12                                                             
         NI    NEWTYP,X'FF'-SUBFSW                                              
         B     FL02                                                             
*                                                                               
         BRAS  RE,PSUFMSG          PUT OUT SUB-FOOTER MSG                       
*                                                                               
         OC    SVENDMSG,SVENDMSG   ANY END MSG                                  
         BZ    FL02                NO                                           
         BRAS  RE,PENDMSG          PUT OUT END MSG                              
         B     FL02                                                             
*                                                                               
FL15     CLC   SVRECTYP,=C'PC'     DONE WITH PATTERN COMMENT                    
         BE    FL18                                                             
         CLC   SVRECTYP,=C'FC'     DONE WITH FOOTER                             
         BNE   FL20                                                             
*                                                                               
FL18     OI    NEWTYP,SUBFSW       TURN ON SUB-FOOTER                           
*                                                                               
         OC    SVSUFMSG,SVSUFMSG   ANY SUB-FOOTER MSG                           
         BNZ   *+18                                                             
         NI    NEWTYP,X'FF'-SUBFSW                                              
FL19     MVC   SVRECTYP,0(R6)      SAVE THIS REC TYPE                           
         B     FL02                NO END MSG YET                               
*                                                                               
         BRAS  RE,PSUFMSG          PUT OUT SUB-FOOTER MSG                       
*                                                                               
FL20     OC    SVSUFMSG,SVSUFMSG   ANY SUB-FOOTER MSG                           
         BZ    *+8                 NO                                           
         BRAS  RE,PSUFMSG          PUT OUT SUB-FOOTER MSG                       
         NI    NEWTYP,X'FF'-SUBFSW                                              
*                                                                               
FL22     OC    SVENDMSG,SVENDMSG   ANY END MSG                                  
         BZ    *+8                 NO                                           
         BRAS  RE,PENDMSG          PUT OUT END MSG                              
*                                                                               
         MVC   SVRECTYP,0(R6)      SAVE THIS REC TYPE                           
         CLC   SVRECTYP,=C'RC'                                                  
         BE    FL25                                                             
         CLC   SVRECTYP,=C'FC'                                                  
         BE    FL25                                                             
         CLC   SVRECTYP,=C'CO'                                                  
         BE    FL25                                                             
         CLC   SVRECTYP,=C'DL'                                                  
         BNE   *+12                                                             
FL25     OI    NEWTYP,SUBHSW       TURN ON NEW REC TYPE SUB-HEADER              
         B     *+8                                                              
         OI    NEWTYP,HEADSW       TURN ON NEW REC TYPE                         
         B     FL02                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
* CHECK IF NEED TO PRINT COMMENT SUBFOOTER AND FOOTER                           
*                                                                               
CMTEMSG  DS    0H                                                               
         LAY   RF,CMTTAB           COMMENT MSG TAB                              
         LA    R0,CMTTYLN                                                       
CMTE10   CLC   SVRECTYP,0(RF)                                                   
         BE    CMTE12              NO, NOT YET                                  
         LA    RF,2(RF)                                                         
         BCT   R0,CMTE10                                                        
         B     CMTNEQ                                                           
*                                                                               
CMTE12   XC    SVENDMSG(L'SVENDMSG+L'SVSUFMSG),SVENDMSG  NOT READY              
*        XC    SVSUFMSG,SVSUFMSG    FOR END OR SUB-FOOTER MSG                   
         CR    RB,RB                                                            
         B     CMTEX                                                            
*                                                                               
CMTNEQ   LTR   RB,RB                                                            
CMTEX    BR    RE                                                               
*                                                                               
*                                                                               
* CHECK IF NEED TO PRINT CML FOOTER                                             
*                                                                               
CMLCHKF  DS    0H                                                               
         LAY   RF,CMLFTAB          CML MSG TAB                                  
         LA    R0,CMLFTYLN                                                      
CMLF10   CLC   SVRECTYP,0(RF)                                                   
         BE    CMLF12                                                           
         LA    RF,2(RF)                                                         
         BCT   R0,CMLF10                                                        
         B     CMLFNEQ                                                          
*                                                                               
CMLF12   CLC   SVSUBTY1,=C'CT'     DID COMTEXT ?                                
         BE    CMLFX                                                            
         XC    SVENDMSG,SVENDMSG    CLEAR FOOTER                                
         MVI   SVSUBTY1,0                                                       
         CR    RB,RB                                                            
         B     CMLFX                                                            
*                                                                               
CMLFNEQ  LTR   RB,RB                                                            
CMLFX    BR    RE                                                               
*                                                                               
*                                                                               
*==========================================================                     
*  CHECK IF TWO RECORDS ARE SIMILAR                                             
*==========================================================                     
*                                                                               
ILEQCHK  NTR1                                                                   
         CLC   SVRECTYP,=C'CN'     CML NINS GEN                                 
         BNE   ILEQ05                                                           
*                                                                               
         LAY   RF,CNEQTYP          CN EQUATE ENTRIES                            
         LA    R0,CNEQTYLN         TABLE LEN                                    
*                                                                               
ILEQ02   CLC   0(2,R6),0(RF)       MATCH ON REC TYPE                            
         BE    ILX                                                              
         LA    RF,2(RF)                                                         
         BCT   R0,ILEQ02                                                        
         CR    RB,RC                                                            
         B     ILX                                                              
*                                                                               
ILEQ05   LAY   RF,ILEQTYP          TABLE OF EQUAL ENTRIES                       
         LA    R0,ILEQTYLN         TABLE LEN                                    
*                                                                               
ILEQ10   CLC   0(2,R6),0(RF)       MATCH ON REC TYPE                            
         BE    ILEQ20                                                           
         LA    RF,4(RF)                                                         
         BCT   R0,ILEQ10                                                        
         CR    RB,RC                                                            
         B     ILX                                                              
*                                                                               
ILEQ20   CLC   SVSUBTY1,0(R6)      ALREADY PROCESSING THIS REC TYPE             
         BE    ILEQX                YES                                         
*                                                                               
         CLC   SVSUBTY1,2(RF)      SIMILAR RECORDS                              
         BNE   ILX                                                              
         XC    AROUT,AROUT         YES, CLR END OF MSG ADDR RTN                 
ILX      XIT1                                                                   
*                                                                               
*                                                                               
*==========================================================                     
*  SEE IF STILL PROCESSING INSTRUCTION LINE RECORD                              
*==========================================================                     
*                                                                               
ILTYP    NTR1                                                                   
         LAY   RF,ILR1TYP          MULTIPLE ENTRIES FOR EACH REC TYPE           
         LA    R0,ILR1TYLN         TABLE LEN                                    
*                                                                               
ILTY10   CLC   0(2,R6),0(RF)       MATCH ON REC TYPE                            
         BE    ILTY20                                                           
         LA    RF,4(RF)                                                         
         BCT   R0,ILTY10                                                        
         B     ILTY40                                                           
                                                                                
ILTY20   CLC   SVSUBTY1,0(R6)      ALREADY PROCESSING THIS REC TYPE             
         BE    ILXIT                YES                                         
                                                                                
         CLC   SVSUBTY1,2(RF)      IF PREV REC TYPE MATCH                       
         BNE   ILTY25                                                           
         XC    AROUT,AROUT         CLEAR ADDR OF END MSG ROUTINE                
         MVC   SVSUBTY1,0(R6)                                                   
         B     ILEQX               NO NEED FOR HEADING YET                      
                                                                                
* FLAG TO PUT OUT HEADER/SUB-HEADER OF NEXT REC, THIS REC SUB-FOOTER            
ILTY25   OI    NEWTYP,HEADSW+SUBHSW+SUBFSW                                      
         MVC   SVSUBTY1,0(R6)      SAVE THIS RECORD TYPE                        
         B     ILEQX                                                            
                                                                                
ILTY40   MVC   SVSUBTY1,0(R6)      YES, SAVE THIS REC TYPE                      
         LAY   RF,ILRTYP           SINGLE ENTRIES FOR EACH REC TYPE             
         LA    R0,ILRTYLN                                                       
                                                                                
ILTY45   CLC   0(2,R6),0(RF)                                                    
         BE    ILXIT                                                            
                                                                                
         LA    RF,2(RF)                                                         
         BCT   R0,ILTY45                                                        
                                                                                
         CR    RB,RC               CC NE                                        
         B     *+6                                                              
ILEQX    CR    RB,RB                                                            
                                                                                
ILXIT    J     EXIT                                                             
*                                                                               
*                                                                               
*===============================================                                
*  SEE IF STILL PROCESSING CML RECORD                                           
*===============================================                                
*                                                                               
CMLTYP   NTR1                                                                   
*                                                                               
* DESCRIPTION SUB-FOOTER                                                        
         CLC   =C'CD',0(R6)        COMMERCIAL DESCRIPTION                       
         BNE   *+14                                                             
         MVC   SVSUBTY1,0(6)                                                    
         B     CMLT10                                                           
                                                                                
         CLC   SVSUBTY1,=C'CD'     CML DESCRIPTION REC IN PROGRESS              
         BE    CMLT10                                                           
                                                                                
* COMMERCIAL SUB-FOOTER                                                         
CMLT02   CLC   =C'CS',0(R6)        STANDARD CML RECORD                          
         BNE   *+14                                                             
         MVC   SVSUBTY1,0(R6)      SAVE CML RECORD SUB TYPE                     
         B     CMLT10                                                           
                                                                                
         CLC   SVSUBTY1,=C'CS'     STAND CML REC ALREADY IN PROGRESS            
         BE    CMLT04                                                           
                                                                                
         CLC   =C'PC',0(R6)        COMTEXT CML RECORD                           
         BNE   *+14                                                             
         CLC   SVSUBTY1,=C'PC'     DOING PATTERN COMMENT ALREADY?               
         BE    CMLEQX                                                           
                                                                                
         CLC   =C'CT',0(R6)        COMTEXT CML RECORD                           
         BNE   CMLT04                                                           
         CLC   SVSUBTY1,=C'CT'     COMTEXT REC ALREADY IN PROGRESS              
         BE    CMLT10                                                           
                                                                                
         OI    NEWTYP,HEADSW       COMTEXT HEADER                               
*NOP     OI    NEWTYP,HEADSW+SUBFSW COMTEXT HEADER AND CML SUB-FOOTER           
         MVC   SVSUBTY1,0(R6)      SAVE CML RECORD SUB TYPE                     
         B     CMLT10                                                           
                                                                                
CMLT04   LAY   RF,CML1TYP          SEE IF NEED SUB-FOOTER MSG                   
         LA    R0,CML1TYLN                                                      
CMLT06   CLC   0(2,R6),0(RF)                                                    
         BE    CMLEQX              NO, NOT YET                                  
         LA    RF,2(RF)                                                         
         BCT   R0,CMLT06                                                        
                                                                                
         OI    NEWTYP,SUBFSW       TURN ON SUB-FOOTER FLAG                      
                                                                                
CMLT10   MVC   SVSUBTY1,0(6)                                                    
         LAY   RF,CMLRTYP                                                       
         LA    R0,CMLRTYLN                                                      
                                                                                
CMLT15   CLC   0(2,R6),0(RF)                                                    
         BE    CMLEQX                                                           
                                                                                
         LA    RF,2(RF)                                                         
         BCT   R0,CMLT15                                                        
                                                                                
         CR    RB,RC               CC NE                                        
CMLEQX   J     EXIT                                                             
*                                                                               
*                                                                               
UNTTYP   NTR1                                                                   
         LAY   RF,UNTTAB           SEE IF NEED SUB-FOOTER MSG                   
         LA    R0,UNTTYLN                                                       
UNT02    CLC   0(2,R6),0(RF)                                                    
         BE    UNTEQX              NO, NOT YET                                  
         LA    RF,2(RF)                                                         
         BCT   R0,UNT02                                                         
                                                                                
         OI    NEWTYP,SUBFSW       TURN ON SUB-FOOTER FLAG                      
         MVC   SVSUBTY1,0(6)                                                    
         CR    RB,RC               CC NE                                        
UNTEQX   J     EXIT                                                             
*                                                                               
*                                                                               
*===============================================                                
*  SEE IF STILL PROCESSING TBA RECORD                                           
*===============================================                                
*                                                                               
TBATYP   NTR1                                                                   
* SCHEDULE SUMMARY SUB-HEADER                                                   
         CLC   =C'ST',0(R6)        TBA SCHEDULE SUMMARY                         
         BNE   TBAT05                                                           
         MVC   SVSUBTY1,0(6)                                                    
         OI    NEWTYP,HEADSW       PRINT HEADING                                
         B     TBAT08                                                           
                                                                                
TBAT05   CLC   SVSUBTY1,=C'ST'     TBA SCHEDULE SUMMARY                         
         BNE   TBAT08                                                           
                                                                                
         OI    NEWTYP,SUBHSW       TURN ON SUB-HEADER FLAG                      
         XC    SVSUBTY1,SVSUBTY1                                                
                                                                                
TBAT08   LAY   RF,TBARTYP                                                       
         LA    R0,TBARTYLN                                                      
                                                                                
TBAT10   CLC   0(2,R6),0(RF)                                                    
         BE    TBAEQX                                                           
                                                                                
         LA    RF,2(RF)                                                         
         BCT   R0,TBAT10                                                        
                                                                                
         CR    RB,RC               CC NE                                        
TBAEQX   J     EXIT                                                             
*                                                                               
RANKWK   DCB   DDNAME=RANKWK,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               MACRF=(GM,PM),                                          X        
               EODAD=ENDXML                                                     
*                                                                               
ENDXML   DS    0H                                                               
         OC    AROUT,AROUT         ANYTHING TO PUT OUT                          
         BZ    ENDX10                                                           
                                                                                
         OI    NEWTYP,TYPNEQ       RECORD SUB-TYPE CHANGED                      
         BRAS  RE,PROUT                                                         
                                                                                
ENDX10   OC    SVSUFMSG,SVSUFMSG   ANY SUB-FOOTER MSG                           
         BZ    *+8                                                              
         BRAS  RE,PSUFMSG          PUT OUT SUB-FOOTER MSG                       
                                                                                
         OC    SVENDMSG,SVENDMSG   ANY END MSG                                  
         BZ    *+8                 NO                                           
         BRAS  RE,PENDMSG          PUT OUT END MSG                              
                                                                                
         OC    SVESMSG,SVESMSG     ANY END SCHEDULE MSG                         
         BZ    *+8                 NO                                           
         BRAS  RE,PESMSG           PUT OUT END SCHEDULE MSG                     
                                                                                
         BRAS  RE,XMLENDMS         XML END MSG                                  
                                                                                
         OC    AMFDATA,AMFDATA                                                  
         BNZ   ENDX12                                                           
                                                                                
         CLOSE (RANKWK)                                                         
                                                                                
ENDX12   CLI   OUTMODE,C'D'        XML OUTPUT TO DATASET                        
         BNE   ENDX15                                                           
                                                                                
         CLOSE (XMLOUT)                                                         
ENDX15   J     EXIT                                                             
*                                                                               
AFFROUT  DC    A(FFROUT)          A(FLAT FILE ROUTINE)                          
*                                                                               
*                                                                               
CXHED    NTR1                                                                   
***      MVI   OUTMODE,C'D'        XML TO DATASET                               
         MVI   OUTMODE,C'Q'        XML TO PQ                                    
*                                                                               
*                                  PATCH TO 'D' FOR DATASET                     
         CLI   OUTMODE,C'D'        OUTPUT TO DATASET ?                          
         BNE   CXH10                                                            
                                                                                
         OPEN  (XMLOUT,(OUTPUT))                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*=======================================================                        
* CREATE XML HEADING (BXF MESSAGE)                                              
*=======================================================                        
*                                                                               
CXH10    L     R4,AFRECP4          POINT TO ACTUAL DATA                         
         SR    R2,R2                                                            
         ICM   R2,3,BXFMSL         MESSAGE LEN                                  
         LA    R3,BXFMSG           POINT TO BXF MESSAGE                         
         BRAS  RE,PUTR                                                          
                                                                                
         J     EXIT                                                             
*                                                                               
*=======================================================                        
* BXF MESSAGE                                                                   
*=======================================================                        
*                                                                               
BXFMSL   DC    AL2(BXFMSX-BXFMSL)                                               
BXFMSG   DC    AL1(CONQ),AL2(0,128),C'<TrafficHeaderInfo xmlns="http://X        
               www.mediaocean.com/spectra/motrafficbxf" xmlns:xsi="httpX        
               ://www.w3.org/2001/XMLSchema-instance">'                         
         DC    AL1(EORQ),AL2(4+128)                                             
                                                                                
         DC    AL1(CONQ),AL2(0,15),C'<ReportDetails>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,52),C'<ReportName>NETWORK COMMERCIAL SCHX        
               EDULE</ReportName>'                                              
         DC    AL1(EORQ),AL2(4+52)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
BXFMSX   EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  HEADER SEND MESSAGE                                                          
*================================================================               
*                                                                               
HSMSL    DC    AL2(HSMSX-HSMSL)                                                 
HSMSG    DC    AL1(CONQ),AL2(0,10),C'<ReportId>'                                
         DC    AL1(FVDQ),AL2(10,4),AL2(HSID-HSD)                                
         DC    AL1(CONQ),AL2(14,11),C'</ReportId>'                              
         DC    AL1(EORQ),AL2(4+10+4+11)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,19),C'<ReportRunDateTime>'                       
         DC    AL1(FVDQ),AL2(19,10),AL2(0)                                      
         DC    AL1(CONQ),AL2(29,1),C'T'                                         
         DC    AL1(FVDQ),AL2(30,8),AL2(0)                                       
         DC    AL1(CONQ),AL2(38,20),C'</ReportRunDateTime>'                     
         DC    AL1(EORQ),AL2(4+19+10+9+20)                                      
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<ReportInitiator>'                         
         DC    AL1(FVDQ),AL2(17,3),AL2(0)                                       
         DC    AL1(CONQ),AL2(20,18),C'</ReportInitiator>'                       
         DC    AL1(EORQ),AL2(4+17+3+18)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,22),C'<ReportRevisionNumber>'                    
         DC    AL1(FVDQ),AL2(22,3),AL2(0)                                       
         DC    AL1(CONQ),AL2(25,23),C'</ReportRevisionNumber>'                  
         DC    AL1(EORQ),AL2(4+22+3+23)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'</ReportDetails>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
HSMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  HEADER USER MESSAGE                                                          
*================================================================               
*                                                                               
HUMSL    DC    AL2(HUMSX-HUMSL)                                                 
HUMSG    DC    AL1(CONQ),AL2(0,8),C'<Agency>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,19),C'<AgencyCompanyName>'                       
         DC    AL1(FVDQ),AL2(19,43),AL2(HUNAME-HUD)                             
         DC    AL1(CONQ),AL2(62,20),C'</AgencyCompanyName>'                     
         DC    AL1(EORQ),AL2(4+19+43+20)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,15),C'<AgencyAddress>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,14),C'<AddressLine1>'                            
         DC    AL1(FVDQ),AL2(14,43),AL2(0)                                      
         DC    AL1(CONQ),AL2(57,15),C'</AddressLine1>'                          
         DC    AL1(EORQ),AL2(4+14+43+15)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'</AgencyAddress>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
HUMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  HEADER CONTACT MESSAGE                                                       
*================================================================               
*                                                                               
HCMSL    DC    AL2(HCMSX-HCMSL)                                                 
HCMSG    DC    AL1(CONQ),AL2(0,15),C'<AgencyContact>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<ContactName>'                             
         DC    AL1(FVDQ),AL2(13,40),AL2(HCNME-HCD)                              
         DC    AL1(CONQ),AL2(53,14),C'</ContactName>'                           
         DC    AL1(EORQ),AL2(4+13+40+14)                                        
         DC    AL1(EOPQ),AL2(0)                                                 
                                                                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,14),C'<ContactPhone>'                            
         DC    AL1(FVDQ),AL2(20,24),AL2(0)                                      
         DC    AL1(CONQ),AL2(44,15),C'</ContactPhone>'                          
         DC    AL1(EORQ),AL2(4+14+24+21)                                        
         DC    AL1(EOPQ),AL2(0)                                                 
                                                                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,12),C'<ContactFax>'                              
         DC    AL1(FVDQ),AL2(12,24),AL2(0)                                      
         DC    AL1(CONQ),AL2(36,13),C'</ContactFax>'                            
         DC    AL1(EORQ),AL2(4+12+24+13)                                        
         DC    AL1(EOPQ),AL2(0)                                                 
                                                                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,14),C'<ContactEmail>'                            
         DC    AL1(FVDQ),AL2(14,40),AL2(0)                                      
         DC    AL1(CONQ),AL2(54,15),C'</ContactEmail>'                          
         DC    AL1(EORQ),AL2(4+14+40+15)                                        
         DC    AL1(EOPQ),AL2(0)                                                 
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'</AgencyContact>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,9),C'</Agency>'                                  
         DC    AL1(EORQ),AL2(4+9)                                               
                                                                                
         DC    AL1(ENDQ)                                                        
HCMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  HEADER NETWORK MESSAGE                                                       
*================================================================               
*                                                                               
HNMSL    DC    AL2(HNMSX-HNMSL)                                                 
HNMSG    DC    AL1(CONQ),AL2(0,9),C'<Network>'                                  
         DC    AL1(EORQ),AL2(4+9)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<NetworkCode>'                             
         DC    AL1(FVDQ),AL2(13,8),AL2(HNCDE-HND)                               
         DC    AL1(CONQ),AL2(21,14),C'</NetworkCode>'                           
         DC    AL1(EORQ),AL2(4+13+8+14)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<NetworkName>'                             
         DC    AL1(FVDQ),AL2(13,34),AL2(0)                                      
         DC    AL1(CONQ),AL2(47,14),C'</NetworkName>'                           
         DC    AL1(EORQ),AL2(4+13+34+14)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<NetworkAddress>'                          
         DC    AL1(CONQ),AL2(16,14),C'<AddressLine1>'                           
         DC    AL1(FVDQ),AL2(30,30),AL2(0)                                      
         DC    AL1(CONQ),AL2(60,15),C'</AddressLine1>'                          
         DC    AL1(CONQ),AL2(75,17),C'</NetworkAddress>'                        
         DC    AL1(EORQ),AL2(4+16+14+30+15+17)                                  
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'</Network>'                                
         DC    AL1(EORQ),AL2(4+10)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
HNMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  ADVERTISER MESSAGE                                                           
*================================================================               
*                                                                               
ADMSL    DC    AL2(ADMSX-ADMSL)                                                 
ADMSG    DC    AL1(CONQ),AL2(0,8),C'<Client>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,12),C'<ClientCode>'                              
         DC    AL1(FVDQ),AL2(12,8),AL2(ADCDE-ADVD)                              
         DC    AL1(CONQ),AL2(21,13),C'</ClientCode>'                            
         DC    AL1(EORQ),AL2(4+12+8+13)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,12),C'<ClientName>'                              
         DC    AL1(FVDQ),AL2(12,30),AL2(0)                                      
         DC    AL1(CONQ),AL2(42,13),C'</ClientName>'                            
         DC    AL1(EORQ),AL2(4+12+30+13)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,9),C'</Client>'                                  
         DC    AL1(EORQ),AL2(4+9)                                               
                                                                                
         DC    AL1(ENDQ)                                                        
ADMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  HEADER PERIOD MESSAGE                                                        
*================================================================               
*                                                                               
HPMSL    DC    AL2(HPMSX-HPMSL)                                                 
HPMSG    DC    AL1(CONQ),AL2(0,11),C'<StartDate>'                               
         DC    AL1(FVDQ),AL2(11,10),AL2(HSPER-HPD)                              
         DC    AL1(CONQ),AL2(21,12),C'</StartDate>'                             
         DC    AL1(EORQ),AL2(4+11+10+12)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,9),C'<EndDate>'                                  
         DC    AL1(FVDQ),AL2(9,10),AL2(HEPER-HPD)                               
         DC    AL1(CONQ),AL2(19,10),C'</EndDate>'                               
         DC    AL1(EORQ),AL2(4+9+10+10)                                         
                                                                                
         DC    AL1(ENDQ)                                                        
HPMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  HEADER PRODUCT MESSAGE                                                       
*================================================================               
*                                                                               
PDMSL    DC    AL2(PDMSX-PDMSL)                                                 
PDMSG    DC    AL1(CONQ),AL2(0,7),C'<Brand>'                                    
         DC    AL1(EORQ),AL2(4+7)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,11),C'<BrandCode>'                               
         DC    AL1(FVDQ),AL2(11,10),AL2(HPRCDE-HPRD)                            
         DC    AL1(CONQ),AL2(21,12),C'</BrandCode>'                             
         DC    AL1(EORQ),AL2(4+11+10+12)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,11),C'<BrandName>'                               
         DC    AL1(FVDQ),AL2(11,30),AL2(0)                                      
         DC    AL1(CONQ),AL2(41,12),C'</BrandName>'                             
         DC    AL1(EORQ),AL2(4+11+30+12)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,8),C'</Brand>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(ENDQ)                                                        
PDMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  HEADER PRODUCT VARIOUS MESSAGE                                               
*================================================================               
*                                                                               
VRMSL    DC    AL2(VRMSX-VRMSL)                                                 
VRMSG    DC    AL1(CONQ),AL2(0,7),C'<Brand>'                                    
         DC    AL1(EORQ),AL2(4+7)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,24),C'<BrandCode> </BrandCode>'                  
         DC    AL1(EORQ),AL2(4+24)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,30),C'<BrandName>VARIOUS</BrandName>'            
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,8),C'</Brand>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(ENDQ)                                                        
VRMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  PRODUCT MESSAGE                                                              
*================================================================               
*                                                                               
PRMSL    DC    AL2(PRMSX-PRMSL)                                                 
PRMSG    DC    AL1(CONQ),AL2(0,7),C'<Brand>'                                    
         DC    AL1(EORQ),AL2(4+7)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,11),C'<BrandCode>'                               
         DC    AL1(FVDQ),AL2(11,10),AL2(HPRCDE-HPRD)                            
         DC    AL1(CONQ),AL2(21,12),C'</BrandCode>'                             
         DC    AL1(EORQ),AL2(4+11+10+12)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,11),C'<BrandName>'                               
         DC    AL1(FVDQ),AL2(11,30),AL2(0)                                      
         DC    AL1(CONQ),AL2(41,12),C'</BrandName>'                             
         DC    AL1(EORQ),AL2(4+11+30+12)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,8),C'</Brand>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(ENDQ)                                                        
PRMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  HEADER PGROUP MESSAGE                                                        
*================================================================               
*                                                                               
GRMSL    DC    AL2(GRMSX-GRMSL)                                                 
GRMSG    DC    AL1(CONQ),AL2(0,7),C'<Brand>'                                    
         DC    AL1(EORQ),AL2(4+7)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<BrandGroupCode>'                          
         DC    AL1(FVDQ),AL2(16,10),AL2(HGRCDE-HGRD)                            
         DC    AL1(CONQ),AL2(26,17),C'</BrandGroupCode>'                        
         DC    AL1(EORQ),AL2(4+16+10+17)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<BrandGroupName>'                          
         DC    AL1(FVDQ),AL2(16,34),AL2(0)                                      
         DC    AL1(CONQ),AL2(50,17),C'</BrandGroupName>'                        
         DC    AL1(EORQ),AL2(4+16+34+17)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,8),C'</Brand>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(ENDQ)                                                        
GRMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  HEADER CUTIN MKT/STA MESSAGE                                                 
*================================================================               
*&&DO                                                                           
CIMSL    DC    AL2(CIMSX-CIMSL)                                                 
CIMSG    DC    AL1(SHDQ),AL2(0,16),C'<MarketStations>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,8),C'<Market="'                                  
         DC    AL1(FVDQ),AL2(8,10),AL2(CIMKT-CID)                               
         DC    AL1(CONQ),AL2(18,2),C'">'                                        
         DC    AL1(EORQ),AL2(4+8+10+2)                                          
                                                                                
         DC    AL1(CONQ),AL2(0,9),C'<Station="'                                 
         DC    AL1(FVDQ),AL2(9,14),AL2(CISTA-CID)                               
         DC    AL1(CONQ),AL2(23,2),C'">'                                        
         DC    AL1(EORQ),AL2(4+9+14+2)                                          
                                                                                
         DC    AL1(SENQ),AL2(0,17),C'</MarketStations>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
CIMSX    EQU   *                                                                
*&&                                                                             
*                                                                               
*================================================================               
*  DAYPART MESSAGE                                                              
*================================================================               
*                                                                               
DPMSL    DC    AL2(DPMSX-DPMSL)                                                 
DPMSG    DC    AL1(SHDQ),AL2(0,10),C'<Dayparts>'                                
         DC    AL1(EORQ),AL2(4+10)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,20),C'<Daypart daypartID="'                      
         DC    AL1(FVDQ),AL2(20,6),AL2(DPID-DPD)                                
         DC    AL1(CONQ),AL2(26,2),C'">'                                        
         DC    AL1(EORQ),AL2(4+20+6+2)                                          
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'</Daypart>'                                
         DC    AL1(EORQ),AL2(4+10)                                              
                                                                                
         DC    AL1(SENQ),AL2(0,11),C'</Dayparts>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
DPMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  HEADER CUTIN MKT/STA MESSAGE                                                 
*================================================================               
*                                                                               
CIMSL    DC    AL2(CIMSX-CIMSL)                                                 
CIMSG    DC    AL1(SHDQ),AL2(0,8),C'<Cutins>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<Cutin cutinID="'                          
         DC    AL1(FVDQ),AL2(16,6),AL2(CIID-CID)                                
         DC    AL1(CONQ),AL2(22,2),C'">'                                        
         DC    AL1(EORQ),AL2(4+16+6+2)                                          
                                                                                
         DC    AL1(CONQ),AL2(0,14),C'<instr:Market>'                            
         DC    AL1(FVDQ),AL2(14,4),AL2(0)                                       
         DC    AL1(CONQ),AL2(18,15),C'</instr:Market>'                          
         DC    AL1(EORQ),AL2(4+14+4+15)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,15),C'<instr:Station>'                           
         DC    AL1(FVDQ),AL2(15,4),AL2(0)                                       
         DC    AL1(CONQ),AL2(19,16),C'</instr:Station>'                         
         DC    AL1(EORQ),AL2(4+15+4+16)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,8),C'</Cutin>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(SENQ),AL2(0,9),C'</Cutins>'                                  
         DC    AL1(EORQ),AL2(4+9)                                               
                                                                                
         DC    AL1(ENDQ)                                                        
CIMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  FEED MESSAGE                                                                 
*================================================================               
*                                                                               
FDMSL    DC    AL2(FDMSX-FDMSL)                                                 
FDMSG    DC    AL1(CONQ),AL2(0,14),C'<RotationFeed>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'<RotationFeedCode>'                        
         DC    AL1(FVDQ),AL2(18,16),AL2(FDID-FDD)                               
         DC    AL1(CONQ),AL2(34,19),C'</RotationFeedCode>'                      
         DC    AL1(EORQ),AL2(4+18+16+19)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'<RotationFeedName>'                        
         DC    AL1(FVDQ),AL2(18,70),AL2(0)                                      
         DC    AL1(CONQ),AL2(88,19),C'</RotationFeedName>'                      
         DC    AL1(EORQ),AL2(4+18+70+19)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,15),C'</RotationFeed>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
FDMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL HIDEF MESSAGE                                                     
*================================================================               
*                                                                               
CHMSL    DC    AL2(CHMSX-CHMSL)                                                 
CHMSG    DC    AL1(CONQ),AL2(0,18),C'<CommercialFormat>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,24),C'<CommercialFormatName>HD'                  
         DC    AL1(CONQ),AL2(24,23),C'</CommercialFormatName>'                  
         DC    AL1(EORQ),AL2(4+24+23)                                           
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<CommercialCode>'                          
         DC    AL1(FVDQ),AL2(16,12),AL2(CHCDE-CHD)                              
         DC    AL1(CONQ),AL2(28,17),C'</CommercialCode>'                        
         DC    AL1(EORQ),AL2(4+16+12+17)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,19),C'</CommercialFormat>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
CHMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL CENTRCUT MESSAGE                                                  
*================================================================               
*                                                                               
CCMSL    DC    AL2(CCMSX-CCMSL)                                                 
CCMSG    DC    AL1(CONQ),AL2(0,18),C'<CommercialFormat>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,24),C'<CommercialFormatName>CT'                  
         DC    AL1(CONQ),AL2(24,23),C'</CommercialFormatName>'                  
         DC    AL1(EORQ),AL2(4+24+23)                                           
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<CommercialCode>'                          
         DC    AL1(FVDQ),AL2(16,12),AL2(CCCDE-CCD)                              
         DC    AL1(CONQ),AL2(28,17),C'</CommercialCode>'                        
         DC    AL1(EORQ),AL2(4+16+12+17)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,19),C'</CommercialFormat>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
CCMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL ALTERNATE MESSAGE                                                 
*================================================================               
*                                                                               
CAMSL    DC    AL2(CAMSX-CAMSL)                                                 
CAMSG    DC    AL1(CONQ),AL2(0,18),C'<CommercialFormat>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,24),C'<CommercialFormatName>AL'                  
         DC    AL1(CONQ),AL2(24,23),C'</CommercialFormatName>'                  
         DC    AL1(EORQ),AL2(4+24+23)                                           
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<CommercialCode>'                          
         DC    AL1(FVDQ),AL2(16,30),AL2(CACDE-CAD)                              
         DC    AL1(CONQ),AL2(46,17),C'</CommercialCode>'                        
         DC    AL1(EORQ),AL2(4+16+30+17)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,19),C'</CommercialFormat>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
CAMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL TYPE MESSAGE                                                      
*================================================================               
*                                                                               
TPMSL    DC    AL2(TPMSX-TPMSL)                                                 
TPMSG    DC    AL1(CONQ),AL2(0,16),C'<CommercialType>'                          
         DC    AL1(FVDQ),AL2(16,4),AL2(TYPE-TPR)                                
         DC    AL1(CONQ),AL2(20,17),C'</CommercialType>'                        
         DC    AL1(EORQ),AL2(4+16+4+17)                                         
                                                                                
         DC    AL1(ENDQ)                                                        
TPMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL ROTATION MESSAGE                                                  
*================================================================               
*                                                                               
RRMSL    DC    AL2(RRMSX-RRMSL)                                                 
RRMSG    DC    AL1(DSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'<RotationPosition>'                        
         DC    AL1(FVDQ),AL2(18,1),AL2(ROTATN-RRD)                              
         DC    AL1(CONQ),AL2(29,19),C'</RotationPosition>'                      
         DC    AL1(EORQ),AL2(4+18+1+19)                                         
                                                                                
         DC    AL1(SUFQ),AL2(0,21),C'</RotationCommercial>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(SENQ),AL2(0,22),C'</RotationCommercials>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
RRMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL DESTROY DATE AND TIME MESSAGE                                     
*================================================================               
*                                                                               
DTMSL    DC    AL2(DTMSX-DTMSL)                                                 
DTMSG    DC    AL1(DSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<DestroyDateTime>'                         
         DC    AL1(FVDQ),AL2(17,10),AL2(DTDTE-DTD)                              
         DC    AL1(CONQ),AL2(27,1),C'T'                                         
         DC    AL1(FVDQ),AL2(28,8),AL2(0)                                       
         DC    AL1(CONQ),AL2(36,18),C'</DestroyDateTime>'                       
         DC    AL1(EORQ),AL2(4+17+10+9+18)                                      
                                                                                
         DC    AL1(SUFQ),AL2(0,21),C'</RotationCommercial>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
DTMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL DESTROY DATE MESSAGE                                              
*================================================================               
*                                                                               
DDMSL    DC    AL2(DDMSX-DDMSL)                                                 
DDMSG    DC    AL1(DSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<DestroyDateTime>'                         
         DC    AL1(FVDQ),AL2(19,10),AL2(DDDTE-DDD)                              
         DC    AL1(CONQ),AL2(29,18),C'</DestroyDateTime>'                       
         DC    AL1(EORQ),AL2(4+17+10+18)                                        
                                                                                
         DC    AL1(SUFQ),AL2(0,21),C'</RotationCommercial>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
DDMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMTEXT MESSAGE                                                              
*================================================================               
*                                                                               
CTMSL    DC    AL2(CTMSX-CTMSL)                                                 
CTMSG    DC    AL1(DSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(SHDQ),AL2(0,20),C'<CommercialComments>'                      
         DC    AL1(EORQ),AL2(4+20)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<CommentText>'                             
         DC    AL1(FVDQ),AL2(13,80),AL2(CTTXT-CTD)                              
         DC    AL1(CONQ),AL2(93,14),C'</CommentText>'                           
         DC    AL1(EORQ),AL2(4+13+80+14)                                        
                                                                                
         DC    AL1(SUFQ),AL2(0,21),C'</CommercialComments>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(SENQ),AL2(0,21),C'</RotationCommercial>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(EMSQ),AL2(0,22),C'</RotationCommercials>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
CTMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL LENGTH MESSAGE                                                    
*================================================================               
*                                                                               
CLMSL    DC    AL2(CLMSX-CLMSL)                                                 
CLMSG    DC    AL1(CONQ),AL2(0,18),C'<CommercialLength>'                        
         DC    AL1(FVDQ),AL2(18,3),AL2(CLLEN-CLD)                               
         DC    AL1(CONQ),AL2(21,19),C'</CommercialLength>'                      
         DC    AL1(EORQ),AL2(4+18+3+19)                                         
                                                                                
         DC    AL1(ENDQ)                                                        
CLMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL LENGTH OVERRIDE MESSAGE                                           
*================================================================               
*                                                                               
LOMSL    DC    AL2(LOMSX-LOMSL)                                                 
LOMSG    DC    AL1(CONQ),AL2(0,17),C'<OverrideLength1>'                         
         DC    AL1(FVDQ),AL2(17,3),AL2(LOSEG1-LOD)                              
         DC    AL1(CONQ),AL2(20,18),C'</OverrideLength1>'                       
         DC    AL1(EORQ),AL2(4+26+3+19)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<OverrideLength2>'                         
         DC    AL1(FVDQ),AL2(17,3),AL2(0)                                       
         DC    AL1(CONQ),AL2(20,18),C'</OverrideLength2>'                       
         DC    AL1(EORQ),AL2(4+26+3+19)                                         
                                                                                
         DC    AL1(ENDQ)                                                        
LOMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL PERCENTAGE ALLOCATION                                             
*================================================================               
*                                                                               
PAMSL    DC    AL2(PAMSX-PAMSL)                                                 
PAMSG    DC    AL1(CONQ),AL2(0,20),C'<RotationPercentage>'                      
         DC    AL1(FVDQ),AL2(20,3),AL2(PAPCT-PAD)                               
         DC    AL1(CONQ),AL2(23,21),C'</RotationPercentage>'                    
         DC    AL1(EORQ),AL2(4+20+3+21)                                         
                                                                                
         DC    AL1(SUFQ),AL2(0,21),C'</RotationCommercial>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(SENQ),AL2(0,22),C'</RotationCommercials>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
PAMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  PROGRAM  MESSAGE                                                             
*================================================================               
*                                                                               
PGMSL    DC    AL2(PGMSX-PGMSL)                                                 
PGMSG    DC    AL1(SHDQ),AL2(0,9),C'<Program>'                                  
         DC    AL1(EORQ),AL2(4+9)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<ProgramCode>'                             
         DC    AL1(FVDQ),AL2(13,16),AL2(PGCDE-PGD)                              
         DC    AL1(CONQ),AL2(29,14),C'</ProgramCode>'                           
         DC    AL1(EORQ),AL2(4+13+16+14)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<ProgramName>'                             
         DC    AL1(FVDQ),AL2(13,30),AL2(0)                                      
         DC    AL1(CONQ),AL2(43,14),C'</ProgramName>'                           
         DC    AL1(EORQ),AL2(4+12+30+14)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'</Program>'                                
         DC    AL1(EORQ),AL2(4+10)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
PGMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  PROGRAM INFO DETAIL RECORD MESSAGE  (NINS GEN)                               
*================================================================               
*                                                                               
PIMSL    DC    AL2(PIMSX-PIMSL)                                                 
PIMSG    DC    AL1(CONQ),AL2(0,9),C'<Program>'                                  
         DC    AL1(EORQ),AL2(4+9)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<ProgramCode>'                             
         DC    AL1(FVDQ),AL2(13,6),AL2(PICODE-PID)                              
         DC    AL1(CONQ),AL2(19,14),C'</ProgramCode>'                           
         DC    AL1(EORQ),AL2(4+13+6+14)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<ProgramName>'                             
         DC    AL1(FVDQ),AL2(13,16),AL2(0)                                      
         DC    AL1(CONQ),AL2(19,14),C'</ProgramName>'                           
         DC    AL1(EORQ),AL2(4+13+16+14)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,12),C'<ProgramDay>'                              
         DC    AL1(FVDQ),AL2(12,7),AL2(0)                                       
         DC    AL1(CONQ),AL2(19,13),C'</ProgramDay>'                            
         DC    AL1(EORQ),AL2(4+12+7+13)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'<ProgramStartTime>'                        
         DC    AL1(FVDQ),AL2(19,5),AL2(0)                                       
         DC    AL1(CONQ),AL2(24,19),C'</ProgramStartTime>'                      
         DC    AL1(EORQ),AL2(4+18+5+19)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<ProgramEndTime>'                          
         DC    AL1(FVDQ),AL2(16,5),AL2(0)                                       
         DC    AL1(CONQ),AL2(21,17),C'</ProgramEndTime>'                        
         DC    AL1(EORQ),AL2(4+16+5+17)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,10),C'</Program>'                                
         DC    AL1(EORQ),AL2(4+10)                                              
*                                                                               
         DC    AL1(ENDQ)                                                        
PIMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  DIFFERENT PROGRAM INFO MESSAGE  (NINS GEN)                                   
*================================================================               
*                                                                               
DGMSL    DC    AL2(DGMSX-DGMSL)                                                 
DGMSG    DC    AL1(CONQ),AL2(0,18),C'<RotationPrograms>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<RotationProgram>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,26),C'<ProgramAlternateDateTime>'                
         DC    AL1(EORQ),AL2(4+26)                                              
                                                                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,13),C'<ProgramName>'                             
         DC    AL1(FVDQ),AL2(13,16),AL2(DGPGM-DGD)                              
         DC    AL1(CONQ),AL2(29,14),C'</ProgramName>'                           
         DC    AL1(EORQ),AL2(4+13+16+14)                                        
         DC    AL1(EOPQ),AL2(0)                                                 
                                                                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,18),C'<ProgramStartTime>'                        
         DC    AL1(FVDQ),AL2(18,5),AL2(0)                                       
         DC    AL1(CONQ),AL2(33,19),C'</ProgramStartTime>'                      
         DC    AL1(EORQ),AL2(4+18+5+19)                                         
         DC    AL1(EOPQ),AL2(0)                                                 
                                                                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,16),C'<ProgramEndTime>'                          
         DC    AL1(FVDQ),AL2(16,5),AL2(0)                                       
         DC    AL1(CONQ),AL2(33,17),C'</ProgramEndTime>'                        
         DC    AL1(EORQ),AL2(4+16+5+17)                                         
         DC    AL1(EOPQ),AL2(0)                                                 
                                                                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,13),C'<ProgramDays>'                             
         DC    AL1(FVDQ),AL2(13,7),AL2(0)                                       
         DC    AL1(CONQ),AL2(20,14),C'</ProgramDays>'                           
         DC    AL1(EORQ),AL2(4+13+7+14)                                         
         DC    AL1(EOPQ),AL2(0)                                                 
                                                                                
         DC    AL1(CONQ),AL2(0,27),C'</ProgramAlternateDateTime>'               
         DC    AL1(EORQ),AL2(4+27)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'</RotationProgram>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,19),C'</RotationPrograms>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
DGMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  HEADER COMMENT MESSAGE                                                       
*================================================================               
*                                                                               
COMSL    DC    AL2(COMSX-COMSL)                                                 
COMSG    DC    AL1(PSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(PESQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(SUHQ),AL2(0,16),C'<HeaderComments>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<CommentText>'                             
         DC    AL1(FVDQ),AL2(13,80),AL2(COTXT-COD)                              
         DC    AL1(CONQ),AL2(93,14),C'</CommentText>'                           
         DC    AL1(EORQ),AL2(4+13+80+14)                                        
                                                                                
         DC    AL1(SUFQ),AL2(0,17),C'</HeaderComments>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
COMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  REVISION HEADER COMMENT  MESSAGE                                             
*================================================================               
*                                                                               
RCMSL    DC    AL2(RCMSX-RCMSL)                                                 
RCMSG    DC    AL1(PSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(PESQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
*WAS SHDQ                                                                       
         DC    AL1(SUHQ),AL2(0,18),C'<RevisionComments>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<CommentText>'                             
         DC    AL1(FVDQ),AL2(13,80),AL2(RCTXT-RCD)                              
         DC    AL1(CONQ),AL2(93,14),C'</CommentText>'                           
         DC    AL1(EORQ),AL2(4+13+80+14)                                        
                                                                                
         DC    AL1(SUFQ),AL2(0,19),C'</RevisionComments>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
RCMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  PRODUCT LEVEL FOOTER COMMENT  MESSAGE                                        
*================================================================               
*                                                                               
DCMSL    DC    AL2(DCMSX-DCMSL)                                                 
DCMSG    DC    AL1(PSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(PESQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(SUHQ),AL2(0,22),C'<ProductLevelComments>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<CommentText>'                             
         DC    AL1(FVDQ),AL2(13,80),AL2(DCTXT-DCD)                              
         DC    AL1(CONQ),AL2(93,14),C'</CommentText>'                           
         DC    AL1(EORQ),AL2(4+13+80+14)                                        
                                                                                
         DC    AL1(SUFQ),AL2(0,23),C'</ProductLevelComments>'                   
         DC    AL1(EORQ),AL2(4+23)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
DCMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  FOOTER COMMENT MESSAGE (STEXT)                                               
*================================================================               
*                                                                               
FCMSL    DC    AL2(FCMSX-FCMSL)                                                 
FCMSG    DC    AL1(PSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(PESQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
*NOP     DC    AL1(SHDQ),AL2(0,16),C'<FooterComments>'                          
****     DC    AL1(EORQ),AL2(4+16)                                              
                                                                                
         DC    AL1(SUHQ),AL2(0,16),C'<FooterComments>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<CommentText>'                             
         DC    AL1(FVDQ),AL2(13,80),AL2(FCTXT-FCD)                              
         DC    AL1(CONQ),AL2(93,14),C'</CommentText>'                           
         DC    AL1(EORQ),AL2(4+13+80+14)                                        
                                                                                
         DC    AL1(SUFQ),AL2(0,17),C'</FooterComments>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
FCMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  DISTRIBUTION LIST MESSAGE                                                    
*================================================================               
*                                                                               
DLMSL    DC    AL2(DLMSX-DLMSL)                                                 
DLMSG    DC    AL1(PSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(PESQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
*WAS SHDQ                                                                       
         DC    AL1(SUHQ),AL2(0,18),C'<CopiesToComments>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<CommentText>'                             
         DC    AL1(FVDQ),AL2(13,80),AL2(DLNAME-DLR)                             
         DC    AL1(CONQ),AL2(93,14),C'</CommentText>'                           
         DC    AL1(EORQ),AL2(4+13+80+14)                                        
                                                                                
         DC    AL1(SUFQ),AL2(0,19),C'</CopiesToComments>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
DLMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  INSTRUCTION LINE MESSAGE                                                     
*================================================================               
*                                                                               
ILMSL    DC    AL2(ILMSX-ILMSL)                                                 
ILMSG    DC    AL1(PSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(PESQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(SHDQ),AL2(0,17),C'<ScheduleSection>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,19),C'<RotationStartTime>'                       
         DC    AL1(FVDQ),AL2(19,5),AL2(ILSTME-ILD)                              
         DC    AL1(CONQ),AL2(24,20),C'</RotationStartTime>'                     
         DC    AL1(EORQ),AL2(4+19+5+20)                                         
                                                                                
         DC    AL1(EOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,19),C'<RotationStartDate>'                       
         DC    AL1(FVDQ),AL2(19,10),AL2(0)                                      
         DC    AL1(CONQ),AL2(29,20),C'</RotationStartDate>'                     
         DC    AL1(EORQ),AL2(4+19+10+20)                                        
                                                                                
         DC    AL1(EOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,17),C'<RotationEndTime>'                         
         DC    AL1(FVDQ),AL2(17,5),AL2(0)                                       
         DC    AL1(CONQ),AL2(23,18),C'</RotationEndTime>'                       
         DC    AL1(EORQ),AL2(4+17+5+18)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<RotationEndDate>'                         
         DC    AL1(FVDQ),AL2(17,10),AL2(0)                                      
         DC    AL1(CONQ),AL2(27,18),C'</RotationEndDate>'                       
         DC    AL1(EORQ),AL2(4+17+10+18)                                        
                                                                                
         DC    AL1(SESQ),AL2(0,18),C'</ScheduleSection>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
ILMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  INSTRUCTION LINE MESSAGE (NINS GEN)                                          
*================================================================               
*                                                                               
INMSL    DC    AL2(INMSX-INMSL)                                                 
INMSG    DC    AL1(PESQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(SHDQ),AL2(0,17),C'<ScheduleSection>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,19),C'<RotationStartDate>'                       
         DC    AL1(FVDQ),AL2(16,10),AL2(INNADT-ILD)                             
         DC    AL1(CONQ),AL2(29,20),C'</RotationStartDate>'                     
         DC    AL1(EORQ),AL2(4+19+10+20)                                        
                                                                                
         DC    AL1(SESQ),AL2(0,18),C'</ScheduleSection>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
INMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  NATIONAL UNIT RECORD MESSAGE  (NINS GEN)                                     
*================================================================               
*                                                                               
NTMSL    DC    AL2(NTMSX-NTMSL)                                                 
NTMSG    DC    AL1(CONQ),AL2(0,14),C'<RotationFeed>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,12),C'<isNational>'                              
         DC    AL1(FVDQ),AL2(12,1),AL2(NTYORN-NTD)                              
         DC    AL1(CONQ),AL2(13,13),C'</isNational>'                            
         DC    AL1(EORQ),AL2(4+12+1+13)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,15),C'</RotationFeed>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
NTMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  UNIT POSITION RECORD MESSAGE  (NINS GEN)                                     
*================================================================               
*                                                                               
UPMSL    DC    AL2(UPMSX-UPMSL)                                                 
UPMSG    DC    AL1(SHDQ),AL2(0,13),C'<UnitDetails>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,14),C'<UnitPosition>'                            
         DC    AL1(FVDQ),AL2(14,4),AL2(UPOSN-UPD)                               
         DC    AL1(CONQ),AL2(18,15),C'</UnitPosition>'                          
         DC    AL1(EORQ),AL2(4+14+4+15)                                         
                                                                                
         DC    AL1(SENQ),AL2(0,14),C'</UnitDetails>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
UPMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  UNIT COMMERCIAL INFO RECORD MESSAGE  (NINS GEN)                              
*================================================================               
*                                                                               
UIMSL    DC    AL2(UIMSX-UIMSL)                                                 
UIMSG    DC    AL1(SHDQ),AL2(0,13),C'<UnitDetails>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,20),C'<UnitCommercialInfo>'                      
         DC    AL1(FVDQ),AL2(20,3),AL2(UICML-UID)                               
         DC    AL1(CONQ),AL2(23,21),C'</UnitCommercialInfo>'                    
         DC    AL1(EORQ),AL2(4+20+3+21)                                         
                                                                                
         DC    AL1(SENQ),AL2(0,14),C'</UnitDetails>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
UIMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  UNIT REVISION NUMBER RECORD MESSAGE  (NINS GEN)                              
*================================================================               
*                                                                               
RVMSL    DC    AL2(RVMSX-RVMSL)                                                 
RVMSG    DC    AL1(SHDQ),AL2(0,13),C'<UnitDetails>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,20),C'<UnitRevisionNumber>'                      
         DC    AL1(FVDQ),AL2(20,4),AL2(REVNO-RVD)                               
         DC    AL1(CONQ),AL2(24,21),C'</UnitRevisionNumber>'                    
         DC    AL1(EORQ),AL2(4+20+4+21)                                         
                                                                                
         DC    AL1(SENQ),AL2(0,14),C'</UnitDetails>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
RVMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  UNIT TYPE RECORD MESSAGE  (NINS GEN)                                         
*================================================================               
*                                                                               
UTMSL    DC    AL2(UTMSX-UTMSL)                                                 
UTMSG    DC    AL1(SHDQ),AL2(0,13),C'<UnitDetails>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<UnitType>'                                
         DC    AL1(FVDQ),AL2(10,13),AL2(UTCML-UTD)                              
         DC    AL1(CONQ),AL2(23,11),C'</UnitType>'                              
         DC    AL1(EORQ),AL2(4+10+13+11)                                        
                                                                                
         DC    AL1(SENQ),AL2(0,14),C'</UnitDetails>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
UTMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  UNIT COST RECORD MESSAGE  (NINS GEN)                                         
*================================================================               
*                                                                               
UCMSL    DC    AL2(UCMSX-UCMSL)                                                 
UCMSG    DC    AL1(SHDQ),AL2(0,13),C'<UnitDetails>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<UnitCost>'                                
         DC    AL1(FVDQ),AL2(10,13),AL2(UCCML-UCD)                              
         DC    AL1(CONQ),AL2(23,11),C'</UnitCost>'                              
         DC    AL1(EORQ),AL2(4+10+13+11)                                        
                                                                                
         DC    AL1(SENQ),AL2(0,14),C'</UnitDetails>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
UCMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  UNIT ESTIMATE RECORD MESSAGE  (NINS GEN)                                     
*================================================================               
*                                                                               
ESMSL    DC    AL2(ESMSX-ESMSL)                                                 
ESMSG    DC    AL1(SHDQ),AL2(0,13),C'<UnitDetails>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,14),C'<EstimateCode>'                            
         DC    AL1(FVDQ),AL2(14,3),AL2(EST-ESD)                                 
         DC    AL1(CONQ),AL2(17,15),C'</EstimateCode>'                          
         DC    AL1(EORQ),AL2(4+14+3+15)                                         
                                                                                
         DC    AL1(SENQ),AL2(0,14),C'</UnitDetails>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
ESMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  UNIT PACKAGE RECORD MESSAGE  (NINS GEN)                                      
*================================================================               
*                                                                               
PKMSL    DC    AL2(PKMSX-PKMSL)                                                 
PKMSG    DC    AL1(SHDQ),AL2(0,13),C'<UnitDetails>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,9),C'<Package>'                                  
         DC    AL1(FVDQ),AL2(09,3),AL2(PKGE-PKD)                                
         DC    AL1(CONQ),AL2(12,10),C'</Package>'                               
         DC    AL1(EORQ),AL2(4+9+3+10)                                          
                                                                                
         DC    AL1(SENQ),AL2(0,14),C'</UnitDetails>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
PKMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  INSTRUCTION LINE CUTIN MESSAGE (NINS GEN)                                    
*================================================================               
*                                                                               
ICMSL    DC    AL2(ICMSX-ICMSL)                                                 
ICMSG    DC    AL1(SHDQ),AL2(0,18),C'<InstructionLines>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<InstructionLine>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,14),C'<CutinStation>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,15),C'<instr:AirDate>'                           
         DC    AL1(FVDQ),AL2(15,10),AL2(INNADT-ILD)                             
         DC    AL1(CONQ),AL2(25,16),C'</instr:AirDate>'                         
         DC    AL1(EORQ),AL2(4+15+10+16)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,23),C'<instr:MarketStationId='                   
         DC    AL1(FVDQ),AL2(23,6),AL2(ILNMSID-ILD)                             
         DC    AL1(CONQ),AL2(29,3),C'"/>'                                       
         DC    AL1(CONQ),AL2(32,24),C'</instr:MarketStationId>'                 
         DC    AL1(EORQ),AL2(4+23+6+3+24)                                       
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'</InstructionLine>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
*NOP     DC    AL1(SENQ),AL2(0,19),C'</InstructionLines>'                       
         DC    AL1(SUFQ),AL2(0,19),C'</InstructionLines>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
ICMSX    EQU   *                                                                
*                                                                               
*&&DO                                                                           
* THIS MOVED TO SSMSG PER ASH                                                   
*================================================================               
*  RESTRICTION BY PROGRAM INSTRUCTION LINE MESSAGE                              
*================================================================               
*                                                                               
RGMSL    DC    AL2(RGMSX-RGMSL)                                                 
RGMSG    DC    AL1(CONQ),AL2(0,18),C'<RotationPrograms>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<RotationProgram>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,21),C'<RotationProgramCode>'                     
         DC    AL1(FVDQ),AL2(21,16),AL2(RGPRG-RGD)                              
         DC    AL1(CONQ),AL2(37,22),C'</RotationProgramCode>'                   
         DC    AL1(EORQ),AL2(4+21+16+22)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'</RotationProgram>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,19),C'</RotationPrograms>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
RGMSX    EQU   *                                                                
*&&                                                                             
*                                                                               
*================================================================               
*  ALL PROGRAMS MESSAGE INSTRUCTION LINE MESSAGE                                
*================================================================               
*                                                                               
APMSL    DC    AL2(APMSX-APMSL)                                                 
APMSG    DC    AL1(CONQ),AL2(0,20),C'<applyToAllPrograms>'                      
         DC    AL1(FVDQ),AL2(20,1),AL2(APPRG-APD)                               
         DC    AL1(CONQ),AL2(21,21),C'</applyToAllPrograms>'                    
         DC    AL1(EORQ),AL2(4+20+1+21)                                         
                                                                                
         DC    AL1(ENDQ)                                                        
APMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  RESTRICTION BY DAYPART INSTRUCTION LINE MESSAGE                              
*================================================================               
*                                                                               
RDMSL    DC    AL2(RDMSX-RDMSL)                                                 
RDMSG    DC    AL1(CONQ),AL2(0,17),C'<RotationDaypart>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,21),C'<RotationDaypartCode>'                     
         DC    AL1(FVDQ),AL2(21,16),AL2(RDDPID-RDD)                             
         DC    AL1(CONQ),AL2(37,22),C'</RotationDaypartCode>'                   
         DC    AL1(EORQ),AL2(4+21+16+22)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'</RotationDaypart>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
RDMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  RESTRICTION BY FEED INSTRUCTION LINE MESSAGE                                 
*================================================================               
*                                                                               
RFMSL    DC    AL2(RFMSX-RFMSL)                                                 
RFMSG    DC    AL1(CONQ),AL2(0,14),C'<RotationFeed>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'<RotationFeedCode>'                        
         DC    AL1(FVDQ),AL2(18,6),AL2(RFFEED-RFD)                              
         DC    AL1(CONQ),AL2(24,19),C'</RotationFeedCode>'                      
         DC    AL1(EORQ),AL2(4+18+6+19)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'<RotationFeedName>'                        
         DC    AL1(FVDQ),AL2(18,80),AL2(0)                                      
         DC    AL1(CONQ),AL2(98,19),C'</RotationFeedName>'                      
         DC    AL1(EORQ),AL2(4+18+80+19)                                        
                                                                                
                                                                                
         DC    AL1(CONQ),AL2(0,15),C'</RotationFeed>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
RFMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  P/B PRODUCT INSTRUCTION LINE MESSAGE                                         
*================================================================               
*                                                                               
P2MSL    DC    AL2(P2MSX-P2MSL)                                                 
P2MSG    DC    AL1(CONQ),AL2(0,18),C'<RotationBrandCode'                        
         DC    AL1(FVDQ),AL2(18,1),AL2(P2NUM-P2D)                               
         DC    AL1(CONQ),AL2(19,1),C'>'                                         
         DC    AL1(FVDQ),AL2(20,13),AL2(0)                                      
         DC    AL1(CONQ),AL2(33,19),C'</RotationBrandCode'                      
         DC    AL1(FVDQ),AL2(42,1),AL2(0)                                       
         DC    AL1(CONQ),AL2(43,1),C'>'                                         
         DC    AL1(EORQ),AL2(4+20+13+21)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'<RotationBrandName'                        
         DC    AL1(FVDQ),AL2(18,1),AL2(0)                                       
         DC    AL1(CONQ),AL2(19,1),C'>'                                         
         DC    AL1(FVDQ),AL2(20,30),AL2(0)                                      
         DC    AL1(CONQ),AL2(50,19),C'</RotationBrandName'                      
         DC    AL1(FVDQ),AL2(69,1),AL2(0)                                       
         DC    AL1(CONQ),AL2(70,1),C'>'                                         
         DC    AL1(EORQ),AL2(4+20+30+21)                                        
                                                                                
         DC    AL1(ENDQ)                                                        
P2MSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
* SINGLE PRODUCT INSTRUCTION LINE MESSAGE                                       
*================================================================               
*                                                                               
P1MSL    DC    AL2(P1MSX-P1MSL)                                                 
P1MSG    DC    AL1(CONQ),AL2(0,20),C'<RotationBrandCode1>'                      
         DC    AL1(FVDQ),AL2(20,13),AL2(P1PRD1-P1D)                             
         DC    AL1(CONQ),AL2(33,21),C'</RotationBrandCode1>'                    
         DC    AL1(EORQ),AL2(4+20+13+21)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,20),C'<RotationBrandName1>'                      
         DC    AL1(FVDQ),AL2(20,30),AL2(0)                                      
         DC    AL1(CONQ),AL2(50,21),C'</RotationBrandName1>'                    
         DC    AL1(EORQ),AL2(4+20+30+21)                                        
                                                                                
         DC    AL1(ENDQ)                                                        
P1MSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
* SINGLE LENGTH INSTRUCTION LINE MESSAGE                                        
*================================================================               
*                                                                               
L1MSL    DC    AL2(L1MSX-L1MSL)                                                 
L1MSG    DC    AL1(CONQ),AL2(0,17),C'<RotationLength1>'                         
         DC    AL1(FVDQ),AL2(17,8),AL2(L1SLN1-L1D)                              
         DC    AL1(CONQ),AL2(25,18),C'</RotationLength1>'                       
         DC    AL1(EORQ),AL2(4+17+8+18)                                         
                                                                                
         DC    AL1(ENDQ)                                                        
L1MSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
* P/B LENGTH INSTRUCTION LINE MESSAGE                                           
*================================================================               
*                                                                               
L2MSL    DC    AL2(L2MSX-L2MSL)                                                 
L2MSG    DC    AL1(CONQ),AL2(0,15),C'<RotationLength'                           
         DC    AL1(FVDQ),AL2(15,1),AL2(L2NUM-L2D)                               
         DC    AL1(CONQ),AL2(16,1),C'>'                                         
         DC    AL1(FVDQ),AL2(17,3),AL2(0)                                       
         DC    AL1(CONQ),AL2(20,16),C'</RotationLength'                         
         DC    AL1(FVDQ),AL2(36,1),AL2(L2NUM-L2D)                               
         DC    AL1(CONQ),AL2(37,1),C'>'                                         
         DC    AL1(EORQ),AL2(4+15+5+16+2)                                       
                                                                                
         DC    AL1(ENDQ)                                                        
L2MSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
* RUN TIMES DAILY                                                               
*================================================================               
*                                                                               
DYMSL    DC    AL2(DYMSX-DYMSL)                                                 
DYMSG    DC    AL1(DSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,21),C'<CommercialStartTime>'                     
         DC    AL1(FVDQ),AL2(21,6),AL2(DYSTIME-DYD)                             
         DC    AL1(CONQ),AL2(27,22),C'</CommercialStartTime>'                   
         DC    AL1(EORQ),AL2(4+21+6+22)                                         
         DC    AL1(EOPQ),AL2(0)                                                 
                                                                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,19),C'<CommercialEndTime>'                       
         DC    AL1(FVDQ),AL2(19,5),AL2(0)                                       
         DC    AL1(CONQ),AL2(24,20),C'</CommercialEndTime>'                     
         DC    AL1(EORQ),AL2(4+19+5+20)                                         
         DC    AL1(EOPQ),AL2(0)                                                 
                                                                                
         DC    AL1(CONQ),AL2(0,09),C'<IsDaily>'                                 
         DC    AL1(FVDQ),AL2(09,1),AL2(0)                                       
         DC    AL1(CONQ),AL2(10,10),C'</IsDaily>'                               
         DC    AL1(EORQ),AL2(4+09+1+10)                                         
                                                                                
         DC    AL1(SUFQ),AL2(0,21),C'</RotationCommercial>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(SENQ),AL2(0,22),C'</RotationCommercials>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
DYMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL TBA MESSAGE                                                       
*================================================================               
*                                                                               
CBMSL    DC    AL2(CBMSX-CBMSL)                                                 
CBMSG    DC    AL1(PSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,21),C'<RotationCommercials>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,20),C'<RotationCommercial>'                      
         DC    AL1(EORQ),AL2(4+20)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<CommercialCode>'                          
         DC    AL1(CONQ),AL2(16,3),C'TBA'                                       
         DC    AL1(CONQ),AL2(19,17),C'</CommercialCode>'                        
         DC    AL1(EORQ),AL2(4+16+3+17)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<CommercialName1>'                         
         DC    AL1(CONQ),AL2(17,15),C'TO BE ANNOUNCED'                          
         DC    AL1(CONQ),AL2(32,18),C'</CommercialName1>'                       
         DC    AL1(EORQ),AL2(4+17+15+18)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,21),C'</RotationCommercial>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,22),C'</RotationCommercials>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
CBMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL MESSAGE                                                           
*================================================================               
*                                                                               
CMMSL    DC    AL2(CMMSX-CMMSL)                                                 
CMMSG    DC    AL1(PSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(PSOQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(SHDQ),AL2(0,21),C'<RotationCommercials>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,20),C'<RotationCommercial>'                      
         DC    AL1(EORQ),AL2(4+20)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<CommercialCode>'                          
         DC    AL1(FVDQ),AL2(16,12),AL2(CPCML-CPD)                              
         DC    AL1(CONQ),AL2(28,17),C'</CommercialCode>'                        
         DC    AL1(EORQ),AL2(4+16+12+17)                                        
                                                                                
         DC    AL1(ENDQ)                                                        
CMMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL MESSAGE  NINS GEN                                                 
*================================================================               
*                                                                               
CNMSL    DC    AL2(CNMSX-CNMSL)                                                 
CNMSG    DC    AL1(PSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(PSOQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(SHDQ),AL2(0,21),C'<RotationCommercials>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,20),C'<RotationCommercial>'                      
         DC    AL1(EORQ),AL2(4+20)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<CommercialCode>'                          
         DC    AL1(FVDQ),AL2(16,12),AL2(CPCML-CPD)                              
         DC    AL1(CONQ),AL2(28,17),C'</CommercialCode>'                        
         DC    AL1(EORQ),AL2(4+16+12+17)                                        
                                                                                
         DC    AL1(SUFQ),AL2(0,21),C'</RotationCommercial>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(SENQ),AL2(0,22),C'</RotationCommercials>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
CNMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL DESCRIPTION MESSAGE (1,2,3)                                       
*================================================================               
*                                                                               
CDMSL    DC    AL2(CDMSX-CDMSL)                                                 
CDMSG    DC    AL1(CONQ),AL2(0,15),C'<CommercialName'                           
         DC    AL1(FVDQ),AL2(15,1),AL2(CDLNUM-CDD)                              
         DC    AL1(CONQ),AL2(16,1),C'>'                                         
         DC    AL1(FVDQ),AL2(17,34),AL2(0)                                      
         DC    AL1(CONQ),AL2(41,16),C'</CommercialName'                         
         DC    AL1(FVDQ),AL2(67,1),AL2(CDLNUM-CDD)                              
         DC    AL1(CONQ),AL2(18,1),C'>'                                         
         DC    AL1(EORQ),AL2(4+17+34+18)                                        
                                                                                
         DC    AL1(ENDQ)                                                        
CDMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  P/B PERCENTAGE MESSAGE                                                       
*================================================================               
*                                                                               
BPMSL    DC    AL2(BPMSX-BPMSL)                                                 
BPMSG    DC    AL1(SHDQ),AL2(0,21),C'<RotationCommercials>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,20),C'<RotationCommercial>'                      
         DC    AL1(EORQ),AL2(4+20)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<CommercialCode>'                          
         DC    AL1(FVDQ),AL2(16,12),AL2(CPCML-CPD)                              
         DC    AL1(CONQ),AL2(28,17),C'</CommercialCode>'                        
         DC    AL1(EORQ),AL2(4+16+12+17)                                        
                                                                                
         DC    AL1(ENDQ)                                                        
BPMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  COMMERCIAL ROTATION MESSAGE                                                  
*================================================================               
*                                                                               
CRMSL    DC    AL2(CRMSX-CRMSL)                                                 
CRMSG    DC    AL1(DSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'<RotationPosition>'                        
         DC    AL1(FVDQ),AL2(18,2),AL2(CRPOS-CRD)                               
         DC    AL1(CONQ),AL2(20,19),C'</RotationPosition>'                      
         DC    AL1(EORQ),AL2(4+18+2+19)                                         
                                                                                
         DC    AL1(SUFQ),AL2(0,21),C'</RotationCommercial>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(SENQ),AL2(0,22),C'</RotationCommercials>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
CRMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  P/B ROTATION MESSAGE                                                         
*================================================================               
*                                                                               
BRMSL    DC    AL2(BRMSX-BRMSL)                                                 
BRMSG    DC    AL1(SHDQ),AL2(0,21),C'<RotationCommercials>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,20),C'<RotationCommercial>'                      
         DC    AL1(EORQ),AL2(4+20)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<CommercialCode>'                          
         DC    AL1(FVDQ),AL2(16,12),AL2(CPCML-CPD)                              
         DC    AL1(CONQ),AL2(28,17),C'</CommercialCode>'                        
         DC    AL1(EORQ),AL2(4+16+12+17)                                        
                                                                                
*NOP     DC    AL1(SUFQ),AL2(0,22),C'</RotationCommercials>'                    
******   DC    AL1(EORQ),AL2(4+22)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
BRMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  IS BILLBOARD MESSAGE                                                         
*================================================================               
*                                                                               
IBMSL    DC    AL2(IBMSX-IBMSL)                                                 
IBMSG    DC    AL1(DSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<IsBillboard>'                             
         DC    AL1(FVDQ),AL2(13,1),AL2(IBYORN-IBD)                              
         DC    AL1(CONQ),AL2(14,14),C'</IsBillboard>'                           
         DC    AL1(EORQ),AL2(4+13+1+14)                                         
                                                                                
         DC    AL1(ENDQ)                                                        
IBMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  BILLBOARD POSITION MESSAGE                                                   
*================================================================               
*                                                                               
BSMSL    DC    AL2(BSMSX-BSMSL)                                                 
BSMSG    DC    AL1(CONQ),AL2(0,19),C'<BillboardPosition>'                       
         DC    AL1(FVDQ),AL2(19,4),AL2(IBYORN-IBD)                              
         DC    AL1(CONQ),AL2(23,20),C'</BillboardPosition>'                     
         DC    AL1(EORQ),AL2(4+19+4+20)                                         
* OR SUFQ ???                                                                   
         DC    AL1(SENQ),AL2(0,14),C'</UnitDetails>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
BSMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  BILLBOARD LENGTH MESSAGE                                                     
*================================================================               
*                                                                               
BLMSL    DC    AL2(BLMSX-BLMSL)                                                 
BLMSG    DC    AL1(DSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,28),C'<BillboardCommercialLength1>'              
         DC    AL1(FVDQ),AL2(28,3),AL2(BLSLN-BLD)                               
         DC    AL1(CONQ),AL2(31,29),C'</BillboardCommercialLength1>'            
         DC    AL1(EORQ),AL2(4+28+3+29)                                         
* OR SUFQ ???                                                                   
         DC    AL1(SENQ),AL2(0,14),C'</UnitDetails>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
BLMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  BILLBOARD COMMERCIAL MESSAGE                                                 
*================================================================               
*                                                                               
BCMSL    DC    AL2(BCMSX-BCMSL)                                                 
BCMSG    DC    AL1(DSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,26),C'<BillboardCommercialCode1>'                
         DC    AL1(FVDQ),AL2(26,12),AL2(BLSLN-BLD)                              
         DC    AL1(CONQ),AL2(38,27),C'</BillboardCommercialCode1>'              
         DC    AL1(EORQ),AL2(4+28+12+29)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,26),C'<BillboardCommercialCode2>'                
         DC    AL1(FVDQ),AL2(26,12),AL2(0)                                      
         DC    AL1(CONQ),AL2(38,27),C'</BillboardCommercialCode2>'              
         DC    AL1(EORQ),AL2(4+28+12+29)                                        
* OR SUFQ ???                                                                   
         DC    AL1(SENQ),AL2(0,14),C'</UnitDetails>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
BCMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  P/B ROTATION MAIN COMMERCIAL MESSAGE                                         
*================================================================               
*                                                                               
MCMSL    DC    AL2(MCMSX-MCMSL)                                                 
MCMSG    DC    AL1(DSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<MainCommercial>'                          
         DC    AL1(FVDQ),AL2(16,12),AL2(MCCML-MCD)                              
         DC    AL1(CONQ),AL2(28,17),C'</MainCommercial>'                        
         DC    AL1(EORQ),AL2(4+16+12+17)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<IsPiggyBack>'                             
         DC    AL1(FVDQ),AL2(13,1),AL2(0)                                       
         DC    AL1(CONQ),AL2(14,14),C'</IsPiggyBack>'                           
         DC    AL1(EORQ),AL2(4+13+1+14)                                         
                                                                                
         DC    AL1(SUFQ),AL2(0,21),C'</RotationCommercial>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
                                                                                
         DC    AL1(SENQ),AL2(0,22),C'</RotationCommercials>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
MCMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
* INSTRUCTION LINE ROTATION LIST MESSAGE                                        
*================================================================               
*                                                                               
RTMSL    DC    AL2(RTMSX-RTMSL)                                                 
RTMSG    DC    AL1(CONQ),AL2(0,18),C'<RotationSequence>'                        
         DC    AL1(FVDQ),AL2(18,60),AL2(ROT-RTD)                                
         DC    AL1(CONQ),AL2(78,19),C'</RotationSequence>'                      
         DC    AL1(EORQ),AL2(4+18+60+19)                                        
                                                                                
         DC    AL1(ENDQ)                                                        
RTMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
* UNIT ROTATION MESSAGE                                                         
*================================================================               
*                                                                               
URMSL    DC    AL2(URMSX-URMSL)                                                 
URMSG    DC    AL1(CONQ),AL2(0,14),C'<RotationDays>'                            
         DC    AL1(FVDQ),AL2(14,8),AL2(URROT-URD)                               
         DC    AL1(CONQ),AL2(22,15),C'</RotationDays>'                          
         DC    AL1(EORQ),AL2(4+14+8+15)                                         
                                                                                
         DC    AL1(ENDQ)                                                        
URMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  SCHEDULE SUMMARY MESSAGE OF PROGRAM ROTATIONS                                
*================================================================               
*                                                                               
SSMSL    DC    AL2(SSMSX-SSMSL)                                                 
SSMSG    DC    AL1(SUHQ),AL2(0,18),C'<RotationPrograms>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<RotationProgram>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,21),C'<RotationProgramCode>'                     
         DC    AL1(FVDQ),AL2(21,6),AL2(SSPRG-SSD)                               
         DC    AL1(CONQ),AL2(27,22),C'</RotationProgramCode>'                   
         DC    AL1(EORQ),AL2(4+21+6+22)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,21),C'<RotationProgramName>'                     
         DC    AL1(FVDQ),AL2(21,16),AL2(0)                                      
         DC    AL1(CONQ),AL2(37,22),C'</RotationProgramName>'                   
         DC    AL1(EORQ),AL2(4+21+16+22)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,24),C'<RotationProgramProduct>'                  
         DC    AL1(FVDQ),AL2(24,13),AL2(0)                                      
         DC    AL1(CONQ),AL2(37,25),C'</RotationProgramProduct>'                
         DC    AL1(EORQ),AL2(4+24+13+25)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,25),C'<RotationProgramDateTime>'                 
         DC    AL1(FVDQ),AL2(25,10),AL2(0)                                      
         DC    AL1(CONQ),AL2(35,26),C'</RotationProgramDateTime>'               
         DC    AL1(EORQ),AL2(4+25+10+26)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,21),C'<RotationProgramDays>'                     
         DC    AL1(FVDQ),AL2(14,7),AL2(0)                                       
         DC    AL1(CONQ),AL2(21,22),C'</RotationProgramDays>'                   
         DC    AL1(EORQ),AL2(4+21+7+22)                                         
                                                                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,21),C'<RotationProgramCost>'                     
         DC    AL1(FVDQ),AL2(21,14),AL2(0)                                      
         DC    AL1(CONQ),AL2(35,22),C'</RotationProgramCost>'                   
         DC    AL1(EORQ),AL2(4+21+14+22)                                        
         DC    AL1(EOPQ),AL2(0)                                                 
                                                                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,22),C'<RotationProgramUnits>'                    
         DC    AL1(FVDQ),AL2(14,3),AL2(0)                                       
         DC    AL1(CONQ),AL2(17,23),C'</RotationProgramUnits>'                  
         DC    AL1(EORQ),AL2(4+22+3+23)                                         
         DC    AL1(EOPQ),AL2(0)                                                 
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'</RotationProgram>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(SENQ),AL2(0,19),C'</RotationPrograms>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
*                                                                               
*** EMSQ DOES NOT WORK WELL WITH MULTIPLE ENTRIES!!!                            
*NOP     DC    AL1(EMSQ),AL2(0,19),C'</RotationPrograms>'                       
                                                                                
         DC    AL1(ENDQ)                                                        
SSMSX    EQU   *                                                                
                                                                                
*&&DO                                                                           
*================================================================               
*  TAG MESSAGE   WAITING ON DARREN                                              
*================================================================               
*                                                                               
TGMSL    DC    AL2(TGMSX-TGMSL)                                                 
TGMSG    DC    AL1(CONQ),AL2(0,12),C'<TagComment>'                              
         DC    AL1(EORQ),AL2(4+12)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,61),C'<instr:Line lineNumber="1">Unit wiX        
               th tag exists.</instr:Line>'                                     
         DC    AL1(EORQ),AL2(4+61)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'</TagComment>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
TGMSX    EQU   *                                                                
*&&                                                                             
*                                                                               
*================================================================               
*  TBA INSTRUCTION LINE MESSAGE                                                 
*================================================================               
*                                                                               
TBMSL    DC    AL2(TBMSX-TBMSL)                                                 
TBMSG    DC    AL1(PSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(PESQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
* MAKE IT CONQ SAME AS IN TNMSG ???                                             
         DC    AL1(SHDQ),AL2(0,17),C'<ScheduleSection>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,19),C'<RotationStartDate>'                       
         DC    AL1(FVDQ),AL2(19,10),AL2(TBPSTR-TBD)                             
         DC    AL1(CONQ),AL2(29,20),C'</RotationStartDate>'                     
         DC    AL1(EORQ),AL2(4+19+10+20)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<RotationEndDate>'                         
         DC    AL1(FVDQ),AL2(17,10),AL2(0)                                      
         DC    AL1(CONQ),AL2(27,18),C'</RotationEndDate>'                       
         DC    AL1(EORQ),AL2(4+17+10+18)                                        
                                                                                
         DC    AL1(SESQ),AL2(0,18),C'</ScheduleSection>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
TBMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  TBA (CUTIN) INSTRUCTION LINE MESSAGE                                         
*================================================================               
*                                                                               
TCMSL    DC    AL2(TCMSX-TCMSL)                                                 
TCMSG    DC    AL1(SHDQ),AL2(0,22),C'<UnassignedCutinUnits>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,7),C'<Units>'                                    
         DC    AL1(EORQ),AL2(4+7)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<AirDate="'                                
         DC    AL1(FVDQ),AL2(10,10),AL2(TCNADT-TBD)                             
         DC    AL1(CONQ),AL2(20,3),C'"/>'                                       
         DC    AL1(EORQ),AL2(4+10+10+3)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<MarketStationId>'                         
         DC    AL1(FVDQ),AL2(17,6),AL2(TCNMSID-TBD)                             
         DC    AL1(CONQ),AL2(23,18),C'</MarketStationId>'                       
         DC    AL1(EORQ),AL2(4+17+6+18)                                         
                                                                                
         DC    AL1(SUFQ),AL2(0,8),C'</Units>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
*NOP     DC    AL1(CONQ),AL2(0,8),C'</Units>'                                   
*****    DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(SENQ),AL2(0,23),C'</UnassignedCutinUnits>'                   
         DC    AL1(EORQ),AL2(4+23)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
TCMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  TBA (NINS GEN) INSTRUCTION LINE MESSAGE                                      
*================================================================               
*                                                                               
TNMSL    DC    AL2(TNMSX-TNMSL)                                                 
TNMSG    DC    AL1(PSSQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(PESQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(SHDQ),AL2(0,17),C'<ScheduleSection>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,19),C'<RotationStartDate>'                       
         DC    AL1(FVDQ),AL2(19,10),AL2(TNNADT-TBD)                             
         DC    AL1(CONQ),AL2(29,20),C'</RotationStartDate>'                     
         DC    AL1(EORQ),AL2(4+19+10+20)                                        
                                                                                
         DC    AL1(SESQ),AL2(0,18),C'</ScheduleSection>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
TNMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  PATTERN COMMENT  MESSAGE                                                     
*================================================================               
*                                                                               
PCMSL    DC    AL2(PCMSX-PCMSL)                                                 
PCMSG    DC    AL1(SHDQ),AL2(0,18),C'<ScheduleComments>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<CommentText>'                             
         DC    AL1(FVDQ),AL2(13,80),AL2(PCMNT-PCD)                              
         DC    AL1(CONQ),AL2(93,14),C'</CommentText>'                           
         DC    AL1(EORQ),AL2(4+13+80+14)                                        
                                                                                
         DC    AL1(SUFQ),AL2(0,19),C'</ScheduleComments>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
PCMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  SUB-FOOTER MESSAGE                                                           
*================================================================               
*                                                                               
         DS    0H                                                               
SFMSL    DC    AL2(SFMSX-SFMSL)                                                 
SFMSG    DC    AL1(VSFQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
SFMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  XML END MESSAGE                                                              
*================================================================               
*                                                                               
         DS    0H                                                               
XMEMSL   DC    AL2(XMEMSX-XMEMSL)                                               
XMEMSG   DC    AL1(CONQ),AL2(0,20),C'</TrafficHeaderInfo>'                      
         DC    AL1(EORQ),AL2(4+20)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
XMEMSX   EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  END MESSAGE                                                                  
*================================================================               
*                                                                               
         DS    0H                                                               
ENMSL    DC    AL2(ENMSX-ENMSL)                                                 
ENMSG    DC    AL1(VENQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
ENMSX    EQU   *                                                                
*                                                                               
*                                                                               
*================================================================               
*  END SCHEDULE MESSAGE                                                         
*================================================================               
*                                                                               
         DS    0H                                                               
ECMSL    DC    AL2(ECMSX-ECMSL)                                                 
ECMSG    DC    AL1(PESQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
ECMSX    EQU   *                                                                
*                                                                               
*                                                                               
*IF PREV=THESE, LATER CHECK IF STILL PROCESSING CMLS                            
         DS    0D                                                               
CMLTAB   DC    C'CM'                                                            
         DC    C'CB'               CML TBA                                      
         DC    C'CN'                                                            
         DC    C'BC'               BB CML                                       
         DC    C'BS'               BB POS                                       
         DC    C'CT'                                                            
         DC    C'MC'                                                            
         DC    C'IB'               IS BB                                        
         DC    C'CL'                                                            
         DC    C'BL'               BB LEN                                       
         DC    C'CD'                                                            
         DC    C'BR'                                                            
         DC    C'BP'                                                            
         DC    C'CG'                                                            
         DC    C'DT'                                                            
         DC    C'DD'                                                            
         DC    C'DY'                                                            
CMLTYLN  EQU   (*-CMLTAB)/2                                                     
*                                                                               
*                                                                               
*IF PREV=THESE, NO CML END MESSAGE                                              
         DS    0D                                                               
CMLFTAB  DC    C'CM'                                                            
         DC    C'CB'                                                            
         DC    C'CN'                                                            
         DC    C'IB'               IS BB                                        
         DC    C'BS'               BB POS                                       
         DC    C'BL'               BB LEN                                       
         DC    C'BC'               BB CML                                       
         DC    C'CG'                                                            
         DC    C'BR'                                                            
         DC    C'BP'                                                            
CMLFTYLN EQU   (*-CMLFTAB)/2                                                    
*                                                                               
*                                                                               
* UNIT DETAILS RECORDS                                                          
         DS    0D                                                               
UNTTAB   DC    C'UP'                                                            
         DC    C'RV'                                                            
         DC    C'UI'                                                            
         DC    C'UT'                                                            
         DC    C'UC'                                                            
         DC    C'IB'               IS BB                                        
         DC    C'BS'               BB POSN                                      
         DC    C'BC'               BB CML                                       
         DC    C'BL'               BB LEN                                       
         DC    C'ES'                                                            
         DC    C'PK'                                                            
UNTTYLN  EQU   (*-UNTTAB)/2                                                     
*                                                                               
*                                                                               
*                                                                               
* COMMENTS TABLE                                                                
*IF MATCH ON ANY OF THESE THEN DO NOT PRINT END MESSAGE YET                     
         DS    0D                                                               
CMTTAB   DC    C'DL'                                                            
         DC    C'FC'                                                            
*NOP     DC    C'PC'       <<< FIXES  NO HEAD ISSUE                             
         DC    C'DC'                                                            
         DC    C'CO'                                                            
         DC    C'RC'                                                            
CMTTYLN  EQU   (*-CMTTAB)/2                                                     
*                                                                               
*                                                                               
**********************************************************************          
*        FLAT FILE INSTRUCTION LINE RECORD TYPES                                
*        (ONE RECORD FOR EACH TYPE)                                             
**********************************************************************          
*                                                                               
         DS    0D                                                               
ILRTYP   DC    C'P1'                                                            
         DC    C'P2'                                                            
         DC    C'L1'                                                            
         DC    C'L2'                                                            
         DC    C'DY'                                                            
         DC    C'RD'                                                            
         DC    C'RF'                                                            
*NOP     DC    C'RG'                                                            
         DC    C'AP'                                                            
         DC    C'TG'                                                            
ILRTYLN  EQU   (*-ILRTYP)/2                                                     
*                                                                               
*                                                                               
**********************************************************************          
*        GROUP THESE ENTRIES UNDER ONE COMMERCIAL TAG                           
**********************************************************************          
*                                                                               
         DS    0D                                                               
CNEQTYP  DC    C'CD'                                                            
         DC    C'CL'                                                            
         DC    C'BC'               BB CML                                       
         DC    C'BS'               BB POS                                       
         DC    C'BL'               BB LEN                                       
         DC    C'IB'               IS BB                                        
         DC    C'CB'                                                            
         DC    C'CH'                                                            
         DC    C'CC'                                                            
         DC    C'CA'                                                            
         DC    C'DT'                                                            
         DC    C'DD'                                                            
         DC    C'DY'                                                            
         DC    C'LO'                                                            
         DC    C'TP'                                                            
         DC    C'CT'    ???                                                     
CNEQTYLN EQU   (*-CNEQTYP)/2                                                    
*                                                                               
*                                                                               
**********************************************************************          
* TABLE OF EQUIVALENT TYPES (EG. 'BP' IS EQUIV TO 'MP')                         
*********************************************************************           
*                                                                               
         DS    0D                                                               
ILEQTYP  DC    C'BP',C'CT'    P/B CML WITH COMMENTS                             
         DC    C'BR',C'CT'                                                      
         DC    C'BP',C'MP'                                                      
         DC    C'MP',C'BP'                                                      
         DC    C'BR',C'MR'                                                      
         DC    C'MR',C'BR'                                                      
ILEQTYLN  EQU   (*-ILEQTYP)/4                                                   
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*********************************************************************           
* FLAT FILE MULTIPLE RECORD OF EACH TYPE ON INSTRUCTION  LINE       *           
*     (2-THIS REC TYPE, 2-PREV REC TYPE, 1- Y=TRUE MATCH)           *           
*********************************************************************           
*                                                                               
         DS    0D                                                               
ILR1TYP  DC    C'BP',C'MP'                                                      
         DC    C'MP',C'BP'                                                      
         DC    C'BR',C'MR'                                                      
         DC    C'MR',C'BR'                                                      
         DC    C'CP',C'CP'                                                      
         DC    C'CR',C'CR'                                                      
         DC    C'PC',C'PC'                                                      
         DC    C'SS',C'SS'                                                      
ILR1TYLN  EQU   (*-ILR1TYP)/4                                                   
*                                                                               
*                                                                               
**********************************************************************          
*        FLAT FILE COMMERCIAL RECORD TYPES                                      
**********************************************************************          
*                                                                               
         DS    0D                                                               
CMLRTYP  DC    C'CM'                                                            
         DC    C'CB'                                                            
         DC    C'CN'                                                            
         DC    C'BC'               BB CML                                       
         DC    C'BS'               BB POS                                       
         DC    C'BP'                                                            
         DC    C'CG'                                                            
         DC    C'CD'                                                            
         DC    C'CL'                                                            
         DC    C'BL'               BB LEN                                       
         DC    C'CS'                                                            
         DC    C'CC'                                                            
         DC    C'CH'                                                            
         DC    C'CA'                                                            
         DC    C'CT'                                                            
         DC    C'DT'                                                            
         DC    C'DD'                                                            
         DC    C'PA'                                                            
         DC    C'DY'                                                            
         DC    C'CR'                                                            
         DC    C'RR'                                                            
         DC    C'MC'                                                            
         DC    C'IB'               IS BB                                        
         DC    C'TP'                                                            
         DC    C'LO'                                                            
         DC    C'BR'                                                            
CMLRTYLN EQU   (*-CMLRTYP)/2                                                    
*                                                                               
* SUB-FOOTER MESSAGE DRIVER                                                     
* IF MATCH ON ENTRIES NO MESSAGE NEEDED                                         
         DS    0D                                                               
CML1TYP  DC    C'CS'                                                            
         DC    C'CC'                                                            
         DC    C'CH'                                                            
         DC    C'CL'                                                            
         DC    C'LO'                                                            
         DC    C'PA'                                                            
         DC    C'RR'                                                            
         DC    C'CR'    <<<<                                                    
         DC    C'TP'                                                            
         DC    C'DT'                                                            
         DC    C'DD'                                                            
         DC    C'DY'                                                            
         DC    C'CT'                                                            
         DC    C'MC'                                                            
CML1TYLN EQU   (*-CML1TYP)/2                                                    
*                                                                               
*                                                                               
*                                                                               
**********************************************************************          
*        FLAT FILE TBA RECORD TYPES                                             
**********************************************************************          
*                                                                               
         DS    0D                                                               
TBARTYP  DC    C'P1'                                                            
         DC    C'P2'                                                            
         DC    C'L1'                                                            
         DC    C'L2'                                                            
         DC    C'ST'                                                            
TBARTYLN EQU   (*-TBARTYP)/2                                                    
*                                                                               
**********************************************************************          
*        FLAT FILE ROUTINES TABLE                                               
**********************************************************************          
*                                                                               
FFROUT   DS    0D                                                               
*                                                                               
FLATAB   DC    C'HS',AL4(AHSMSG) HEADER USER NAME/ADDRESS                       
         DC    C'HU',AL4(AHUMSG) HEADER SEND DATE                               
         DC    C'HC',AL4(AHCMSG) HEADER CONTACT (ISSUED BY)                     
         DC    C'HN',AL4(AHNMSG) HEADER NETWORK                                 
         DC    C'AD',AL4(AADMSG) ADVERTISER                                     
         DC    C'HP',AL4(AHPMSG) HEADER PERIOD                                  
         DC    C'PR',AL4(APRMSG) L HEADER PRODUCT                               
         DC    C'GR',AL4(AGRMSG) HEADER PGROUP                                  
         DC    C'PD',AL4(APDMSG) HEADER PRODUCT                                 
         DC    C'VR',AL4(AVRMSG) HEADER PRODUCT VARIOUS                         
         DC    C'FD',AL4(AFDMSG) FEED DEFINITION                                
         DC    C'CI',AL4(ACIMSG) CUTIN DEFINITION                               
         DC    C'CO',AL4(ACOMSG) HEADER COMMENT                                 
         DC    C'RC',AL4(ARCMSG) REVISION COMMENT                               
         DC    C'DC',AL4(ADCMSG) PRODUCT LEVEL COMMENT                          
         DC    C'FC',AL4(AFCMSG) FOOTER COMMENT                                 
         DC    C'DL',AL4(ADLMSG) DISTRIBUTION LIST                              
         DC    C'CM',AL4(ACMMSG) COMMERCIAL DEFINITION                          
         DC    C'CN',AL4(ACNMSG) COMML DEFINITION NINS GEN                      
         DC    C'CB',AL4(ACBMSG) COMMERCIAL TBA                                 
         DC    C'CD',AL4(ACDMSG) COMMERCIAL DESCRIPTION                         
         DC    C'CH',AL4(ACHMSG) COMMERCIAL HIDEF                               
         DC    C'CC',AL4(ACCMSG) COMMERCIAL CENTRCUT                            
         DC    C'CA',AL4(ACAMSG) COMMERCIAL ALTERNATE                           
         DC    C'DT',AL4(ADTMSG) COMMERCIAL DESTROY TIME                        
         DC    C'DD',AL4(ADDMSG) COMMERCIAL DESTROY DATE                        
         DC    C'CT',AL4(ACTMSG) COMTEXT                                        
         DC    C'CL',AL4(ACLMSG) COMMERCIAL LENGTH                              
         DC    C'LO',AL4(ALOMSG) COMMERCIAL LENGTH OVERRIDE                     
         DC    C'TP',AL4(ATPMSG) COMMERCIAL TYPE                                
         DC    C'PG',AL4(APGMSG) PROGRAM                                        
         DC    C'PI',AL4(APIMSG) PROGRAM INFO DETAIL RECORD                     
         DC    C'DG',AL4(ADGMSG) DIFFERENT PROGRAM INFO                         
         DC    C'TB',AL4(ATBMSG) TBA                                            
         DC    C'TN',AL4(ATNMSG) TBA (NINS GEN)                                 
         DC    C'IL',AL4(AILMSG) INSTRUCTION LINE                               
         DC    C'IN',AL4(AINMSG) INSTRUCTION LINE (NINS GEN)                    
         DC    C'NT',AL4(ANTMSG) NATIONAL UNIT (NINS GEN)                       
         DC    C'UP',AL4(AUPMSG) UNIT POSITION (NINS GEN)                       
         DC    C'RV',AL4(ARVMSG) UNIT REVISION (NINS GEN)                       
         DC    C'UI',AL4(AUIMSG) UNIT CML INFO NEW/CHG                          
         DC    C'UT',AL4(AUTMSG) UNIT TYPE BONUS..(NINS GEN)                    
         DC    C'UC',AL4(AUCMSG) UNIT COST (NINS GEN)                           
         DC    C'IB',AL4(AIBMSG) IS BILLBOARD (NINS GEN)                        
         DC    C'BS',AL4(ABSMSG) BILLBOARD POSN (NINS GEN)                      
         DC    C'BC',AL4(ABCMSG) BILLBOARD CML (NINS GEN)                       
         DC    C'BL',AL4(ABLMSG) BILLBOARD LEN(NINS GEN)                        
         DC    C'ES',AL4(AESMSG) UNIT ESTIMATE (NINS GEN)                       
         DC    C'PK',AL4(APKMSG) UNIT PACKAGE (NINS GEN)                        
         DC    C'IC',AL4(AICMSG) INSTR LINE CUTIN (NINS GEN)                    
         DC    C'RD',AL4(ARDMSG) DPT RESTRIC INSTR LINE                         
         DC    C'RF',AL4(ARFMSG) FEED RESTRIC INSTR LINE                        
         DC    C'AP',AL4(AAPMSG) ALL PROGRAMS INSTRUC LINE                      
         DC    C'P1',AL4(AP1MSG) SINGLE PROD INSTRUC LINE                       
         DC    C'P2',AL4(AP2MSG) P/B PROD INSTRUCTION LINE                      
         DC    C'L1',AL4(AL1MSG) SINGLE LEN INSTRUCTION LINE                    
         DC    C'L2',AL4(AL2MSG) P/B LEN INSTRUCTION LINE                       
         DC    C'DY',AL4(ADYMSG) TIMES DAILY                                    
         DC    C'PA',AL4(APAMSG) COMML PERCENTAGE ALLOC                         
         DC    C'RR',AL4(ARRMSG) COMML ROTATION POSITION                        
***      DC    C'CP',AL4(ACPMSG) COMMERCIAL PERCENTAGE                          
         DC    C'CR',AL4(ACRMSG) COMMERCIAL ROTATION                            
         DC    C'BP',AL4(ABPMSG) P/B PERCENTAGE                                 
         DC    C'CG',AL4(ABPMSG) P/B NINS GEN                                   
         DC    C'BR',AL4(ABRMSG) P/B ROTATION                                   
         DC    C'MC',AL4(AMCMSG) P/B MAIN COMMERCIAL                            
         DC    C'RT',AL4(ARTMSG) ROTATION LIST                                  
         DC    C'UR',AL4(AURMSG) UNIT ROTATION                                  
         DC    C'PC',AL4(APCMSG) PATTERN COMMENT                                
         DC    C'SS',AL4(ASSMSG) SCHEDULE SUMMARY                               
         DC    C'ST',AL4(ASSMSG) TBA SCHEDULE SUMMARY                           
FLATABLN EQU   (*-FLATAB)/6                                                     
*                                                                               
*                                                                               
BXFMS    DC     A(BXFMSL)          BXF MESSAGE                                  
AHSMSG   DC     A(HSMSL)           HEADER SEND MESSAGE                          
*NSMSG   DC     A(NSMSL)           HEADER SEND MESSAGE (NINS GEN)               
AHUMSG   DC     A(HUMSL)           HEADER USER MESSAGE                          
AHCMSG   DC     A(HCMSL)           HEADER CONTACT                               
AHNMSG   DC     A(HNMSL)           HEADER NETWORK                               
AADMSG   DC     A(ADMSL)           ADVERTISER                                   
AHPMSG   DC     A(HPMSL)           HEADER PERIOD                                
APRMSG   DC     A(PRMSL)           HEADER PRODUCT                               
AVRMSG   DC     A(VRMSL)           HEADER PRODUCT VARIOUS                       
AGRMSG   DC     A(GRMSL)           HEADER PGROUP                                
ACIMSG   DC     A(CIMSL)           HEADER CUTIN MKT/STA                         
*DPMSG   DC     A(DPMSL)           DAYPART                                      
AFDMSG   DC     A(FDMSL)           FEED                                         
ACOMSG   DC     A(COMSL)           HEADER COMMENT                               
ARCMSG   DC     A(RCMSL)           REVISION COMMENT                             
ADCMSG   DC     A(DCMSL)           PRODUCT LEVEL COMMENT                        
AFCMSG   DC     A(FCMSL)           FOOTER COMMENT                               
ADLMSG   DC     A(DLMSL)           DISTRIBUTION LIST                            
ACMMSG   DC     A(CMMSL)           COMMERCIAL DEFINITION                        
ACNMSG   DC     A(CNMSL)           COMMERCIAL DEFINITION NIN GEN                
ACBMSG   DC     A(CBMSL)           COMMERCIAL TBA                               
ACDMSG   DC     A(CDMSL)           COMMERCIAL DESCRIPTION                       
*CSMSG   DC     A(CSMSL)  *NOP     COMMERCIAL STANDARD CODE                     
ACHMSG   DC     A(CHMSL)           COMMERCIAL HIDEF                             
ACCMSG   DC     A(CCMSL)           COMMERCIAL CENTRCUT                          
ACAMSG   DC     A(CAMSL)           COMMERCIAL ALTERNATE                         
ATPMSG   DC     A(TPMSL)           COMMERCIAL TYPE                              
ADTMSG   DC     A(DTMSL)           COMMERCIAL DESTROY DATE/TIME                 
ADDMSG   DC     A(DDMSL)           COMMERCIAL DESTROY DATE                      
ACTMSG   DC     A(CTMSL)           COMTEXT                                      
ACLMSG   DC     A(CLMSL)           COMMERCIAL LENGTH                            
ALOMSG   DC     A(LOMSL)           COMMERCIAL LENGTH OVERRIDE                   
APGMSG   DC     A(PGMSL)           PROGRAM                                      
APIMSG   DC     A(PIMSL)           PROGRAM INFO DETAILS (NINS GEN)              
APDMSG   DC     A(PDMSL)           HEADER PRODUCT RECORD                        
ADGMSG   DC     A(DGMSL)           DIFFERENT PROGRAM INFO (NINS GEN)            
*ATGMSG   DC     A(TGMSL)           TAG                                         
ATBMSG   DC     A(TBMSL)           TBA                                          
ATNMSG   DC     A(TNMSL)           TBA (NINS GEN)                               
ATCMSG   DC     A(TCMSL)           TBA CUTINS (NINS GEN)                        
AILMSG   DC     A(ILMSL)           INSTRUCTION LINE                             
AINMSG   DC     A(INMSL)           INSTRUCTION LINE (NINS GEN)                  
ANTMSG   DC     A(NTMSL)           NATIONAL UNIT (NINS GEN)                     
AUPMSG   DC     A(UPMSL)           UNIT POSITION (NINS GEN)                     
ARVMSG   DC     A(RVMSL)           UNIT REVISION NO.(NINS GEN)                  
AUIMSG   DC     A(UIMSL)           UNIT CML INFO (NINS GEN)                     
AUTMSG   DC     A(UTMSL)           UNIT TYPE (NINS GEN)                         
AUCMSG   DC     A(UCMSL)           UNIT COST (NINS GEN)                         
AIBMSG   DC     A(IBMSL)           IS BILLBOARD (NINS GEN)                      
ABSMSG   DC     A(BSMSL)           BB POSN (NINS GEN)                           
ABCMSG   DC     A(BCMSL)           BB CML    (NINS GEN)                         
ABLMSG   DC     A(BLMSL)           BB LEN    (NINS GEN)                         
AESMSG   DC     A(ESMSL)           UNIT ESTIMATE (NINS GEN)                     
APKMSG   DC     A(PKMSL)           UNIT PACKAGE (NINS GEN)                      
AICMSG   DC     A(ICMSL)           INSTRUCTION LINE CUTIN (NINS GEN)            
ARFMSG   DC     A(RFMSL)           FEED RESTRICT INSTRUCTION LINE               
ARDMSG   DC     A(RDMSL)           DAYPART RESTRICT INSTRUCTION LINE            
AAPMSG   DC     A(APMSL)           ALL PROGRAM INSTRUCTION LINE                 
AP1MSG   DC     A(P1MSL)           SINGLE PROD INSTRUCTION LINE                 
AP2MSG   DC     A(P2MSL)           P/B PROD INSTRUCTION LINE                    
AL1MSG   DC     A(L1MSL)           SINGLE LEN INSTRUCTION LINE                  
AL2MSG   DC     A(L2MSL)           P/B LEN INSTRUCTION LINE                     
ADYMSG   DC     A(DYMSL)           RUN TIMES DAILY                              
*CPMSG   DC     A(CPMSL)           COMMERCIAL PERCENTAGE                        
APAMSG   DC     A(PAMSL)           PERCENTAGE ALLOCATION                        
ARRMSG   DC     A(RRMSL)           ROTATION                                     
ACRMSG   DC     A(CRMSL)           COMMERCIAL ROTATION                          
ABPMSG   DC     A(BPMSL)           P/B PERCENTAGE                               
ABRMSG   DC     A(BRMSL)           P/B ROTATION                                 
AMCMSG   DC     A(MCMSL)           P/B MAIN COMMERCIAL                          
ARTMSG   DC     A(RTMSL)           ROTATION LIST                                
AURMSG   DC     A(URMSL)           UNIT ROTATION (NINS GEN)                     
APCMSG   DC     A(PCMSL)           PATTERN COMMENT                              
ASSMSG   DC     A(SSMSL)           SCHEDULE SUMMARY                             
AENDMSG  DC     A(ENMSL)           END MESSAGE                                  
AECMSG   DC     A(ECMSL)           END SCHEDULE MESSAGE                         
ASUFMSG  DC     A(SFMSL)           SUB-FOOTER MSG                               
AXMEMSG  DC     A(XMEMSL)          SUB-FOOTER MSG                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*==================================================                             
* XML WRAP ROUTINE FOR ALL MESSAGES                                             
*==================================================                             
*                                                                               
XMWP     NTR1                                                                   
         L     R4,AFRECP4          POINT TO ACTUAL DATA                         
         LR    R3,R1               ADDRESS OF XML                               
         SR    R2,R2                                                            
         ICM   R2,3,0(R3)          LEN OF XML MAP DATA                          
         LA    R3,2(R3)            XML MAP CODE (PAST LEN)                      
         BRAS  RE,PUTR                                                          
*                                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
*==================================================                             
* SUB-FOOTER MESSAGE                                                            
*==================================================                             
*                                                                               
PSUFMSG  NTR1                                                                   
         LARL  RE,SFMSL                                                         
         SR    R2,R2                                                            
         ICM   R2,3,0(RE)          MESSAGE LEN                                  
         LARL  R3,SFMSG            POINT TO BXF MESSAGE                         
         BRAS  RE,PUTR                                                          
*                                                                               
         XC    SVSUFMSG,SVSUFMSG   CLEAR SUB-FOOTER MSG                         
         NI    NEWTYP,X'FF'-SUBFSW   AND FLAG                                   
*                                                                               
         J     EXIT                                                             
*                                                                               
*==================================================                             
* END MESSAGE                                                                   
*==================================================                             
*                                                                               
PENDMSG  NTR1                                                                   
*                                                                               
         LARL  RE,ENMSL            MESSAGE LEN                                  
         SR    R2,R2                                                            
         ICM   R2,3,0(RE)          MESSAGE LEN                                  
         LARL  R3,ENMSG            POINT TO BXF MESSAGE                         
         BRAS  RE,PUTR                                                          
*                                                                               
         XC    SVENDMSG,SVENDMSG   CLEAR END MSG AREA                           
*                                                                               
         J     EXIT                                                             
*                                                                               
*==================================================                             
* END SCHEDULE MESSAGE                                                          
*==================================================                             
*                                                                               
PESMSG   NTR1                                                                   
*                                                                               
         LARL  RE,ECMSL                                                         
         SR    R2,R2                                                            
         ICM   R2,3,0(RE)          MESSAGE LEN                                  
         LARL  R3,ECMSG            POINT TO BXF MESSAGE                         
         BRAS  RE,PUTR                                                          
*                                                                               
         XC    SVESMSG,SVESMSG     CLEAR END SCHEDULE MSG AREA                  
*                                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
*==================================================                             
* XML END MESSAGE                                                               
*==================================================                             
*                                                                               
XMLENDMS NTR1                                                                   
*                                                                               
         LARL  RE,XMEMSL                                                        
         SR    R2,R2                                                            
         ICM   R2,3,0(RE)          MESSAGE LEN                                  
         LARL  R3,XMEMSG           POINT TO BXF MESSAGE                         
         BRAS  RE,PUTR                                                          
*                                                                               
         J     EXIT                                                             
*                                                                               
*==================================================                             
* PROCESS ROUTIN FROM MAP TABLE                                                 
*==================================================                             
PROUT    NTR1                                                                   
*                                                                               
*SAVE XML MAP RECORD TO WORKING STORAGE                                         
         LA    R0,XMLBUF           CLEAR                                        
         LA    R1,L'XMLBUF                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R0,ACENTRY          A(XML MAP)                                   
         A     R0,RELO             MOVE FROM                                    
         LR    RF,R0                                                            
         SHI   RF,2                POINT TO LEN                                 
         SR    R3,R3                                                            
         ICM   R3,3,0(RF)          R3=LEN                                       
         LR    RF,R3                                                            
                                                                                
         LA    RE,XMLBUF           MOVE TO                                      
         BCTR  RF,0                                                             
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
                                                                                
         LA    RE,XMLBUF           A(XML MAP)                                   
         SR    RF,RF                                                            
         ICM   RF,3,SVDISP         DISP                                         
         AR    RF,RE               POINT TO XML LABLE TO PROCESS                
         LR    R3,RF               POINT TO ROUTIN IN MAP TABLE                 
         ST    R3,AROUT                                                         
         MVI   AROUT,X'FF'         PROCESSING AROUT                             
         BRAS  RE,PUTR                                                          
*                                                                               
         XC    AROUT,AROUT                                                      
         NI    NEWTYP,X'FF'-TYPNEQ  RESET SUB-TYPE CHANGED                      
*                                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
*********************************************************************           
* GENERATE XML RECORDS FROM LINE ITEM                               *           
*  NTRY R2= XML MAP RECORD LEN                                      *           
*       R3=A(XML MAP RECORD)                                        *           
*       R4=A(DATA ON FLAT FILE)                                     *           
*********************************************************************           
         USING MAPD,R3                                                          
PUTR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   AROUT,X'FF'         PROCESSING AROUT?                            
         BE    PUTR10                                                           
         ST    R3,ANENTRY          SAVE ADDRESS OF NEW ENTRY                    
*                                                                               
*SAVE XML MAP RECORD TO WORKING STORAGE                                         
         LA    R0,XMLBUF           CLEAR                                        
         LA    R1,L'XMLBUF                                                      
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    RE,XMLBUF           XML WORKING STORAGE                          
         BCTR  R2,0                                                             
         LR    RF,R2                                                            
         LR    R0,R3               MOVE FROM                                    
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         LA    R3,XMLBUF           POINT TO XML MAP IN WORKING STORAGE          
                                                                                
PUTR10   L     R0,AMAP             CLEAR IO TO SPACES                           
         LA    R1,L'MAP                                                         
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AMAP                                                          
         XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)            PAST RECORD LENGTH BYTES                     
*                                                                               
PUTR12   DS    0H                                                               
         CLI   MAPTYP,SESQ         SAVE END SCHEDULE MESSAGE                    
         BNE   PUTR18                                                           
*                                                                               
         LA    R5,SVESMSG          POINT TO SAVE MSG AREA                       
         B     PUTR55                                                           
*                                                                               
PUTR18   CLI   MAPTYP,EMSQ         END MESSAGE CONSTANT                         
         BNE   PUTR20                                                           
*                                                                               
         OC    AROUT,AROUT         ANY THING TO PROCESS                         
         BZ    *+16                                                             
         TM    NEWTYP,TYPNEQ       HAS REC TYPE CHANGED                         
         BO    PUTR50               YES, PUT OUT END MSG                        
         B     PUTRX                                                            
                                                                                
         ST    R3,AROUT            SAVE ADDRESS OF ROUTINE                      
         LA    RE,XMLBUF                                                        
         LR    RF,R3                                                            
         SR    RF,RE               DISPLACEMENT                                 
         STCM  RF,3,SVDISP                                                      
         MVC   ACENTRY,ANENTRY     SAVE ADDRESS OF CURR ENTRY                   
         B     PUTRX                                                            
*                                                                               
PUTR20   CLI   MAPTYP,FVDQ         VAR LEN DATA FROM FILE                       
         BNE   PUTR25                                                           
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,3,MAPFLD                                                      
         AR    R5,R2               R5=A(OUTPUT FIELD)                           
                                                                                
         L     RF,AFREC                                                         
         SR    R1,R1                                                            
         ICM   R1,3,0(RF)          RECORD LEN                                   
         AR    RF,R1                                                            
         ST    RF,EOR              SAVE A(END OF RECORD)                        
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,MAPDATA        DISPLACEMENT TO DATA ON FILE                 
         LTR   RE,RE                                                            
         BNZ   *+14                                                             
         L     R1,ANEXTPOS         A(NEXT POS) TO DATA ON FILE                  
         LR    RE,R1               SAVE START OF DATA                           
         B     PF06                                                             
*                                                                               
         AR    RE,R4               POINT TO START OF DATA ON FILE               
         LR    R1,RE                                                            
*                                                                               
PF06     CLI   0(R1),FDELIM                                                     
         BE    PF06D                                                            
*                                                                               
         C     R1,EOR                                                           
         BNL   PUTR50              FIELD DELIMETER MISSING                      
*                                                                               
         LA    R1,1(R1)            NEXT POSITION                                
         B     PF06                                                             
*                                                                               
PF06D    LR    RF,R1               SAVE END OF DATA                             
                                                                                
         LA    R1,1(R1)            BUMP PAST SEMI-COLON                         
         ST    R1,ANEXTPOS         SAVE A(NEXT POSITION)TO DATA ON FILE         
                                                                                
         SR    RF,RE               GET LENGTH OF DATA                           
         BP    PF07                IF FIELD LEN=0                               
         MVI   VLEN,1              TREAT AS 1                                   
         MVI   VTXT,C' '                                                        
         B     PF08                                                             
*                                                                               
PF07     STC   RF,VLEN             LENGTH OF VARIABLE FIELD                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   VTXT(0),0(RE)       SAVE TEXT                                    
*                                                                               
PF08     ICM   R6,3,MAPLEN         R6=LENGTH OF DATA                            
         STC   R6,FLEN                                                          
         CLC   VLEN,FLEN                                                        
         BNH   *+6                                                              
         DC    H'0'                FIELD DATA TOO LONG                          
*                                                                               
         LLC   RF,VLEN             MOVE VARIABLE TEXT                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,(R5)),VTXT                                                   
*                                                                               
         BAS   RE,FIXBUMP          FIX DISP TO OUTPUT AND BUMP                  
         B     PUTR80                                                           
*                                                                               
PUTR25   DS    0H                                                               
         CLI   MAPTYP,SOPQ         START OF OPTIONAL FIELD                      
         BNE   PUTR28                                                           
*                                                                               
         ICM   R5,3,MAPFLD         SAVE DISP TO OUTPUT                          
*                                                                               
         LA    R3,3(R3)            BUMP TO NEXT RECORD TYPE                     
         LR    R6,R3               SAVE PT TO RECORD IN MAP                     
*                                                                               
         BAS   RE,FDATA            FIND OPTIONAL DATA                           
         BNE   PUTR12              DATA NOT FOUND                               
*                                                                               
         LR    R3,R6               RESTORE PT IN MAP                            
         B     PUTR12              PROCESS THIS RECORD                          
*                                                                               
PUTR28   CLI   MAPTYP,EOPQ         END OF OPTIONAL FIELD                        
         BNE   PUTR40                                                           
         LA    R3,3(R3)            BUMP TO NEXT REC IN MAP                      
         B     PUTR12                                                           
*                                                                               
PUTR40   CLI   MAPTYP,VSFQ         VAR LEN SUB-FOOTER MSG                       
         BNE   PUTR40A                                                          
* GET MESSAGE LENGTH                                                            
         LA    RE,SVSUFMSG         POINT TO MESSAGE                             
         LA    R1,L'SVSUFMSG-1(RE)                                              
         B     PUTR41                                                           
*                                                                               
PUTR40A  CLI   MAPTYP,DSSQ         DELETE SUB-FOOTER MESSAGE                    
         BNE   PUTR40B                                                          
         XC    SVSUFMSG,SVSUFMSG                                                
         B     PUTR40E                                                          
*                                                                               
PUTR40B  CLI   MAPTYP,PSSQ         PRINT SUB-FOOTER MESSAGE                     
         BNE   PUTR40B1                                                         
         OC    SVESMSG,SVESMSG     ONLY IF THERE IS ANY END MSG                 
         BZ    PUTR40E                                                          
         OC    SVSUFMSG,SVSUFMSG   ANYTHING TO PRINT                            
         BZ    PUTR40E                                                          
         LA    RE,SVSUFMSG         POINT TO MESSAGE                             
         LA    R1,L'SVSUFMSG-1(RE)                                              
         B     PUTR41                                                           
*                                                                               
PUTR40B1 CLI   MAPTYP,PSOQ         PRINT SUB-OUTER-FOOTER MESSAGE               
         BNE   PUTR40D                                                          
         OC    SVESMSG,SVESMSG     ONLY IF THERE IS ANY END MSG                 
         BZ    PUTR40E                                                          
         OC    SVENDMSG,SVENDMSG   ANYTHING TO PRINT                            
         BZ    PUTR40E                                                          
         LA    RE,SVENDMSG         POINT TO MESSAGE                             
         LA    R1,L'SVENDMSG-1(RE)                                              
         B     PUTR41                                                           
*                                                                               
PUTR40D  CLI   MAPTYP,PESQ         PRINT END SCHEDULE MESSAGE                   
         BNE   PUTR40G                                                          
         OC    SVESMSG,SVESMSG     ANYTHING TO PRINT                            
         BNZ   *+12                                                             
PUTR40E  BAS   RE,FIXBUMP                                                       
         B     PUTR90                                                           
                                                                                
         LA    RE,SVESMSG          POINT TO MESSAGE                             
         LA    R1,L'SVESMSG-1(RE)                                               
         B     PUTR41                                                           
*                                                                               
PUTR40G  CLI   MAPTYP,VENQ         VAR LEN END MESSAGE                          
         BNE   PUTR50                                                           
*                                                                               
* GET MESSAGE LENGTH                                                            
         LA    RE,SVENDMSG         POINT TO MESSAGE                             
         LA    R1,L'SVENDMSG-1(RE)                                              
PUTR41   CLI   0(R1),X'40'                                                      
         BH    PUTR42                                                           
         CLI   0(R1),C'>'          END OF MSG                                   
         BE    PUTR42                                                           
         BCTR  R1,0                                                             
         B     PUTR41                                                           
*                                                                               
PUTR42   AHI   R1,1                ADJ LEN                                      
         SR    R1,RE               MESSAGE LEN                                  
         BP    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                                                               
         STCM  R1,3,MAPLEN         SAVE MSG LEN                                 
*                                                                               
         STC   R1,VLEN             LENGTH OF VARIABLE FIELD                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   VTXT(0),0(RE)       SAVE TEXT                                    
*                                                                               
         LLC   RF,VLEN             MOVE VARIABLE TEXT                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,(R2)),VTXT      OUTPUT                                       
*                                                                               
         CLI   MAPTYP,PSSQ         PRINT SUB-FOOTER MESSAGE                     
         BNE   *+10                                                             
         XC    SVSUFMSG,SVSUFMSG   CLEAR                                        
*                                                                               
         CLI   MAPTYP,PSOQ         PRINT SUB-OUTER-FOOTER MESSAGE               
         BNE   *+10                                                             
         XC    SVENDMSG,SVENDMSG   CLEAR                                        
*                                                                               
         CLI   MAPTYP,PESQ         PRINT END SCHEDULE MESSAGE                   
         BNE   *+10                                                             
         XC    SVESMSG,SVESMSG     CLEAR                                        
*                                                                               
         BAS   RE,FIXBUMP          FIX DISP TO OUTPUT AND BUMP IN MAP           
         B     PUTR80                                                           
*                                                                               
* DONE WITH THIS RECORD                                                         
*                                                                               
PUTR50   SR    R5,R5                                                            
         ICM   R5,3,MAPFLD                                                      
         AR    R5,R2               R5=A(OUTPUT FIELD)                           
         SR    R6,R6                                                            
         ICM   R6,3,MAPLEN         R6=LENGTH OF DATA                            
         STC   R6,VLEN                                                          
*                                                                               
         CLI   MAPTYP,SHDQ         CHK TO SKIP HEADER MESSAGE                   
         BNE   PUTR52                                                           
*                                                                               
         TM    NEWTYP,HEADSW       PRINT HEADING                                
         BZ    *+16                                                             
         NI    NEWTYP,X'FF'-HEADSW                                              
         OI    NEWTYP,SUBHSW       PRINT SUB-HEADING                            
         B     PUTR55                                                           
*                                                                               
         BAS   RE,FIXBUMP          NO, BYPASS THIS RECORD                       
         B     PUTR90                                                           
*                                                                               
PUTR52   CLI   MAPTYP,SENQ         SAVE END MESSAGE                             
         BNE   PUTR52C                                                          
         XC    SVENDMSG,SVENDMSG                                                
         LA    R5,SVENDMSG         POINT TO SAVE MSG AREA                       
         B     PUTR55                                                           
*                                                                               
PUTR52C  CLI   MAPTYP,SUFQ         SUB-FOOTER MSG                               
         BNE   PUTR52E                                                          
         XC    SVSUFMSG,SVSUFMSG                                                
         LA    R5,SVSUFMSG         POINT TO SAVE MSG AREA                       
         B     PUTR55                                                           
*                                                                               
PUTR52E  CLI   MAPTYP,SUHQ         SUB-HEADING                                  
         BNE   PUTR53                                                           
*                                                                               
         TM    NEWTYP,SUBHSW                                                    
         BZ    *+12                                                             
         NI    NEWTYP,X'FF'-SUBHSW TURN OFF PRT SUB-HEADING                     
         B     PUTR55                                                           
                                                                                
         BAS   RE,FIXBUMP                                                       
         B     PUTR90                                                           
*                                                                               
PUTR53   CLI   MAPTYP,EMSQ         END MSG CONSTANT ?                           
         BE    *+12                                                             
         CLI   MAPTYP,CONQ         IS DATA A CONSTANT ?                         
         BNE   PUTR60                                                           
                                                                                
PUTR55   BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),MAPCON      MOVE CONSTANT TO OUTPUT                      
                                                                                
         CLI   MAPTYP,SESQ         SAVE END SCHEDULE MESSAGE                    
         BE    PUTRX                                                            
         CLI   MAPTYP,SENQ         SAVE END MESSAGE                             
         BNE   PUTR58                                                           
         CLC   =C'CN',0(R4)        CANT EXIT JUST YET                           
         BE    PUTR58C                                                          
         CLC   =C'CT',0(R4)        CANT EXIT JUST YET                           
         BE    PUTR58C             EMSQ TO FOLLOW                               
         B     PUTRX                                                            
PUTR58   CLI   MAPTYP,SUFQ         SUB-FOOTER MSG                               
         BNE   *+12                                                             
PUTR58C  BAS   RE,FIXBUMP          FIX DISP TO OUTPUT AND BUMP IN MAP           
         B     PUTR90              DO NOT ADD TO FILE YET                       
                                                                                
         BAS   RE,FIXBUMP          FIX DISP TO OUTPUT AND BUMP IN MAP           
         B     PUTR80                                                           
*                                                                               
PUTR60   CLI   MAPTYP,EORQ                                                      
         BE    PUTR80                                                           
         CLI   MAPTYP,ENDQ         ARE WE ENDING OUTER LOOP                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,1(R3)                                                         
*                                                                               
PUTR80   CLI   0(R3),EORQ          END OF RECORD                                
         BNE   PUTR12                                                           
*                                                                               
         L     R2,AMAP                                                          
                                                                                
         CLI   OUTMODE,C'D'        OUTPUT TO DATASET                            
         BNE   PUTR88                                                           
                                                                                
         MVC   0(2,R2),1(R3)       SET RECORD LENGTH                            
                                                                                
         PUT   XMLOUT,(R2)                                                      
         B     PUTR90                                                           
                                                                                
PUTR88   ICM   RF,3,1(R3)                                                       
         BCTR  RF,0                ADJ LEN FOR EX MVC                           
         EX    RF,MVCPRT                                                        
         MVI   LINE,2            <<<<<<<<<  PREVENT HOOKS                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     *+10                                                             
MVCPRT   MVC   P(0),4(R2)                                                       
*                                                                               
PUTR90   DS    0H                                                               
         LA    R3,3(R3)                                                         
         CLI   0(R3),ENDQ          TEST END OF LOOP                             
         BNE   PUTR10                                                           
         LA    R3,1(R3)                                                         
PUTRX    J     EXIT                                                             
*                                                                               
*                                                                               
* FIX DISPLACEMENT TO OUTPUT AND BUMP TO NEXT ENTRY IN MAP TABLE                
FIXBUMP  SR    R5,R5                                                            
         ICM   R5,3,MAPFLD         DISP TO OUTPUT                               
         LLC   R6,VLEN             FIELD LEN                                    
         AR    R5,R6                                                            
                                                                                
         CLI   MAPTYP,EMSQ         END OF MESSAGE CONSTANT                      
         BE    FDIS02                                                           
                                                                                
         CLI   MAPTYP,SHDQ         SKIP HEADER MESSAGE                          
         BE    FDIS02                                                           
                                                                                
         CLI   MAPTYP,SENQ         SAVE END MESSAGE                             
         BE    FDIS02                                                           
                                                                                
         CLI   MAPTYP,SUFQ         SAVE SUB-FOOTER MSG                          
         BE    FDIS02                                                           
                                                                                
         CLI   MAPTYP,SUHQ         SUB-HEADING                                  
         BE    FDIS02                                                           
                                                                                
         CLI   MAPTYP,CONQ                                                      
         BNE   *+14                                                             
FDIS02   LA    RF,MAPCON-MAPD(R6)  BUMP TO NEXT ENTRY IN TABLE                  
         AR    R3,RF                                                            
         B     *+8                                                              
         LA    R3,MAPLNQ(R3)       BUMP TO NEXT ENTRY IN TABLE                  
                                                                                
         CLI   0(R3),EOPQ          END OF OPTIONAL DATA                         
         BNE   *+8                                                              
         LA    R3,3(R3)                                                         
                                                                                
         CLI   0(R3),EORQ          IF END OF RECORD                             
         BNE   *+8                                                              
         AHI   R5,4                ADD 4 FOR REC LEN FIELD                      
         STCM  R5,3,MAPFLD                                                      
         BR    RE                                                               
*                                                                               
*==============================================                                 
* FIND OPTIONAL DATA ON FILE                                                    
* ON ENTRY R5=MAPFLD (DISP TO OUTPUT)                                           
*==============================================                                 
*                                                                               
FDATA    NTR1                                                                   
*                                                                               
FDAT01   SR    R6,R6                                                            
         ICM   R6,3,MAPLEN                                                      
*                                                                               
         CLI   MAPTYP,EOPQ         END OF OPTIONAL DATA                         
         BNE   *+16                                                             
         LA    R3,3(R3)                                                         
         STCM  R5,3,MAPFLD         OUTPUT DISPLACEMENT                          
         B     FDATNE              DATA NOT FOUND                               
*                                                                               
         CLI   MAPTYP,FVDQ         FLAT FILE VARIABLE LEN DATA                  
         BE    FDAT10                                                           
*                                                                               
         CLI   MAPTYP,SHDQ         SKIP HEADER MESSAGE                          
         BE    FDAT02                                                           
                                                                                
         CLI   MAPTYP,SENQ         SAVE END MESSAGE                             
         BE    FDAT02                                                           
                                                                                
         CLI   MAPTYP,SUFQ         SAVE SUB-FOOTER MSG                          
         BE    FDAT02                                                           
                                                                                
         CLI   MAPTYP,SUHQ         SUB-HEADING                                  
         BE    FDAT02                                                           
                                                                                
         CLI   MAPTYP,CONQ                                                      
         BNE   *+14                                                             
FDAT02   LA    RF,MAPCON-MAPD(R6)  BUMP TO NEXT ENTRY IN TABLE                  
         AR    R3,RF                                                            
         B     *+8                                                              
         LA    R3,MAPLNQ(R3)       BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
         CLI   MAPTYP,EORQ                                                      
         BNE   FDAT01                                                           
         LA    R3,3(R3)                                                         
*                                                                               
         CLI   MAPTYP,EOPQ         END OF OPTIONAL DATA                         
         BNE   FDAT01                                                           
         LA    R3,3(R3)            BUMP TO NEXT RECORD                          
         B     FDATNE              AND GET OUT                                  
*                                                                               
FDAT10   L     RF,AFREC                                                         
         SR    R1,R1                                                            
         ICM   R1,3,0(RF)          RECORD LEN                                   
         AR    RF,R1                                                            
         ST    RF,EOR              SAVE A(END OF RECORD)                        
*                                                                               
         L     R1,ANEXTPOS         A(NEXT POS) TO DATA ON FILE                  
         CR    RF,R1               AT END OF RECORD                             
         BNH   FDAT20              YES                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,MAPDATA        DISPLACEMENT TO DATA ON FILE                 
         LTR   RF,RF                                                            
         BNZ   *+14                                                             
         L     R1,ANEXTPOS         A(NEXT POS) TO DATA ON FILE                  
         LR    RE,R1               SAVE START OF DATA                           
         B     FDAT12                                                           
*                                                                               
         LR    R1,R4               START OF RECORD ON FILE                      
*                                  LOOK FOR FIELD DELIMITER                     
         CLI   2(R1),FDELIM        3RD FIELD IS SEMI COLON                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,3(R1)            BUMP PAST RECORD ID                          
         LR    RE,R1               SAVE START OF DATA                           
*                                                                               
FDAT12   DS    0H                                                               
         CLI   0(R1),FDELIM                                                     
         BE    FDAT14                                                           
*                                                                               
         C     R1,EOR                                                           
         BNL   FDATNE              FIELD DELIMETER MISSING                      
*                                                                               
         LA    R1,1(R1)            NEXT POSITION                                
         B     FDAT12                                                           
*                                                                               
FDAT14   DS    0H                                                               
         LR    RF,R1               SAVE END OF DATA                             
                                                                                
         SR    RF,RE               GET LENGTH OF DATA                           
         BP    FDATEQ              DATA FOUND                                   
                                                                                
* DATA NOT FOUND BUMP TO END OF OPTION DATA (EOPQ)                              
         LA    R1,1(R1)            BUMP PAST SEMI-COLON                         
         ST    R1,ANEXTPOS         SAVE A(NEXT POSITION)TO DATA ON FILE         
FDAT20   LA    R3,MAPLNQ(R3)       BUMP TO NEXT ENTRY IN TABLE                  
         CLI   MAPTYP,EORQ                                                      
         BNE   FDAT01                                                           
         LA    R3,3(R3)                                                         
*                                                                               
         CLI   MAPTYP,EOPQ         END OF OPTIONAL DATA                         
         BNE   FDAT01                                                           
         LA    R3,3(R3)                                                         
         STCM  R5,3,MAPFLD         OUTPUT DISPLACEMENT                          
*                                                                               
FDATNE   LTR   RB,RB               DATA NOT FOUND                               
         B     *+6                                                              
FDATEQ   CR    RB,RB                                                            
         XIT1  REGS=(R3)                                                        
*                                                                               
*                                                                               
XMLOUT   DCB   DDNAME=XMLOUT,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               MACRF=PM,                                               X        
               EODAD=TEMPX                                                      
TEMPX    DS    0H                                                               
*                                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*                                                                               
       ++INCLUDE SPTRNEBID                                                      
       ++INCLUDE DDSPLWORKD                                                     
*                                                                               
       ++INCLUDE SPTRAFFD                                                       
          PRINT ON                                                              
          ORG   CONTAGH                                                         
          PRINT OFF                                                             
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
          PRINT ON                                                              
*********************************************************************           
* LOCAL WORKING STORAGE                                             *           
*********************************************************************           
                                                                                
LWSD     DSECT                                                                  
*                                                                               
AMAP     DS    A                                                                
AFREC    DS    A                                                                
AFRECP4  DS    A                                                                
AFRECX   DS    A                                                                
ANEXTPOS DS    A                                                                
AROUT    DS    A                   A(START OF MAP DATA)                         
ANENTRY  DS    A                   A(NEW XML ENTRY)                             
ACENTRY  DS    A                   A(CURR XML ENTRY FOR AROUT)                  
EOR      DS    A                                                                
AMFDATA  DS    A                   TABLE PASSED BY 1F                           
AMFDFRO  DS    A                                                                
*                                                                               
RELO     DS    A                                                                
*                                                                               
SVDISP   DS    CL2                 DISPLACEMENT IN XML TABLE FOR AROUT          
VLEN     DS    X                                                                
VTXT     DS    XL132                                                            
FLEN     DS    X                                                                
OUTMODE  DS    CL1                 XML INSTRUCTIONS MODE                        
*                                                                               
MAP      DS    CL220                                                            
FREC     DS    XL220               OUTPUT FILE RECORD                           
*                                                                               
NEWTYP   DS    CL1                 NEW RECORD TYPE FLAG                         
HEADSW   EQU   X'80'               PUT OUT HEADING                              
SUBHSW   EQU   X'40'               PUT OUT SUB HEADING                          
SUBFSW   EQU   X'20'               PUT OUT SUB FOOTER                           
TYPNEQ   EQU   X'10'               REC TYPE NOT EQ                              
*                                                                               
SVRECTYP DS    CL2                 SAVE FLAT FILE RECORD TYPE                   
SVSUBTY1 DS    CL2                 SAVE FLAT FILE RECORD SUB TYPE               
                                                                                
SVMSG    DS    0CL90                                                            
SVENDMSG DS    CL30                SAVE END MESSAGE AREA                        
SVSUFMSG DS    CL30                SAVE SUB-FOOTER MSG AREA                     
SVESMSG  DS    CL30                SAVE END SCHEDULE MESSAGE AREA               
*                                                                               
XMLBUF   DS    CL1000                                                           
*                                                                               
LWSX     EQU   *                                                                
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPTRA0F   03/25/19'                                      
         END                                                                    
