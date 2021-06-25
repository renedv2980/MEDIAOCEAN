*          DATA SET SPLFM34    AT LEVEL 043 AS OF 05/01/02                      
*PHASE T21934A,+0                                                               
*INCLUDE XSORT                                                                  
         TITLE 'T21934 - DAYPART HEADER'                                        
         PRINT NOGEN                                                            
T21934   CSECT                                                                  
         NMOD1 0,T21934,RR=R2                                                   
         L     RC,0(R1)                                                         
         ST    R2,RELO                                                          
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING DPTHDRD,R8                                                       
         CLI   SVFMTSW,0                                                        
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   ERRCD,NOFNDERR                                                   
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(5),SVKEY                                                     
         BNE   LFMERR              DOESN'T EXIST                                
         GOTO1 GETREC                                                           
*                                                                               
FM100    LA    R2,DPTSTRTH         CLEAR SCREEN                                 
*                                                                               
FM105    ZIC   RF,0(R2)                                                         
         SH    RF,=H'9'            SUBTRACT HEADER LENGTH +1                    
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    RF,=H'8'            SUBTRACT EXTENDED HEADER LENGTH              
*                                                                               
         EX    RF,*+8              PAD WITH BLANKS                              
         B     *+10                                                             
         OC    8(0,R2),=80X'40'                                                 
         EX    RF,*+8              TEST FIELD EMPTY                             
         B     *+10                                                             
         CLC   8(0,R2),=80X'40'                                                 
         BE    FM110               YES                                          
         EX    RF,*+8              NO - CLEAR FIELD                             
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
FM110    ZIC   R0,0(R2)            BUMP                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             TEST END OF SCREEN                           
         BNE   FM105                                                            
*                                                                               
         LA    R4,DPTCODES         PUT OUT DAYPART DEFINITIONS                  
         LA    R5,DPTEXTN                                                       
         LA    R2,DPTSTRTH                                                      
FM120    CLI   0(R4),0             END                                          
         BE    FMEXT                                                            
         MVC   8(1,R2),0(R4)       DAYPART LETTER                               
         ZIC   RE,1(R4)            TRANSLATE HEX TO LETTERS                     
         SRDL  RE,4                                                             
         AR    RE,RE                                                            
         IC    RE,HEXTAB+1(RE)                                                  
         STC   RE,9(R2)                                                         
         SRL   RF,28                                                            
         AR    RF,RF                                                            
         IC    RF,HEXTAB+1(RF)                                                  
         STC   RF,10(R2)                                                        
         MVI   11(R2),C'='         DISPLAY TIME SHEET PRINT                     
         MVC   12(3,R2),2(R4)                                                   
         CLC   DPTLEN,=H'212'      IF RECORD HAS EXTENSION ELEMENT              
         BNH   FM130                                                            
         CLC   0(3,R5),=C'   '     AND THIS DAYPART EXTN IS NOT SPACES          
         BE    FM130                                                            
         MVI   15(R2),C'='         THEN DISPLAY THIS EXTENSION                  
         MVC   16(3,R2),0(R5)                                                   
*                                                                               
FM130    OI    6(R2),X'80'                                                      
         LA    R4,5(R4)            NEXT DAYPART                                 
         LA    R5,5(R5)                                                         
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               NEXT SCREEN SLOT                             
         CLI   0(R2),9                                                          
         BNE   FM120               END OF SCREEN                                
*                                                                               
FMEXT    B     EXXMOD                                                           
         EJECT                                                                  
EDT      DS    0H                                                               
         CLI   SVACT,C'A'          ADD - AD100                                  
         BE    AD100                                                            
*                                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         MVC   DPTLEN,=H'212'                                                   
         MVC   ELEM(186),DPTCODES                                               
         XC    DPTCODES,DPTCODES   REBUILD ELEMENT                              
         BAS   RE,EDTSCRN                                                       
*                                                                               
* CHECK THAT NO DAYPARTS HAVE BEEN DELETED                                      
         MVI   ERRCD,NOCHGERR                                                   
         LA    R2,DPTSTRTH         CURSOR                                       
         LA    R3,ELEM                                                          
CH100    CLI   0(R3),0                                                          
         BE    CH200                                                            
         LA    R4,DPTCODES                                                      
CH120    CLI   0(R4),0                                                          
         BE    LFMERR                                                           
         CLC   0(1,R3),0(R4)       OLD DAYPARTS STILL EXIST                     
         BE    CH140                                                            
         LA    R4,5(R4)                                                         
         B     CH120                                                            
CH140    LA    R3,5(R3)                                                         
         B     CH100                                                            
*                                                                               
CH200    MVC   KEY,SVKEY                                                        
         ST    R8,AREC                                                          
         GOTO1 PUTREC                                                           
         B     FM100                                                            
         EJECT                                                                  
AD100    MVC   DPTLEN,=H'212'                                                   
         BAS   RE,EDTSCRN                                                       
         MVC   KEY,SVKEY                                                        
         ST    R8,AREC                                                          
         MVC   REC(13),KEY                                                      
         GOTO1 ADDREC                                                           
         B     FM100                                                            
         EJECT                                                                  
EDTSCRN  ST    RE,FULL                                                          
         XC    WORK2(6),WORK2      SAVE 'Z' ENTRY HERE/WORK2+5 = COUNT          
         MVC   ZEXTN,=C'   '       SAVE 'Z' EXTENSION HERE                      
         LA    R2,DPTSTRTH                                                      
         MVI   ERRCD,INVDPT                                                     
         CLI   5(R2),0                                                          
         BE    LFMERR              MUST BE 1                                    
         LA    R4,DPTEL                                                         
         MVC   0(2,R4),=X'01BC'    SET UP ELEMENT                               
         LA    R4,2(R4)                                                         
         MVI   ELEMFLAG,C'N'                                                    
         XC    ELEM2,ELEM2                                                      
         LA    R6,ELEM2                                                         
         MVC   0(2,R6),=X'02BC'    SET UP EXTENSION ELEMENT                     
         LA    R6,2(R6)                                                         
SE100    MVI   ERRCD,INVDPT                                                     
         CLI   5(R2),0                                                          
         BE    SE200                                                            
         CLI   5(R2),7             LENGTH FOR FIELDS WITH NO EXTENTION          
         BE    SE110                                                            
         CLI   15(R2),C'='         EXTENSION FIELDS NUST HAVE C'='              
         BNE   LFMERR                                                           
         CLI   5(R2),9             AND AT LEAST ON CHAR FOR THE EXTN            
         BL    LFMERR                                                           
SE110    CLI   8(R2),C'$'                                                       
         BE    SE140                                                            
         CLI   8(R2),C'+'          SPECIAL CODES FOR COKE                       
         BE    SE140               AND ANYONE ELSE STUPID ENOUGH                
         CLI   8(R2),C'-'          TO TYPE ONE OF THEM IN                       
         BE    SE140                                                            
         CLI   8(R2),X'F0'                                                      
         BL    SE120                                                            
         CLI   8(R2),X'F9'                                                      
         BH    LFMERR                                                           
         B     SE140                                                            
SE120    CLI   8(R2),X'C1'                                                      
         BL    LFMERR                                                           
         CLI   8(R2),X'E9'                                                      
         BH    LFMERR                                                           
SE140    BAS   RE,CHKDUP                                                        
*                                                                               
         MVC   0(1,R4),8(R2)       LETTER                                       
         LA    R7,9(R2)            VALIDATE AND TRANSLATE TO HEX                
         BAS   RE,HEXTRAN                                                       
         SLL   R7,4                SHIFT TO HI ORDER NIBBLE                     
         LR    R5,R7                                                            
         LA    R7,10(R2)                                                        
         BAS   RE,HEXTRAN                                                       
         AR    R7,R5               BRING TOGETHER                               
         STC   R7,1(R4)                                                         
         MVC   2(3,R4),12(R2)                                                   
         CLI   0(R4),C'Z'          IS IT A Z                                    
         BNE   SE160               NO - ERROR                                   
         MVI   ERRCD,DUPENTRY                                                   
         OC    WORK2(5),WORK2                                                   
         BNZ   LFMERR                                                           
         MVC   WORK2(5),0(R4)                                                   
         XC    0(5,R4),0(R4)                                                    
         CLI   5(R2),7             IF 'Z' EXTENSION IS GIVEN                    
         BNH   SE200                                                            
         MVC   ZEXTN,16(R2)        MOVE IT TO ZEXTN                             
         OC    ZEXTN,=C'   '                                                    
         MVI   ELEMFLAG,C'Y'                                                    
         GOTO1 CHKEXTN,DMCB,ZEXTN                                               
         B     SE200                                                            
*                                                                               
SE160    CLI   5(R2),7             IF EXTENSION IS GIVEN                        
         BNH   SE170                                                            
         MVC   0(3,R6),16(R2)      MOVE IT TO EXTENSION ELEMENT                 
         OC    0(3,R6),=C'   '                                                  
         MVI   ELEMFLAG,C'Y'                                                    
         GOTO1 CHKEXTN,DMCB,(R6)                                                
*                                                                               
SE170    OC    0(3,R6),=C'   '                                                  
         LA    R4,5(R4)            ADVANCE DPT TABLE                            
         LA    R6,5(R6)                                                         
         ZIC   RE,WORK2+5                                                       
         LA    RE,1(RE)                                                         
         STC   RE,WORK2+5          COUNT OF ENTRIES                             
SE200    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         CLI   0(R2),9                                                          
         BNE   SE100               CHECK FOR LAST DPT ENTRY                     
         SPACE                                                                  
         MVI   ERRCD,MSSNGDPT                                                   
         OC    WORK2(5),WORK2                                                   
         BZ    LFMERR                                                           
         SPACE 2                                                                
* SORT ON TOTAL CODES                                                           
         ZIC   R5,WORK2+5                                                       
         CH    R5,=H'1'                                                         
         BNH   SE300                                                            
         GOTO1 =V(XSORT),DMCB,DPTCODES,(R5),5,2,1,RR=RELO                       
SE300    MVC   0(5,R4),WORK2                                                    
         MVC   0(3,R6),ZEXTN                                                    
*                                                                               
         CLI   ELEMFLAG,C'Y'                                                    
         BNE   SE310                                                            
         MVC   DPTEL2(188),ELEM2                                                
         MVC   DPTLEN,=H'400'                                                   
*                                                                               
SE310    L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
LFMERR   GOTO1 ERROR                                                            
         SPACE 2                                                                
CHKDUP   LA    R3,DPTCODES                                                      
         MVI   ERRCD,DUPENTRY                                                   
CD100    CLI   0(R3),0             END OF LIST                                  
         BER   RE                  YES - THEN ADD THIS                          
         CLC   0(1,R3),8(R2)                                                    
         BE    LFMERR                                                           
         LA    R3,5(R3)            NEXT ENTRY                                   
         B     CD100                                                            
         SPACE 2                                                                
HEXTRAN  MVI   ERRCD,INVERR                                                     
         LA    R3,HEXTAB                                                        
TR100    CLI   1(R3),0                                                          
         BE    LFMERR                                                           
         CLC   0(1,R7),1(R3)                                                    
         BE    TR200                                                            
         LA    R3,2(R3)                                                         
         B     TR100                                                            
TR200    ZIC   R7,0(R3)                                                         
         BR    RE                                                               
*                                                                               
HEXTAB   DC    X'00F001F102F203F304F405F506F607F708F809F90AC10BC20CC30DX        
               C40EC50FC60000'                                                  
         SPACE 2                                                                
CHKEXTN  MVI   ERRCD,INVERR                                                     
         L     RF,0(R1)                                                         
         LA    R1,CHKEXTAB                                                      
*                                                                               
CE10     CLI   0(R1),X'FF'                                                      
         BE    LFMERR                                                           
         CLC   0(3,RF),0(R1)                                                    
         BER   RE                                                               
         LA    R1,3(R1)                                                         
         B     CE10                                                             
*                                                                               
CHKEXTAB DC    C'PR ',C'EM ',C'DA ',C'EF ',C'EN ',C'PA ',C'LN ',C'SP '          
         DC    C'CH ',C'LF ',X'FF'                                              
RELO     DS    A                                                                
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMD4D                                                       
         EJECT                                                                  
DPTHDRD  DSECT                                                                  
       ++INCLUDE SPGENDAYPT                                                     
         EJECT                                                                  
GENOLD   DSECT                                                                  
         ORG   REC2                                                             
INVDPT   EQU   21                  INVALID DAYPART                              
MSSNGDPT EQU   172                 MISSING LAST DAYPART 'Z'                     
INVMENU  EQU   171                 ADD RECORD MUST BE MENU 0                    
ZEXTN    DS    CL3                 EXTENSION FOR Z DAYPART                      
ELEMFLAG DS    C                   C'Y' IF AT LEAST ONE EXTN GIVEN              
ELEM2    DS    XL188               AREA FOR EXTENTION ELEMENT                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043SPLFM34   05/01/02'                                      
         END                                                                    
