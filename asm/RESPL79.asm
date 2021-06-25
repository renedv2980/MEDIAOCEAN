*          DATA SET RESPL79    AT LEVEL 007 AS OF 05/01/02                      
*          DATA SET RESPL79    AT LEVEL 005 AS OF 06/01/88                      
*PHASE T80879A                                                                  
         TITLE 'T80879 - EDIT FOR OVERNIGHT DELETE'                             
T80879   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DEED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T808FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              EDIT FIELDS                                                      
         SPACE 2                                                                
         MVI   PAVMEDIA,C'T'                                                    
         MVI   DMSOURCE,C'A'                                                    
         LA    RE,TITWORK          CLEAR WORK AREA                              
         LA    RF,TITWORKX                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         LA    R2,TITSTATH         STATION                                      
         MVI   OPTION,C'Y'         (OUTPUT MARKET NAME)                         
         MVC   TITMKT,SPACES                                                    
         OI    TITMKTH+6,X'80'                                                  
         MVI   RDOKOPT,C'Y'        (OPTION TO IGNORE STATION PROBLEM)           
         GOTO1 VALISTAT                                                         
*                                                                               
         LA    R2,TITSVCH          SERVICE                                      
         CLI   5(R2),0                                                          
         BE    ED005                                                            
         CLI   8(R2),C'A'          ARB                                          
         BE    ED005                                                            
         CLI   8(R2),C'N'          NSI                                          
         BE    ED005                                                            
         CLI   8(R2),C'S'          SRC                                          
         BNE   ED900                                                            
*                                                                               
ED005    LA    R2,TITDPTH          DAYPART                                      
         XC    DPLIST,DPLIST                                                    
         CLI   5(R2),0                                                          
         BE    ED010                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   E10                                                              
         MVC   DPLIST(16),DPTBL                                                 
         B     ED010                                                            
         SPACE 1                                                                
E10      LA    RF,8(R2)            MAKE SURE INPUT IS IN TABLE                  
         ZIC   R1,5(R2)                                                         
E20      LA    RE,DPTBL                                                         
E30      CLC   0(1,RE),0(RF)                                                    
         BE    E40                                                              
         LA    RE,1(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   E30                                                              
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
E40      LA    RF,1(RF)                                                         
         BCT   R1,E20                                                           
         SPACE 1                                                                
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     ED010                                                            
         MVC   DPLIST(0),8(R2)                                                  
*                                                                               
ED010    LA    R2,TITDETH          DETAILS                                      
         CLI   5(R2),0                                                          
         BE    ED030                                                            
         CLI   8(R2),C'Y'                                                       
         BE    ED030                                                            
         CLI   8(R2),C'N'                                                       
         BNE   ED900                                                            
*                                                                               
ED030    LA    R2,TITDELH          DELETE                                       
         GOTO1 ANY                                                              
         LA    R5,8(R2)                                                         
         LA    R6,2                                                             
*                                                                               
ED040    CLI   0(R5),C'T'                                                       
         BNE   *+12                                                             
         OI    DELBYTE,X'02'       TEXT                                         
         B     ED050                                                            
         CLI   0(R5),C'I'                                                       
         BNE   *+12                                                             
         OI    DELBYTE,X'04'       INV                                          
         B     ED050                                                            
         CLI   0(R5),C'A'                                                       
         BNE   ED900                                                            
         OI    DELBYTE,X'01'       ALL                                          
*                                                                               
ED050    LA    R5,1(R5)                                                         
         CLI   5(R2),1                                                          
         BNH   *+8                                                              
         BCT   R6,ED040                                                         
         TM    DELBYTE,X'03'       DON'T ALLOW ALL AND TEXT                     
         BO    ED900                                                            
         TM    DELBYTE,X'05'       DON'T ALLOW ALL AND INV                      
         BO    ED900                                                            
         TM    DELBYTE,X'01'       DON'T ALLOW ALL AND SERVICE OPTION           
         BZ    ED060                                                            
         LA    R2,TITSVCH                                                       
         CLI   5(R2),0                                                          
         BNE   ED900                                                            
*                                                                               
ED060    LA    R2,TITTYPH          TEXT TYPE                                    
         TM    DELBYTE,X'02'                                                    
         BO    ED070                                                            
         CLI   5(R2),0                                                          
         BE    ED080                                                            
         B     ED900                                                            
*                                                                               
ED070    CLI   5(R2),0                                                          
         BE    ED080                                                            
         CLI   8(R2),C'A'          ALL TEXT                                     
         BE    ED076                                                            
         CLI   8(R2),C'I'          INVENTORY TEXT                               
         BE    ED076                                                            
         CLI   8(R2),C'M'          MARKET TEXT                                  
         BE    ED074                                                            
         CLI   8(R2),C'S'          STATION TEXT                                 
         BNE   ED900                                                            
*                                                                               
ED074    CLI   DELBYTE,X'02'       DON'T ALLOW ONLY MKT/STN TEXT                
         BNE   ED076                       AND DAYPART                          
         LA    R2,TITDPTH                                                       
         CLI   5(R2),0                                                          
         BNE   ED900                                                            
         B     ED080                                                            
*                                                                               
ED076    LA    R2,TITDPTH          CHECK FOR DAYPART                            
         GOTO1 ANY                                                              
*                                                                               
ED080    LA    R2,TITBKRGH         BOOKS/RANGE                                  
         TM    DELBYTE,X'06'                                                    
         BNZ   ED090               ONLY ALLOW FOR TEXT OR INV DELETE            
         CLI   5(R2),0                                                          
         BE    ED140                                                            
         B     ED900                                                            
*                                                                               
ED090    GOTO1 ANY                                                              
         MVC   DMCB+8(4),=C',=,-'                                               
         GOTO1 SCANNER,DMCB,(R2),(4,BLOCK)                                      
         LA    R4,BLOCK                                                         
         CLI   DMCB+4,1                                                         
         BH    ED100               MORE THAN 1 FIELD - TRY BOOK                 
         BL    ED900                                                            
         CLI   1(R4),0             1 FIELD - TEST LEN OF 2ND HALF               
         BNE   ED110                         NON ZERO - TRY RANGE               
*                                                                               
ED100    MVI   MAX,4               MAX 4 BOOKS                                  
         GOTO1 VALIBOOK                                                         
         LA    R4,DELBOOKS                                                      
         LA    R5,BOOKS                                                         
         ZIC   R6,ACTUAL                                                        
*                                                                               
ED105    MVC   0(3,R4),0(R5)                                                    
         LA    R4,3(R4)                                                         
         LA    R5,3(R5)                                                         
         BCT   R6,ED105                                                         
         MVI   0(R4),X'FF'                                                      
         B     ED140                                                            
*                                                                               
ED110    TM    DELBYTE,X'06'       DON'T ALLOW RANGE FOR TEXT AND               
         BO    ED900                INV DELETE TOGETHER                         
         OI    DELBYTE,X'08'       RANGE                                        
         TM    DELBYTE,X'02'                                                    
         BZ    ED120                                                            
         OC    4(4,R4),4(R4)       CHECK TEXT RANGE                             
         BZ    ED900                                                            
         OC    8(4,R4),8(R4)                                                    
         BZ    ED900                                                            
         CLC   4(4,R4),8(R4)                                                    
         BH    ED900                                                            
         MVC   TXRANGE(2),6(R4)                                                 
         MVC   TXRANGE+2(2),10(R4)                                              
         B     ED140                                                            
*                                  CHECK INV RANGE                              
ED120    LA    R0,2                                                             
         LR    R5,R4                                                            
         LA    R6,12(R4)                                                        
         LA    R7,INRANGE                                                       
         MVI   WORK,C'A'                                                        
*                                                                               
ED130    CLI   0(R5),3             MUST HAVE QTR HR NO (2 BYTES) & DAY          
         BL    ED900                                                            
         XC    DUB(3),DUB          CHECK QTR HR NO AND DAY ARE NUMERIC          
         MVZ   DUB(3),0(R6)                                                     
         CLC   DUB(3),=C'000'                                                   
         BNE   ED900                                                            
         PACK  DUB,0(2,R6)                                                      
         CVB   R1,DUB                                                           
         STC   R1,0(R7)            QTR HR NO (BINARY)                           
         MVC   1(1,R7),2(R6)       DAY CODE (NUMERIC)                           
         MVC   2(1,R7),WORK                                                     
         CLI   0(R5),4                                                          
         BH    ED900                                                            
         BL    *+10                                                             
         MVC   2(1,R7),3(R6)       PROGRAM LENGTH CODE (ALPHANUMERIC)           
         LA    R5,1(R4)                                                         
         LA    R6,22(R4)                                                        
         LA    R7,3(R7)                                                         
         MVI   WORK,C'9'                                                        
         BCT   R0,ED130                                                         
*                                                                               
ED140    LA    R2,TITSTRTH         ALL START DATE                               
         LA    R3,STDATE                                                        
         BAS   RE,CHKDATE                                                       
         BNE   ED900                                                            
*                                                                               
         LA    R2,TITENDH          ALL END DATE                                 
         LA    R3,ENDATE                                                        
         BAS   RE,CHKDATE                                                       
         BNE   ED900                                                            
*                                                                               
         TM    DELBYTE,X'01'                                                    
         BZ    ED999                                                            
         LA    R2,TITSTRTH                                                      
         OC    ENDATE,ENDATE                                                    
         BNZ   ED150                                                            
         OC    STDATE,STDATE                                                    
         BZ    ED990                                                            
         B     ED999                                                            
*                                                                               
ED150    CLC   STDATE,ENDATE       CHECK START DATE LE END DATE                 
         BNH   ED999                                                            
*                                                                               
ED900    MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
*                                                                               
ED990    MVI   ERROR,MISSING                                                    
         B     EXIT                                                             
*                                                                               
ED999    LA    R2,TITSTATH                                                      
*                                                                               
EXIT     XIT1  REGS=(R2)                                                        
         SPACE 3                                                                
DPTBL    DC    C'MDERATLWKFNPVSJO' THESE ARE INCLUDED IN ALL LIST               
         DC    C'XYU'              THESE ARE VALID AS INDIVIDUAL DPTS           
         DC    X'FF'                                                            
         EJECT                                                                  
CHKDATE  NTR1                                                                   
*                                                                               
*        ROUTINE TO CHECK THE START/END DATE FIELDS                             
*        INPUT:  R2=HEADER ADDR                                                 
*                R3=A(COMPRESSED DATE)                                          
*        OUTPUT: CC EQ VALID                                                    
*                   NE INVALID                                                  
*                                                                               
         CLI   5(R2),0                                                          
         BE    CD999                                                            
         TM    DELBYTE,X'01'                                                    
         BZ    CD900                                                            
*                                                                               
CD010    GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         CLI   DMCB+3,0                                                         
         BE    CD900                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(2,0(R3))                                   
         CR    RB,RB               CC EQ                                        
         B     CD999                                                            
*                                                                               
CD900    LTR   RB,RB               CC NE                                        
*                                                                               
CD999    B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RESPLFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESPLD9D                                                       
DPLIST   DS    CL20                DAYPART LIST                                 
DELBYTE  DS    X                   X'01' ALL DELETE                             
*                                  X'02' TEXT DELETE                            
*                                  X'04' INV DELETE                             
*                                  X'08' DELETE RANGE                           
TXRANGE  DS    XL4                 TEXT RANGE                                   
INRANGE  DS    XL6                 INV RANGE                                    
DELBOOKS DS    CL15                TABLE OF BOOKS                               
STDATE   DS    XL2                 START DATE                                   
ENDATE   DS    XL2                 END DATE                                     
*                                                                               
TITWORKX DS    0H                                                               
         PRINT OFF                                                              
       ++INCLUDE RESPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007RESPL79   05/01/02'                                      
         END                                                                    
