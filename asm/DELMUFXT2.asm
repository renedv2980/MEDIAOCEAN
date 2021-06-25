*          DATA SET DELMUFXT2  AT LEVEL 028 AS OF 01/07/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMUF2A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE MODLINK                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'LOCAL MONTHLIES DFSORT EXIT: FILTER PATHNAMES'                  
***********************************************************************         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM.                                 *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* THIS EXIT IS INVOKED BY ICETOOL PAN SOURCE MODULE:                  *         
*  DELMINDNR (MODE=NORMAL)                                            *         
*  DELMINDDP (MODE=DPCUME)                                            *         
*  DELMINDPU (MODE=PGMAMUP)                                           *         
*                                                                     *         
* THIS MODULE IS A COMPONENT OF THE LOCAL MONTHLIES PREPROCESSOR.     *         
* IT READS AND VALIDATES THE PARSED PARAMETER CARDS. IT ALSO EXPLODES *         
* CERTAIN PARAMETER CARDS INTO MULTIPLES WHERE APPROPRIATE.           *         
*                                                                     *         
* EACH INPUT RECORD IS OF THE FORMAT:                                 *         
*  KEYWORD DS CL10                                                    *         
*  VALUE   DS CL10                                                    *         
* (SEE DELMDSECT, DSECT PNA_PARMD)                                    *         
*                                                                     *         
***********************************************************************         
DELMUF2  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ENTRY E35                 MUST BE "E35" (FOR DFSORT)                   
         ENTRY SSB                                                              
*                                                                               
         REQUS                                                                  
*                                                                               
         USING E35,RF              RF = WHERE WE ARE LOADED                     
E35      SAVE  (14,12),,DELMUF2    SAVE DFSORT'S REGISTERS                      
         STMH  GR0,GRF,DFSORTHH    SAVE DFSORT'S REGS (HIGH HALVES)             
         DROP  RF                                                               
*                                                                               
*                                  INITIALIZE OUR USUAL RD CHAIN                
         LR    RB,RF               USE RB AS PROGRAM BASE                       
         USING E35,RB                                                           
         L     RE,=V(REGSAVE)      GET OUR SAVE AREA CHAIN                      
         ST    RD,4(,RE)           SAVE BACKWARD POINTER IN OUR AREA            
         ST    RE,8(,RD)                                                        
         LR    RD,RE               SET OUR SAVE AREA                            
*                                                                               
         L     R3,0(,R1)           GET A(RECORD PASSED BY DFSORT)               
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE,=CL60'LOCAL MONTHLIES PARAMETER CARD VALIDATION'           
*                                                                               
         LTR   R3,R3               EOF?                                         
         BZ    EOFCHK              YES: CONFIRM REQUIRED PARAMETERS             
*                                                                               
         USING PNA_PARMD,R3                                                     
*                                                                               
*                                       COPY PARSED PARAMETER CARD TO P         
         MVC   P_PARM_KEYWORD,PNA_PARM_KEYWORD                                  
         MVC   P_PARM_VALUE,PNA_PARM_VALUE                                      
*                                                                               
         CLC   PNA_PARM_KEYWORD,DATA_KEYWORD      DATA=                         
         BNE   VALSRVC                                                          
*                                                                               
         GOTO1 =V(PRINTER)                 PRINT PARSED PARAMETER CARD          
         CLC   =C'TEST ',PNA_PARM_VALUE    DATA=TEST ?                          
         BNE   *+10                                                             
         MVC   ASRVICES,=A(TEST_SERVICES)  TEST DATA IS BEING READ              
         B     DELREC                                                           
*                                                                               
VALSRVC  DS    0H                                                               
         CLC   PNA_PARM_KEYWORD,SERVICE_KEYWORD   SERVICE=                      
         BNE   VALDATE                                                          
*                                                                               
         GOTO1 =V(PRINTER)              PRINT PARSED PARAMETER CARD             
         TM    PRMFLAGS,SRVCFLAG        THIS PARAMETER SEEN ALREADY?            
         BO    DUPPARM                  YES: INVALID!                           
*                                                                               
         OI    PRMFLAGS,SRVCFLAG        WE HAVE A SERVICE=                      
         L     RF,ASRVICES              A(VALID SERVICE TABLE)                  
VALSRVC5 CLC   PNA_PARM_VALUE,0(RF)     VALID SERVICE?                          
         BE    KEEPREC                  YES                                     
         LA    RF,L'PNA_PARM_VALUE(RF)  BUMP TO NEXT                            
         CLI   0(RF),X'FF'              EOT?                                    
         BNE   VALSRVC5                 NO                                      
*                                                                               
         MVC   P(40),=C'*** ERROR *** INVALID SERVICE= PARAMETER'               
         GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR               ERROR EXIT                                   
*                                                                               
VALDATE  DS    0H                                                               
         CLC   PNA_PARM_KEYWORD,DATE_KEYWORD      DATE=                         
         BNE   VALMODE                                                          
*                                                                               
         GOTO1 =V(PRINTER)         PRINT PARSED PARAMETER CARD                  
         TM    PRMFLAGS,DATEFLAG   THIS PARAMETER SEEN ALREADY?                 
         BO    DUPPARM             YES: INVALID!                                
*                                                                               
         CLC   =C'repo ',PNA_PARM_VALUE      DATE=repo                          
         BE    VALDATE5                      YES: USE REPOSITORY ONLY           
*                                                                               
         CLC   =C'LATEST ',PNA_PARM_VALUE    DATE=LATEST ?                      
         BNE   VALDATE3                      YES: SUBSTITUTE HIGH DATE          
*                                                                               
         OPEN  HIGHDATE                                                         
         MVC   PNA_PARM_VALUE,SPACES         CLEAR STRING "LATEST"              
         GET   HIGHDATE,PNA_PARM_VALUE       READ IN HIGHDATE: YYYYMMDD         
         CLOSE HIGHDATE                                                         
*                                                                               
VALDATE3 DS    0H                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(2),PNA_PARM_VALUE+4      MM                                 
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),PNA_PARM_VALUE+6    DD                                 
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(4),PNA_PARM_VALUE      YYYY                               
         GOTO1 =V(DATVAL),DMCB,(0,WORK),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   VALDATE5                                                         
*                                                                               
         MVC   P(37),=C'*** ERROR *** INVALID DATE= PARAMETER'                  
         GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR                                                            
*                                                                               
VALDATE5 DS    0H                                                               
         OI    PRMFLAGS,DATEFLAG   WE HAVE A DATE=                              
         B     KEEPREC                                                          
*                                                                               
VALMODE  DS    0H                                                               
         CLC   PNA_PARM_KEYWORD,MODE_KEYWORD      MODE=                         
         BNE   VALSTRM                                                          
*                                                                               
         CLC   =C'NORMAL ',PNA_PARM_VALUE    MODE=NORMAL?                       
         BE    VALMODE3                                                         
         CLC   =C'PGNAMUP ',PNA_PARM_VALUE   MODE=PGNAMUP?                      
         BE    VALMODE3                                                         
         CLC   =C'DPCUME ',PNA_PARM_VALUE    MODE=DPCUME?                       
         BE    VALMODE5                                                         
*                                                                               
         GOTO1 =V(PRINTER)         PRINT PARSED PARAMETER CARD                  
         MVC   P(37),=C'*** ERROR *** INVALID MODE= PARAMETER'                  
         GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR                                                            
*                                                                               
VALMODE3 DS    0H                                                               
         IF (TM,PRMFLAGS,MODEFLAG,O) ALREADY SAW A MODE= CARD?                  
           GOTO1 =V(PRINTER)         YES: PRINT PARSED PARAMETER CARD           
           B     DUPPARM             INVALID                                    
         ENDIF ,                                                                
*                                                                               
         TM    PRMFLAGS,DATEFLAG+SRVCFLAG+BOOKFLAG+MKTFLAG+STRMFLAG+EXM+        
               KTFLG                                                            
         BZ    VALMODE4            NO OTHER PARMS SEEN YET: OKAY                
*                                                                               
         GOTO1 =V(PRINTER)         PRINT PARSED PARAMETER CARD                  
         MVC   P(38),=C'*** ERROR *** MODE= MUST BE FIRST CARD'                 
         GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR                                                            
*                                                                               
VALMODE4 DS    0H                                                               
         OI    PRMFLAGS,MODEFLAG   WE HAVE A MODE= CARD                         
         MVC   SAVEMODE,PNA_PARM_VALUE  SAVE THE MODE VALUE                     
         GOTO1 =V(PRINTER)         PRINT PARSED PARAMETER CARD                  
         B     KEEPREC                                                          
*                                                                               
VALMODE5 DS    0H                                                               
         CLI   FRSTTIME,C'Y'       MODE=DPCUME: FIRST TIME IN?                  
         BNE   VALMODE7                                                         
         IF (TM,PRMFLAGS,MODEFLAG,O) ALREADY SAW A MODE= CARD?                  
           GOTO1 =V(PRINTER)         YES: PRINT PARSED PARAMETER CARD           
           B     DUPPARM             INVALID                                    
         ENDIF ,                                                                
*                                                                               
         OI    PRMFLAGS,MODEFLAG   NO: WE HAVE A MODE= CARD                     
         MVC   SAVEMODE,PNA_PARM_VALUE  SAVE THE MODE VALUE                     
         GOTO1 =V(PRINTER)         PRINT PARSED PARAMETER CARD                  
         TM    PRMFLAGS,DATEFLAG+SRVCFLAG+BOOKFLAG+MKTFLAG+STRMFLAG+EXM+        
               KTFLG                                                            
         BZ    VALMODE6            NO OTHER PARMS SEEN YET: OKAY                
         MVC   P(38),=C'*** ERROR *** MODE= MUST BE FIRST CARD'                 
         GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR                                                            
*                                                                               
VALMODE6 DS    0H                                                               
         MVI   FRSTTIME,C'N'                                                    
         MVC   ACURSTRM,=A(STANDARD_STREAMS) SET A(STREAM TABLE)                
*                                                                               
VALMODE7 DS    0H                                                               
         L     R4,ACURSTRM         A(CURRENT STREAM TABLE ENTRY)                
         CLI   0(R4),X'FF'         EOT?                                         
         BE    KEEPREC             YES: RELEASE "MODE=DPCUME" RECORD            
*                                                                               
NEW      USING PNA_PARMD,NEWREC                                                 
*                                  NO: BUILD STREAM= RECORD TO INSERT           
         OI    PRMFLAGS,STRMFLAG   PRETEND WE GOT A STREAM= CARD                
         MVC   NEW.PNA_PARM_KEYWORD,STREAM_KEYWORD                              
         MVC   NEW.PNA_PARM_VALUE,0(R4) PLUG IN THE STREAM (L1 OR L7)           
         LA    R4,L'PNA_PARM_VALUE(R4)  BUMP TO NEXT STREAM                     
         ST    R4,ACURSTRM         REMEMBER WHERE WE ARE FOR NEXT TIME          
         B     ADDREC              INSERT A SINGLE-STREAM CARD                  
         DROP  NEW                                                              
*                                                                               
VALSTRM  DS    0H                                                               
         CLC   PNA_PARM_KEYWORD,STREAM_KEYWORD                                  
         BNE   VALMKT                                                           
*                                                                               
         CLC   =C'DPCUME ',SAVEMODE    MODE=DPCUME?                             
         BNE   VALSTRM2                                                         
         GOTO1 =V(PRINTER)         YES: PRINT PARSED PARAMETER CARD             
         MVC   P(46),=C'*** ERROR *** STREAM= INVALID WHEN MODE=DPCUME'         
         GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR                                                            
*                                                                               
VALSTRM2 DS    0H                                                               
         OI    PRMFLAGS,STRMFLAG   WE GOT A STREAM= CARD                        
         CLC   =C'ALL ',PNA_PARM_VALUE       STREAM=ALL CARD?                   
         BNE   *+12                                                             
         OI    PRMFLAG2,STRM_ALL             YES: REMEMBER THAT FACT            
         B     VALSTRM7                                                         
         CLC   =C'ST ',PNA_PARM_VALUE        NO: STREAM=ST(ANDARD)?             
         BE    VALSTRM5                      YES                                
*                                                                               
         GOTO1 =V(PRINTER)              PRINT PARSED PARAMETER CARD             
         LA    RF,ALL_STREAMS           A(VALID STREAM TABLE)                   
VALSTRM3 CLC   PNA_PARM_VALUE,0(RF)     VALID STREAM?                           
         BE    KEEPREC                  YES                                     
         LA    RF,L'PNA_PARM_VALUE(RF)  BUMP TO NEXT                            
         CLI   0(RF),X'FF'              EOT?                                    
         BNE   VALSTRM3                 NO                                      
*                                                                               
         MVC   P(39),=C'*** ERROR *** INVALID STREAM= PARAMETER'                
         GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR                                                            
*                                                                               
VALSTRM5 DS    0H                                                               
         CLI   FRSTTIME,C'Y'       STREAM=ST: FIRST TIME IN?                    
         BNE   VALSTRM7                                                         
         MVI   FRSTTIME,C'N'       YES                                          
         MVC   ACURSTRM,=A(STANDARD_STREAMS) SET A(STREAM TABLE)                
*                                                                               
VALSTRM7 DS    0H                                                               
         L     R4,ACURSTRM         A(CURRENT STREAM TABLE ENTRY)                
         CLI   0(R4),X'FF'         EOT?                                         
         BNE   VALSTRM9            NO                                           
         GOTO1 =V(PRINTER)         YES: PRINT PARSED PARAMETER CARD             
         B     DELREC              PURGE EXTRANEOUS STREAM= RECORD              
*                                                                               
VALSTRM9 DS    0H                                                               
NEW      USING PNA_PARMD,NEWREC                                                 
*                                  BUILD RECORD TO INSERT                       
         MVC   NEW.PNA_PARM_KEYWORD,PNA_PARM_KEYWORD                            
         MVC   NEW.PNA_PARM_VALUE,0(R4) SUBSTITUTE STREAM FOR "ALL"             
         LA    R4,L'PNA_PARM_VALUE(R4)  BUMP TO NEXT STREAM                     
         ST    R4,ACURSTRM         REMEMBER WHERE WE ARE FOR NEXT TIME          
         B     ADDREC              INSERT A SINGLE-STREAM CARD                  
         DROP  NEW                                                              
*                                                                               
VALMKT   DS    0H                                                               
         CLC   PNA_PARM_KEYWORD,MARKET_KEYWORD   MARKET=                        
         BNE   VALEXMKT                                                         
*                                                                               
         OI    PRMFLAGS,MKTFLAG    WE HAVE A MARKET= CARD                       
         CLC   =C'ALL ',PNA_PARM_VALUE MARKET=ALL?                              
         BE    VALMKT10            YES                                          
*                                                                               
* MARKET=>N MEANS: ONLY PROCESS MARKET NUMBERS GREATER THAN N                   
         IF (CLI,PNA_PARM_VALUE,EQ,C'>') "MARKET=>N" SYNTAX?                    
           IF (TM,PRMFLAG2,STRT_MKT,Z)   YES. FIRST TIME THROUGH ONLY:          
             GOTO1 =V(PRINTER)             PRINT PARSED PARAMETER CARD          
             CLI   PNA_PARM_VALUE+4,C' '   MAKE SURE IT'S A 3-DIGIT NO.         
             BNE   BADMKT                  IT ISN'T                             
             MVC   THREE,PNA_PARM_VALUE+1                                       
             MVN   THREE,=C'000'                                                
             CLC   THREE,=C'000'                                                
             BNE   BADMKT                  NOT NUMERIC: INVALID                 
             PACK  DUB,PNA_PARM_VALUE+1(3) E.G., MARKET=>150 MEANS...           
             CVB   R0,DUB                  ...THE LOWEST MARKET TO...           
             AHI   R0,1                    ...PROCESS IS 151                    
             STH   R0,CURMKT#              SAVE THAT STARTING POINT             
             OI    PRMFLAG2,STRT_MKT       REMEMBER: ONLY DO THIS ONCE          
           ENDIF ,                                                              
           B     VALMKT10          NOW TREAT SIMILARLY TO "MARKET=ALL"          
         ENDIF ,                                                                
*                                                                               
         GOTO1 =V(PRINTER)         PRINT PARSED PARAMETER CARD                  
         CLI   PNA_PARM_VALUE+3,C' '  MAKE SURE IT'S 3 CHARACTERS LONG          
         BNE   BADMKT                                                           
         MVC   DUB(3),PNA_PARM_VALUE                                            
         MVN   DUB(3),=C'000'                                                   
         CLC   DUB(3),=C'000'                                                   
         BE    KEEPREC                                                          
*                                                                               
BADMKT   DS    0H                                                               
         MVC   P(39),=C'*** ERROR *** INVALID MARKET= PARAMETER'                
         GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR                                                            
*                                                                               
VALMKT10 DS    0H                                                               
         LH    R4,CURMKT#          CURRENT MARKET NUMBER                        
         IF (CH,R4,EQ,=AL2(LOWMKT#))    FIRST MARKET?                           
           GOTO1 =V(PRINTER)       YES: PRINT PARSED PARAMETER CARD             
         ENDIF ,                                                                
         CH    R4,=AL2(MAXMKT#)    ANY MORE POSSIBLE MARKETS?                   
         BH    DELREC              NO: PURGE THE "MARKET=ALL" RECORD            
*                                                                               
*                                  SKIP SRA MARKETS WITH NO DMA DEMOS           
         CHI   R4,290              GREAT BEND                                   
         BE    VALMKT20                                                         
         CHI   R4,341              HAYS-GOODLAND                                
         BE    VALMKT20                                                         
         CHI   R4,342              ENSIGN-GARDEN CITY                           
         BNE   VALMKT30                                                         
*                                                                               
VALMKT20 DS    0H                                                               
         AHI   R4,1                BUMP TO NEXT MARKET                          
         STH   R4,CURMKT#                                                       
         B     VALMKT10                                                         
*                                                                               
VALMKT30 DS    0H                                                               
NEW      USING PNA_PARMD,NEWREC                                                 
*                                  BUILD RECORD TO INSERT                       
         MVC   NEW.PNA_PARM_KEYWORD,PNA_PARM_KEYWORD                            
         MVC   NEW.PNA_PARM_VALUE,SPACES                                        
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  NEW.PNA_PARM_VALUE(3),DUB 3-DIGIT MARKET NUMBER                  
         AHI   R4,1                BUMP TO NEXT MARKET                          
         STH   R4,CURMKT#          REMEMBER WHERE WE ARE FOR NEXT TIME          
         B     ADDREC              INSERT A SINGLE-MARKET CARD                  
         DROP  NEW                                                              
*                                                                               
VALEXMKT DS    0H                                                               
         CLC   PNA_PARM_KEYWORD,EXMARKET_KEYWORD   EXMARKET=                    
         BNE   VALBOOK                                                          
*                                                                               
         OI    PRMFLAGS,EXMKTFLG   WE HAVE AN EXMARKET= CARD                    
         GOTO1 =V(PRINTER)         PRINT PARSED PARAMETER CARD                  
         CLI   PNA_PARM_VALUE+3,C' '  MAKE SURE IT'S 3 CHARACTERS LONG          
         BNE   BADEXMKT                                                         
         MVC   DUB(3),PNA_PARM_VALUE                                            
         MVN   DUB(3),=C'000'                                                   
         CLC   DUB(3),=C'000'                                                   
         BE    KEEPREC                                                          
*                                                                               
BADEXMKT DS    0H                                                               
         MVC   P(41),=C'*** ERROR *** INVALID EXMARKET= PARAMETER'              
         GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR                                                            
*                                                                               
VALBOOK  DS    0H                                                               
         CLC   PNA_PARM_KEYWORD,BOOK_KEYWORD                                    
         BNE   UNKNOWN                                                          
*                                                                               
         GOTO1 =V(PRINTER)         PRINT PARSED PARAMETER CARD                  
         TM    PRMFLAGS,BOOKFLAG   THIS PARAMETER SEEN ALREADY?                 
         BO    DUPPARM             YES: INVALID!                                
*                                                                               
         OI    PRMFLAGS,BOOKFLAG   WE HAVE A BOOK=                              
         GOTO1 =V(DATVAL),DMCB,(2,PNA_PARM_VALUE),YYMMDD                        
         OC    0(4,R1),0(R1)                                                    
         BNZ   VALBOOK5            BOOK IS VALID                                
*                                                                               
         MVC   P(37),=C'*** ERROR *** INVALID BOOK= PARAMETER'                  
         GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR                                                            
*                                                                               
VALBOOK5 DS    0H                                                               
*                                  CONVERT TO YYYYMMDD                          
         GOTO1 =V(DATCON),DMCB,YYMMDD,(20,PNA_PARM_VALUE)                       
         B     KEEPREC             RETURN RECORD WITH FORMATTED BOOK            
*                                                                               
UNKNOWN  DS    0H                                                               
         GOTO1 =V(PRINTER)         UNKNOWN PARAMETER: JUST PRINT IT             
         B     DELREC              AND IGNORE IT FOR NOW                        
*                                                                               
DUPPARM  DS    0H                                                               
         MVC   P(41),=C'*** ERROR *** INVALID DUPLICATE PARAMETER'              
         GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR                                                            
*                                                                               
EOFCHK   DS    0H                                                               
*                            CANNOT USE STREAM=ALL WITH MARKET=> OPTION         
         TM    PRMFLAG2,STRM_ALL+STRT_MKT                                       
         BNO   *+14                                                             
         MVC   P(37),=C'*** ERROR *** INCONSISTENT PARAMETERS'                  
         B     EOFERR                                                           
*                                                                               
         TM    PRMFLAGS,MODEFLAG+DATEFLAG+SRVCFLAG+BOOKFLAG+MKTFLAG+STR+        
               MFLAG               ALL REQUIRED PARAMETERS PRESENT?             
         BO    EOF                 YES: EXIT AND DO NOT RETURN                  
*                                                                               
         MVC   P(40),=C'*** ERROR *** MISSING REQUIRED PARAMETER'               
*                                                                               
EOFERR   DS    0H                                                               
         GOTO1 =V(PRINTER)         PRINT ERROR MESSAGE                          
         BRAS  RE,SNAPIT           PRODUCE SNAP DUMP                            
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
SNAPIT   DS    0H                                                               
         ST    RE,SAVERE           SAVE RETURN ADDRESS                          
*                                                                               
         SR    RE,RB               DISPLACEMENT TO SNAPIT CALL                  
         STCM  RE,7,THREE          24-BIT ADDRESSING MODE ASSUMED               
         LA    R2,THREE                                                         
         LHI   R0,L'THREE                                                       
         LA    R1,HDR1HEXD         A(OUTPUT AREA)                               
SNAPIT10 LLC   RE,0(R2)                                                         
         SLL   RE,24                                                            
         SRDL  RE,28               ISOLATE HIGH-ORDER NIBBLE                    
         SRL   RF,28               ISOLATE LOW-ORDER NIBBLE                     
         LLC   RE,HEXTAB(RE)                                                    
         STC   RE,0(R1)                                                         
         LLC   RF,HEXTAB(RF)                                                    
         STC   RF,1(R1)                                                         
         LA    R2,1(R2)            BUMP TO NEXT BYTE                            
         LA    R1,2(R1)                                                         
         BCT   R0,SNAPIT10                                                      
*                                                                               
         LLC   R1,4(RB)            LENGTH OF EXIT NAME                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HDR1NAME(0),5(RB)   MOVE EXIT NAME TO OUTPUT AREA                
*                                                                               
*                                     R3 = A(SORT RECORD)                       
         LA    R4,PNA_PARMD_LENQ(R3)  R4 = A(JUST BEYOND SORT RECORD)           
         OPEN  (SNAPDUMP,OUTPUT)   OPEN SNAP DUMP DATASET                       
*                                                                               
*                                  DUMP PSW, REGS, CSECT, AND SORT REC.         
         SNAP  DCB=SNAPDUMP,PDATA=(PSW,REGS,SA,SAH,SUBTASKS,JPA),      +        
               STORAGE=((R3),(R4)),STRHDR=HDR1L                                 
         LTR   RF,RF               WAS SNAP SUCCESSFUL?                         
         BZ    SNAPITX             YES                                          
         ABEND 301                 ABEND IF WE CAN'T GET A SNAP DUMP            
         CLOSE SNAPDUMP                                                         
SNAPITX  L     RE,SAVERE                                                        
         BSM   0,RE                                                             
*                                                                               
KEEPREC  DS    0H                                                               
         SGR   GRF,GRF             SET RC=0: KEEP RECORD                        
         SGR   GR1,GR1                                                          
         LR    R1,R3               SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         LGHI  GRF,4               SET RC=4: DELETE RECORD                      
         B     GOBACK                                                           
*                                                                               
ADDREC   DS    0H                                                               
         LGHI  GRF,12              SET RC=12: INSERT RECORD                     
         SGR   GR1,GR1                                                          
         LA    R1,NEWREC           SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
ERROR    DS    0H                                                               
         LGHI  GRF,16              DFSORT WILL TERMINATE WITH RC=16             
         B     GOBACK                                                           
*                                                                               
EOF      DS    0H                                                               
         GOTO1 =V(PRINT),DMCB,=C'CLOSE' CLOSE SYSPRINT (PREVENT SC03)           
*                                                                               
         LGHI  GRF,8               SET RC=8:EOF                                 
*                                                                               
GOBACK   DS    0H                                                               
         LMH   GR0,GR0,DFSORTHH    RESTORE DFSORT'S HIGH HALVES                 
         LMH   GR2,GRE,DFSORTHH+8                                               
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE DFSORT'S REGS                        
         BSM   0,RE                RETURN TO DFSORT                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         ORG   DELMUF2+(((*-DELMUF2)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
HIGHDATE DCB   DDNAME=HIGHDATE,MACRF=GM,DSORG=PS,LRECL=8,RECFM=FB               
SNAPDUMP DCB   DDNAME=SNAPDUMP,DSORG=PS,RECFM=VBA,MACRF=(W),LRECL=125, +        
               BLKSIZE=1632                                                     
HEXTAB   DC    C'0123456789ABCDEF'                                              
*                                                                               
HDR1L    DC    AL1(HDR1LQ)         L'HEADER                                     
HDR1     DC    C'*** YOU DIED AT OFFSET '                                       
HDR1HEXD DS    CL6                 HEX OFFSET WITHIN CSECT                      
         DC    C' IN DFSORT EXIT '                                              
HDR1NAME DC    CL8' '              EXIT NAME                                    
         DC    C'. SORT RECORD FOLLOWS ***'                                     
HDR1LQ   EQU   *-HDR1                                                           
*                                                                               
DUB      DS    D                                                                
SAVERE   DS    F                   FOR INTERNAL SUBROUTINES                     
DFSORTHH DS    16F                 HIGH HALVES OF DFSORT'S REGISTERS            
DMCB     DS    6F                                                               
THREE    DS    XL3                                                              
YYMMDD   DS    CL6                                                              
WORK     DS    CL17                                                             
FRSTTIME DC    C'Y'                                                             
ASRVICES DC    A(PROD_SERVICES)    ASSUME PRODUCTION DATA BEING READ            
SAVEMODE DS    CL(L'PNA_PARM_KEYWORD)   SAVED MODE VALUE                        
*                                                                               
PRMFLAGS DC    X'00'               BIT ON = WE SAW THAT PARAMETER CARD          
MODEFLAG EQU   X'80'               MODE=                                        
DATEFLAG EQU   X'40'               DATE=                                        
SRVCFLAG EQU   X'20'               SERVICE=                                     
BOOKFLAG EQU   X'10'               BOOK=                                        
MKTFLAG  EQU   X'08'               MARKET=                                      
STRMFLAG EQU   X'04'               STREAM=                                      
EXMKTFLG EQU   X'02'               EXMARKET=                                    
*                                                                               
PRMFLAG2 DC    X'00'                                                            
STRM_ALL EQU   X'80'               STREAM=ALL                                   
STRT_MKT EQU   X'40'               MARKET=>N (MARKET GREATER THAN N)            
ACURSTRM DC    A(ALL_STREAMS)      A(CURRENT ENTRY IN STREAM TABLE)             
*                                                                               
         DS    0H                                                               
CURMKT#  DC    AL2(LOWMKT#)        CURRENT POSSIBLE MARKET NUMBER               
*                                   (START WITH LOWEST POSSIBLE NUMBER)         
LOWMKT#  EQU   2                   LOWEST VALID MARKET NUMBER                   
MAXMKT#  EQU   999                 HIGHEST POSSIBLE MARKET NUMBER               
*                                                                               
MODE_KEYWORD     DC CL(L'PNA_PARM_KEYWORD)'MODE'                                
DATA_KEYWORD     DC CL(L'PNA_PARM_KEYWORD)'DATA'                                
SERVICE_KEYWORD  DC CL(L'PNA_PARM_KEYWORD)'SERVICE'                             
DATE_KEYWORD     DC CL(L'PNA_PARM_KEYWORD)'DATE'                                
STREAM_KEYWORD   DC CL(L'PNA_PARM_KEYWORD)'STREAM'                              
BOOK_KEYWORD     DC CL(L'PNA_PARM_KEYWORD)'BOOK'                                
MARKET_KEYWORD   DC CL(L'PNA_PARM_KEYWORD)'MARKET'                              
EXMARKET_KEYWORD DC CL(L'PNA_PARM_KEYWORD)'EXMARKET'                            
*                                                                               
NEWREC   DS    CL(PNA_PARMD_LENQ)  BUILD INSERTED RECORD HERE                   
         SPACE 3                                                                
ALL_STREAMS      DS 0C                  TABLE OF ALL VALID STREAMS              
         DC    CL(L'PNA_PARM_VALUE)'LS'                                         
         DC    CL(L'PNA_PARM_VALUE)'L3'                                         
         DC    CL(L'PNA_PARM_VALUE)'LO'                                         
         DC    CL(L'PNA_PARM_VALUE)'LS_R0'                                      
         DC    CL(L'PNA_PARM_VALUE)'L3_R0'                                      
         DC    CL(L'PNA_PARM_VALUE)'LO_R0'                                      
STANDARD_STREAMS DS 0C                  TABLE OF STANDARD STREAMS               
         DC    CL(L'PNA_PARM_VALUE)'L1'                                         
         DC    CL(L'PNA_PARM_VALUE)'L7'                                         
         DC    CL(L'PNA_PARM_VALUE)'L7DMA'                                      
         DC    CL(L'PNA_PARM_VALUE)'L7_R0'                                      
         DC    X'FF'                                                            
         SPACE 3                                                                
PROD_SERVICES DS 0C                  TABLE OF VALID PRODUCTION SERVICES         
         DC    CL(L'PNA_PARM_VALUE)'NSI'                                        
         DC    CL(L'PNA_PARM_VALUE)'NHSI'                                       
         DC    CL(L'PNA_PARM_VALUE)'NSIBLK'                                     
         DC    CL(L'PNA_PARM_VALUE)'NSIHSP'                                     
         DC    CL(L'PNA_PARM_VALUE)'SVIP'                                       
         DC    CL(L'PNA_PARM_VALUE)'NSIOLY'                                     
         DC    CL(L'PNA_PARM_VALUE)'NSIX'                                       
         DC    CL(L'PNA_PARM_VALUE)'NSIXHSP'                                    
         DC    CL(L'PNA_PARM_VALUE)'NCM'                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
TEST_SERVICES DS 0C                  TABLE OF VALID TEST SERVICES               
         DC    CL(L'PNA_PARM_VALUE)'NSIT'                                       
         DC    CL(L'PNA_PARM_VALUE)'NHSIT'                                      
         DC    CL(L'PNA_PARM_VALUE)'NSIBLKT'                                    
         DC    CL(L'PNA_PARM_VALUE)'NSIHSPT'                                    
         DC    CL(L'PNA_PARM_VALUE)'NSIOLYT'                                    
         DC    CL(L'PNA_PARM_VALUE)'NSIX'                                       
         DC    CL(L'PNA_PARM_VALUE)'NSIXHSP'                                    
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL16'******SSB*******'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOMTIND            SYSTEM DATAMGR FLAGS                         
         DC    AL1(SSOWRTN)        WRITE=NO (DON'T OPEN FOR UPDATE)             
         ORG                                                                    
         EJECT                                                                  
*                                                                               
       ++INCLUDE DELMINDXD                                                      
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         ORG   P                                                                
P_PARM_KEYWORD DS CL(L'PNA_PARM_KEYWORD)                                        
         DS    C                                                                
P_PARM_VALUE   DS CL(L'PNA_PARM_VALUE)                                          
         ORG                                                                    
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028DELMUFXT2 01/07/21'                                      
         END                                                                    
