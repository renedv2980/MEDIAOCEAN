*          DATA SET GENLAYXIT1 AT LEVEL 001 AS OF 02/08/21                      
*PHASE GENLYX1A                                                                 
*INCLUDE REGSAVE                                                                
***********************************************************************         
*                                                                     *         
* THIS IS A DFSORT USER EXIT. IT IS CALLED BY THE COMPRESSED DATE     *         
* LAYOUT TABLE GENERATOR.                                             *         
*                                                                     *         
* INPUT FILE CONSISTS OF ORDINARY (DS/DC) SYMBOL RECORDS. THEY ARE    *         
* SORTED TOGETHER BY ESDID. WITHIN EACH ESDID, ALL COMPRESSED DATE    *         
* SYMBOLS APPEAR BEFORE ALL NON-DATE SYMBOLS.                         *         
*                                                                     *         
* THIS EXIT RETURNS ***ERROR MESSAGES***. I.E., IF THE INPUT IS OKAY, *         
* THEN WE DISCARD THE RECORD UPON RETURN. IF THE SYMBOL HAS AN ISSUE, *         
* THEN WE BUILD AN ERROR MESSAGE AND RETURN IT TO DFSORT IN PLACE OF  *         
* THE ORIGINAL SYMBOL RECORD.                                         *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDASMADATA                                                     
         PRINT ON                                                               
         EJECT                                                                  
GENLAYX1 CSECT                                                                  
       PRINT NOGEN                                                              
*                                                                               
       ENTRY E35                 MUST BE "E35" (FOR DFSORT)                     
*                                                                               
       REQUS                                                                    
*                                                                               
       USING E35,RF              RF = WHERE WE ARE LOADED                       
E35    SAVE  (14,12),,GENLAYX1   SAVE DFSORT'S REGISTERS                        
       STMH  GR0,GRF,DFSORTHH    SAVE DFSORT'S REGS (HIGH HALVES)               
       DROP  RF                                                                 
*                                                                               
*                                INITIALIZE OUR USUAL RD CHAIN                  
       LR    RB,RF               USE RB AS PROGRAM BASE                         
       USING E35,RB                                                             
       L     RE,=V(REGSAVE)      GET OUR SAVE AREA CHAIN                        
       ST    RD,4(,RE)           SAVE BACKWARD POINTER IN OUR AREA              
       ST    RE,8(,RD)                                                          
       LR    RD,RE               SET OUR SAVE AREA                              
*                                                                               
       L     R3,0(,R1)           LOAD A(RECORD PASSED BY DFSORT)                
       LTR   R3,R3               EOF?                                           
       BZ    EOF                 YES: DO NOT RETURN                             
                                                                                
       USING ASMADATA,R3                                                        
       CLC   ADATA_RECTYPE,=AL2(ADATA_RECSYM)                                   
       JNE   *+2                 ONLY SYMBOL RECORDS ARE EXPECTED!              
       CLI   ADSYM_TYPE,ADSYM_TYPE_ORDINARY                                     
       JNE   *+2                 ONLY DS/DC SYMBOLS ARE EXPECTED!               
       CLC   ADSYM_LOCTR,=A(MAXDISP-1) CAN WE HANDLE THIS DISPLACEMENT?         
       JH    *+2                 NO: INCREASE MAXDISP                           
                                                                                
* AS WE ENCOUNTER EACH SYMBOL, WE LOOK AT ITS ATTRIBUTES, CHECKING FOR          
* ANY INCONSISTENCIES WITH THE ATTRIBUTES OF ANY PREVIOUSLY-SEEN SYMBOL         
* THAT HAS AN OVERLAPPING DISPLACEMENT.                                         
* FIELD "DATARRAY" CONTAINS THE ACCUMULATED STATUS INFORMATION FOR EACH         
* SYMBOL IN THE CURRENT STRUCT (DSECT). IT IS INDEXED BY THE                    
* DISPLACEMENT WITHIN THE STRUCT.                                               
* WE ALSO MAINTAIN AN INDEXED ARRAY OF SYMBOL NAMES, SO THAT IF WE              
* GENERATE AN ERROR MESSAGE, WE CAN SHOW WHICH SYMBOL WAS OVERLAPPED.           
                                                                                
       IF (CLC,ADSYM_ESDID,NE,SAVE_ESDID)  NEW STRUCT (I.E., DSECT)?            
         MVC   SAVE_ESDID,ADSYM_ESDID        YES: SAVE THE NEW ESDID            
         XCEFL DATARRAY,'MAXDISP'            CLEAR DATE ARRAY                   
         L     RF,=A(MAXDISP*L'NAMARRAY)     BLANK-PREFILL NAME ARRAY           
         LA    RE,NAMARRAY                                                      
         IILF  GR1,X'40000000'               (PAD CHARACTER IS BLANK)           
         MVCL  RE,R0                                                            
       ENDIF ,                                                                  
                                                                                
       MVC   OUTSYMNM,SPACES        PREPARE FOR POSSIBLE ERROR MESSAGE          
       ICM   RF,15,ADSYM_NAME_OFF   SOFT OFFSET TO SYMBOL NAME                  
       LA    RF,4(R3,RF)            RF = A(SYMBOL NAME)                         
       ICM   R1,15,ADSYM_NAME_LEN   SYMBOL NAME LENGTH                          
       BCTR  R1,0                                                               
       EX    R1,*+8                                                             
       J     *+10                                                               
       MVC   OUTSYMNM(0),0(RF)      SAVE THE SYMBOL NAME                        
       MVC   OUTSTMT#,ADSYM_STMT    SOURCE STATEMENT NUMBER                     
                                                                                
       XC    WRKENTRY,WRKENTRY      RESET WORK BYTES                            
*                                                                               
       IF (ICM,R2,15,ADSYM_DUP,Z)   DUPLICATION FACTOR OF ZERO...               
         LHI R2,1                   ...IS EQUIVALENT TO DUP. FACTOR ONE         
       ENDIF ,                      R2 = EFFECTIVE DUPLICATION FACTOR           
                                                                                
* DATE FIELDS COME BEFORE NON-DATE FIELDS, AND ARE HANDLED DIFFERENTLY.         
                                                                                
       IF (CLI,ADSYM_PROGRAM_TYPE+3,EQ,DT02),OR,                                
          (CLI,ADSYM_PROGRAM_TYPE+3,EQ,DT02X),OR,                               
          (CLI,ADSYM_PROGRAM_TYPE+3,EQ,DT02N)                                   
                                                                                
         IF (CLC,ADSYM_BYTE_LEN,NE,=F'2') L'COMPRESSED DATE FIELD               
           MVC OUTMSG,=CL40'LENGTH OF DATE FIELD MUST BE 2'                     
           J   KEEPREC             *** EXIT TO DFSORT ***                       
         ENDIF ,                                                                
                                                                                
* IT'S A DATE FIELD. DERIVE ITS ATTRIBUTES, AND SAVE THEM IN FIELD              
* 'WRKENTRY'.                                                                   
* THIS CODE ASSUMES THAT THE COMPRESSED DATE PROGRAM TYPE VALUES                
* ARE WITHIN THE RANGE 1..15 (I.E., THEY MUST FIT IN LOW-ORDER NIBBLE)          
*                                                                               
         MVC WRKENTRY_B1,ADSYM_PROGRAM_TYPE+3 (DT02/DT02X/DT02N)                
         OI  WRKENTRY_B1,B'00010000'          TAG: "FIELD BYTE 1"               
         MVC WRKENTRY_B2,ADSYM_PROGRAM_TYPE+3 (DT02/DT02X/DT02N)                
         OI  WRKENTRY_B2,B'00100000'          TAG: "FIELD BYTE 2"               
         IF (TM,ADSYM_PROGRAM_TYPE,X'40',O)   DTSPCL (PROGRAM TYPE HOB)         
           OI WRKENTRY_B1,SPECIAL                                               
           OI WRKENTRY_B2,SPECIAL                                               
         ENDIF ,                                                                
*                                                                               
* WORK BYTES ARE NOW SET. START AT THE FIRST BYTE OF THE CURRENT                
* SYMBOL'S DISPLACEMENT IN 'DATARRAY', AND ITERATE BASED ON THE                 
* DUPLICATION FACTOR.                                                           
*                                                                               
         L   R4,ADSYM_LOCTR        LOCATION COUNTER (DISPLACEMENT)              
         LR  R5,R4                 INDEX INTO THE NAME ARRAY                    
         LA  R4,DATARRAY(R4)       INDEX INTO THE DATE ARRAY                    
         MHI R5,L'NAMARRAY                                                      
         LA  R5,NAMARRAY(R5)                                                    
         DO  FROM=(R2)             REPEAT FOR DUPLICATION FACTOR:               
*                                                                               
* IF THE DATARRAY ENTRY IS CURRENTLY UNPOPULATED, THEN FILL IT IN WITH          
* THE WORK BYTES FROM THE CURRENT SYMBOL. O/W, MAKE SURE THAT ANY               
* DATE WE'VE ALREADY SEEN IS CONSISTENT WITH THE CURRENT SYMBOL.                
*                                                                               
           IF (OC,0(2,R4),0(R4),Z)   IF CURRENTLY UNPOPULATED:                  
             MVC 0(2,R4),WRKENTRY      POPULATE IT!                             
             MVC 0(L'NAMARRAY,R5),OUTSYMNM  SAVE SYMBOL NAME (BYTE 1)           
             MVC L'NAMARRAY(L'NAMARRAY,R5),OUTSYMNM (ALSO FOR BYTE 2)           
*                                                                               
* NOTE: DUPLICATE DATE FIELDS ARE DISCARDED DOWNSTREAM FROM THIS EXIT.          
* NEVERTHELESS, IF THERE ARE DUPLICATE DATE FIELDS, AND IF ANY ONE OF           
* THEM IS TAGGED DTSPCL, THEN *ALL* DUPLICATES MUST ALSO BE TAGGED AS           
* "SPECIAL." STRICTLY SPEAKING, THIS MAY NOT BE NECESSARY, BUT WE WANT          
* TO SEE CONSISTENCY IN THE DEVELOPER'S INTENTIONS.                             
*                                                                               
           ELSEIF (CLC,WRKENTRY,NE,0(R4)),AND,   O/W:                           
              (TM,0(R4),SPECIAL,Z),AND,  CURRENT ENTRY MUST EITHER...           
              (TM,1(R4),SPECIAL,Z)       MATCH OR BE TAGGED "SPECIAL"           
             MVC OUTMSG(17),=C'OVERLAP W/ DATE: '                               
             IF (CLC,0(L'NAMARRAY,R5),NE,SPACES) OVERLAPPED SYMBOL NAME         
               MVC OUTMSG+17(23),0(R5)            ...COULD BE AT +0...          
             ELSE ,                                                             
               MVC OUTMSG+17(23),L'NAMARRAY(R5)   ...OR +1                      
             ENDIF ,                                                            
             J   KEEPREC             *** EXIT TO DFSORT ***                     
           ENDIF ,                                                              
           LA  R4,2(,R4)             BUMP TO NEXT DATE IN DATARRAY              
           LA  R5,L'NAMARRAY*2(,R5)  BUMP TO NEXT ENTRY IN NAMARRAY             
                                                                                
         ENDDO ,                                                                
                                                                                
       ELSE ,                                                                   
*                                                                               
* THE SYMBOL IS *NOT* A DATE. NOT A SINGLE BYTE IN THE FIELD MAY                
* OVERLAP WITH ANY COMPRESSED DATE FIELD (UNLESS THE DATE IS TAGGED AS          
* "SPECIAL").                                                                   
*                                                                               
         MH  R2,ADSYM_BYTE_LEN+2   DUP * LENGTH = TOTAL #BYTES                  
         L   R4,ADSYM_LOCTR        LOCATION COUNTER (DISPLACEMENT)              
         LR  R5,R4                                                              
         LA  R4,DATARRAY(R4)       INDEX INTO THE DATE ARRAY                    
         MHI R5,L'NAMARRAY                                                      
         LA  R5,NAMARRAY(R5)       INDEX INTO THE NAME ARRAY                    
         DO  FROM=(R2)             FOR EACH BYTE IN THE NON-DATE FIELD:         
           IF (TM,0(R4),X'FF'-SPECIAL,NZ),AND,  DOES IT OVERLAP W/ DATE         
              (TM,0(R4),SPECIAL,Z)              ...THAT ISN'T SPECIAL?          
             MVC OUTMSG(17),=C'OVERLAP W/ DATE: '  YES: ERROR!                  
             IF (CLC,0(L'NAMARRAY,R5),NE,SPACES) OVERLAPPED SYMBOL NAME         
               MVC OUTMSG+17(23),0(R5)            ...COULD BE AT +0...          
             ELSE ,                                                             
               MVC OUTMSG+17(23),L'NAMARRAY(R5)   ...OR +1                      
             ENDIF ,                                                            
             J   KEEPREC             *** EXIT TO DFSORT ***                     
           ENDIF ,                                                              
           LA  R4,1(,R4)           BUMP TO NEXT BYTE IN DATARRAY                
           LA  R5,L'NAMARRAY(,R5)  BUMP TO NEXT ENTRY IN NAMARRAY               
         ENDDO ,                                                                
                                                                                
       ENDIF ,                                                                  
                                                                                
       DROP  R3                                                                 
                                                                                
* IF WE REACH THIS POINT, THEN THERE WAS NO PROBLEM WITH THE SYMBOL.            
* TELL DFSORT TO DISCARD IT (I.E., THERE'S NO ERROR MESSAGE TO RETURN).         
                                                                                
       B  DELREC                   DELETE THE INPUT RECORD                      
*                                                                               
         EJECT                                                                  
*                                                                               
KEEPREC  DS    0H                                                               
         SGR   GRF,GRF             SET RC=0: KEEP RECORD                        
         SGR   GR1,GR1                                                          
         LA    R1,OUTREC           SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         LGHI  GRF,4               SET RC=4: DELETE RECORD                      
         B     GOBACK                                                           
*                                                                               
*&&DO                                                                           
ADDREC   DS    0H                                                               
         LGHI  GRF,12              SET RC=12: INSERT RECORD                     
         SGR   GR1,GR1                                                          
         LA    R1,RECRDW           SET RECORD POINTER TO NEW RECORD             
         B     GOBACK                                                           
*&&                                                                             
*                                                                               
ERROR    DS    0H                                                               
         LGHI  GRF,16              DFSORT WILL TERMINATE WITH RC=16             
         B     GOBACK                                                           
*                                                                               
EOF      DS    0H                                                               
         LGHI  GRF,8               SET RC=8:EOF                                 
*                                                                               
GOBACK   DS    0H                                                               
         LMH   GR0,GR0,DFSORTHH                                                 
         LMH   GR2,GRE,DFSORTHH+8                                               
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE REGS                                 
         BSM   0,RE                RETURN                                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         ORG   GENLAYX1+(((*-GENLAYX1)/256)+1)*256 FOR I-CACHE PIPELINE         
*                                                                               
DFSORTHH DS    16F                 HIGH HALVES OF DFSORT'S REGISTERS            
SAVE_ESDID DC  XL4'00'             (CLEAR ARRAYS ON CHANGE OF ESDID)            
SPACES   DC    CL256' '                                                         
*                                                                               
SPECIAL  EQU   X'80'               RELATED TO DTSPCL                            
WRKENTRY DS    BL2                 WORK AREA FOR DATARRAY ELEMENT               
         ORG   WRKENTRY                                                         
WRKENTRY_B1 DS B                   WORK BYTE 1                                  
WRKENTRY_B2 DS B                   WORK BYTE 2                                  
*                                                                               
* THE STRUCTURE OF THIS OUTPUT RECORD MUST BE KEPT IN SYNC WITH THE             
* ICETOOL MODULE THAT INVOKED THIS EXIT. I.E., THIS IS THE FORMAT OF            
* THE ERROR MESSAGE THAT THE DFSORT LOGIC EXPECTS UPON RETURN FROM THE          
* MODS E35 STATEMENT.                                                           
OUTREC   DS    0F                                                               
         DC    AL2(OUTRECLQ)       RDW                                          
         DC    H'0'                                                             
OUTSYMNM DS    CL63                PROBLEMATIC SYMBOL NAME                      
OUTMSG   DS    CL40                ERROR MESSAGE                                
OUTSTMT# DS    XL4                 SOURCE STATEMENT #                           
OUTRECLQ EQU   *-OUTREC                                                         
*                                                                               
MAXDISP  EQU   2048                MAXIMUM DISPLACEMENT TO DATE FIELD           
*                                   (THIS CAN BE INCREASED IF NEEDED)           
*                                                                               
         DS    0D                                                               
         DC    C'DATARRAY'                                                      
DATARRAY DS    (MAXDISP)X          BYTE STATUS, INDEXED BY DISPLACEMENT         
*                                                                               
         DS    0D                                                               
         DC    C'NAMARRAY'                                                      
NAMARRAY DS    (MAXDISP)CL32       SYMBOL NAME ARRAY (INDEXED BY DISP.)         
*                                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001GENLAYXIT102/08/21'                                      
         END                                                                    
