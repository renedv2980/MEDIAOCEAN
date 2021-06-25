*          DATA SET RERE01A    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T80701A,*                                                                
*INCLUDE HEXOUT                    DUMMY INCLUDE.  SEE BELOW.                   
REQ1     TITLE 'T80701 - REREQ01 - NEW REP REQUEST SCREEN BUILDER'              
*                                                                               
**********************************************************************          
*                                                                    *          
*    REREQ01 (T80701) --- REQUEST PROGRAM - BUILDS SCN               *          
*                                                                    *          
*   ** NOTE **  'HEXOUT' INCLUDED SO THE LINK-EDITOR WILL TELL US    *          
*               THE END OF THIS CSECT (AFTER THE 'LEVEL' LITERAL).   *          
*               THE REREQDEF OVERLAY WILL BE LOADED AT THE ADDRESS.  *          
*                                                                    *          
*                                                                    *          
*--------------------------------------------------------------------*          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
*  12/12/89  PJS  TAKEN FROM RENEW01 AND MODIFIED FOR NEW RQST.      *          
*                                                                    *          
*  AUG13/90 (MRR) --- CHANGED REQUEST TABLE AND LABELS DUE TO SIZE   *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
         PRINT NOGEN                                                            
T80701   CSECT                                                                  
         NMOD1 0,T80701,RR=R2                                                   
*                                                                               
         L     R9,0(R1)            A(WORK AREA)                                 
         USING REQWRK,R9                                                        
*                                                                               
         ST    R2,SUBRELO          SUB OVERLAY RELOCATION FACTOR                
*                                                                               
         L     RA,ASAVE                                                         
         USING TWAD,RA             TWA/SCREEN AREA                              
*                                                                               
         MVC   GOPROC,SELECTSW     SET 0/^0                                     
*                                                                               
*- SEE IF SCREEN IS LOADABLE FROM DISK OR IF IT NEEDS TO BE BUILT.              
         LA    R1,LOADTBL                                                       
MAIN100  CLI   0(R1),0                                                          
         BE    MAIN200             NOT LOADABLE                                 
*                                                                               
         CLC   ACTION,0(R1)                                                     
         BE    MAIN120                                                          
*                                                                               
         LA    R1,2(R1)            NEXT ENTRY                                   
         B     MAIN100                                                          
*                                                                               
*- LOAD SCREEN AND EXIT                                                         
MAIN120  EQU   *                                                                
         MVI   GOPROC,1            GO DIRECTLY TO PROCESS OVLY                  
*                                                                               
         CLC   SCRNBILT(1),1(R1)   SCREEN ALREADY UP?                           
         BE    OKEXIT5                                                          
*                                                                               
         MVC   SCRNBILT(1),1(R1)   SAVE SCREEN NUMBER WE ARE LOADING            
*                                                                               
         LA    R2,RQSLAST          LOAD POINT                                   
         ZIC   R3,1(R1)            SCREEN NUMBER                                
         GOTO1 CALLOV,P1,((R3),(R2)),0                                          
         CLI   P2,X'FF'                                                         
         BNE   *+6                                                              
         DC    H'0'                ERROR LOADING SCREEN FROM DISK               
*                                                                               
         B     OKEXIT5                                                          
         SPACE 2                                                                
*                                                                               
*- ACTIONS WITH LOADABLE SCREENS.                                               
LOADTBL  DS    0XL2                X'ACTION EQU',X'SCREEN NUMBER'               
         DC    AL1(EQLIST),AL1(SCLIST)                                          
         DC    X'00'               EOT                                          
         DS    0H                  ALIGNMENT                                    
         SPACE 2                                                                
*                                                                               
*- SCREEN NEEDS TO BE BUILT ON THE FLY FROM REQTBL ENTRY.                       
*  LOAD IN REREQ10 AND 11 - TABLE DEFINITION PHASE.  CALL PHASE TO              
*  SET UP AREQTBL (RELOCATED TABLE).  THEN CALL 'FINDREQ'                       
*  IN BASE TO FIND REQUEST WE NEED TO BUILD.                                    
*                                                                               
*  ONCE TABLE ENTRY IS FOUND, MOVE ENTRY TO WORK AREA.                          
*  DEFAULTS ARE ALSO SAVED (IN THE TWA)                                         
*                                                                               
MAIN200  EQU   *                                                                
         L     R2,=V(HEXOUT)       LOAD AND CALL TBL DEF OVLY                   
         A     R2,SUBRELO                                                       
         GOTO1 AGETOVLY,P1,('OVTBL1',(R2))                                      
         L     R2,P1               GET NEW OVERLAY END                          
         GOTO1 AGETOVLY,P1,('OVTBL2',(R2))                                      
*                                                                               
         GOTO1 AFINDREQ,P1,0       FIND REQTBL ENTRY                            
         BZ    *+6                                                              
         DC    H'0'                RQST NOT FOUND IN REQTBL?                    
*                                                                               
         LR    RC,RA               MOVE REQTBL ENTRY TO WRK AREA                
         AH    RC,=H'4096'                                                      
         USING TWAD+4096,RC        FOR ADDRESSABILITY                           
*                                                                               
         L     RE,P1               A(REQTBL ENTRY)                              
         SR    R1,R1                                                            
         ICM   R1,3,RQLNTRY(RE)    LENGTH TO MOVE                               
*                                                                               
         LA    RF,REQDEFX-2        DON'T EXCEED WRK AREA                        
         LA    R0,REQDEF                                                        
         AR    R0,R1                                                            
         CR    R0,RF                                                            
         BNH   *+6                                                              
         DC    H'0'                REQTBL ENTRY WON'T FIT IN WRK AREA.          
*                                                                               
         MOVE  (REQDEF,(R1)),(RE)                                               
*                                                                               
         XC    DEFAULTS,DEFAULTS   SAVE DEFAULTS IN TWA                         
         L     RF,P1               A(REQTBL ENTRY IN OVLY)                      
         SR    RE,RE                                                            
         ICM   RE,7,RQDFLT(RF)     A(DEFAULT LIST, RELATIVE TO REQTBL)          
         A     RE,AREQTBLC         RELOCATE                                     
*                                                                               
         LR    R1,RE                                                            
MAIN205  CLC   =XL2'00',0(R1)                                                   
         BE    MAIN210                                                          
         ZIC   RF,2(R1)            DATA LEN                                     
         LA    R1,3(RF,R1)         + ENTRY OVERHEAD = A(NEXT ENTRY)             
         B     MAIN205                                                          
*                                                                               
MAIN210  LA    R1,2(R1)            INCLUDE THE XL2'00' IN MOVE                  
         SR    R1,RE                                                            
         MOVE  (DEFAULTS,(R1)),(RE)                                             
         LA    RE,REQDEF           POINT TO RQST DEF IN WRK AREA                
         ST    RE,AREQNTRY                                                      
         ST    RE,AREQTBLC                                                      
*                                                                               
         SR    R1,R1               MOVE OUT END OF ENTRY MARKER                 
         ICM   R1,3,RQLNTRY(RE)                                                 
         AR    RE,R1                                                            
         MVC   0(2,RE),=XL2'00'                                                 
*                                                                               
         DROP  RC                                                               
*                                                                               
         CLC   SCRNBILT,RQSNUM     SAVE RPT ID FROM SCREEN                      
         BE    OKEXIT5                                                          
*                                                                               
*- START BUILDING SCREEN                                                        
         L     RE,AREQNTRY         REQTBL ENTRY TO BUILD SCREEN FROM            
         LA    RE,RQFIELD(RE)      POINT TO FIELD DEFINITIONS                   
         ST    RE,WKFLD                                                         
*                                                                               
         LA    RE,RQSLAST          START BUILDING HERE                          
         ST    RE,WKTWA                                                         
*                                                                               
         LA    RE,REQMAP           BUILD MAP HERE                               
         ST    RE,WKMAP                                                         
         MVC   0(2,RE),=XL2'00'    END OF MAP INDICATOR                         
*                                                                               
         MVI   HAVEREQD,0          ASSUME NO REQUIRED FLDS                      
*                                                                               
MAIN220  EQU   *                                                                
         L     RE,WKFLD            END OF FIELDS?                               
         CLC   =XL2'00',0(RE)                                                   
         BE    MAIN300                                                          
*                                                                               
         BAS   RE,FINDFLD          FIND FIELD IN FIELD TBL                      
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,MOVEFLD          MOVE FLD TO TWA                              
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,MAPIT            MAP SCREEN TO VAL RTN.                       
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,NEXTDEF          POINT TO NEXT FLD DEF                        
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,NEXTTWA          POINT TO NEXT FLD TWA                        
         BNZ   ERROR                                                            
         B     MAIN220                                                          
*                                                                               
*- SCREEN IS UP.                                                                
*  SET TRANSMIT BITS FOR HEADER FIELDS (CLEARS RESIDUAL SCREEN FLDS)            
*  IF NO REQUIRED FIELDS IN THIS REQUEST, TURN ON 'MODIFIED' BIT                
*     IN ACTION TO PREVENT 'NO INPUT' MESSAGE                                   
*  PUT OUT LAST FIELD INDICATOR AND EXIT                                        
MAIN300  EQU   *                                                                
         LA    RE,RQSMSGH          1ST LINE OF BASE SCREEN                      
         L     RF,WKTWA            A(END OF SCREEN)                             
*                                                                               
MAIN320  OI    6(RE),OI1T          TRANSMIT                                     
         ZIC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         CR    RE,RF               AT END OF BASE SCREEN?                       
         BL    MAIN320             NO. LOOP BACK FOR NEXT FIELD                 
*                                                                               
         CLI   HAVEREQD,0                                                       
         BNE   MAIN330             REQ'D FIELDS ON SCREEN                       
*                                                                               
         OI    RQSACTH+6,X'01'     TREAT AS MODIFIED                            
*                                                                               
MAIN330  EQU   *                                                                
         L     RE,WKTWA                                                         
         MVC   0(3,RE),=X'000100'     LAST FIELD                                
*                                                                               
*- IF SCREEN HAS NO FIELDS, GO DIRECTLY TO PROCESS OVLY                         
         CLC   =XL2'00',REQMAP                                                  
         BNE   MAIN340                                                          
         MVI   GOPROC,1                                                         
*                                                                               
MAIN340  EQU   *                                                                
         B     OKEXIT                                                           
         SPACE                                                                  
OKEXIT   EQU   *                                                                
         MVC   SCRNBILT,RQSNUM     SAVE RPT ID FROM SCREEN                      
OKEXIT5  SR    R0,R0               GOOD CC                                      
         B     EXIT                                                             
*                                                                               
ERROR    EQU   *                                                                
         XC    SCRNBILT,SCRNBILT   FORCE NEW SCREEN                             
         LTR   RD,RD               BAD CC                                       
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- FINDFLD -- FIND FIELD NUMBER FROM DEFINITION ENTRY IN FIELD TBL              
*                                                                               
*  INPUT:  WKFLD -- A(FIELD DEFINITION ENTRY)                                   
*  RETURN:  AFLD -- A(FIELD TBL ENTRY)                                          
*                                                                               
FINDFLD  NTR1                                                                   
         L     RE,WKFLD                                                         
         LA    RF,FLDTBL                                                        
*                                                                               
FINDF20  EQU   *                                                                
         CLI   0(RF),0             END OF FIELD TBL                             
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   1(2,RF),FDNUM(RE)   FLDTBL -VS- FLD DEF ENTRY                    
         BE    FINDF40                                                          
*                                                                               
         ZIC   R0,0(RF)            LOOK AT NEXT FLDTBL ENTRY                    
         AR    RF,R0                                                            
         B     FINDF20                                                          
*                                                                               
FINDF40  EQU   *                                                                
         ST    RF,AFLD             PASS BACK A(FLDTBL ENTRY)                    
         B     OKEXIT                                                           
         EJECT                                                                  
*                                                                               
*- MOVEFLD -- MOVE FIELD FROM FIELD TABLE TO TWA                                
*                                                                               
*  INPUT:  AFLD  - A(FLDTBL ENTRY)                                              
*          WKFLD - A(FIELD DEFINITION ENTRY)                                    
*          WKTWA - A(SCREEN OUTPUT AREA)                                        
*                                                                               
MOVEFLD  NTR1                                                                   
*                                                                               
         L     R2,WKTWA            A(OUTPUT AREA)                               
         L     R3,AFLD             A(FLDTBL ENTRY)                              
         L     R4,WKFLD            A(FLD DEF ENTRY)                             
*                                                                               
*- BUILD FIELD IN TWA AREA.                                                     
*                                                                               
         SPACE                                                                  
*                                                                               
*- MAKE SPACE FOR HEADER                                                        
         XC    0(8,R2),0(R2)                                                    
*                                                                               
*- DETERMINE DATA LENGTH AND FIELD SIZE (DATA LEN + 8 BYTE HEADER)              
         ZIC   RF,0(R3)            FLDTBL ENTRY LEN                             
         LA    R0,FLDLHEAD                                                      
         SR    RF,R0               - FLDTBL ENTRY HEADER                        
         STC   RF,7(R2)            = DATA LENGTH (SAVE IN FIELD HEADER)         
*                                                                               
         LA    R0,8(RF)            LENGTH OF TWA FIELD (DATA+HEADER)            
         STC   R0,0(R2)                                                         
*                                                                               
*- MOVE FIELD DATA FROM TBL TO TWA                                              
         BCTR  RF,0                                                             
         EX    RF,FLD2TWA          MOVE FROM FIELD TO TWA                       
*                                                                               
*- SET SCREEN ADDRESS IN HEADER  (ROW-1)*80+(COL-1)                             
         ZIC   RE,FDROW(R4)        ROW NUMBER                                   
         BCTR  RE,0                -1                                           
         MH    RE,=H'80'           * 80 (BYTE PER ROW)                          
*                                                                               
         ZIC   RF,FDCOL(R4)        COLUMN NUMBER                                
         BCTR  RF,0                -1                                           
*                                                                               
         AR    RE,RF               (ROW-1)*80 + (COL-1)                         
         STCM  RE,3,2(R2)          SCREEN ADDRESS IN FLD HEADER                 
*                                                                               
*- ALWAYS TRANSMIT THE FIELD                                                    
         MVI   6(R2),X'80'         TRANSMIT THIS FIELD                          
*                                                                               
*- IS FIELD PROTECTED?                                                          
         TM    FDCNTL(R4),FCCMT                                                 
         BZ    MFLD120                                                          
         OI    1(R2),X'20'         TURN ON PROTECT BIT                          
*                                                                               
*- SET HI-INTENSITY IF REQUIRED                                                 
MFLD120  EQU   *                                                                
         CLI   WHEN,EQSOON         RUNNING SOON?                                
         BNE   MFLD140                                                          
         TM    FDCNTL(R4),FCRQSOON FIELD REQ. FOR SOON?                         
         BZ    MFLD140                                                          
         OI    1(R2),X'08'         SET HI-INTENSITY                             
*                                                                               
MFLD140  EQU   *                                                                
         CLI   WHEN,EQOVRN         RUNNING OVERNIGHT?                           
         BNE   MFLD160                                                          
         TM    FDCNTL(R4),FCRQOVRN FIELD REQ. FOR OVERNIGHT                     
         BZ    MFLD160                                                          
         OI    1(R2),X'08'         SET HI-INTENSITY                             
*                                                                               
*- NUMERIC?                                                                     
MFLD160  EQU   *                                                                
         TM    FDCNTL(R4),FCNUM                                                 
         BZ    MFLD180                                                          
         OI    1(R2),X'10'         TURN ON NUMERIC BIT                          
*                                                                               
MFLD180  EQU   *                                                                
         B     OKEXIT                                                           
*                                                                               
FLD2TWA  MVC   8(0,R2),FLDLHEAD(R3)    MOVE FIELD DATA TO TWA                   
         EJECT                                                                  
*                                                                               
*- MAPIT -- ESTABLISH LINKAGE BETWEEN SCREEN FIELDS AND VALIDATION              
*           ROUTINES.  (ESSENTIALLY, A LIST OF UNPROTECTED FIELDS)              
*                                                                               
*  INPUT:  WKTWA - A(SCREEN FIELD WE JUST BUILT)                                
*          WKFLD - A(FIELD DEFINITION ENTRY IN BASE)                            
*          WKMAP - A(NEXT MAP AREA)  * UPDATED ON RETURN *                      
*                                                                               
MAPIT    NTR1                                                                   
         L     R2,WKTWA                                                         
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    MAPIT90             YES.  OUT.                                   
*                                                                               
         L     R3,WKFLD                                                         
         L     R4,WKMAP                                                         
         MVC   MAPVAL(2,R4),FDVAL(R3) VAL ROUTINE                               
         MVC   MAPFMT(1,R4),FDFMT(R3) FORMAT BITS                               
         MVC   MAPCNTL(1,R4),FDCNTL(R3) CONTROL BITS                            
         SR    R2,RA               TWA FLD ADDRESS - TWA START                  
         ST    R2,MAPFLD(R4)       FIELD HEADER DISP                            
*                                                                               
         LA    RE,REQMAPX                                                       
         CR    R4,RE                                                            
         BL    *+6                                                              
         DC    H'0'                REQMAP EXCEEDED.                             
*                                                                               
         LA    R4,MAPLNTRY(R4)                                                  
         ST    R4,WKMAP                                                         
         MVC   0(2,R4),=XL2'00'    NEW END OF LIST                              
*                                                                               
MAPIT90  B     OKEXIT                                                           
         EJECT                                                                  
*                                                                               
*- NEXTDEF -- POINT TO NEXT FIELD DEFINITION ENTRY                              
NEXTDEF  NTR1                                                                   
         L     RE,WKFLD                                                         
         LA    RE,FDLNTRY(RE)                                                   
         ST    RE,WKFLD                                                         
         B     OKEXIT                                                           
         SPACE 2                                                                
*                                                                               
*- NEXTTWA -- POINT TO NEXT FIELD TWA OUTPUT AREA                               
*             CHECK FOR SCREEN MAX EXCEEDED.                                    
NEXTTWA  NTR1                                                                   
         L     RE,WKTWA                                                         
         ZIC   RF,0(RE)            FIELD LEN (HEADER + DATA)                    
         AR    RE,RF                                                            
*                                                                               
         LA    RF,SCREENX-1        DON'T EXCEED SCREEN AREA                     
         CR    RE,RF                 (THE -1 IS FOR LAST FIELD BYTE)            
         BNH   *+6                                                              
         DC    H'0'                SCREEN EXCEEDS MAX SPACE                     
         ST    RE,WKTWA                                                         
         B     OKEXIT                                                           
         SPACE 2                                                                
         LTORG                                                                  
       ++INCLUDE REREQFLD                                                       
*                                                                               
*- REQUEST TWA AND WORK AREA DSECTS                                             
         PRINT OFF                                                              
       ++INCLUDE REREQTWA                                                       
       ++INCLUDE REREQWRK                                                       
         PRINT OFF                                                              
         SPACE                                                                  
         ORG   USERWORK                                                         
SUBRELO  DS    F                   SUB OVERLAY RELOCATION FACTOR                
WKFLD    DS    A                   A(REQTBL FLD DEFINITION FOR BUILD)           
WKTWA    DS    A                   A(SCREEN OUTPUT AREA)                        
AFLD     DS    A                   A(FLDTBL ENTRY)                              
WKMAP    DS    A                   A(REQMAP ENTRY)                              
*                                                                               
HAVEREQD DS    X                   ^0 = REQUIRED FLD ON SCREEN                  
*                                                                               
         SPACE 2                                                                
AMTLOCAL EQU   USERWRKX-*          AVAILABLE USERWORK LEN                       
         DS    (AMTLOCAL)X                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006RERE01A   05/01/02'                                      
         END                                                                    
