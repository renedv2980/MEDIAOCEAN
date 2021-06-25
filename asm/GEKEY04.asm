*          DATA SET GEKEY04    AT LEVEL 028 AS OF 01/13/16                      
*PHASE TF0504B                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: TF0504 - PFM INTERFACE OVERLAY FOR PRINT SYSTEM             *         
*                                                                     *         
*  CALLED FROM: PFM INTERFACE CONTROLLER (TF0500), WHICH CALLS        *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  OUTPUTS: KEY FOR THE RECORD TYPE ACCORDING TO USER INPUT           *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
*INCLUDE PUBVAL                                                                 
         TITLE 'TF0504 - PFM OVERLAY FOR PRINT SYSTEM'                          
TF0504   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TF0504*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
*                                                                               
EXIT04   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE SCREEN                                                 *         
***********************************************************************         
VR       DS    0H                                                               
         LA    R2,CONRCRDH         POINT TO RECORD FIELD FIRST                  
         CLI   5(R2),0             NO RECORD?                                   
         BE    MISSFLD             MISSING RECORD                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         LA    R4,RECTABLE         POINT TO RECTABLE                            
         CLI   8(R2),C'?'          USER ASKED FOR HELP?                         
         BNE   VKRECLP             NOPE, CHECK ENTRY                            
*                                                                               
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         OI    STATFLAG,X'01'      HELP INVOKED                                 
         USING CONHEADH-64,RA                                                   
         XC    CONRCRD,CONRCRD     NULL THE DATA                                
         OI    CONRCRDH+6,X'80'    TRANSMIT THE DATA                            
         GOTO1 CLRSCN,DMCB,CONP0H                                               
         GOTO1 DISPHELP,DMCB,RECTABLE                                           
         B     EXIT04                                                           
*                                                                               
VKRECLP  CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    INVLFLD                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R4)                                                    
         BE    VKPARTS             PARTS OF THE KEY                             
         LA    R4,L'RECTABLE(R4)                                                
         B     VKRECLP                                                          
         EJECT                                                                  
VKPARTS  DS    0H                                                               
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         TM    STATFLAG,X'01'      HELP INVOKED ?                               
         USING CONHEADH-64,RA                                                   
         BZ    VKPARTS5                                                         
         GOTO1 CLRSCN,DMCB,CONP0H  CLEAR THE SCREEN                             
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         NI    STATFLAG,X'FF'-X'01'  HELP INVOKED ?                             
         USING CONHEADH-64,RA                                                   
VKPARTS5 L     R4,8(R4)            GET ADDRESS OF KEY COMPONENTS                
         L     R1,RELO             ADDRESS IS RELATIVE                          
         AR    R4,R1                   TO RELOCATION FACTOR                     
         NI    6(R2),X'FF'-X'40'   DON`T PUT CURSOR HERE AGAIN                  
         LA    R2,CONP0H           POINT TO 1ST PROTECTED FIELD                 
         ZIC   R1,0(R2)            GET LENGTH TO NEXT FIELD                     
         AR    R1,R2               POINT TO THE 1ST INPUT FIELD                 
         OI    6(R1),X'40'         CURSOR SHOULD BE HERE                        
         LR    R3,R4               SAVE POSITION IN TABLE                       
VKPARTLP CLI   0(R4),X'FF'         NO MORE PARTS OF THE KEY?                    
         BNE   VKPART10            THERE`S MORE                                 
         LR    R4,R3               DONE, NOW VALIDATE COMPONENTS                
*                                                                               
VKCLR    DS    0H                                                               
         XC    8(L'CONP0,R2),8(R2) CLEAR FIELD                                  
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R2)            NO, BUMP TO INPUT FIELD                      
         AR    R2,R1                                                            
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    6(R2),X'80'         TRANSMIT                                     
         XC    8(L'CONP0,R2),8(R2) CLEAR FIELD                                  
         LA    R1,CONPFH           ARE WE AT THE END YET?                       
         CR    R2,R1                                                            
         BNL   VK00                YES,GO VALIDATE INPUT                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               BUMP TO NEXT PROTECTED FIELD                 
         B     VKCLR                                                            
*                                                                               
VKPART10 OI    6(R2),X'80'         TRANSMIT                                     
         OI    1(R2),X'08'         SET TO HIGH INTENSITY                        
         MVC   8(8,R2),0(R4)                                                    
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO THE INPUT FIELD                         
         NI    1(R2),X'FF'-X'20'   UNPROTECT FIELD                              
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO NEXT PROTECTED FIELD                    
         LA    R4,LTAB(R4)         NEXT COMPONENT IN KEY                        
         B     VKPARTLP                                                         
         EJECT                                                                  
***********************************************************************         
* CODE TO EXPEDITE THE NEW TABLES FOR RECORD ENTRIES                  *         
***********************************************************************         
VK00     LA    R2,CONI0H           LOAD A(FIRST INPUT FIELD)                    
         XC    INTKEY,INTKEY       CLEAR KEY FIRST                              
         XC    CXFLAG,CXFLAG                                                    
         MVC   INTKEY(9),=C'K,(   )00'                                          
*                                                                               
VK02     DS    0H                  BYTE AFTER X'FF' IS RECORD CODE              
*                                                                               
         CLI   0(R4),X'FF'         LAST FIELD DONE?                             
         BNE   VK05                YES - X'FF' FOUND - FINISH KEY               
         CLI   1(R4),X'81'         DONT INSERT RECORD CODES                     
         BE    VK50                FOR THESE                                    
         CLI   1(R4),X'85'                                                      
         BE    VK50                                                             
         GOTO1 HEXOUT,DMCB,1(R4),INTKEY+7,1                                     
         B     VK50                INSERT RECORD CODE                           
*                                                                               
VK05     DS    0H                                                               
*                                                                               
         MVC   LENGTH,5(R2)        GET LENGTH OF FIELD                          
         CLC   LENGTH,8(R4)        INPUT LEN LESS THAN MIN LEN?                 
         BL    INVLFLD             YES, INVALID ENTRY                           
         CLC   LENGTH,9(R4)        INPUT LEN GREATER THAN MAX LEN?              
         BH    INVLFLD             YES, INVALID ENTRY                           
         MVC   PADNUM,9(R4)        GET DEFAULT NUMBER OF CHARS.                 
*                                                                               
         L     RF,10(R4)           GET A(BUILD ROUTINE)                         
         L     R1,RELO             ADDRESS IS RELATIVE                          
         AR    RF,R1                  TO RELOCATION FACTOR                      
         BASR  RE,RF               GET FIELD, INSERT INTO KEY                   
         LA    R4,LTAB(R4)         NEXT KEY COMPONENT                           
         LA    R2,DENTRY(R2)       BUMP TO NEXT SCREEN HEADER                   
         B     VK02                                                             
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
*                                                                               
VK50     DS    0H                                                               
*                                                                               
         MVC   XKEY,INTKEY         SET TRANSFER KEY                             
         MVC   DISKADDR(L'DISKA+L'INTKEY),DISKA                                 
         USING CONHEADH-64,RA                                                   
         B     EXIT04                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH AGENCY                                               *         
***********************************************************************         
VHEAD1   NTR1                                                                   
         MVC   INTKEY+3(2),8(R2)   MOVE AGENCY                                  
         B     EXIT04                                                           
***********************************************************************         
* BUILD KEY WITH MEDIA                                                *         
***********************************************************************         
VHEAD2   NTR1                                                                   
         MVC   INTKEY+5(1),8(R2)   MOVE MEDIA                                   
         MVI   CXFLAG,X'FF'        NEXT IS RECORD CODE                          
         MVI   INSERT,X'09'        RECORD CODE IS ALWAYS A HEX STRING           
         B     EXIT04                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH CHARACTERS                                           *         
***********************************************************************         
VCHAR    NTR1                                                                   
         LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   CXFLAG,X'FF'        HEX STRING SO FAR?                           
         BNE   VCHAR05                                                          
         MVI   0(R3),C'('          YES, INSERT CHAR. HEADER                     
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
VCHAR05  DS    0H                                                               
         ZIC   R4,PADNUM                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES      DEFAULT TO SPACES                            
         ZIC   R4,PADNUM                                                        
         LA    R3,0(R3,R4)                                                      
         MVI   0(R3),C')'                                                       
         SR    R3,R4               POINT R3 TO START OF FIELD                   
         ZIC   R5,INSERT                                                        
         LA    R5,0(R4,R5)         GET NEXT INSERT POSITION                     
         STC   R5,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT04              STOP HERE                                    
*                                                                               
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       MOVE FIELD TO KEY                            
         B     EXIT04                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH CHARACTERS WITH RIGHT JUSTIFICATION                  *         
***********************************************************************         
VRCHAR   NTR1                                                                   
         LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   CXFLAG,X'FF'        HEX STRING SO FAR?                           
         BNE   VRCHAR05                                                         
         MVI   0(R3),C'('          YES, INSERT CHAR. HEADER                     
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
VRCHAR05 DS    0H                                                               
         ZIC   R4,PADNUM                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES      DEFAULT TO SPACES                            
         ZIC   R4,PADNUM                                                        
         LA    R3,0(R3,R4)                                                      
         MVI   0(R3),C')'                                                       
         ZIC   R5,INSERT                                                        
         LA    R5,0(R4,R5)         GET NEXT INSERT POSITION                     
         STC   R5,INSERT                                                        
         ZIC   R4,LENGTH                                                        
         SR    R3,R4                                                            
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT04              STOP HERE                                    
*                                                                               
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       MOVE FIELD TO KEY                            
         B     EXIT04                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH CHAR. NUMBERS                                        *         
***********************************************************************         
VNUM     NTR1                                                                   
         LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   CXFLAG,X'FF'        HEX STRING SO FAR?                           
         BNE   VNUM05                                                           
         MVI   0(R3),C'('          YES, INSERT CHAR. HEADER                     
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
VNUM05   DS    0H                                                               
         ZIC   R4,PADNUM                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                DEFAULT TO ZEROS                             
         MVC   0(0,R3),=10C'0'                                                  
         LA    R3,1(R3,R4)                                                      
         MVI   0(R3),C')'                                                       
         ZIC   R5,INSERT                                                        
         LA    R5,1(R4,R5)         GET NEXT INSERT POSITION                     
         STC   R5,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT04              STOP HERE                                    
*                                                                               
         TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH                                                        
         SR    R3,R4                                                            
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       MOVE FIELD TO KEY                            
         B     EXIT04                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BINARY                                               *         
***********************************************************************         
VEBIN    NTR1                                                                   
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    VEBIN03                                                          
         CLC   =C'ALL',8(R2)       IF 'ALL' DONT TEST FOR NUMERIC               
         BE    VEBIN03                                                          
         TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)                                                   
         CVB   R4,DUB                                                           
         C     R4,=F'65535'        BOUNDARY CHECK                               
         BH    INVLFLD                                                          
         STCM  R4,3,WORK1                                                       
*                                                                               
VEBIN03  LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   CXFLAG,X'00'        CHAR STRING SO FAR?                          
         BNE   VEBIN05                                                          
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VEBIN05  DS    0H                                                               
         CLC   =C'ALL',8(R2)                                                    
         BE    VEBIN10                                                          
         MVC   0(4,R3),=4C'0'                                                   
         ZIC   R4,INSERT                                                        
         LA    R4,4(R4)            NEXT INSERT POSITION                         
         STC   R4,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT04                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,WORK1,0(R3),2                                        
         B     EXIT04                                                           
*                                                                               
VEBIN10  MVC   0(4,R3),=4C'F'                                                   
         ZIC   R4,INSERT                                                        
         LA    R4,4(R4)            NEXT INSERT POSITION                         
         STC   R4,INSERT                                                        
         BE    EXIT04                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BINARY                                               *         
***********************************************************************         
VBIN     NTR1                                                                   
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    VBIN00                                                           
         TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH           CHECK BOUNDARY                               
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)                                                   
         CVB   R4,DUB                                                           
         CH    R4,=H'255'          BOUNDARY CHECK                               
         BH    INVLFLD                                                          
         STCM  R4,1,WORK1                                                       
*                                                                               
VBIN00   LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   CXFLAG,X'00'        CHAR STRING SO FAR?                          
         BNE   VBIN05                                                           
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VBIN05   DS    0H                                                               
         MVC   0(2,R3),=C'01'      START WITH X'01'                             
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)            GET NEXT INSERT POSITION                     
         STC   R4,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT04                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,WORK1,0(R3),1                                        
         B     EXIT04                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH                                                      *         
***********************************************************************         
VYM      NTR1                                                                   
         LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   CXFLAG,X'00'        CHAR STRING SO FAR?                          
         BNE   VYM05                                                            
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VYM05    DS    0H                                                               
         MVC   0(5,R3),=C'0000)'                                                
         ZIC   R4,INSERT                                                        
         LA    R4,4(R4)            GET NEXT INSERT POSITION                     
         STC   R4,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT04                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,(X'02',8(R2)),WORK1                                  
         CLC   DMCB(4),=F'0'                                                    
         BE    INVLFLD                                                          
         PACK  DUB(8),WORK1(2)     CONVERT YEAR                                 
         CVB   R4,DUB                                                           
         STC   R4,WORK2                                                         
         GOTO1 HEXOUT,DMCB,WORK2,0(R3),1                                        
         PACK  DUB(8),WORK1+2(2)   CONVERT MONTH                                
         CVB   R4,DUB                                                           
         STC   R4,WORK2                                                         
         GOTO1 HEXOUT,DMCB,WORK2,2(R3),1                                        
         B     EXIT04                                                           
***********************************************************************         
* BUILD KEY WITH YEAR, MONTH, DAY                                     *         
***********************************************************************         
VYMD     NTR1                                                                   
         LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   CXFLAG,X'00'        CHAR STRING SO FAR?                          
         BNE   VYMD05                                                           
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VYMD05   DS    0H                                                               
         MVC   0(6,R3),=6C'0'                                                   
         ZIC   R4,INSERT                                                        
         LA    R4,6(R4)            GET NEXT INSERT POSITION                     
         STC   R4,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT04                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,(X'00',8(R2)),WORK1                                  
         CLC   DMCB(4),=F'0'                                                    
         BE    INVLFLD                                                          
         PACK  DUB(8),WORK1(2)     CONVERT YEAR                                 
         CVB   R4,DUB                                                           
         STC   R4,WORK2                                                         
         GOTO1 HEXOUT,DMCB,WORK2,0(R3),1                                        
         PACK  DUB(8),WORK1+2(2)   CONVERT MONTH                                
         CVB   R4,DUB                                                           
         STC   R4,WORK2                                                         
         GOTO1 HEXOUT,DMCB,WORK2,2(R3),1                                        
         PACK  DUB(8),WORK1+4(2)   CONVERT DAY                                  
         CVB   R4,DUB                                                           
         STC   R4,WORK2                                                         
         GOTO1 HEXOUT,DMCB,WORK2,4(R3),1                                        
         B     EXIT04                                                           
***********************************************************************         
* BUILD KEY WITH PUBLICATION                                          *         
***********************************************************************         
VPUB     NTR1                                                                   
         MVC   PUBLEN,LENGTH                                                    
         CLI   PUBLEN,X'00'        NOTHING IN PUBLICATION                       
         BE    EXIT04              STOP                                         
         XC    WORK1,WORK1         CLEAR WORK AREA                              
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK1(0),8(R2)      MOVE PUBLICATION TO WORK1                    
         LA    R3,WORK1            A(INPUT)                                     
         ICM   R3,8,PUBLEN         L(INPUT)                                     
         LA    R4,WORK2            A(OUTPUT)                                    
         ICM   R4,8,=X'00'         6-BYTE PRINTPAK                              
         GOTO1 =V(PUBVAL),DMCB,(PUBLEN,WORK1),(X'00',WORK2),RR=RELO             
         CLI   DMCB,X'FF'                                                       
         BE    INVLFLD                                                          
         B     EXIT04                                                           
***********************************************************************         
* BUILD KEY WITH ZONE                                                 *         
***********************************************************************         
VZONE    NTR1                                                                   
         CLI   PUBLEN,X'00'        NO PUBLICATION                               
         BNE   VZONE05                                                          
         CLI   LENGTH,X'00'        INVALID IF ZONE WITH NO PUBLICATION          
         BNE   INVLFLD                                                          
         B     EXIT04                                                           
VZONE05  LA    R3,WORK1            POINT TO PUB CODE                            
         ZIC   R4,PUBLEN                                                        
         LA    R3,0(R3,R4)         BUMP TO ZONE                                 
         MVI   0(R3),C','                                                       
         LA    R4,1(R4)                                                         
         STC   R4,PUBLEN                                                        
         CLI   LENGTH,X'00'        NO ZONE                                      
         BE    EXIT04                                                           
*                                                                               
         ZIC   R4,LENGTH           R4 HAS LENGTH OF ZONE                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),8(R2)                                                    
         ZIC   R3,PUBLEN           ADD LEN OF ZONE TO PUBLEN                    
         LA    R3,1(R3,R4)                                                      
         STC   R3,PUBLEN                                                        
*                                                                               
         LA    R3,WORK1            VALIDATE PUBLICATION AND ZONE                
         ICM   R3,8,PUBLEN                                                      
         LA    R4,WORK2                                                         
         ICM   R4,8,=X'00'                                                      
         GOTO1 =V(PUBVAL),DMCB,(R3),(R4),RR=RELO                                
         CLI   DMCB,X'FF'                                                       
         BE    INVLFLD                                                          
         B     EXIT04                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH EDITION                                              *         
***********************************************************************         
VEDIT    NTR1                                                                   
         CLI   PUBLEN,X'00'        NO PUBLICATION AND ZONE                      
         BNE   VEDIT00                                                          
         CLI   LENGTH,X'00'        INVALID IF EDITION AND NO PUB,ZONE           
         BNE   INVLFLD                                                          
         B     VEDIT10                                                          
VEDIT00  LA    R3,WORK1            ADD A COMMA                                  
         ZIC   R4,PUBLEN                                                        
         LA    R3,0(R3,R4)                                                      
         MVI   0(R3),C','                                                       
         LA    R4,1(R4)            ADD 1 TO PUBLEN                              
         STC   R4,PUBLEN                                                        
         CLI   LENGTH,X'00'        NO EDTION                                    
         BNE   VEDIT05                                                          
         BCTR  R4,0                TO GET RID OF TRAILING COMMA                 
         STC   R4,PUBLEN                                                        
         BCTR  R3,0                POINT BACK 1                                 
         CLI   0(R3),C','          IF NO EDITION                                
         BNE   VEDIT08             SUBTRACT LENGTH OF PUB CODE BY 1             
         BCTR  R4,0                TO GET RID OF TRAILING COMMA                 
         STC   R4,PUBLEN                                                        
         B     VEDIT08                                                          
VEDIT05  ZIC   R4,LENGTH           R4 HAS LENGTH OF ZONE                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),8(R2)                                                    
         ZIC   R3,PUBLEN           ADD LEN OF ZONE TO PUBLEN                    
         LA    R3,1(R3,R4)                                                      
         STC   R3,PUBLEN                                                        
VEDIT08  DS    0H                                                               
         LA    R3,WORK1            A(INPUT)                                     
         ICM   R3,8,PUBLEN         L(INPUT)                                     
         LA    R4,WORK2            A(OUTPUT)                                    
         ICM   R4,8,=X'00'         6-BYTE PRINTPAK KEY                          
         GOTO1 =V(PUBVAL),DMCB,(R3),(R4),RR=RELO                                
         CLI   DMCB,X'FF'                                                       
         BE    INVLFLD                                                          
*                                                                               
VEDIT10  LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   CXFLAG,X'00'        CHAR STRING SO FAR?                          
         BNE   VEDIT20                                                          
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VEDIT20  DS    0H                                                               
         MVC   0(12,R3),=12C'0'                                                 
         ZIC   R4,INSERT                                                        
         LA    R4,12(R4)           GET NEXT INSERT POSITION                     
         STC   R4,INSERT                                                        
         CLI   PUBLEN,0            IF NO ENTRY                                  
         BE    EXIT04                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,WORK2,0(R3),6                                        
         B     EXIT04                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH PUB CODE                                             *         
***********************************************************************         
VPUBC    NTR1                                                                   
         LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   CXFLAG,X'00'        CHAR STRING SO FAR?                          
         BNE   VPUBC10                                                          
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VPUBC10  DS    0H                                                               
         MVC   0(12,R3),=12C'0'                                                 
         ZIC   R4,INSERT                                                        
         LA    R4,12(R4)           GET NEXT INSERT POSITION                     
         STC   R4,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT04                                                           
         CLC   8(3,R2),=C'ALL'     ALL DEFAULT                                  
         BNE   VPUBC20                                                          
         MVC   0(12,R3),=12C'F'                                                 
         B     EXIT04                                                           
*                                                                               
VPUBC20  LA    R5,8(R2)            A(INPUT)                                     
         ICM   R5,8,LENGTH         L(INPUT)                                     
         LA    R4,WORK2            A(OUTPUT)                                    
         ICM   R4,8,=X'00'         6-BYTE PRINTPAK KEY                          
         GOTO1 =V(PUBVAL),DMCB,(R5),(R4),RR=RELO                                
         CLI   DMCB,X'FF'                                                       
         BE    INVLFLD                                                          
         GOTO1 HEXOUT,DMCB,WORK2,0(R3),6                                        
         B     EXIT04                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH ACTIVE PRODUCT CODE                                  *         
***********************************************************************         
VACTPRD  NTR1                                                                   
         LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   LENGTH,0            IF NO ENTRY                                  
         BNE   VACT05              PAD WITH HEX ZEROS                           
         CLI   CXFLAG,X'00'        CHAR STRING SO FAR?                          
         BNE   VACT03                                                           
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VACT03   DS    0H                  IF NONE=3X'00'                               
         MVC   0(6,R3),=6C'0'                                                   
         ZIC   R4,INSERT                                                        
         LA    R4,6(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         B     EXIT04              DONE                                         
*                                                                               
VACT05   DS    0H                                                               
         CLI   CXFLAG,X'FF'        HEX STRING SO FAR?                           
         BNE   VACT10                                                           
         MVI   0(R3),C'('          YES, INSERT CHAR. HEADER                     
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
VACT10   DS    0H                                                               
         ZIC   R4,PADNUM                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES                                                   
         ZIC   R4,PADNUM           RESTORE R4                                   
         LA    R3,0(R3,R4)                                                      
         MVI   0(R3),C')'                                                       
         SR    R3,R4               POINT R3 TO START OF FIELD                   
         ZIC   R5,INSERT                                                        
         LA    R5,0(R4,R5)         GET NEXT INSERT POSITION                     
         STC   R5,INSERT                                                        
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
         B     EXIT04                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH CLIENT CODE                                          *         
***********************************************************************         
VCLT     NTR1                                                                   
         LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   CXFLAG,X'FF'        HEX STRING SO FAR?                           
         BNE   VCLT05                                                           
         MVI   0(R3),C'('          YES, INSERT CHAR. HEADER                     
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
VCLT05   DS    0H                                                               
         ZIC   R4,PADNUM                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES      DEFAULT TO SPACES                            
         ZIC   R4,PADNUM                                                        
         LA    R3,0(R3,R4)                                                      
         MVI   0(R3),C')'                                                       
         SR    R3,R4               POINT R3 TO START OF FIELD                   
         ZIC   R5,INSERT                                                        
         LA    R5,0(R4,R5)         GET NEXT INSERT POSITION                     
         STC   R5,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT04              STOP HERE                                    
         CLC   =C'ALL',8(R2)                                                    
         BNE   VCLT30                                                           
         CLI   CXFLAG,X'FF'        HEX STRING SO FAR?                           
         BE    VCLT25                                                           
         BCTR  R3,0                MOVE BACK 1 TO ERASE '('                     
         ZIC   R4,INSERT                                                        
         LA    R4,5(R4)            NEXT INSERT POSITION=6-1                     
         STC   R4,INSERT           6 FOR 6 F'S AND 1 FOR MOVING BACK            
         MVI   CXFLAG,X'FF'                                                     
VCLT25   MVC   0(6,R3),=C'FFFFFF'                                               
         B     EXIT04                                                           
*                                                                               
VCLT30   ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       MOVE FIELD TO KEY                            
         CLI   8(R2),C'*'                                                       
         BNE   EXIT04                                                           
         MVI   0(R3),X'FF'                                                      
         B     EXIT04                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH MEDIA (PUBREC)                                       *         
***********************************************************************         
VPUBR1   NTR1                                                                   
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(5),=C'K,( )'                                              
         MVC   INTKEY+3(1),8(R2)   MOVE MEDIA                                   
         MVC   INTKEY+21(2),=C'81' MOVE RECORD CODE                             
         MVI   CXFLAG,X'00'                                                     
         MVI   INSERT,X'04'                                                     
         B     EXIT04                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH MEDIA (LTLREC)                                       *         
***********************************************************************         
VPUBR2   NTR1                                                                   
         XC    INTKEY,INTKEY       CLEAR KEY                                    
         MVC   INTKEY(5),=C'K,( )'                                              
         MVC   INTKEY+3(1),8(R2)   MOVE MEDIA                                   
         MVC   INTKEY+21(2),=C'85' MOVE RECORD CODE                             
         MVI   CXFLAG,X'00'                                                     
         MVI   INSERT,X'04'                                                     
         B     EXIT04                                                           
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MYERROR  GOTO1 ERREX2                                                           
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
         EJECT                                                                  
*                                                                               
*                                CL8 - NAME OF THE RECORD TYPE                  
*                                AL4 - ADDRESS OF KEY PORTIONS                  
*                                                                               
RECTABLE DS    0CL12                                                            
         DC    C'AGENCY  ',AL4(PAGYREC)                                         
         DC    C'CLIENT  ',AL4(PCLTREC)                                         
         DC    C'DIVISION',AL4(PDIVREC)                                         
         DC    C'REGION  ',AL4(PREGREC)                                         
         DC    C'DISTRICT',AL4(PDSTREC)                                         
         DC    C'PRODUCT ',AL4(PPRDREC)                                         
         DC    C'ESTIMATE',AL4(PESTREC)                                         
         DC    C'BILLING ',AL4(PBILREC)                                         
         DC    C'BUCKET  ',AL4(PEBKREC)                                         
         DC    C'PGEST   ',AL4(PPGEREC)                                         
         DC    C'GFEST   ',AL4(PGFEREC)                                         
         DC    C'CONTRACT',AL4(PCONREC)                                         
         DC    C'REP     ',AL4(PREPREC)                                         
         DC    C'AOR     ',AL4(PAGRREC)                                         
         DC    C'JOB     ',AL4(PJOBREC)                                         
         DC    C'INS     ',AL4(PINSREC)                                         
         DC    C'OAN     ',AL4(POTHREC)                                         
         DC    C'PLIST   ',AL4(PLISREC)                                         
         DC    C'BUDGET  ',AL4(PBUDREC)                                         
         DC    C'BUY     ',AL4(PBUYREC)                                         
         DC    C'BUY2    ',AL4(PPBUREC)                                         
         DC    C'USERP   ',AL4(PUSEREC)                                         
         DC    C'COMMENT ',AL4(PCOMREC)                                         
         DC    C'NVTEXT  ',AL4(PNVTREC)                                         
         DC    C'MANUAL  ',AL4(PBI2REC)                                         
         DC    C'PUBREC  ',AL4(PPUBREC)                                         
         DC    C'LTLREC  ',AL4(PLTLREC)                                         
         DC    C'LITTLE  ',AL4(PLTLREC)                                         
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*              CL8 - NAME OF THE FIELD TO INPUT                                 
*              XL1 - MINIMUM LENGTH OF FIELD                                    
*              XL1 - MAXIMUM LENGTH OF FIELD                                    
*              AL4 - ADDRESS OF VALIDATING ROUTINE                              
*                    IF 0'S, ACCEPT ANY CHARACTERS                              
*                                                                               
*      RECORD CODE COMES AFTER THE LAST FIELD 'FF' MARKER                       
*                                                                               
*                FIELD     MIN   MAX   VAL                                      
*                NAME      LEN   LEN   ADD                                      
*              ------------------------------                                   
PAGYREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
STA      DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
STA1     DC    X'FF',X'01'                                                      
PCLTREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    X'FF',X'02'                                                      
PDIVREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'DIVISION',X'00',X'03',AL4(VNUM)                                
         DC    X'FF',X'03'                                                      
PREGREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'DIVISION',X'00',X'03',AL4(VNUM)                                
         DC    C'REGION  ',X'00',X'03',AL4(VNUM)                                
         DC    X'FF',X'04'                                                      
PDSTREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'DIVISION',X'00',X'03',AL4(VNUM)                                
         DC    C'REGION  ',X'00',X'03',AL4(VNUM)                                
         DC    C'DISTRICT',X'00',X'03',AL4(VNUM)                                
         DC    X'FF',X'05'                                                      
PPRDREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PRODUCT ',X'00',X'03',AL4(VCHAR)                               
         DC    X'FF',X'06'                                                      
PESTREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PRODUCT ',X'00',X'03',AL4(VCHAR)                               
         DC    C'ESTIMATE',X'00',X'03',AL4(VEBIN)                               
         DC    X'FF',X'07'                                                      
PBILREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PRODUCT ',X'00',X'03',AL4(VCHAR)                               
         DC    C'ESTIMATE',X'00',X'03',AL4(VEBIN)                               
         DC    C'MON SVC ',X'00',X'0F',AL4(VYM)                                 
         DC    C'BILL MON',X'00',X'0F',AL4(VYM)                                 
         DC    C'BILL NO ',X'00',X'05',AL4(VEBIN)                               
         DC    X'FF',X'08'                                                      
PEBKREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PRODUCT ',X'00',X'03',AL4(VCHAR)                               
         DC    C'ESTIMATE',X'00',X'03',AL4(VEBIN)                               
         DC    X'FF',X'09'                                                      
PPGEREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PRODUCT ',X'00',X'03',AL4(VCHAR)                               
         DC    C'ESTIMATE',X'00',X'03',AL4(VEBIN)                               
         DC    X'FF',X'0A'                                                      
PGFEREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PRODUCT ',X'00',X'03',AL4(VCHAR)                               
         DC    C'ESTIMATE',X'00',X'03',AL4(VEBIN)                               
         DC    X'FF',X'0B'                                                      
PCONREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PUBL    ',X'00',X'0A',AL4(VPUB)                                
         DC    C'ZONE    ',X'00',X'02',AL4(VZONE)                               
         DC    C'EDITION ',X'00',X'03',AL4(VEDIT)                               
         DC    C'CONTRACT',X'00',X'05',AL4(VEBIN)                               
         DC    X'FF',X'10'                                                      
PREPREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'REP     ',X'00',X'04',AL4(VNUM)                                
         DC    X'FF',X'11'                                                      
PAGRREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PRODUCT ',X'00',X'03',AL4(VCHAR)                               
         DC    C'ESTIMATE',X'00',X'05',AL4(VEBIN)                               
         DC    X'FF',X'14'                                                      
PJOBREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PRODUCT ',X'00',X'03',AL4(VCHAR)                               
         DC    C'JOB CODE',X'00',X'06',AL4(VCHAR)                               
         DC    X'FF',X'15'                                                      
PINSREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PRODUCT ',X'00',X'03',AL4(VCHAR)                               
         DC    C'JOB CODE',X'00',X'06',AL4(VCHAR)                               
         DC    C'PUB CODE',X'00',X'11',AL4(VPUBC)                               
         DC    X'FF',X'15'                                                      
POTHREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'OTHER AG',X'00',X'02',AL4(VCHAR)                               
         DC    X'FF',X'16'                                                      
PLISREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'LIST COD',X'00',X'03',AL4(VCHAR)                               
         DC    C'LINE NUM',X'00',X'03',AL4(VBIN)                                
         DC    X'FF',X'17'                                                      
PBUDREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PRODUCT ',X'00',X'03',AL4(VCHAR)                               
         DC    C'ESTIMATE',X'00',X'03',AL4(VNUM)                                
         DC    C'REGION  ',X'00',X'03',AL4(VNUM)                                
         DC    C'DISTRICT',X'00',X'03',AL4(VNUM)                                
         DC    X'FF',X'18'                                                      
PBUYREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PRODUCT ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PUB     ',X'00',X'0A',AL4(VPUB)                                
         DC    C'ZONE    ',X'00',X'02',AL4(VZONE)                               
         DC    C'EDITION ',X'00',X'03',AL4(VEDIT)                               
         DC    C'INS DATE',X'00',X'0F',AL4(VYMD)                                
         DC    C'EST NUM ',X'00',X'03',AL4(VEBIN)                               
         DC    C'ACT PRD ',X'00',X'03',AL4(VACTPRD)                             
         DC    C'LINE NUM',X'00',X'03',AL4(VBIN)                                
         DC    X'FF',X'20'                                                      
PPBUREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PUB     ',X'00',X'0A',AL4(VPUB)                                
         DC    C'ZONE    ',X'00',X'02',AL4(VZONE)                               
         DC    C'EDITION ',X'00',X'03',AL4(VEDIT)                               
         DC    C'PRODUCT ',X'00',X'03',AL4(VCHAR)                               
         DC    C'INS DATE',X'00',X'0F',AL4(VYMD)                                
         DC    C'EST NUM ',X'00',X'03',AL4(VEBIN)                               
         DC    C'ACT PRD ',X'00',X'03',AL4(VACTPRD)                             
         DC    C'LINE NUM',X'00',X'03',AL4(VBIN)                                
         DC    X'FF',X'21'                                                      
PUSEREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'TYPE REC',X'00',X'04',AL4(VCHAR)                               
         DC    X'FF',X'30'                                                      
PCOMREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'COMMENT#',X'00',X'06',AL4(VRCHAR)                              
         DC    X'FF',X'40'                                                      
PNVTREC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCLT)                                
         DC    X'FF',X'41'                                                      
PBI2REC  DC    C'AGENCY  ',X'02',X'02',AL4(VHEAD1)                              
         DC    C'MEDIA   ',X'01',X'01',AL4(VHEAD2)                              
         DC    C'CLIENT  ',X'00',X'03',AL4(VCHAR)                               
         DC    C'PRODUCT ',X'00',X'03',AL4(VCHAR)                               
         DC    C'ESTIMATE',X'00',X'03',AL4(VEBIN)                               
         DC    C'MON SVC ',X'00',X'0F',AL4(VYM)                                 
         DC    C'BILL MON',X'00',X'0F',AL4(VYM)                                 
         DC    C'BILL NO ',X'00',X'05',AL4(VEBIN)                               
         DC    X'FF',X'88'                                                      
PPUBREC  DC    C'MEDIA   ',X'01',X'01',AL4(VPUBR1)                              
         DC    C'PUB CODE',X'00',X'11',AL4(VPUBC)                               
         DC    C'AGENCY  ',X'00',X'02',AL4(VCHAR)                               
         DC    X'FF',X'81'                                                      
PLTLREC  DC    C'MEDIA   ',X'01',X'01',AL4(VPUBR2)                              
         DC    C'PUB CODE',X'00',X'11',AL4(VPUBC)                               
         DC    C'AGENCY  ',X'00',X'02',AL4(VCHAR)                               
         DC    X'FF',X'85'                                                      
*                                                                               
DCODE    EQU   CONP1H-CONP0H                                                    
DENTRY   EQU   CONI1H-CONI0H                                                    
LTAB     EQU   STA1-STA                                                         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALANG                                                         
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         PRINT ON                                                               
       ++INCLUDE GEKEYFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
       ++INCLUDE GEPFMSAVE         (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE GEKEYWORKD        (SYSTEM AREAS)                               
         PRINT ON                                                               
* MY STORAGE AREA                                                               
         ORG   SYSSPARE                                                         
DISKA    DS    XL4                                                              
INTKEY   DS    CL60                FLAG FOR CONDITION RECOUND FOUND             
SAVER3   DS    F                   SAVE CONTENT OF REGISTER 3                   
SAVER4   DS    F                   SAVE CONTENT OF REGISTER 4                   
INSERT   DS    X                   NEXT AVAILABLE INTKEY ENTRY                  
LENGTH   DS    X                   LENGTH OF FIELD                              
CXFLAG   DS    X                   X'00' IF CHAR, X'FF' IF HEX                  
PUBLEN   DS    X                   LEN OF PUB CODE STRING IN WORK1              
WORK1    DS    CL25                WORK AREA                                    
WORK2    DS    CL25                WORK AREA                                    
PADNUM   DS    X                   NUMBER OF PADDING CHARACTERS                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028GEKEY04   01/13/16'                                      
         END                                                                    
