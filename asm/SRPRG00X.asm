*          DATA SET SRPRG00X   AT LEVEL 024 AS OF 05/01/02                      
*PHASE T14100A,+0                                                               
         TITLE '$PROG - DISPLAY FACPAK PROGRAM LIST'                            
PROG     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,**$PROG*                                               
         USING WRKD,RC             RC=A(W/S)                                    
         USING SRPARMD,R1                                                       
         L     RA,SRPARM6          RA=A(TWA)                                    
         USING SRPRGFFD,RA                                                      
         L     R9,SRPARM1          R9=A(SYSFACS)                                
         USING SYSFACD,R9                                                       
         L     R8,SRPARM4          R8=A(COMFACS)                                
         USING COMFACSD,R8                                                      
         EJECT                                                                  
* VALIDATE SYSTEM (P1)                                                          
*                                                                               
PRG0     LA    R2,SRVP1H                                                        
         CLI   5(R2),0                                                          
         BE    ERROR1                                                           
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R3,0(R1)                                                         
         L     R3,FASYSLST-FACTSD(R3)                                           
         LA    R3,6(R3)                                                         
         USING SYSLSTD,R3          R3=A(SYSTEM LIST)                            
         SR    R1,R1               SET UP FOR EXECUTED COMPARE                  
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
*                                                                               
PRG2     CLI   SYSLNUM,0                                                        
         BE    ERROR2                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),8(R2)                                                
         BE    *+12                                                             
         LA    R3,SYSLLEN(R3)                                                   
         B     PRG2                                                             
*                                                                               
         L     R5,VSELIST                                                       
         BAS   RE,SETBXLE                                                       
         USING SELISTD,R5                                                       
*                                  SEARCH SELIST FOR EQUATED SYSTEM             
         CLC   SEOVSYS,SYSLNUM                                                  
         BE    *+12                                                             
         BXLE  R5,R6,*-10                                                       
         B     ERROR2                                                           
         L     R5,SEPGMS           GET A(PGMLST)                                
         ST    R5,APGMLST                                                       
         EJECT                                                                  
* VALIDATE PROGRAM RANGE (P2,P3)                                                
*                                                                               
PRG3     CLC   SRVSRV+1(4),=C'DPRG'                                             
         BE    *+14                                                             
         CLC   SRVSRV+1(4),=C'UPRG'                                             
         BNE   PRG3X                                                            
         CLI   SRVP2H+5,0                                                       
         BE    PRG3X                                                            
         BAS   RE,SETBXLE                                                       
         USING PGMLSTD,R5                                                       
         LA    R2,SRVP2H                                                        
         ZIC   R1,SRVP2H+5                                                      
         BCTR  R1,0                                                             
PRG3A    EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRVP2(0),PGMNAME                                                 
         BE    *+12                                                             
         BXLE  R5,R6,PRG3A                                                      
         B     ERROR3                                                           
         ST    R5,APGM1                                                         
         CLI   SRVP3H+5,0                                                       
         BE    PRG3D                                                            
         LA    R2,SRVP3H                                                        
         ZIC   R1,SRVP3H+5                                                      
         BCTR  R1,0                                                             
         B     PRG3C                                                            
PRG3B    EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRVP3(0),PGMNAME                                                 
         BE    PRG3D                                                            
PRG3C    BXLE  R5,R6,PRG3B                                                      
         B     ERROR3                                                           
PRG3D    ST    R5,APGM2                                                         
         EJECT                                                                  
         L     R5,APGM1            A(1ST PROGRAM TO ALTER)                      
PRG3E    MVC   BYTE,PGMNUM         SAVE PROGRAM NUMBER                          
         LR    R4,R5               SAVE LIST POINTER                            
         L     R5,APGMLST          START AT BEGINNING AND LOOK FOR. . .         
         BAS   RE,SETBXLE          . . . ALL PROGRAMS WITH THIS NUMBER          
*                                                                               
PRG3F    CLC   PGMNUM,BYTE         IS THIS A SYNONYM?                           
         BNE   PRG3J               NO                                           
*                                                                               
         CLC   SRVSRV+1(4),=C'UPRG'                                             
         BE    PRG3H                                                            
         OI    PGMIND,PGMINOP                                                   
         MVC   MESSAGE+25(8),=C'DISABLED'                                       
         B     PRG3I                                                            
PRG3H    NI    PGMIND,255-PGMINOP                                               
         MVC   MESSAGE+25(8),=C'ENABLED '                                       
PRG3I    GOTO1 VTICTOC,DUB,C'SSET'                                              
         L     RE,VSSB                                                          
         MVC   MESSAGE+4(3),SSBSYSNA-SSBD(RE)                                   
         MVC   MESSAGE+9(L'SYSLNAME),SYSLNAME                                   
         MVC   MESSAGE+17(L'PGMNAME),PGMNAME                                    
         GOTO1 VDMOD000,DMCB,VWCTYPE,MESSAGE,LMESSAGE,C'LVL1'                   
         GOTO1 VTICTOC,DUB,C'RSET'                                              
*                                                                               
PRG3J    BXLE  R5,R6,PRG3F         LOOK FOR MORE SYNONYMS                       
*                                                                               
         LR    R5,R4               NOW LOOK FOR NEXT PROGRAM                    
         AR    R5,R6                                                            
         C     R5,APGM2                                                         
         BNH   PRG3E                                                            
         DROP  R3                                                               
         EJECT                                                                  
* DISPLAY PROGRAM INDICATORS                                                    
*                                                                               
PRG3X    L     R5,APGMLST          GET A(PGMLST)                                
         BAS   RE,SETBXLE                                                       
         USING PGMLSTD,R5                                                       
         XC    COUNT,COUNT         SCREEN FIELD COUNT                           
*                                                                               
         LA    R2,SRVP4H           STARTING PROGRAM NUMBER                      
         CLI   5(R2),0             ANY NUMBER GIVEN?                            
         BE    PRG4                NO                                           
         CLI   5(R2),2             MUST BE TWO CHARACTER PROGRAM NUMBER         
         BNE   ERROR4              IT ISN'T                                     
         GOTO1 CHEXIN,DMCB,SRVP4,BYTE,2                                         
         OC    DMCB+12(4),DMCB+12  WAS INPUT VALID HEX?                         
         BZ    ERROR4              NO                                           
*                                                                               
         CLC   PGMNUM,BYTE         DO WE HAVE A MATCH ON PROGRAM NUM?           
         BE    PRG4                YES                                          
         BXLE  R5,R6,*-10          NO -- TRY AGAIN                              
         B     ERROR5              PROGRAM NUMBER DOES NOT EXIST                
*                                                                               
         USING LINED,R2            FORMAT LINE                                  
PRG4     BAS   RE,GETADDR                                                       
         BNZ   PRG5                                                             
         MVC   SRVP4(2),LINNUM                                                  
         OI    SRVP5H+6,X'40'                                                   
         B     PRGX                                                             
PRG5     LR    R2,R1                                                            
         MVC   LINNAM,PGMNAME                                                   
         GOTO1 CHEXOUT,DMCB,PGMNUM,LINNUM,1,=C'TOG'                             
         MVC   SRVP4(2),=C'  '                                                  
         OI    SRVP5H+6,X'40'                                                   
         LA    R3,LININD                                                        
         LA    RE,INDTAB                                                        
PRG6     CLI   0(RE),0                                                          
         BE    PRG8                                                             
         ZIC   R1,0(RE)            R1=TM EXECUTE MASK (PGMIND)                  
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    PGMIND,0                                                         
         BZ    *+14                                                             
         MVC   0(1,R3),1(RE)       BIT ON OUTPUT VALUE                          
         B     *+10                                                             
         MVC   0(1,R3),2(RE)       BIT OFF OUTPUT VALUE                         
         LA    RE,L'INDTAB(RE)                                                  
         LA    R3,1(R3)                                                         
         B     PRG6                                                             
PRG8     BXLE  R5,R6,PRG4                                                       
*                                                                               
PRGX     LA    R2,SRVSRVH                                                       
         B     OKEXIT                                                           
         EJECT                                                                  
* GET A(NEXT TWA OUTPUT FIELD) IN R1                                            
*                                                                               
GETADDR  L     R1,COUNT                                                         
         LA    RF,1(R1)                                                         
         ST    RF,COUNT                                                         
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         D     R0,=F'16'                                                        
         CH    R1,=H'3'                                                         
         BH    GETADDRX                                                         
         MH    R0,=H'86'                                                        
         MH    R1,=H'20'                                                        
         LA    R1,SRVL1(R1)                                                     
         AR    R1,R0                                                            
         LR    RF,R1                                                            
GETADDRX LTR   R1,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
SETBXLE  LH    R6,0(R5)            SET BXLE REGS                                
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         BR    RE                                                               
*                                  HANDLE EXIT                                  
ERROR1   MVC   SRVMSG,ERR1                                                      
         B     EXIT                                                             
ERROR2   MVC   SRVMSG,ERR2                                                      
         B     EXIT                                                             
ERROR3   MVC   SRVMSG,ERR3                                                      
         B     EXIT                                                             
ERROR4   MVC   SRVMSG,ERR4                                                      
         B     EXIT                                                             
ERROR5   MVC   SRVMSG,ERR5                                                      
         B     EXIT                                                             
OKEXIT   MVC   SRVMSG,OKMESS                                                    
         B     EXIT                                                             
*                                                                               
EXIT     NI    SRVSRVH+6,X'BF'     UNSET & INSERT CURSOR                        
         OI    6(R2),X'40'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
MESSAGE  DC    C'*FACXXX* SSSSSSS PPPPPPP XXXXXXXX'                             
LMESSAGE EQU   *-MESSAGE                                                        
         SPACE 1                                                                
ERR1     DC    CL60'** ERROR ** MISSING INPUT FIELD'                            
ERR2     DC    CL60'** ERROR ** INVALID SYSTEM NAME'                            
ERR3     DC    CL60'** ERROR ** INVALID PROGRAM NAME'                           
ERR4     DC    CL60'** ERROR ** INVALID PROGRAM NUMBER'                         
ERR5     DC    CL60'** ERROR ** PROGRAM DOES NOT EXIST'                         
OKMESS   DC    CL60'DATA DISPLAYED. ENTER NEXT REQUEST'                         
DOTS     DC    8CL1'.'                                                          
         SPACE 1                                                                
INDTAB   DS    0XL3                                                             
         DC    AL1(PGMINOP),C'*.'                                               
         DC    AL1(PGMIAHW),C'H.'                                               
         DC    AL1(PGMIACC),C'R.'                                               
         DC    AL1(PGMIIOB),C'I.'                                               
         DC    AL1(PGMIAOR),C'O.'                                               
         DC    AL1(PGMIRFU),C'C.'                                               
         DC    AL1(PGMIROP),C'.U'                                               
         DC    AL1(0)                                                           
         EJECT                                                                  
* DSECT TO COVER WORKING STORAGE                                                
*                                                                               
WRKD     DSECT                                                                  
DUB      DS    D                                                                
COUNT    DS    F                                                                
DMCB     DS    6F                                                               
APGMLST  DS    A                                                                
APGM1    DS    A                                                                
APGM2    DS    A                                                                
BYTE     DS    X                                                                
WRKX     EQU   *                                                                
         SPACE 1                                                                
* DSECT TO COVER OUTPUT LINE                                                    
*                                                                               
LINED    DSECT                                                                  
LINNAM   DS    CL7                                                              
         DS    CL1                                                              
LINNUM   DS    CL2                                                              
         DS    CL1                                                              
LININD   DS    CL7                                                              
         SPACE 1                                                                
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         EJECT                                                                  
SRPRGFFD DSECT                                                                  
         DS    CL64                                                             
* SRPRGFFDX                                                                     
       ++INCLUDE SRPRGFFDX                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SRPRG00X  05/01/02'                                      
         END                                                                    
